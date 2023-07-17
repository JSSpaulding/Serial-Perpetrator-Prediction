#### Prediction of Serial Perpetrator Anchor Point
#### Perpetrator Trek Model Vignette - Michigan Coed Murders (Collins)
#### JS Spaulding 2023

#### REQUIRED PACKAGES & DIRECTORIES ====
pkgs <- c("data.table","ggmap","KernSmooth","leafem","leaflet","maptools","osrm",
          "pals","pbapply","plyr","raster","rgdal","rgeos","sp", "spatstat",
          "tidyverse","tmap")
#install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE) #loads required packages

maindir <- getwd()

#### Enter Google API Credentials =====
# for info: https://developers.google.com/maps/documentation/javascript/get-api-key
register_google("**INSERT PERSONAL CREDENTIALS HERE**")
getOption("ggmap") #see credential limitations


#### STEP 1 - Import Data =====
data <- read.csv("trek-collins-data.csv", header=TRUE, stringsAsFactors = FALSE)
residence <- data.frame(name = "619 Emmet St Residence",
                        lat = 42.2446486,
                        lon = -83.6216652)
# EMU was believed to be a focal point of hunting area
EMU <- data.frame(name = "EMU",
                  lat = 42.25068,
                  lon = -83.62408)


#### STEP 2 - Get Combinations of Incidents ====
# NOTE: Repeat STEPS 2-5 for each leave-n-out (default = 3)
leaveout <- 1 # Specify Leave-N-Out number 
#leaveout <- 2 
#leaveout <- 3 
n <- nrow(data) - leaveout
df_len <- seq(1:nrow(data))
combinations <- data.frame(combn(df_len, n))

incident_combos <- NULL
jj <- 1
for (i in 1:ncol(combinations)) {
  incident_combos[[jj]] <- slice(data, combinations[,i])
  jj <- jj+1
}


#### STEP 3 - Integrate Incident Information/Beliefs ====
# Must follow format of 'data'
beliefs <- c("EMU", "N/A", EMU$lat, EMU$lon,
             EMU$lat, EMU$lon, 25)


#### STEP 4 - Leave-N-Out Recursive Routing Function ====
# Returns prediction for each Leave-N-Out incident combination
source("trek_routing_prediction_function.R")
pred_out <- pblapply(incident_combos, routing_prediction)
pred_out <- data.frame(matrix(unlist(pred_out), nrow=length(pred_out), byrow=T),
                       stringsAsFactors=FALSE)

#### STEP 5 - Leave-N-Out Results Collation ====
lat <- pred_out$X1
lon <- pred_out$X2
points <- cbind(lon, lat)
assign(paste("lnoa", leaveout, "points", sep=""),cbind(lon, lat))
# NOTE: Return back to STEP 2, change Leave-N-Out number, repeat STEPS 2-5

#### STEP 6 - Prediction of Perpetrator Anchor Point ====
prediction <- routing_prediction(data)


#### STEP 7 - Kernel Density Estimation of Leave-N-Out Predictions ====
## ANALYSIS STARTPOINT: read in processed vignette results
load("Collins-trek-results.RData")

## Repeat Leave-1-Out and Leave-2-Out to the same magnitude of Leave-3-Out
newlnoa1pts <- data.frame(lon=rep(lnoa1points[,1],ceiling(length(lnoa3points)/length(lnoa1points))),lat=rep(lnoa1points[,2],ceiling(length(lnoa3points)/length(lnoa1points))))
newlnoa2pts <- data.frame(lon=rep(lnoa2points[,1],ceiling(length(lnoa3points)/length(lnoa2points))),lat=rep(lnoa2points[,2],ceiling(length(lnoa3points)/length(lnoa2points))))
combinedpts <- data.frame(rbind(newlnoa1pts,newlnoa2pts, lnoa3points))

bwlat <- 5 * bw.nrd0(combinedpts$lat) #calculate bandwidth (lat) for KDE function
bwlon <- 5 * bw.nrd0(combinedpts$lon) #calculate bandwidth (lon) for KDE function
kde <- bkde2D(combinedpts[,c("lon", "lat")], #order of lon and lat is important
              bandwidth=c(bwlon, bwlat), gridsize = c(100,100))


#### STEP 8 - Generate Results Map ====
## Create raster from kernel density output
KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))

## Create palette function for coloring the raster
palRaster <- colorNumeric(parula(200), domain = KernelDensityRaster@data@values)

## Interactive Leaflet map with raster
leaflet() %>% addTiles() %>% 
  addRasterImage(KernelDensityRaster, colors = palRaster, opacity = .8) %>%
  addCircleMarkers(as.numeric(data$found_lon), as.numeric(data$found_lat),
                   radius = 5, color="red", stroke = FALSE, fill = TRUE, 
                   fillOpacity = 0.8, popup=paste0(data$name, " (Abandonment Site)")) %>%
  addCircleMarkers(as.numeric(data$last_lon), as.numeric(data$last_lat), 
                   radius = 5, color="blue", stroke = FALSE, fill = TRUE, 
                   fillOpacity = 0.8, popup=paste0(data$name, " (Encounter Site)")) %>%
  addLegend(pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = "Likelihood of Residence") %>%
  addAwesomeMarkers(lng = residence$lon, lat = residence$lat, 
                    icon = awesomeIcons(icon = 'home', markerColor = 'green'), 
                    popup = "Residence") 

#### END  ####
routing_prediction <- function(data){
  #### Find Mean Center of all Pick-Up Sites =====
  l_centerx <- mean(data[["last_lat"]], na.rm=TRUE) 
  l_centery <- mean(data[["last_lon"]], na.rm=TRUE) 
  
  #### Find Mean Center of all Body Abandonment Sites =====
  a_centerx <- mean(data[["found_lat"]], na.rm=TRUE) 
  a_centery <- mean(data[["found_lon"]], na.rm=TRUE) 
  
  #### Data Prep ====
  # Bind pick-up site latitude and longitude in single string
  data_pu <- subset(data, !(is.na(last_lat) | is.na(last_lon)))
  pu_sites <- cbind(paste(data_pu$last_lat, data_pu$last_lon, sep = ", "))
  # Bind mean center pick-up site latitude and longitude in single string
  from_pu <- paste(l_centerx, l_centery, sep = ", ")
  # Bind abandonment site latitude and longitude in single string
  data_aband <- subset(data, !(is.na(found_lat) | is.na(found_lon)))
  aband_sites <- cbind(paste(data_aband$found_lat, data_aband$found_lon, sep = ", "))
  # Bind abandonment site latitude and longitude in single string
  from_aband <- paste(a_centerx, a_centery, sep = ", ")
  
  #### Calculate Distance/Time Weight (Pick-Up Centroid & Each Encounter Site) ====
  l_distout <- NULL
  l_durout <- NULL
  jj <- 1
  for (i in 1:nrow(data_pu)) {
    route <- osrmRoute(src = c("A", l_centery, l_centerx),
                       dst = c("B", data_pu[i,"last_lon"], data_pu[i,"last_lat"]),
                       sp = TRUE, overview = "full")
    l_distout[jj] <- route@data$distance
    l_durout[jj] <- route@data$duration
    jj <- jj+1
  }
  Sys.sleep(3)
  # Normalize to closest incident (distance/duration)
  l_normdist <- NULL
  l_normdur <- NULL
  jj <- 1
  for (i in l_distout) {
    l_normdist[jj] <- (1/(i/min(l_distout)))*10
    l_normdur[jj] <- (1/(i/min(l_durout)))*10
    jj <- jj+1
  }
  pu_site_wt <- cbind.data.frame(data_pu$name,l_normdist,l_normdur)
  
  #### Calculate Distance/Time Weight (Abandonment Centroid & Each Abandonment Site) ====
  a_distout <- NULL
  a_durout <- NULL
  jj <- 1
  for (i in 1:nrow(data_aband)) {
    route <- osrmRoute(src = c("A", a_centery, a_centerx),
                       dst = c("B", data_aband[i,"found_lon"], data_aband[i,"found_lat"]),
                       sp = TRUE, overview = "full")
    a_distout[jj] <- route@data$distance
    a_durout[jj] <- route@data$duration
    jj <- jj+1
  }
  Sys.sleep(3)
  # Normalize to closest incident (distance/duration)
  a_normdist <- NULL
  a_normdur <- NULL
  jj <- 1
  for (i in a_distout) {
    a_normdist[jj] <- (1/(i/min(d_distout)))*10
    a_normdur[jj] <- (1/(i/min(d_durout)))*10
    jj <- jj+1
  }
  aband_site_wt <- cbind.data.frame(data_aband$name,a_normdist,a_normdur)
  
  #### Calculate Trek Overlap/Intersections ====
  # Detailed Trek Information for Mapping/Intersections
  # for difference between route() and trek(): https://cran.r-project.org/web/packages/ggmap/readme/README.html
  routes <- cbind(paste(data$last_lat, data$last_lon, sep=", "),
                  paste(data$found_lat, data$found_lon, sep=", "))
  routes_out <- list()
  for (i in 1:nrow(routes)) {
    a <- trek(routes[i,1], routes[i,2], structure = 'route')
    coordinates(a) <- c("lon","lat")
    routes_out[[i]] <- SpatialLines(list(Lines(list(Line(coordinates(a))),"X")))
  }
  merged.lines <- do.call(rbind, routes_out)
  merged_psp <- as.psp(merged.lines)
  route_int <- selfcrossing.psp(merged_psp)
  r_int <- cbind(route_int$x,route_int$y)
  route_wt <- cbind(mean(r_int[,2]),mean(r_int[,1]))
  
  #### Calculate Distance/Time Weight (Treks) ====
  trek_distout <- NULL
  trek_durout <- NULL
  jj <- 1
  for (i in 1:nrow(data)) {
    route <- osrmRoute(src = c("A", data[i,"last_lon"], data[i,"last_lat"]),
                       dst = c("B", data[i,"found_lon"], data[i,"found_lat"]),
                       sp = TRUE, overview = "full")
    trek_distout[jj] <- route@data$distance
    trek_durout[jj] <- route@data$duration
    jj <- jj+1
  }
  # Normalize to closest incident (distance/duration)
  trek_normdist <- NULL
  trek_normdur <- NULL
  jj <- 1
  for (i in trek_distout) {
    trek_normdist[jj] <- (1/(i/min(trek_distout)))*10
    trek_normdur[jj] <- (1/(i/min(trek_durout)))*10
    jj <- jj+1
  }
  trek_wt <- cbind.data.frame(data$name,trek_normdist,trek_normdur)
  
  ##### Bind and Combine Weights =====
  wt <- NULL
  jj <- 1
  for(i in 1:nrow(data)){
    pu <- mean(as.numeric(pu_site_wt[i,2:3]))
    aband <- mean(as.numeric(aband_site_wt[i,2:3]))
    trek <- mean(as.numeric(trek_wt[i,2:3]))
    wt[i] <- mean(pu,aband,trek)
    jj <- jj+1  
  }
  data$weight <- wt
  route_int <- c("Route Intersection Wt.", "N/A", mean(r_int[,2]), mean(r_int[,1]),
                 mean(r_int[,2]), mean(r_int[,1]), 25)
  # Incorporate beliefs
  if(exists("beliefs")){
    data <- rbind(data,route_int,beliefs)
  } else{
    data <- rbind(data,route_int)
  }
  # For locations with only a found location, move location to last so it is considered
  for(i in 1:length(data$last_lat)){
    if(is.na(data$last_lat[i])==TRUE){data$last_lat[i] <- data$found_lat[i]}
    if(is.na(data$last_lon[i])==TRUE){data$last_lon[i] <- data$found_lon[i]}
  }
  
  #### Predict Residence Using Weights ====
  predictionx <- weighted.mean(as.numeric(data$last_lat), as.numeric(data$weight), na.rm = T)
  predictiony <- weighted.mean(as.numeric(data$last_lon), as.numeric(data$weight), na.rm = T)
  prediction <- cbind(predictionx, predictiony)
  return(prediction)
}


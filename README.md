# Prediction of Serial Perpetrator Residence Repository
This repository contains the scripts of a new framework for the geographic profiling problem which assesses and integrates the travel environment of road networks; beliefs and assumptions formed through the investigation process about the perpetrator; and information derived from the analysis of evidence. These approaches are demonstrated in my manuscripts cited below.

## Centrographic model
In the simplest instance, the residence of the perpetrator lies at the very centre of a distribution of the incidents they perpetrate and can be found through the spatial mean. However, this is unrealistic and more complex patterns are the norm. Crime patterns are also distorted by real world factors such as street layouts and traffic, or the nature of the target backcloth (environment). These real-world factors limit the ability of the spatial mean to determine criminal residence. However, when no other information beyond the location of the abandonment or murder site is available, a centrographic prediction may be all that is possible.
Implementations of these approaches can be found in the `rgeoprofile` package on [CRAN](https://cran.r-project.org/package=rgeoprofile), my [GitHub repo](https://github.com/JSSpaulding/rgeoprofile), and outlined in the following manuscript: 

JS Spaulding and KB Morris. "An Open-Source Implementation of Geographic Profiling Methods for Serial Crime Analysis," *Journal of Geographical Systems*, 2023. [Paper Link](https://doi.org/10.1007/s10109-023-00417-w).

## Perpetrator trek model
The perpetrator trek model is an extension of the centrographic model for instances where both encounter and abandonment/murder sites are known to the investigator. Firstly, two centroids are computed: one using the coordinates for the encounter sites and a second using the abandonment site location coordinates. Next, the recursive routing function was applied three separate times: 1) to determine the routes from the encounter centroid to each encounter site; 2) to determine the routes from the abandonment centroid to each abandonment site coordinate; 3) and to determine the routes from the encounter site to the corresponding abandonment site. Routes from the encounter and abandonment site pairs are plotted and any areas of overlap are extracted. Similar to the centrographic model, the time and distance values are normalised (zero to ten) for each set of queried routes: the encounter site routes, the abandonment site routes, and the encounter to abandonment treks. These normalized time and distance values for each incident were then averaged to calculate a combined weight for that incident. The spatial mean of the route areas which overlapped was also computed and included in the prediction.

## Evidence driven model for geographic profiling
The third developed model is for use on a case-by-case basis since it extends the previous two models to integrate the beliefs or assumptions an investigator has about the case and items of evidence. The evidence-driven model follows the same initial procedure as the centrographic and perpetrator trek models. Additionally, investigative influence factors were quantitatively assessed for the significance they have on the case and were then used in the weighted predictions. See the manuscripts for an application to the Yorkshire Ripper investigation from the view of the 1980 advisory team.

# Manuscripts
JS Spaulding and KB Morris. "Prediction of serial perpetrator residence: Part I Induction of models utilizing spatiotemporal routing functions and investigative information," *Journal of Investigative Psychology and Offender Profiling*, 2023, 20(1):3-18. [Paper Link](https://doi.org/10.1002/jip.1605).

JS Spaulding and KB Morris. "Prediction of serial perpetrator residence: Part II Evaluation of prediction model accuracy," *Journal of Investigative Psychology and Offender Profiling*, 2023, 20(1):97-118. [Paper Link](https://doi.org/10.1002/jip.1606).

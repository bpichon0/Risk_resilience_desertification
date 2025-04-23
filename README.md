# Code for running the analyses on the risk-resilience paper on drylands

> [!NOTE]
> Contact: Benoît Pichon, **benoit.pichon0@gmail.com**


This repository contains the code used to perform the analyses for both main text and supplementary information.
All the code was made on R (*v4.4.1*).
All packages needed are available in *Functions.R*. If you don't have all packages installed, please uncomment line 7 of the *Functions.R* script to install them.


<p align="center">
    <img src="https://github.com/bpichon0/Risk_resilience_desertification/blob/master/Figures/Cocit_img.jpg" width="800">
</p>

# Data

## NDVI timeseries

NDVI data have been download for each sites from the MODIS database using the *MODISTools* R-package.
The geographical localisation of the 148 sites are available in the *./data/Empirical_data.csv* dataframe. 
From these timeseries of NDVI between 2000 and 2023, we computed the mean autocorrelation at lag-1 from the detrended and deseasonalized timeseries of NDVI.

## Images of plant spatial patterns

Three images were collected using Google Earth or Virtual Earth at each of those sites. The spatial Moran I autocorrelation was computed on each binary image and incorporated to the *./data/Empirical_data.csv* dataframe.
Spatial patterns were also used to compute the distance to the desertification point following a method proposed in [this article](https://www.biorxiv.org/content/10.1101/2024.02.20.581244v2).


## Simulations

Simulations were done with the [Kéfi et al model](https://www.sciencedirect.com/science/article/pii/S0040580906001250) and are available in *./data/All_simulations.csv*.
The dataset contains the different parameter used to generate the simulations (c, d, b_ini; 900 parameter sets), the temporal and spatial indicators of resilience.

## Climate

Climate data have been downloaded from [WorldClim](https://worldclim.org/data/cmip6/cmip6_clim30s.html) and are not available in *./data/Climate* folder because of the size of the data. The code needed to extract the data on the locations of the different dryland sites is nevertheless available in the *Analyses_indicators.R* file.

## Bibliography analysis

Following the Web Of Science request available in the supplementary of the paper, we downloaded the .bib and .xls files with all information about the articles and the citations within each article. They are available in the *savedrecs.bib* and *savedrecs.xls* files in the *data* folder.
Then, we screened the articles and only kept 158 related to resilience or risk in drylands.

# Figures and analyses

They are two scripts of analyses. 
*Analyses_indicators.R* performs the analyses on the indicators and plot the corresponding figures.
*Analysis_bibliography.R* performs the analyses on the bibliography related to risk and resilience in drylands, and also plot the corresponding figures.

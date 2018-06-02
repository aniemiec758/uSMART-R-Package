# uSMART-R-Package
Between the winter of 2017 and spring of 2018, I was part of a Purdue University research group named uSMART; during my time there,
I analyzed taxi patterns in New York City and created this package in R. It allows users to input a GIS shapefile, a data table of car
trips, and receive information about each individual street taken for every car trip from origin to destination.

A more proper "ReadMe" can be located at the top of the "uSMART master script," which was intended to run as-is on Purdue University's RICE computing cluster - this .R file does not incorporate the package's functions, but is the precursor to it. This may be helpful if you are working on a similar R or GIS project dealing with spatial analysis and are looking for how to manage imported ESRI shapefiles in RStudio.
"R package testing" walks through how a user might interface with the package's functions; the top section declares all methods so that a user may inspect all functions at once, the middle section creates some mock data, and the bottom section shows an example of how one might call each package function (each section is delimited by # comments)
Finally, the "edg2shp" folder contains all R source code for the functions.

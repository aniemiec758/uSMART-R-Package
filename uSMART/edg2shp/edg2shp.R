# create your own function to manage the shapefile data, called "func," and the ... implies passing in
#	whatever columns of the shapefile that go into func (explained in "R package testing.R"
# an example of how to do this is seen in the "R package testing.R" script

# install and import the library "shapefiles" (plural, not singular - I made that mistake once)

edg2shp <- function(inputDir, origName, outputDir, newName, func, ...) { 
	setwd(inputDir)
	newShp <- read.shapefile(origName)
	newShp$dbf[[1]]$newCol <- myFunc(...)

	setwd(outputDir)
	write.shapefile(newShp, newName, arcgis=T)
}
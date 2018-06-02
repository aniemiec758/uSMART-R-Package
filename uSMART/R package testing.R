library(rgdal)
library(shp2graph)
library(igraph)
library(RANN)
library(dplyr)
library(shapefiles)

### All functions, to test individually:

MAPraw <- function(dir, name) {
	setwd(dir)
	MAPshp <<- readOGR(dsn=".", layer=name)
	return(readshpnw(MAPshp, ELComputed=T))
}

getAttributeTable <- function(mraw) {
	return (mraw[[5]])
}

MAPgraph <- function(mraw, weights=mraw[[4]]) {
	return(nel2igraph(mraw[[2]], mraw[[3]], weight=weights))
}

getPointMatrices <- function(mraw, pLat, pLong, dLat, dLong) {
	startMatrix = matrix(c(pLat,pLong), nrow=length(pLat), ncol=2)
	endMatrix = matrix(c(dLat,dLong), nrow=length(dLat), ncol=2)

	MAPxCoords <- matrix(nrow=length(mraw[[2]][,1]), ncol=1) # x coord for street ("node") intersections
	MAPyCoords <- matrix(nrow=length(mraw[[2]][,1]), ncol=1) # y coord for street ("node") intersections
	CoordRaw <- mraw[[2]][,2]

	for (i in 1:(length(MAPxCoords))) { # gathering coordinates from mraw, the loaded-in shapefile
		MAPxCoords[i] <- CoordRaw[[i]][1] # x column of shapefile coordinates
		MAPyCoords[i] <- CoordRaw[[i]][2] # y column of shapefile coordinates
	}
	streetMatrix = matrix(c(MAPxCoords, MAPyCoords), nrow=length(MAPxCoords), ncol=2) # matrix of all street intersection coordinates

	return (list(startMatrix, endMatrix, streetMatrix))
}

UTMtoLatLong <- function(smatrix, p4strUTM, p4strLL) {
	tempSP <- SpatialPoints(smatrix, proj4string=CRS(p4strUTM))
	tempGEO <- spTransform(tempSP, CRS(p4strLL))
	eval.parent(substitute(smatrix <- matrix(c(tempGEO$coords.x1, tempGEO$coords.x2), nrow=length(tempGEO$coords.x1), ncol=2)))
}

approximateIntersections <- function(startmatrix, endmatrix, streetmatrix, dataset, rad=.002) {
	tripStartNodeIDs <- nn2(data=streetmatrix, query=startmatrix, k=1, treetype="bd", searchtype="radius", radius=rad, eps=0) # radius helps filter out coordinate errors/outliers during nearest-street-intersection approximation
	tripEndNodeIDs <- nn2(data=streetmatrix, query=endmatrix, k=1, treetype="bd", searchtype="radius", radius=rad, eps=0)

	tripStartNodeIDs <- c(tripStartNodeIDs$nn.idx) # appends a column of start IDs to data set; to filter out all 0/null values in start and end nodes
	tripEndNodeIDs <- c(tripEndNodeIDs$nn.idx)

	newdataset <- dataset
	newdataset$startID <- tripStartNodeIDs
	newdataset$endID <- tripEndNodeIDs
	newdataset <- dplyr::filter(newdataset, startID!=0 & endID!=0) # filtering out any values where the start/end ID have no neighbor
	tripStartNodeIDs <- newdataset$startID
	tripEndNodeIDs <- newdataset$endID

	eval.parent(substitute(dataset <- dplyr::filter(newdataset, startID!=0 & endID!=0)))
	return(list(tripStartNodeIDs, tripEndNodeIDs)) # return starting and ending IDs
}

attCatch <- function(starts, ends, mgraph, attribute) {
	catcher <- function(start, end) { # performs attribute catching for a single pair
		path <- get.shortest.paths(mgraph, from=start, to=end) # list of node IDs for path
		if (length(path$vpath[[1]]) > 1) { # viable path
			x1 <- path$vpath[[1]][1:(length(path$vpath[[1]])-1)]
			x2 <- path$vpath[[1]][2:length(path$vpath[[1]])]
			edgeIDs <- get.edge.ids(mgraph, c(rbind(x1,x2)))
			attribs[[1]] <- MAPattribs[edgeIDs]
		}
		return (attribs)
	}

	attribs <- vector(mode='list', length=1)
	MAPattribs <- as.numeric(data.matrix(unlist(attribute)))
	edgeIDs <- vector(mode='list', length=1)
	tripAttribs <- mapply(start = starts, end = ends, FUN = catcher)
	return (tripAttribs)
}

errorReport <- function(numSD=1, data, att, filtered=F, miles=T) {
	relErr <- list() # will hold the relative errors
	len <- vector(mode="double", length=length(data)) # hold len's
	for (i in 1:(length(att))) { # same size as sample of trips
		len[[i]] = sum(att[i][[1]]) # length recalculated per loop iteration
	}
	if (miles==T) {
		real = data * 1609.34
	} else {
		real = data
	}
	relErr <- abs((len-real)/real)
	m = mean(unlist(relErr))
	s = sd(unlist(relErr))
	me = median(unlist(relErr))

	cat("Error report for trip approximation:", '\n')
	cat("Mean relative trip length error, compared to DataSet: ", m*100, "%", '\n', sep="")
	cat("Standard deviation of relative errors: ", s*100, "%", '\n', sep="")
	cat("Median relative error: ", me*100, "%", '\n', sep="")

	if (filtered == T) { # full comparison report
		relErrFiltered <<- unlist(relErr[relErr < (m+s)])

		mFilt <- mean(relErrFiltered)
		sFilt <- sd(relErrFiltered)
		meFilt <- median(relErrFiltered)

		loss <- length(relErr) - length(relErrFiltered)
		lossPercent <- loss/length(relErr) * 100

		cat("---", '\n')
		cat("Error report for data when filtered to ", numSD, " sd:", '\n', sep="")
		cat("Mean relative trip length error: ", mFilt*100, "%", '\n', sep="")
		cat("Standard deviation: ", sFilt*100, "%", '\n', sep="")
		cat("Median: ", mFilt*100, "%", '\n', sep="")
		cat("Number of trips filtered out:", loss, "of", length(data), '\n')
		cat("% of trips filtered out: ", lossPercent, "%", '\n', sep="")
	}
}

edg2shp <- function(inputDir, origName, outputDir, newName, func, ...) { 
	setwd(inputDir)
	newShp <- read.shapefile(origName)
	newShp$dbf[[1]]$newCol <- myFunc(...)

	setwd(outputDir)
	write.shapefile(newShp, newName, arcgis=T)
}

### Making fake data to test above functions: (remove later)

DataYo <- c(40.764203, 40.764542, 12, 40.765798, 40.761005) # lat
DataXo <- c(-73.980790, -73.977256, 20, -73.983240, -73.979412) # long
DataYd <- c(40.763421, 40.762213, 23, 40.764890, 40.763131) # lat
DataXd <- c(-73.981326, -73.982270, 15, -73.976631, -73.974015) # long

trip_distance <- c(3,2,4,8,3) # fake lengths

dataSet = data.frame(a=unlist(DataXo), b=unlist(DataYo), c=unlist(DataXd), d=unlist(DataYd), trip_distance=unlist(trip_distance))

### Actual usable script for next guy: (needs some tweaks due to above section)

# Step 1: *** will have to change directory name ***
mraw <- MAPraw("C:/Users/Niemiec/Desktop/TSquare", "TSquare")
shpTable <- getAttributeTable(mraw) # full attribute table from shapefile

# Step 2: get igraph obj
costMatrix <- mraw[[5]]$FromToCost # this is how edge weights are handled
n = which(costMatrix==-1)
costMatrix[n] <- mraw[[5]]$ToFromCost[n]
mgraph <- MAPgraph(mraw, costMatrix)

# you now have all raw shapefile data in mraw, as well as a plottable
#	igraph object in mgraph; again, shpTable is the shapefile datatable

# Step 3: get some coords
matrices <- getPointMatrices(mraw, DataXo, DataYo, DataXd, DataYd)
startCoords <- matrices[[1]] # all starting coord pairs in dataSet
endCoords <- matrices[[2]] # all ending coord pairs in dataSet
streetIntersecCoords <- matrices[[3]] # all street intersections in shapefile

# Step 4: refine from UTM to lat/long (optional if shapefile isn't UTM)
UTMtoLatLong(smatrix=streetIntersecCoords, p4strUTM="+proj=utm +zone=18 +datum=WGS84", p4strLL="+proj=longlat +datum=WGS84")

# Step 5: finding nearest intersections to points we got from Step 3
NodeIDs <- approximateIntersections(startCoords, endCoords, streetIntersecCoords, dataSet)
startIDs <- NodeIDs[[1]]
endIDs <- NodeIDs[[2]]

# Step 6: harvesting trip attributes by approximating Taxi routes
speeds <- attCatch(startIDs, endIDs, mgraph, mraw[[5]]$SPEED)
lengths <- attCatch(startIDs, endIDs, mgraph, mraw[[5]]$Shape_Leng)

# Step 7: generating an error report
errorReport(data=dataSet$trip_distance, att=lengths, filtered=T, miles=T)

# Step 8/9: getting a filtered copy of the data, outputting the data
#	(end of part 1)

# this step is very customizable, user should refer to the example
#	in the master_script.R file

# - interlude for Time Series analysis

# Step 10: creating a new shapefile (part 2, after time series analysis)
myFunc <- function(spd, len) { # simple CUSTOM function, change input args
	return (len/spd) # *** THE TIME SERIES ANALYSIS EQ. WOULD GO HERE ***
}

edg2shp(inputDir="C:/Users/Niemiec/Desktop/TSquare/", origName="TSquare",
	outputDir="C:/Users/Niemiec/Desktop/newshp/", newName="TSquare_new",
	func=myFunc, shpTable$Shape_Leng, as.numeric(as.vector(shpTable$SPEED)))











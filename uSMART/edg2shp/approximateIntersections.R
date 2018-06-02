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
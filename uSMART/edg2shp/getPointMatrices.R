getPointMatrices <- function(mraw, pLat, pLong, dLat, dLong) {
	startMatrix = matrix(c(pLat,pLong), nrow=length(pLat), ncol=2)
	endMatrix = matric(c(dLat,dLong), nrow=length(dLat), ncol=2)

	MAPxCoords <- matrix(nrow=length(mraw[[2]][,1]), ncol=1) # x coord for street ("node") intersections
	MAPyCoords <- matrix(nrow=length(mraw[[2]][,1]), ncol=1) # y coord for street ("node") intersections
	CoordRaw <- mraw[[2]][,2]

	for (i in 1:(length(MAPxCoords))) { # gathering coordinates from mraw, the loaded-in shapefile
		MAPxCoords[i] <- CoordRaw[[i]][1] # x column of shapefile coordinates
		MAPyCoords[i] <- CoordRaw[[i]][2] # y column of shapefile coordinates
	}
	streetMatrix = matrix(c(MAPxCoords, MAPyCoords), nrow=length(MAPxCoords), ncol=2) # matrix of all street intersection coordinates

	return (c(startMatrix, endMatrix, streetMatrix))
}
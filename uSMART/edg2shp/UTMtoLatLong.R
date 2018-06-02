# p4strUTM includes the zone (i.e. "+proj=utm +zone=18 +datum=WGS84" for NYC), p4strLL excludes this (i.e. normal "+proj=longlat +datum=WGS84")

UTMtoLatLong <- function(smatrix, p4strUTM, p4strLL) {
	tempSP <- SpatialPoints(smatrix, proj4string=CRS(p4strUTM))
	tempGEO <- spTransform(tempSP, CRS(p4strLL))
	eval.parent(substitute(smatrix <- matrix(c(tempGEO$coords.x1, tempGEO$coords.x2), nrow=length(tempGEO$coords.x1), ncol=2)))
}
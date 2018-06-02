# "mraw" is whatever MAPraw() returns, weights is what each edge will be weighted (i.e. the ToFromCost/FromToCost col of NYC shapefile)

MAPgraph <- function(mraw, weights=mraw[[4]]) {
	return(nel2igraph(mraw[[2]], mraw[[3]], weight=weights))
}
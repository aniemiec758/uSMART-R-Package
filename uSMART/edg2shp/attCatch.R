# take in a list of starting and ending nodes inside of the network, and then a specific attribute to look for
#	mgraph is the actual igraph object, created FROM the shapefile via shp2graph()
#	attribute is the column of MAPraw, i.e. MAPraw[[5]]$SPEED is passed in
attCatch <- function(starts, ends, mgraph, attribute) {
	catcher <- function(start, end) { # performs attribute catching for a single pair
		path <- get.shortest.paths(mgraph, from=start, to=end) # list of node IDs for path
		if (length(path$vpath[[1]]) > 1) { # viable path
			x1 <- path$vpath[[1]][1:(length(path$vpath[[1]])-1)]
			x2 <- path$vpath[[1]][2:length(path$vpath[[1]])]
			edgeIDs <- get.edge.ids(MAPgraph, c(rbind(x1,x2)))
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
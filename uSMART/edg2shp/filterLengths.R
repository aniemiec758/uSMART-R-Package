# the "miles" T/F parameter is TRUE when the data set must be converted out of miles into meters
# it is assumed that the shapefile has street lengths in meters
# "data" is the data set column for trip lengths; "att" is the column of attCatch for what get.shortest.paths picked up on,
#	"attTable" is the full table of attributes, "dataset" is the full table of non-shapefile trip data

filterLengths <- function(numSD=1, data, att, attTable, dataset, miles=F) {
	len <- vector(mode="double", length=length(data)) # hold len's
	for (i in 1:(length(attTable)/2)) { # same size as sample of trips
		len[[i]] = sum(att) # length recalculated per loop iteration
	}

	if (miles==T) { # must filter out of miles into meters
		real = data * 1609.34
	}
	relErr <- abs((len-real)/real)

	m = mean(unlist(relErr))
	s = sd(unlist(relErr))
	keepIndices <- which(relErr < (m+s)) # keep where relErr is within the specified SD range

########
	eval.parent(substitute(attTable <- unlist(attTable[keepIndices]))) # substitute attCatch by reference
########
	eval.parent(substitute(dataset <- dataset[keepIndices]))
}
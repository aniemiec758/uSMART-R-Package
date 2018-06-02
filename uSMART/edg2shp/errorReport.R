# when "filtered" is TRUE, gives full comparison report with a filtered data set
# when "miles" is TRUE, must convert dataset trip lengths from miles to meters (assumes shapefile has lengths stored in meters)
# "data" is the streetLength column of the dataset, "att" is the streetLength column of the attCatch

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
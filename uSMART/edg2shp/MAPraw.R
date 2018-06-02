# "dir" is the directory/name of folder the shapefile stuff is in, "name" is the name of it (i.e. shp for shp.dbx)

MAPraw <- function(dir, name) {
	setwd(dir)
	MAPshp <<- readOGR(dsn=".", layer=name)
	return(readshpnw(MAPshp, ELComputed=T))
}
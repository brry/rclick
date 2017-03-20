### Kapitel 16 - Geostatistik ##################################################

# 16.0 im Kapitel benoetigte Pakete
# 16.1 ESRI-Shapefiles laden und bearbeiten
# 16.2 Kriging (also die eigentliche Geostatistik)
# 16.3 Karten im Hintergrund


# 16.0 im Kapitel benoetigte Pakete ---------------------------------------------

# Runterladen der Pakete nur einmal, danach auskommentieren
install.packages("geoR") 
install.packages("rgeos") 
install.packages("maptools") 
install.packages("tmap") 
install.packages("berryFunctions")
install.packages("rgl") 

# Laden der Pakete nach jedem R-Start (oder in Rprofile, siehe Kap 10)
library(maptools) # fuer readShapeSpatial
library(geoR)     # as.geodata, variog, variofit, krige.control, krige.conv,
library(sp)       # geoR bereits installiert                    legend.krige
library(rgeos)    # gCentroid, gArea, gUnion    
library(tmap)     # qtm        
library("berryFunctions") # colPoints, classify
help(package="geoR") # und
library(help="geoR")


data(Europe)
data(World)
# Karten mit richtiger Projektion:
qtm(World, fill = "economy", text="iso_a3", text.size = "AREA", 
    fill.palette="-Blues", theme = "World", fill.title="Economy")
qtm(Europe, fill="gdp_cap_est", text="iso_a3", text.size="pop_est", 
    fill.title="GDP per capita", fill.textNA="Non-European countries")
class(Europe) # SpatialPolygonsDataFrame  entspricht ESRI shapefile


# 16.1 ESRI-Shapefiles laden und bearbeiten ------------------------------------

browseURL("srtm.csi.cgiar.org") # globales DGM
browseURL("http://diva-gis.org/Data") # Grenzen aller Staaten
browseURL("http://spatialreference.org/") # proj4strings
browseURL("https://www.nceas.ucsb.edu/scicomp/usecases/ReadWriteESRIShapeFiles")

# IndiaDaten fuer das Beispiel unten gibt's hier:
# http://globalresourcesuae.com/www.maptell.com/index3e60.html?Itemid=159

india <- maptools::readShapeSpatial("Daten/Shapefiles/india_state.shp")
# der "Fehler in getinfo.shape(fn) : Error opening SHP file"
# bedeutet oft, dass das Arbeitsverzeichnis nicht richtig gesetzt ist -> getwd()
# oder der Dateiname falsch geschrieben ist. -> dir()
class(india) # SpatialPolygonsDataFrame   
plot(india)

# alternativ, projektionsdatei automatisch mit eingelesen:
india_rgdal <- rgdal::readOGR('Daten/Shapefiles/india_state.shp', 'india_state')
india_rgdal@proj4string
plot(india_rgdal)

# ESRI Shapefile in raster umwandeln:
browseURL("https://github.com/brry/misc/blob/master/shp2raster.R")
source("http://raw.githubusercontent.com/brry/misc/master/shp2raster.R")
india_ras <- shp2raster("Daten/Shapefiles/india_state.shp", cell=0.5, column="ID")
plot(india_ras)


# Some states are made up of more than one polygon.
# State names are in the NAME column of the dataframe
View(india)
head(india@data)
str(india[1,])
plot(india, col = india$NAME)
plot(india[india$NAME == "Tamil Nadu",], col = 1:8)

attr(methods(plot),"info") # sp has a plot method
?"SpatialPolygons-class"
methods(class="SpatialPolygonsDataFrame")

plot(india, col=seqPal(169)); axis(1);axis(2,las=1);
colPointsLegend(1:169, nbin=169, col=seqPal(169))
plot(india[72,]); axis(1);axis(2,las=1)
head(india[72,]@polygons[[1]]@Polygons[[1]]@coords)
tail(india[72,]@polygons[[1]]@Polygons[[1]]@coords)



# Extract centroid for a State (this works):
gCentroid(india[india$NAME == "Tamil Nadu",])
# Error arises for this State:
gCentroid(india[india$NAME == "Gujarat",])
# orphaned hole, cannot find containing polygon for hole at index 2
plot(india[india$NAME == "Gujarat",], col = 1:8)



# 16.2 Kriging (also die eigentliche Geostatistik) -----------------------------

# 16.2.1 Daten Beispiel --------------------------------------------------------
x <- c(1,1,2,2,3,3,3,4,4,5,6,6,6)
y <- c(4,7,3,6,2,4,6,2,6,5,1,5,7)
z <- c(5,9,2,6,3,5,9,4,8,8,3,6,7)
plot(x,y, pch="+", cex=z/4)

library(berryFunctions)
colPoints(x,y,z, col=addAlpha(rainbow2(50)), cex=3)

library(geoR)
geodata <- as.geodata( cbind(x,y,z)  )
plot(geodata)


# 16.2.2 Variogramm ------------------------------------------------------------
vario_emp <- variog(geodata, max.dist=40)
vario_fit <- variofit(vario_emp) 
plot(vario_emp); lines(vario_fit)


# 16.2.3 Kriging (Interpolation unter Beruecksichtigung der Semivarianz) -------
ngrid <- 100
grid <- expand.grid(seq(min(geodata$coords[,1]), max(geodata$coords[,1]), len=ngrid),
                    seq(min(geodata$coords[,2]), max(geodata$coords[,2]), len=ngrid))
kri_control <- krige.control(type.krige="OK", obj.model=vario_fit)
kri_object  <- krige.conv(geodata, loc=grid, krige=kri_control)

# 16.2.4 Farbliche Darstellung -------------------------------------------------
colors <- berryFunctions::seqPal(100)
image(kri_object, col=colors)
legpos <- function(i, x1,x2) {p<-par("usr")[i]; p[1]+c(x1,x2)*diff(p)}
legend.krige(values=kri_object$predict, x.leg=legpos(1:2, 0.90,0.99), 
                    col=colors, vert=T, y.leg=legpos(3:4, 0.10,0.80), off=-0.5)
#contour(kri_object, add=TRUE)    
berryFunctions::colPoints(x,y,z, col=colors) 
#points(x,y)



# 16.2.5 3D Darstellung --------------------------------------------------------
colors <- seqPal(100)[classify(kri_object$predict)$index]
library(rgl)
open3d()   # Fenster in Sichtweite ziehen : Maus ziehen und scrollen
plot3d(grid[,1], grid[,2], kri_object$predict, type = "p",
       xlab = "X", ylab = "Y", zlab = "Z", col=colors)
plot3d(grid[,1], grid[,2], kri_object$predict, type = "s", size=1,
       xlab = "X", ylab = "Y", zlab = "Z", col=colors)
# Letzteres mit allen Schatten zu rendern ist ein Heidenaufwand, hab also Geduld


# 16.2.6 3D Mesh (durchgaengige Oberflaeche) -------------------------------------
# aus ?mesh3d:
vertices <- c(    # jeweils x,y,z,winkel; siehe ?rgl::matrices
     -1, -1, 0, 1,
      1, -1, 3, 1,
      1,  1, 0, 1,
     -1,  1, 0, 1  )
indices <- c( 1, 2, 3, 4)
open3d()  
shade3d( qmesh3d(vertices,indices), col=2:5 )
open3d() 
wire3d( qmesh3d(vertices,indices) , col=2:5, lwd=3)

# jetzt fuer Beispieldaten:
dummy <- data.frame(x=grid[,1], y=grid[,2], z=kri_object$predict, w=1)
vertices <- unlist(t(as.matrix(dummy)))  ; rm(dummy)
round(vertices[1:30],2)

open3d()
shade3d( qmesh3d(vertices, rep(1:4, length(vertices)/4)) )
                                              # nimmt nur die ersten 4 Punkte...

# 16.2.7 3D perspective plot ---------------------------------------------------
krmat <-  matrix(kri_object$predict,                 nrow=ngrid)
colors <- matrix(seqPal(100)[classify(krmat)$index], nrow=ngrid)
persp(1:ngrid, 1:ngrid, krmat, col=colors)             # farben stimmen nicht

# mehr zu 3D-Darstelluungen in Kapitel 4.14



# 16.3 Karten im Hintergrund ---------------------------------------------------
browseURL("https://github.com/brry/OSMscale#intro") # install JAVA
if(!requireNamespace("OSMscale")) install.packages("OSMscale")
library("OSMscale")

d <- read.table(header=TRUE, text=
"lon lat elevation
13.01480 52.39975 66.93333
13.01233 52.40032 67.00000
13.00975 52.40110 64.76667")

map <- pointsMap(lat, lon, data=d, type="osm")
lines(projectPoints(d$lat,d$lon, to=posm()) )

map_aer <- pointsMap(lat, lon, data=d, type="bing")

# I find type="maptoolkit-topo" to work very fine as well. For all types, see
browseURL("http://www.r-bloggers.com/the-openstreetmap-package-opens-up")

# Project map to UTM Coordinates
map_utm <- OpenStreetMap::openproj(map, putm(d$lon))
plot(map_utm)
axis(1, line=-2); axis(2, line=-2)


# Reading GPX files, eg from apps like sportractive or OSMtracker:
#install.packages("plotKML")
d <- plotKML::readGPX("Filename.gpx")
options(warn=0)
d <- d$tracks[[1]][[1]]
d$ele <- as.numeric(d$ele)
#d$time <- as.POSIXct(strptime(d$time, format="%Y-%m-%dT%H:%M:%S."))

library(berryFunctions)
colPoints(lon, lat, ele, data=d, add=FALSE, asp=1, lines=TRUE, pch=NA, lwd=3)
d2 <- equidistPoints(lon, lat, ele, data=d, n=25)
points(d2$x, d2$y, pch=3, lwd=3) # 25 segments equally along track

browseURL("http://www.r-bloggers.com/stay-on-track-plotting-gps-tracks-with-r")

# Reading KML files, eg from GoogleMaps:
##install.packages("maptools")
d <- maptools::getKMLcoordinates("Filename.kml",ignoreAltitude=TRUE) 
d <- as.data.frame(do.call(rbind, d[which(sapply(d, class)=="matrix")]))
colnames(d) <- c("lon","lat") 
head(d)


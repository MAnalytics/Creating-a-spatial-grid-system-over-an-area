# Creating-a-Spatial-Grid-System over a study area
# Given a boundary shapefile, this script creates a spatial grid system of a specified unit size over the entire area

#Install packages
install.packages("sp")
install.packages("rgdal")
library(sp)
library(rgdal)

#-----------------------------------
# Inputs parameters:
#-----------------------------------
#1. Boundary shapefile(.shp) in any CRS
area_B <- readOGR(dsn=".", layer="boundary_SS_wgs_84")

#2.Specify the CRS of the area by defining the "PROJ.4 attributes" (Visit:http://spatialreference.org/)
#comments: ensure that the projected CRS chosen is in unit of 'metres' (and not 'feet'). 
#Example: For South Chicago, the "PROJ.4 attributes" is: "+proj=utm +zone=16 +ellps=clrk66 +units=m +no_defs", thus;
proj_Coods <- "+proj=utm +zone=16 +ellps=clrk66 +units=m +no_defs"

#3.Size of grid unit to create (in metres) e.g. 50m, 100m, 150m, and so on.
g_size = 250 

#-----------------------------------
#Creating the spatial grid system:
#-----------------------------------
#transform the boundary into the defined projected CRS  e.g.
#proj_area_B <- spTransform(area_B, CRS("+init=epsg:'the-code-here'")) # {or using PROJ.4 attri e.g.: CRS("+proj=utm +zone=16 +ellps=clrk66 +units=m +no_defs")}

proj_area_B <- spTransform(area_B, CRS(proj_Coods))

#extracting the bounding coordinates of the boundary
#creating the grids
b_coord <- bbox(proj_area_B)

# Offsetting to create space for enough grids
min_x <- round(b_coord[1,1], digits=-2)- 200 #
min_y <- round(b_coord[2,1], digits=-2)- 200
max_x <- round(b_coord[1,2], digits=-2)+ 500 
max_y <- round(b_coord[2,2], digits=-2)+ 500

# Creating sequence of grid coordinates...
x <- seq(min_x,max_x,by=g_size) 
y <- seq(min_y,max_y,by=g_size)

# Generate centroid coordinates of each grid
# Generate centroid coordinates of each grid
id <- 0
centroid_points <- NULL
for(i in 1: length(x)){
	for(j in 1:length(y)){
	id <- id + 1
	centroid_points <- rbind(centroid_points, cbind(id, x[i], y[j]))
		}
	}
colnames(centroid_points) <- c("id", "x", "y")
centroid_points <- as.data.frame(centroid_points)

# Creating the coordinates of the four edges of each grid unit
# Creating the coordinates of the four edges of each grid unit
radius <- g_size/2 #radius in meters
yPlus <- centroid_points$y+radius
xPlus <- centroid_points$x+radius
yMinus <- centroid_points$y-radius
xMinus <- centroid_points$x-radius


ID=centroid_points$id
# Combining the edge coordinates for each unit 
square=cbind(xMinus,yPlus, xPlus,yPlus, xPlus,yMinus, xMinus,yMinus,xMinus,yPlus,xMinus,yPlus)

# Create spatial grid unit system (polygons)---this requires WGS84 CRS as input below
polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(square, row(square)), ID), proj4string=CRS(proj_Coods) )



# Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

# Clipping the intersecting grid units with the boundary
# List of grids that intersect the boundary
area_B <- spTransform(area_B, CRS(proj_Coods))
intersect_grids <- polys.df %over% area_B
intersect_grids <- polys.df[which(intersect_grids[,1]==0),]


# Visulising the results
plot(polys.df)
plot(area_B, add=TRUE)
plot(intersect_grids, add=TRUE, col="red")

# Exporting the grids created
writeOGR(intersect_grids, '.', 'spatial_grid_system', 'ESRI Shapefile', overwrite_layer=T)

library(naturaList)
library(devtools)
library(raster)
library(maptools)
library(rnaturalearth)
library(sf)


pkgs <- c("devtools", "raster", "maptools", "rnaturalearth", "sf")
lapply(pkgs, require, character.only = TRUE)

library(fasterize) ### needed in the package
## install.packages("lwgeom")

A.setosa <- data("A.setosa")
speciaLists <- data("speciaLists")

# creating specialist data frame from frequencies in determined by
all_det_names <- get_det_names(occ = A.setosa, determined.by = "identifiedBy", freq = TRUE)
det_names <- sort(all_det_names,
     decreasing = TRUE)[-c(which(names(sort(all_det_names, decreasing = TRUE)) == "" ),
                                         which(names(sort(all_det_names, decreasing = TRUE)) == "-" ))
                        ] # removing non person characters in determination names
det_1_percent <- names(which((det_names/sum(det_names) * 100) >= 1))
det_5_percent <- names(which((det_names/sum(det_names) * 100) >= 5))
det_7_percent <- names(which((det_names/sum(det_names) * 100) >= 7))



# classify
occ.cl <- classify_occ(A.setosa, speciaLists, spec.ambiguity = "not.spec")
dim(occ.cl) # checking the dimension of data
## check points
occ.cl <-map_module(occ.cl) #delete points in the ocean
dim(occ.cl) # checking the dimensio after remove some points in the map

# download climate data
bioclim <- getData('worldclim', var='bio', res=10)

# Transform occurrence data in SpatialPointsDataFrame
spdf.occ.cl <- SpatialPoints(occ.cl[, c("decimalLongitude", "decimalLatitude")])

# redefine the extent of bioclim layers based on buffer around the occurrences
c.bioclim <- crop(bioclim, buffer(spdf.occ.cl, 100000)) # 100km buffer

# select two layers
raster.temp.prec <- c.bioclim[[c("bio1", "bio12")]]
df.temp.prec <- as.data.frame(raster.temp.prec)

### Define the environmental space for analysis
env.space <- define_env_space(df.temp.prec, buffer.size = 0.05)

# delimit the geographic space
# land area
land <- ne_countries(continent = 'south america')
un.land <- unionSpatialPolygons(land, land$scalerank)

# geo space based on crop
c.geo.space <- crop(un.land, c.bioclim)

#geo space based on intersect
i.geo.space <- intersect(un.land, buffer(spdf.occ.cl, 200000))


cl.eval <- clean_eval(occ.cl,
           env.space = env.space,
           geo.space = c.geo.space,
           r = raster.temp.prec) # add lwgeom in dependencies

cl.eval$area
dim(cl.eval$comp$comp.BC)
dim(cl.eval$comp$comp.AC)
sum(is.na(cl.eval$comp$comp.BC))
sum(is.na(cl.eval$comp$comp.AC))

cl.eval$rich
cl.eval$site.coords

### richness maps
## it makes sense if there are more than one species
rich.before.clean <- rasterFromXYZ(cbind(cl.eval$site.coords,
                                         cl.eval$rich$rich.BC))
rich.after.clean <- rasterFromXYZ(cbind(cl.eval$site.coords,
                                         cl.eval$rich$rich.AC))

plot(rich.before.clean)
plot(rich.after.clean)


# species area map
comp.bc <- as.data.frame(cl.eval$comp$comp.BC)
comp.ac <- as.data.frame(cl.eval$comp$comp.AC)

c.setosa.bc <- rasterFromXYZ(cbind(cbind(cl.eval$site.coords,
                             comp.bc$`Cyathea setosa`)))
c.setosa.ac <- rasterFromXYZ(cbind(cbind(cl.eval$site.coords,
                                         comp.ac$`Cyathea setosa`)))

quartz()
plot(c.setosa.bc)
quartz()
plot(c.setosa.ac)

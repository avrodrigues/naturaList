
grid_filter <- function(occ.cl,
                        grid.resolution = c(0.5,0.5),
                        r = NULL,
                        institution.source = "institutionCode",
                        collection.code = "collectionCode",
                        catalog.number = "catalogNumber",
                        year.event = "year",
                        date.identified = "dateIdentified",
                        scientific.name = "species",
                        determined.by = "identifiedBy",
                        longitude = "decimalLongitude",
                        latitude = "decimalLatitude",
                        basis.of.rec = "basisOfRecord",
                        media.type = "mediaType",
                        occ.id = "occurrenceID"){
  require(sp)
  require(raster)

  natList_column <- "naturaList_levels" %in% colnames(occ.cl)
  if(!natList_column){
    stop("occurrences must has 'naturaList_levels' classification.")
  }

  od1 <- occ.cl[order(occ.cl[,year.event], decreasing = T),]
  od2 <- od1[order(od1[,date.identified], decreasing = T),]
  od3 <- od2[order(od2[,"naturaList_levels"]),]
  row.names(od3) <- 1:nrow(od3)
  x <- od3





  spt.spp_DF <- SpatialPointsDataFrame(x[,c(longitude, latitude)], x)

  if(!is.null(r)){
    if(!class(r) == "RasterLayer"){stop("'r' must be of class RasterLayer")}
  }

  if(is.null(r)){
    resolution <- grid.resolution

    ext <- extent(spt.spp_DF)[1:4]

    new.ext <- c(ext[1] - resolution[1],
                 ext[2] + resolution[2],
                 ext[3] - resolution[1],
                 ext[4] + resolution[2])

    r <- raster(resolution = resolution, ext = extent(new.ext))
  }

  cell.with.pts <- as.numeric(names(table(cellFromXY(r, spt.spp_DF))))

  total <- length(cell.with.pts)

  final.cols <- c(ncol(spt.spp_DF)+1,ncol(spt.spp_DF)+2)

  df.crit <- vector("list", total)
  idx <- 1

  pb <- txtProgressBar(min = 0, max = total, style = 3)
  for (i in 1:total){
    e <- extentFromCells(r,cell.with.pts[i])
    crop.df <- crop(spt.spp_DF, extent(e))


    df.crit[[idx]] <- as.data.frame(crop.df)[1,-final.cols]

    idx <- idx+1
    setTxtProgressBar(pb, i)
  }


  df.occ.crit <- do.call(rbind,df.crit)
  df.occ.crit <- rm.coord.dup(df.occ.crit, latitude, longitude)
  return(df.occ.crit)

}

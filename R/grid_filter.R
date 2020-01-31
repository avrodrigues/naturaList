#' Filter the occurence with most confidence in species identification inside grid cells
#'
#' Filter one occurence record per grid cell selected by the higher level of confidence
#' in species identification made by \code{\link{classify_occ}} function.
#'
#' @param occ.cl Data frame with occurrence records information already
#'   classified by \code{\link{classify_occ}} function.
#' @param grid.resolution Numeric vector with widht and height of grid cell in
#'   decimal degrees.
#' @param r raster from which the grid cell resolution is derived.
#' @param institution.source Column name of \code{occ} with the name of the
#'   institution that provided the data.
#' @param collection.code Column name of \code{occ} with the codes for institution
#'   names.
#' @param catalog.number Column name of \code{occ} with catalog number.
#' @param year.event Column name of \code{occ} the year of the collection event.
#' @param date.identified Column name of \code{occ} with the date of species
#'   determination.
#' @param scientific.name Column name of \code{occ} with the species names.
#' @param determined.by Column name of \code{occ} with the name of who determined the
#'   species.
#' @param longitude Column name of \code{occ} longitude in decimal degrees.
#' @param latitude Column name of \code{occ} latitude in decimal degrees.
#' @param basis.of.rec Column name of \code{occ} with the recording types, as in GBIF.
#' @param media.type Column name of \code{occ} with the media type of recording.
#' @param occ.id Column name of \code{occ} with link or code for the occurence record.
#'
#' @return Data frame with the same columns of \code{occ.cl}.
#'
#' @seealso \code{\link{classify_occ}}
#'
#' @examples
#' occ.class <- classify_occ(A.setosa, speciaLists)
#' occ.grid <- grid.filter(occ.class)
#' occ.grid
#'
#' @author Arthur V. Rodrigues
#'
#' @export

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

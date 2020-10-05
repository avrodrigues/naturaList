#' Internal function of naturaList - reduce data.frama of occurence for a minimal
#' collumn length
#'
#'
#' Reduce columns of occurrence data.frame required by
#' {\link[naturaList]{classify_occ}} to facilitate internal operation
#'
#' @param df occurence data frame provided in {\link[naturaList]{classify_occ}}
#' @param institution.source institution.source = "institutionCode"
#' @param collection.code collection.code = "collectionCode"
#' @param catalog.number catalog.number = "catalogNumber"
#' @param year.event year.event = "year",
#' @param date.identified date.identified = "dateIdentified"
#' @param scientific.name scientific.name = "species"
#' @param determined.by determined.by = "identifiedBy"
#' @param longitude longitude = "decimalLongitude"
#' @param latitude latitude = "decimalLatitude"
#' @param basis.of.rec basis.of.rec = "basisOfRecord"
#' @param media.type media.type = "mediaType"
#' @param occ.id occ.id = "occurrenceID"
#' @param na.rm.coords = TRUE
#'
#' @seealso {\link[naturaList]{classify_occ}}

reduce.df <- function(df,
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
                      occ.id = "occurrenceID",
                      na.rm.coords = TRUE){

  institution.source <- as.character(df[,institution.source])
  collection.code <- as.character(df[,collection.code])
  catalog.number <- as.character(df[,catalog.number])
  year.event <- df[,year.event]
  date.identified <- df[,date.identified]
  scientific.name <- as.character(df[,scientific.name])
  determined.by <- as.character(df[,determined.by])
  longitude <- df[,longitude]
  latitude <- df[,latitude]
  basis.of.rec <- df[,basis.of.rec]
  media.type <- df[,media.type]
  occ.id <- df[,occ.id]
  rowID <- rownames(df)


  data <- data.frame(rowID, occ.id, scientific.name, longitude, latitude,
                     year.event, determined.by, date.identified,
                     institution.source, collection.code,
                     catalog.number,media.type, basis.of.rec, stringsAsFactors = FALSE)
  if(na.rm.coords){
    ll.na <- is.na(data$longitude)
    data <- data[!ll.na,]
    lat.na <- is.na(data$latitude)
    data <- data[!lat.na,]
  }


  data
}

#' Internal function of naturaList - Return abbreviation colapsed
#'
#' Return colapsed abbreviation for a specific line of specialist data frame. It is
#' used as pattern in grep function inside {\link[naturaList]{classify_occ}}
#'
#' @param df spec data frame provided in {\link[naturaList]{classify_occ}}
#' @param line specifies the line of the data frame to be collapsed
#' @seealso {\link[naturaList]{classify_occ}}


abrev.pttn <- function(df, line){

  abv.num <- grep("Abbrev",colnames(df))

  abv1 <- abv.num[1]
  first.L <- paste0("\\<", df[line, abv1])

  nonblank <- df[line,-1]!=""
  nonblank <- c(FALSE, nonblank)

  pttn <- paste(unlist(df[line ,nonblank]), collapse = '|')

  if(length(df[line ,nonblank]) < 2)  pttn <- df[line, abv1]


  res <- list(first.L, pttn)
  return(res)
}

#' Internal function of naturaList - Detect if a string has a specialist name
#'
#' Detect if a string with identifier's name has a specialist name. It is used inside
#'  {\link[naturaList]{classify_occ}}
#'
#' @param sp.df reduced version of occurence data frame provided
#' in {\link[naturaList]{classify_occ}}
#' @param i line number of specialist data frame
#' @param specialist specialist data

func.det.by.esp <- function(sp.df, i, specialist){

  padr.det <- abrev.pttn(specialist, i)

  g.det <- unique(grep(paste(specialist[i,1]), ignore.case = T,
                       sp.df$determined.by))

  g.det.1 <- unique(grep(padr.det[1], ignore.case = F,
                         sp.df$determined.by[g.det]))

  g.det.ok <- unique(grep(padr.det[2], ignore.case = F,
                          sp.df$determined.by[g.det[g.det.1]]))

  g.det[g.det.1[g.det.ok]]

}

#' Internal function of naturaList - Retturn specialists names in a collapesed
#' string
#'
#' Retturn specialists names in a collapesed string to be used in the internal
#' function {\link[naturaList]{specialist.conference}}
#'
#' @param specialist specialist data frame
#'
#'

pttn.all.specialist <- function(specialist){

  pttn <- character(nrow(specialist))
  for(i in 1:nrow(specialist)){

    nonblank <- specialist[i,]!=""
    pttn[i] <- paste(specialist[i ,nonblank], collapse = '|')

  }

  pttn.all <- paste(pttn, collapse = '|')
  pttn.all
}


#' Internal function of naturaList - Verify if a string has unambiguous specialist
#' name
#'
#' Based on pattern generated by {\link[naturaList]{pttn.all.specialist}} it
#' verifies if a string has unambiguous specialist name. It is used in internal
#' function {\link[naturaList]{specialist.conference}}
#'
#' @param pattern a pattern from {\link[naturaList]{pttn.all.specialist}} function
#' @param string string with the name of who identified the specimen
#'

verify.specialist <- function(pattern, string){
  require(stringr)
  collection.new <- gsub(pattern, "", string, ignore.case = T)

  g_zero <- str_replace_all(collection.new,
                           pattern = "\\(.+\\)|[0-9]|[[:punct:]]",
                           "")

  zero <- nchar(str_replace_all(g_zero, pattern = "\\s+", "")) == 0

  if(zero == T) return("")
  if(zero == F) return("_verify")

}

#' Internal function of naturaList - Confirm if an occurrence record was identified by
#' a specialist witout ambiguity
#'
#'  Confirm if an occurrence record was identified by
#' a specialist witout ambiguity. It is used inside
#'  {\link[naturaList]{classify_occ}}
#'
#' @param pt.df a line of the reduced version of the occurrence data frame
#' @param specialist specialist data frame
#'
#'


specialist.conference <- function(pt.df, specialist){
  spe.obs <- which(lapply(specialist[,1],
                          function(x) grep(x, pt.df["determined.by"], ignore.case = T)) == 1)

  pttn.all <- pttn.all.specialist(specialist[spe.obs,])
  verify <- verify.specialist(pttn.all, pt.df["determined.by"])

  crt <- paste0("1_det_by_spec", verify)
}


#' Internal function of naturaList - Identifies if a occurence has a name for
#' the identifier of the specimen
#'
#' Identifies if a occurence has a name for the identifier of the specimen.
#'  It is used inside {\link[naturaList]{classify_occ}}
#'
#' @param sp.df reduced version of occurence data frame provided in
#' {\link[naturaList]{classify_occ}}
#' @param ignore.det.names ignore.det.names character vector indicatiing strings in the determined.by columns
#'    that should be ignored as a taxonomist. See {\link[naturaList]{classify_occ}}.
#'
#'

has.det.ID <- function(sp.df, ignore.det.names = NULL){
  if(is.null(ignore.det.names)){
    sem.det <- paste(c("^NA$",
                       "RRC ID Flag",
                       "^NA $",
                       "^ $",
                       "^$",
                       "^-$",
                       "^_$"), collapse = '|')
  }

  if(!is.null(ignore.det.names)){
    sem.det <- paste(c("^NA$",
                       "RRC ID Flag",
                       "^NA $",
                       "^ $",
                       "^$",
                       "^-$",
                       "^_$",
                       ignore.det.names), collapse = '|')
  }

  g.sem.det <- grep(sem.det, sp.df$determined.by)
  sem.det.ID <- c(which(is.na(sp.df$determined.by)), g.sem.det)

  ID <- !seq_along(sp.df$determined.by) %in%  sem.det.ID

  which(ID)
}

#' Internal function of naturaList - Manual check of ambiguity in specialist
#' name
#'
#' Creates interaction with user in which the user check if a string with the
#' identifier of a specimen has a specialist name. It solves ambiguity in classify
#' an occurence as identified by a specialist. It is used inside
#' {\link[naturaList]{classify_occ}}
#'
#' @param class.occ internal data frame with observation classified according
#' {\link[naturaList]{classify_occ}} criteria
#' @param crit.levels crit.levels choosed by user in {\link[naturaList]{classify_occ}}
#' @param determined.by same as determined.by argumetn in {\link[naturaList]{classify_occ}}
#'
check.spec <- function(class.occ, crit.levels, determined.by){

  sub <- class.occ$naturaList_levels == "1_det_by_spec_verify"
  spec.unique <- unique(class.occ[sub,determined.by])

  if(any(sub)){
    cat(paste("There is", length(spec.unique), "specialists' names to be checked"))

    answer <- vector("character", length(spec.unique))

    for(i in 1:length(spec.unique)){
      ask <- paste(spec.unique[i], "\n", "Is this a specialist? (y/n)")
      answer[i] <- readline(ask)
      if(!any(answer[i] %in% c("y", "n"))) stop("Answer only 'y' or 'n'")
    }

    c.spec <- ifelse(answer == 'y', 1, 2)
    levels_checked <- paste0(c.spec, "_", crit.levels[c.spec])

    for(i in 1:length(spec.unique)){
      g.spec <- grep(spec.unique[i], class.occ[,determined.by])
      for(j in 1:length(g.spec)){
        class.occ[g.spec[j],"naturaList_levels"] <- levels_checked[i]
      }
    }
  }
  class.occ
}

#' Internal funciton of naturaList - Remove duplicate occurrence
#'
#' Remove duplicated occurrence based on coordinates. It is used in
#' {\link[naturaList]{grid_filter}}
#'
#' @param x  data frame with filtered occurrences
#' @param latitude name of column with latitude
#' @param longitude name of column with longitude
#'
#'
rm.coord.dup <- function(x, latitude, longitude){

  unique.row <- !duplicated(x[,c(latitude,longitude)])
  res <- x[unique.row,]
  row.names(res) <- 1:nrow(res)

  res
}

# função 9
#' Internal function of naturaList - Get coordinates from poligons created in leaflet
#' map
#'
#' Get coordinates from poligons created in leaflet map. It is used in
#'  {\link[naturaList]{map_module}}
#'
#' @param input.polig an interactive poligon from leaflet map.
#' \code{input$map_draw_all_features$features[[i]]}
#'
#' @return a data frame with the coordinates


pol.coords <- function(input.polig){

  pol.coords <- data.frame(x=numeric(),y=numeric())
  total <- length(input.polig$geometry$coordinates[[1]])
  long.pol <- numeric()
  lat.pol <- numeric()
  for (i in 1:total){
    long.pol <- input.polig$geometry$coordinates[[1]][[i]][[1]]
    lat.pol <- input.polig$geometry$coordinates[[1]][[i]][[2]]


    coords <- data.frame(x=long.pol,y=lat.pol)

    pol.coords <- rbind(pol.coords, coords)

  }

  pol.coords
}


#' Internal function of naturaList - Create SpatialPoligons from a list of coordinates
#'
#' Create SpatialPoligons from a list of coordinates. It is used in {\link[naturaList]{map_module}}
#'
#' @param df a data frame provided by {\link[naturaList]{pol.coords}}

# função 10
make.polygon <- function(df){

  # and then something like this
  sp <- SpatialPolygons(list(Polygons(list(Polygon(df)), 1)))
  sp

}

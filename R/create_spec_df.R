#' Create specialist data frame from character vector
#'
#' Creates a specialist data frame ready for use in \code{\link[naturaList]{classify_occ}}
#' from a character vector containg the specialists names
#'
#' @param spec.char A character vector with specialist names
#'
#' @examples spec_list <- c("Caetano Veloso", "Antônio Carlos Tom Jobim",
#' "Gilberto Gil", "Vinícius de Morais")
#'
#' create_spec_df(spec_list)
#'
#' @export

create_spec_df <- function(spec.char){
  spec.char <- as.character(spec.char)
  #transform in list
  l.spec <- lapply(spec.char, function(x) x)

  #split strings by space
  l.spec.split <- lapply(l.spec, function(x){
    strsplit(x, " ")
  })

  l.spec.split <- lapply(l.spec.split, "[[", 1)


  # Last name list
  last.name <- lapply(l.spec.split, function(x) x[length(x)])

  # create names list (without last name)
  names <- lapply(l.spec.split, function(x) x[-length(x)])
  names <- lapply(names, function(x){
    gsub("[[:punct:]]","", x)
  })

  #function for abbreviation of names only for upper cases
  abbrev.names <- function(char){
    char1<- substr(char, 1, 1)

    upper <- toupper(char1) == char1
    lower <- tolower(char1) == char1

    if(upper){
      return(char1)
    }
    if(lower){
      return("")
    }

  }

  # abreviation
  l.abrrev <-lapply(names, function(x){
    case <- character()
    for(i in 1:length(x)){
      case[i] <- abbrev.names(x[i])
    }
    case[nchar(case)!=0]
  })

  # remove abreviation from names
  only.names <- lapply(names, function(x){
    onechar <- nchar(x) == 1
    if(any(onechar)){
      str <- gsub(x[onechar], "", x)
    }
    if(!any(onechar)){ str <- x}
    str

  })


  # number of cols for names and abbrev data frames
  ncol.abrev <- max(sapply(l.abrrev, function(x) sum(nchar(x))))

  ncol.names <- max(sapply(only.names, function(x) length(x)))

  # names data frame
  l.names.df <- lapply(only.names, function(x){
    diff.col <- ncol.names - length(x)
    c(x, rep("", diff.col))

  })

  names.df <- as.data.frame(do.call(rbind, l.names.df),stringsAsFactors = F)
  names(names.df) <- paste0("Name", 1:ncol(names.df))

  # abbrev data frame
  l.abrrev.df <- lapply(l.abrrev, function(x){
    diff.col <- ncol.abrev - length(x)
    c(x, rep("", diff.col))

  })

  abrrev.df <- as.data.frame(do.call(rbind, l.abrrev.df),stringsAsFactors = F)
  names(abrrev.df) <- paste0("Abbrev", 1:ncol(abrrev.df))

  # LastName as character vector
  LastName <- do.call(c,last.name)

  # join LastName, names.df, abrrev.df
  df.spec <- data.frame(LastName, names.df, abrrev.df, stringsAsFactors = F)

  # find for special characters
  mtx.spec.alt <- t(apply(df.spec, 1, function(x){
    adj.enc <- iconv(x, to='ASCII//TRANSLIT')
  }))

  # Add lines without special characters
  df.spec.j  <- unique(rbind(df.spec, mtx.spec.alt))
  df.spec.j  <- df.spec.j[order(df.spec.j$LastName),]

  row.names(df.spec.j) <- NULL

  # Specialist data frame
  return(df.spec.j)


}

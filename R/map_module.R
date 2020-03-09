#' Check the occurrence records in a interactive map module
#'
#' Allow to delete occurrence records and to select occurence points by
#' classifcation levels or spatial polygons.
#'
#' @param occ.cl Data frame with occurrence records information already
#'   classified by \code{\link{classify_occ}} function.
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
#' y
#' y
#' y
#' y
#' occ.selected <- map_module(occ.class)
#' occ.selected
#'
#' @author Arthur V. Rodrigues
#'
#' @export

map_module <- function(occ.cl,
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
  library(shiny)
  library(shinyWidgets)
  library(shinydashboard)
  library(leaflet)
  library(raster)
  library(rgdal)
  require(leaflet.extras)
  library(shinyLP)

  final.df <- runApp(list(
  ui = fluidPage(
    titlePanel("Map Module - naturaList"),
    fluidRow(
      column(3,
             box(width = NULL,
                 checkboxGroupButtons(
                   inputId = "grbox", label = "What levels should be maintained in the output data set?",
                   choices = c("Level 1" = "1",
                               "Level 2" = "2",
                               "Level 3" = "3",
                               "Level 4" = "4",
                               "Level 5" = "5",
                               "Level 6" = "6"),
                   justified = T, status = 'info', size = "xs", direction = "vertical",
                   checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                   selected = c("1",
                                "2",
                                "3",
                                "4",
                                "5",
                                "6"),
                   width = "100%"
                 ),
                 materialSwitch("del_mkr_button",
                                label = "Delete points with click",
                                status = "danger")


             ),
             box(width = NULL, title = "Download",
                 solidHeader = T, status = "success",


                 textOutput("sel_display"),
                 #verbatimTextOutput("issue"),
                 #downloadButton("download_grid_filter.csv","Download from grid filter"),br(),br(),
                 textOutput("down.class.text"),
                 actionBttn("done", "Done!", style = "bordered", color = "success", size = "lg")
                 #downloadButton("download_classified.csv","Download from classifier")
             )
      ),
      column(9,
             leafletOutput("map", height = 500)

      )
    )

  ),
  server = function(input, output, session){
    values = reactiveValues()

    ## occurrences categories

    values$occur <- occ.cl


    values$Level_1 <- grep("1",  occ.cl$naturaList_levels)
    values$Level_2 <- grep("2",  occ.cl$naturaList_levels)
    values$Level_3 <- grep("3",  occ.cl$naturaList_levels)
    values$Level_4 <- grep("4",  occ.cl$naturaList_levels)
    values$Level_5 <- grep("5",  occ.cl$naturaList_levels)
    values$Level_6 <- grep("6",  occ.cl$naturaList_levels)





    ####
    ## Pontos a serem deletados
    values$MK <- data.frame(id = character(),
                            x = numeric(),
                            y = numeric())

    ## Linha a ser inserida em values$MK
    values$add_row <- data.frame(id = character(),
                                 x = numeric(),
                                 y = numeric())

    values$pol <- list()

    ## linhas das ocorrencias (grep) selecionadas de acordo com os critérios
    #values$g.cri <- numeric()
    ## lista de poligonos
    values$list.pol.df <- list()

    output$map <- renderLeaflet({

      values$occur[,date.identified]<- as.Date(values$occur[,date.identified])

      values$pt.ctr1 <- values$occur[values$Level_1,]
      values$pt.ctr2 <- values$occur[values$Level_2,]
      values$pt.ctr3 <- values$occur[values$Level_3,]
      values$pt.ctr4 <- values$occur[values$Level_4,]
      values$pt.ctr5 <- values$occur[values$Level_5,]
      values$pt.ctr6 <- values$occur[values$Level_6,]

    leaflet("map") %>%
      # Add two tiles
      addTiles(options = providerTileOptions(noWrap = TRUE),group="StreetMap")%>%
      addProviderTiles("Esri.WorldImagery", group="Satelite")  %>%


      # Add marker groups
      addCircleMarkers(data= values$pt.ctr1,
                       lng= values$pt.ctr1[,longitude] ,
                       lat= values$pt.ctr1[,latitude],
                       radius=6 ,
                       layerId = row.names(values$pt.ctr1),
                       popup = paste(strong("ID:"), values$pt.ctr1[, occ.id],"<br>",
                                     strong("Institution Source:"), values$pt.ctr1[, institution.source], "<br>",
                                     strong("Determined by:"), values$pt.ctr1[, determined.by], "<br>",
                                     strong("Year of data colection:"), values$pt.ctr1[, year.event], "<br>",
                                     strong("Date Identified:"), (values$pt.ctr1[, date.identified]), "<br>"),
                       fillColor="red", stroke = F, fillOpacity = 0.8, group="Level 1") %>%

      addCircleMarkers(data= values$pt.ctr2,
                       lng= values$pt.ctr2[,longitude] ,
                       lat= values$pt.ctr2[,latitude],
                       radius=6 ,
                       layerId = row.names(values$pt.ctr2),
                       popup = paste(strong("ID:"), values$pt.ctr2[, occ.id],"<br>",
                                     strong("Institution Source:"), values$pt.ctr2[, institution.source], "<br>",
                                     strong("Determined by:"), values$pt.ctr2[, determined.by], "<br>",
                                     strong("Year of data colection:"), values$pt.ctr2[, year.event], "<br>",
                                     strong("Date Identified:"), as.Date(as.character(values$pt.ctr2[, date.identified])), "<br>"),
                       fillColor="orange", stroke = F, fillOpacity = 0.8, group="Level 2") %>%

      addCircleMarkers(data= values$pt.ctr3,
                       lng= values$pt.ctr3[,longitude] ,
                       lat= values$pt.ctr3[,latitude],
                       radius=6 ,
                       layerId = row.names(values$pt.ctr3),
                       popup = paste(strong("ID:"), values$pt.ctr3[, occ.id],"<br>",
                                     strong("Institution Source:"), values$pt.ctr3[, institution.source], "<br>",
                                     strong("Determined by:"), values$pt.ctr3[, determined.by], "<br>",
                                     strong("Year of data colection:"), values$pt.ctr3[, year.event], "<br>",
                                     strong("Date Identified:"), (values$pt.ctr3[, date.identified]), "<br>"),
                       fillColor="yellow", stroke = F, fillOpacity = 0.8, group="Level 3") %>%

      addCircleMarkers(data= values$pt.ctr4,
                       lng= values$pt.ctr4[,longitude] ,
                       lat= values$pt.ctr4[,latitude],
                       radius=6 ,
                       layerId = row.names(values$pt.ctr4),
                       popup = paste(strong("ID:"), values$pt.ctr4[, occ.id],"<br>",
                                     strong("Institution Source:"), values$pt.ctr4[, institution.source], "<br>",
                                     strong("Determined by:"), values$pt.ctr4[, determined.by], "<br>",
                                     strong("Year of data colection:"), values$pt.ctr4[, year.event], "<br>",
                                     strong("Date Identified:"), (values$pt.ctr4[, date.identified]), "<br>"),
                       fillColor="darkgreen", stroke = F, fillOpacity = 0.8, group="Level 4") %>%

      addCircleMarkers(data= values$pt.ctr5,
                       lng= values$pt.ctr5[,longitude] ,
                       lat= values$pt.ctr5[,latitude],
                       radius=6 ,
                       layerId = row.names(values$pt.ctr5),
                       popup = paste(strong("ID:"), values$pt.ctr5[, occ.id],"<br>",
                                     strong("Institution Source:"), values$pt.ctr5[, institution.source], "<br>",
                                     strong("Determined by:"), values$pt.ctr5[, determined.by], "<br>",
                                     strong("Year of data colection:"), values$pt.ctr5[, year.event], "<br>",
                                     strong("Date Identified:"), (values$pt.ctr5[, date.identified]), "<br>"),
                       fillColor="blue", stroke = F, fillOpacity = 0.8, group="Level 5") %>%



      addCircleMarkers(data= values$pt.ctr6,
                       lng= values$pt.ctr6[,longitude] ,
                       lat= values$pt.ctr6[,latitude],
                       radius=6 ,
                       layerId = row.names(values$pt.ctr6),
                       popup = paste(strong("ID:"), values$pt.ctr6[, occ.id],"<br>",
                                     strong("Institution Source:"), values$pt.ctr6[, institution.source], "<br>",
                                     strong("Determined by:"), values$pt.ctr6[, determined.by], "<br>",
                                     strong("Year of data colection:"), values$pt.ctr6[, year.event], "<br>",
                                     strong("Date Identified:"), (values$pt.ctr6[, date.identified]), "<br>"),
                       fillColor="purple", stroke = F, fillOpacity = 0.8, group="Level 6") %>%




      # Add the control widget
      addLayersControl(overlayGroups = c("Level 1",
                                         "Level 2",
                                         "Level 3",
                                         "Level 4",
                                         "Level 5",
                                         "Level 6") ,
                       baseGroups = c("StreetMap","Satelite"),
                       options = layersControlOptions(collapsed = TRUE)) %>%

      ## Add tool to design poligons shapes to selection
      addDrawToolbar(
        targetGroup='Markers',
        polylineOptions = F,
        polygonOptions = T,
        circleOptions = F,
        rectangleOptions = F,
        markerOptions = F,
        circleMarkerOptions = F,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))%>%
      addLegend("bottomright", colors = c("red", "orange", "yellow", "darkgreen", "blue", "purple"),
                labels = c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5", "Level 6"),
                opacity = 0.8)
  })

    ## Limpa lista de pontos selecionados
    observeEvent(input$del_mkr_button,{
      if(input$del_mkr_button == FALSE){
        values$add_row <- data.frame(id = character(),
                                     x = numeric(),
                                     y = numeric())
      }

      ## Deleta pontos de ocorrencia que forem clicados
      observeEvent(input$map_marker_click, {
        proxy <- leafletProxy("map")



        if(input$del_mkr_button == TRUE){
          values$add_row <- data.frame(id = input$map_marker_click$id,
                                       x = input$map_marker_click$lng,
                                       y = input$map_marker_click$lat)
          values$MK <-  rbind(values$MK, values$add_row)



          dp <- duplicated(values$MK$id)

          if(sum(dp) >= 1){

            del <- values$MK[,1] %in% values$MK[dp,1]

          }

          id <- values$add_row$id
          proxy %>%
            removeMarker(id)
        }

      })

      ## Cria uma data.frame com os as coordenadas geográficas para criar um poligo
      observeEvent(input$map_draw_all_features, {


        if(length(input$map_draw_all_features$features) == 0){return()}
        if(length(input$map_draw_all_features$features) >0){
          for(i in 1:length(input$map_draw_all_features$features)){
            values$list.pol.df[[i]] <- pol.coords(input$map_draw_all_features$features[[i]])
          }
        }

      })

      ## Pontos de ocorrencia selecionados de acordo com a classificação (criterios)
      observeEvent(input$grbox, {
        # dados de ocorrencia da espécie
        occ.df <- values$occur

        pttn <- paste(input$grbox, collapse="|")
        values$g.cri <- grep(pttn, occ.df$naturaList_levels)


      })

      observeEvent(is.null(input$grbox), {
        if(is.null(input$grbox)){
          g.cri <- NULL
          values$g.cri <- g.cri

        }
      })

      ## Cria o data.frame a ser salvo
      # seleciona pelos critérios e pelos poligonos criados
      observeEvent(input$grbox, {
        output$sel_display <-  renderText({

          if (is.null(values$g.cri)|length(values$g.cri) == 0){
            values$sel.points <- data.frame()
          }else{

            occ.df <- values$occur
            n.id <- as.character(values$MK$id)
            del <- row.names(occ.df)%in% n.id
            c.d <- values$g.cri %in% which(!del)
            values$pol <- list()

            if(length(values$list.pol.df) == 1){
              values$pol <- make.polygon(values$list.pol.df[[1]])
            }


            if(length(values$list.pol.df) > 1){

              values$pol <- bind(lapply(values$list.pol.df, make.polygon))
            }

            df.crit <- values$occur[values$g.cri[c.d],]

            spt.df <- SpatialPointsDataFrame(df.crit[,c(longitude, latitude)], df.crit)


            if(length(values$pol) == 0){
              n.col <- ncol(spt.df)
              values$sel.points <- as.data.frame(spt.df)[,1:n.col]
            }

            if(length(values$pol) >= 1){
              n.col <- ncol(spt.df)
              values$sel.points <- as.data.frame(spt.df[values$pol,])[,1:n.col]
            }
          }

          paste("Selected", nrow(values$sel.points), "of", nrow(values$occur), "occurrence points." )

        })
      })

      observeEvent(input$done, {
        result <- values$sel.points

        stopApp(returnValue = result)
      })

      ### resolvendo problemas
      output$issue <- renderPrint({

      })


    })


  }



))
  return(final.df)
}

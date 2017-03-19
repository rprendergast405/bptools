#' Catchment selection tool.
#'
#' A Shiny function to make picking a catchment for a merchant or retail area easier.
#'
#' @param spending A data.frame of the spend origin with the attributes CAU and SPEND
#' @param location A data.frame with the merchant coordinates as X_COORD, Y_COORD in NZTM projection
#' @param map_shapefile A shapefile from which to draw the map (default is 2013 CAU)
#'
#' @export catchment_picker
catchment_picker <- function(spending, location, map_shapefile = mvldata::nz_cau_13.spdf){
  require(shiny)
  require(dplyr)
  require(leaflet)
  require(sp)
  require(rgdal)
  require(rgeos)
  shinyApp(
    # App ui - pretty basic
    ui = fluidPage(
      column(8, leafletOutput("map", width = 1000, height = 1000)),
      column(4, downloadLink('downloadData', 'Download'),
             selectInput(inputId = "catchment_name", label = "Catchment Name",
                         choices = c("Primary", "Secondary", "Tertiary"),
                         selected = "Primary"),
             tableOutput("catchment_info_1"),
             tableOutput("catchment_info_2"),
             tableOutput("catchment_info_3"))
    ),

    server = function(input, output, session) {
      session$onSessionEnded(stopApp)
      # produce the basic leaflet map of spending
      output$map <- renderLeaflet({
        nz_map <-leaflet(spend_map.spdf) %>%
          addPolygons(fillOpacity = 0.5,
                      stroke = TRUE,
                      weight = 2,
                      fillColor = ~pal(SPEND),
                      layerId = ~CAU) %>%
          addTiles() %>%
          addMarkers(data = store_location.spdf)

        nz_map
      })

      data <- reactiveValues(primary_catchment = c(),
                             secondary_catchment = c(),
                             tertiary_catchment = c(),
                             unselected = c())


      # Create the catchment tables ---------------------------------------------
      # Primary catchment info table
      output$catchment_info_1 <- renderTable(
        spend_map.spdf@data %>%
          mutate(prop = SPEND / sum(SPEND, na.rm = TRUE)) %>%
          filter(CAU %in% data$primary_catchment) %>%
          select(CAU, CAU_NAME, SPEND, prop) %>%
          arrange(desc(SPEND)) %>%
          bind_rows(
            spend_map.spdf@data %>%
              mutate(prop = SPEND / sum(SPEND, na.rm = TRUE)) %>%
              filter(CAU %in% data$primary_catchment) %>%
              group_by(CAU = "TOTAL", CAU_NAME = "TOTAL") %>%
              summarise_each(funs(sum_na), SPEND, prop)
          ) %>%
          mutate(SPEND = dollar(SPEND),
                 prop = percent(prop),
                 catchment = "Primary")
      )
      # secondary catchment info table
      output$catchment_info_2 <- renderTable(
        spend_map.spdf@data %>%
          mutate(prop = SPEND / sum(SPEND, na.rm = TRUE)) %>%
          filter(CAU %in% data$secondary_catchment) %>%
          select(CAU, CAU_NAME, SPEND, prop) %>%
          arrange(desc(SPEND)) %>%
          bind_rows(
            spend_map.spdf@data %>%
              mutate(prop = SPEND / sum(SPEND, na.rm = TRUE)) %>%
              filter(CAU %in% data$secondary_catchment) %>%
              group_by(CAU = "TOTAL", CAU_NAME = "TOTAL") %>%
              summarise_each(funs(sum_na), SPEND, prop)
          ) %>%
          mutate(SPEND = dollar(SPEND),
                 prop = percent(prop),
                 catchment = "Secondary")
      )
      # Tertiary catchment info table
      output$catchment_info_3 <- renderTable(
        spend_map.spdf@data %>%
          mutate(prop = SPEND / sum(SPEND, na.rm = TRUE)) %>%
          filter(CAU %in% data$tertiary_catchment) %>%
          select(CAU, CAU_NAME, SPEND, prop) %>%
          arrange(desc(SPEND)) %>%
          bind_rows(
            spend_map.spdf@data %>%
              mutate(prop = SPEND / sum(SPEND, na.rm = TRUE)) %>%
              filter(CAU %in% data$tertiary_catchment) %>%
              group_by(CAU = "TOTAL", CAU_NAME = "TOTAL") %>%
              summarise_each(funs(sum_na), SPEND, prop)
          ) %>%
          mutate(SPEND = dollar(SPEND),
                 prop = percent(prop),
                 catchment = "Tertiary")
      )


      # Update the map when it is clicked ----

      # When the map is clicked, some pretty hectic shit happens
      click_tract <- eventReactive(input$map_shape_click, {
        return(input$map_shape_click$id)
      })

      # observe the marker click info and update the catchments

      # remove the area from the catchment if it is included
      observeEvent(input$map_shape_click, {
        if((click_tract() %in% c(data$primary_catchment, data$secondary_catchment, data$tertiary_catchment))) {
          #cat("\nBleh!")
            data$primary_catchment <- data$primary_catchment[!(data$primary_catchment %in% input$map_shape_click)]
            data$secondary_catchment <- data$secondary_catchment[!(data$secondary_catchment %in% input$map_shape_click)]
            data$tertiary_catchment <- data$tertiary_catchment[!(data$tertiary_catchment %in% input$map_shape_click)]

            data$unselected <- input$map_shape_click
           # cat("\n", c(data$primary_catchment, data$secondary_catchment, data$tertiary_catchment))

        } else {

        # add the area to the catchment if it isn't already included
        if(!(click_tract() %in% c(data$primary_catchment, data$secondary_catchment, data$tertiary_catchment))) {
          if(input$catchment_name == "Primary") {
            data$primary_catchment <- unique(c(data$primary_catchment, input$map_shape_click$id))
          }
          if(input$catchment_name == "Secondary") {
            data$secondary_catchment <- unique(c(data$secondary_catchment, input$map_shape_click$id))
          }
          if(input$catchment_name == "Tertiary") {
            data$tertiary_catchment <- unique(c(data$tertiary_catchment, input$map_shape_click$id))
          }
        }
        #cat("\n", c(data$primary_catchment, data$secondary_catchment, data$tertiary_catchment))
}
      }
      )

      map_primary <- reactive({
        req(click_tract())
        return(spend_map.spdf[spend_map.spdf$CAU %in% data$primary_catchment, ])
      })

      map_secondary <- reactive({
        req(click_tract())
        return(spend_map.spdf[spend_map.spdf$CAU %in% data$secondary_catchment, ])
      })

      map_tertiary <- reactive({
        req(click_tract())
        return(spend_map.spdf[spend_map.spdf$CAU %in% data$tertiary_catchment, ])
      })

      map_other <- reactive({
        req(click_tract())
        return(spend_map.spdf[spend_map.spdf$CAU %in% data$unselected, ])
      })

      # Draw the map when a click happens ----
      observe({
        req(click_tract())
        proxy <- leafletProxy('map')
        # Draw the primary catchment
        #if (any(!c(is.null(map_primary()), is.null(map_secondary()), is.null(map_tertiary())))) {

          proxy %>%
            removeShape('catchment1') %>%
            clearGroup('catchment1') %>%
            removeShape('catchment2') %>%
            clearGroup('catchment2') %>%
            removeShape('catchment3') %>%
            clearGroup('catchment3') %>%
            addPolygons(data = map_other(),
                        fillColor = ~pal(spend_map.spdf$SPEND)[spend_map.spdf$CAU %in% data$unselected],
                        fillOpacity = 0.5,
                        stroke = TRUE,
                        weight = 2,
                        layerId = ~CAU) %>%
            addPolygons(data = map_primary(),
                        fillColor = ~pal2(spend_map.spdf$SPEND)[spend_map.spdf$CAU %in% data$primary_catchment],
                        fillOpacity = 0.8,
                        weight = 2,
                        color = "red",
                        group = "catchment1",
                        layerId = ~CAU) %>%
            addPolygons(data = map_secondary(),
                        fillColor = ~pal3(spend_map.spdf$SPEND)[spend_map.spdf$CAU %in% data$secondary_catchment],
                        fillOpacity = 0.8,
                        weight = 2,
                        color = "green",
                        group = "catchment2",
                        layerId = ~CAU) %>%
            addPolygons(data = map_tertiary(),
                        fillColor = ~pal4(spend_map.spdf$SPEND)[spend_map.spdf$CAU %in% data$tertiary_catchment],
                        fillOpacity = 0.8,
                        weight = 2,
                        color = "gold",
                        group = "catchment3",
                        layerId = ~CAU)
        #}


      })

      # Download the tables when you're finished ----
      output$downloadData <- downloadHandler(
        filename = function() {
          paste('catchment_', Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
          write.csv(spend_map.spdf@data %>%
                      filter(CAU %in% data$primary_catchment) %>%
                      mutate(catchment = "Primary") %>%
                      select(CAU, CAU_NAME) %>%
                      bind_rows(
                        spend_map.spdf@data %>%
                          filter(CAU %in% data$secondary_catchment) %>%
                          mutate(catchment = "Secondary") %>%
                          select(CAU, CAU_NAME)
                      ) %>%
                      bind_rows(
                        spend_map.spdf@data %>%
                          filter(CAU %in% data$tertiary_catchment) %>%
                          mutate(catchment = "Tertiary") %>%
                          select(CAU, CAU_NAME)
                      ), file, row.names = FALSE)
        }
      )

    },

    # Initialise the map at the start ------
    onStart = function() {
      cat("Initialising\n")
      store_location.spdf <<- SpatialPoints(coords = location, proj4string = CRS("+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs")) %>%
        spTransform(CRS("+proj=longlat +datum=WGS84"))

      cat("Processing Map")
      # add the spending data to the map
      spend_map.spdf <<- map_shapefile %>%
        subset(!(grepl("Chatham",
                       x = map_shapefile@data$CAU_NAME,
                       perl = TRUE)))

      cat("...\n")
      spend_map.spdf <<- spend_map.spdf %>%
        gSimplify(tol = 50, topologyPreserve = TRUE) %>%
        SpatialPolygonsDataFrame(spend_map.spdf@data) %>%
        spTransform(CRS("+proj=longlat +datum=WGS84"))

      spend_map.spdf$CAU <- as.character(spend_map.spdf$CAU)
      spending$CAU <- as.character(spending$CAU)

      cat("Adding Spend Data\n")
      spend_map.spdf@data <<-spend_map.spdf@data %>%
        left_join(spending)

      cat("Finishing Up")
      pal <<- colorNumeric(
        palette = "Blues",
        domain = spend_map.spdf$SPEND
      )

      pal2 <<- colorNumeric(
        palette = "Reds",
        domain = spend_map.spdf$SPEND
      )

      pal3 <<- colorNumeric(
        palette = "Greens",
        domain = spend_map.spdf$SPEND
      )

      pal4 <<- colorNumeric(
        palette = "YlOrBr",
        domain = spend_map.spdf$SPEND
      )

      sum_na <<- function(x) sum(x, na.rm = TRUE)

    }
  )
}

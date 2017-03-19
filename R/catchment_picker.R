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
    ui = fluidPage(
      column(8, leafletOutput("map", width = 1000, height = 1000)),
      column(4, downloadLink('downloadData', 'Download'),
             selectInput(inputId = "catchment_name", label = "Catchment Name",
                         choices = c("Primary", "Secondary", "Tertiary"),
                         selected = "Primary"),
             tableOutput("catchment_info"))
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

      data <- reactiveValues(selected_catchment = c(),
                             unselected = c())


      output$catchment_info <- renderTable(
        spend_map.spdf@data %>%
          mutate(prop = SPEND / sum(SPEND, na.rm = TRUE)) %>%
          filter(CAU %in% data$selected_catchment) %>%
          select(CAU, CAU_NAME, SPEND, prop) %>%
          arrange(desc(SPEND)) %>%
          bind_rows(
            spend_map.spdf@data %>%
              mutate(prop = SPEND / sum(SPEND, na.rm = TRUE)) %>%
              filter(CAU %in% data$selected_catchment) %>%
              group_by(CAU = "TOTAL", CAU_NAME = "TOTAL") %>%
              summarise_each(funs(sum_na), SPEND, prop)
          ) %>%
          mutate(SPEND = dollar(SPEND),
                 prop = percent(prop),
                 catchment = input$catchment_name)
      )

      click_tract <- eventReactive(input$map_shape_click, {
        return(input$map_shape_click$id)
      })

      #  observe the marker click info and print to console when it is changed.
      observeEvent(input$map_shape_click, {
        if(click_tract() %in% data$selected_catchment) {
          data$selected_catchment <- data$selected_catchment[!(data$selected_catchment %in% input$map_shape_click)]
          data$unselected <- input$map_shape_click
        } else data$selected_catchment <- unique(c(data$selected_catchment, input$map_shape_click$id))
      }
      )

      map_catchment <- reactive({
        req(click_tract())
        return(spend_map.spdf[spend_map.spdf$CAU %in% data$selected_catchment, ])
      })

      map_other <- reactive({
        req(click_tract())
        return(spend_map.spdf[spend_map.spdf$CAU %in% data$unselected, ])
      })

      observe({
        req(click_tract())
        proxy <- leafletProxy('map')
        if (!is.null(map_catchment())) {
          proxy %>%
            removeShape('catchment') %>%
            clearGroup('catchment') %>%
            addPolygons(data = map_other(),
                        fillColor = ~pal(spend_map.spdf$SPEND)[spend_map.spdf$CAU %in% data$unselected],
                        fillOpacity = 0.5,
                        stroke = TRUE,
                        weight = 2,
                        layerId = ~CAU) %>%
            addPolygons(data = map_catchment(),
                        fillColor = ~pal2(spend_map.spdf$SPEND)[spend_map.spdf$CAU %in% data$selected_catchment],
                        fillOpacity = 0.8,
                        weight = 2,
                        color = "red",
                        group = "catchment",
                        layerId = ~CAU)
        }
      })


      output$downloadData <- downloadHandler(
        filename = function() {
          paste('catchment_', Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
          write.csv(spend_map.spdf@data %>%
                      filter(CAU %in% data$selected_catchment) %>%
                      select(CAU, CAU_NAME), file, row.names = FALSE)
        }
      )

    },

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

      sum_na <<- function(x) sum(x, na.rm = TRUE)

    }
  )
}

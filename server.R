library(shiny)
library(DT)
library(leaflet)
# library(datasets)
# library(spdep)
# library(devtools)
# library(fillmap)
# library(maptools)
# library(rgdal)
# library(rmapshaper)
# library(rgeos)
# arhf = read.csv("//dir-nas/bcbb/KDsummerproj/Data/ARHF.csv")
# states = read.csv("//dir-nas/bcbb/KDsummerproj/QGIS/USStateOrder.csv")
# states <- states[order(states$NAME),] #order data via state name
# countyRegionList=read.csv("//dir-nas/bcbb/KDsummerproj/Data/CountyRegionList.csv")
# countyRegionList=countyRegionList[order(countyRegionList$NAME),] #order data via county name
# orderRe=read.csv("//dir-nas/bcbb/KDsummerproj/QGIS/USRegionOrder.csv")
# mapRegion=readShapePoly("//dir-nas/bcbb/KDsummerproj/QGIS/SScountyUS.shp")
# simplified <- rmapshaper::ms_simplify(mapRegion)
# coordinates <- coordinates(mapRegion)
# orderRe <- cbind(orderRe,coordinates)
# orderCo=read.csv("//dir-nas/bcbb/KDsummerproj/QGIS/USCountyOrderPR1.csv")
# mapCounties=readShapePoly("//dir-nas/bcbb/KDsummerproj/QGIS/SScountyUSPR1.shp")
# simplifiedCo <- gSimplify(mapCounties,tol=0.001)
# coordinates <- coordinates(mapCounties)
# orderCo <- cbind(orderCo,coordinates)
# SSCountyPar=read.csv("//dir-nas/bcbb/KDsummerproj/Data/SSCountyParticipantNumbers.csv")
# SSRegionPar=read.csv("//dir-nas/bcbb/KDsummerproj/Data/SSRegionParticipantNumbers.csv")
# SSRegionPar <- SSRegionPar[order(SSRegionPar$RegionOrder),]
# countyRegionList <- countyRegionList[order(countyRegionList$GEOID),]
# countyRegionList["Participants"] <- SSCountyPar$freq
# newnames <- c("Water Area in Square Miles (2010)","Farms - Number (2002)","Farmland as a % of Total Land (2002)",	
#               "Population Density per Square Mile (2010)","% Good Air Quality Days (2011)","Daily Fine Particulate Matter (2010)",
#               "Days w/8-hr Avg Ozone ovr NAAQS (2010)",	"# Dsgntd Txc Site Not Undr Cntrl (2012)","Total Number Hospitals (2010)",
#               "STG Hosp w/Breast Cancer Scrn/Mam (2010)","Census Population (2010)","Percent Black/African American Population (2010)",	
#               "Medicaid Eligibles, Total (2008)","% < 65 without Health Insurance (2010)","Median Household Income (2010)",	
#               "% Persons in Poverty (2010)","Percent Urban Population (2010)","Percent Urban Housing Units (2010)",
#               "% Persons 25+ W/4+ Yrs College (2006-10)","% Agric/Forest/Fish/Hunt/Mine Workers (2006-10)",	"Total Births (2010)",
#               "Total Deaths (2010)")
# names(arhf)[c(2:23)] <- newnames
# mapVars = read.csv("//dir-nas/bcbb/KDsummerproj/Data/USRegionVariables.csv")
# a <- colnames ( mapVars[,grepl("X..", colnames(mapVars))] )
# newnames2 <- c("African American", "Medicaid Eligibles", "< 65 W/o Health Insurance",
#               "Persons in Poverty", "Urban Population", "High Education", "Workers")
# names(mapVars)[match(a,names(mapVars))] <- newnames2
# load("//dir-nas/bcbb/KDsummerproj/Code/MapRegionsNoPR.RData")

load("data/MapData.RData") # Loads the workspace of the variables created by the comments above


function(input, output) {
  
  chosenState <- reactive({
    input$state
  })

  
  values <- reactiveValues()
  
  output$countySelector <- renderUI({
    stateabv <<-states[which(states$NAME==chosenState()),2]
    counties <<- countyRegionList[countyRegionList$STUSPS==stateabv,]
    selectInput("county","Choose which county:",
                choices=counties$NAME)
  })
  
  chosenCounty <- reactive({
    input$county
  })



  output$region <- renderText({
    region <<- counties[counties$NAME==chosenCounty(),8]
    string <<- paste(chosenCounty(), "is in Region",as.character(region))
    print(string)
  })
  

  
  output$Info <- renderDataTable({
    fips <<-  counties[which(counties$NAME==chosenCounty()),4]
    z <- as.data.frame(t(arhf[which(arhf$FIPS==fips),]))
    # ranks=rep(NA,23)
    # for (i in 2:23) {
    #   ranks[i] <- rank(arhf[,i])[which(arhf$FIPS==fips)]
    # }
    # z <- cbind(z,ranks)
    # colnames(z) <- c("Total/%", "Rank")
    datatable(z,options = list(pageLength = -1,dom = 't'))

  })
  
  output$RegionInfo <- renderDataTable({
    region <<- counties[counties$NAME==chosenCounty(),8]
    countyNames <-  countyRegionList[which(countyRegionList$PHDnum==as.character(region)),c(4,5,9)]
    z <- countyNames
    datatable(z,rownames = FALSE, options = list(pageLength = -1,dom = 't'))
    
  })
  
  output$regiontotal <- renderText({
    region <<- counties[counties$NAME==chosenCounty(),8]
    countyNames <-  countyRegionList[which(countyRegionList$PHDnum==as.character(region)),c(4,5,9)]
    z <- countyNames
    string <<- paste(as.character(region), "has",sum(z[,3]),"participants")
    print(string)
  })
  
  output$Map <- renderLeaflet({
    region <<- as.character(counties[counties$NAME==chosenCounty(),8])
    rnum <- which(orderRe$NAME==as.character(region))
    fips <<-  counties[which(counties$NAME==chosenCounty()),4]
    cnum <- which(orderCo$GEOID==fips)
    leaflet() %>% 
      # addTiles() %>% 
      # addProviderTiles(providers$Stamen.TonerLite,
      #                  options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = simplified, weight = .8,color="blue", label= ~NAME) %>%
      # addPolygons(data = simplifiedCo, weight = .4,color="red") %>%
      addMarkers(lng=orderCo[cnum,8], lat=orderCo[cnum,9], popup=chosenCounty())%>%
      setView(orderRe[rnum,4],orderRe[rnum,5],zoom=6) 
  }) #END RENDERLEAFLET OUTPUT
  
  output$FullMap <- renderLeaflet({
    bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
    pal <- colorBin("YlOrRd", domain = SSRegionPar$freq, bins = bins)
    labels <- sprintf("<strong>%s</strong><br/>%g participants", SSRegionPar$RegionName, 
                      SSRegionPar$freq) %>% lapply(htmltools::HTML)
    m <- leaflet(SSRegionPar) %>%
      setView(-96, 38, 4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      
      addPolygons(data = simplified, fillColor = ~pal(SSRegionPar$freq), weight = 2, opacity = 1, 
                  color = "white", dashArray = "3", fillOpacity = 0.7, 
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", 
                                               fillOpacity = 0.7,bringToFront = TRUE),
                  label = labels, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                              textsize = "15px", direction = "auto")) %>% 
      addLegend(pal = pal, values = bins, opacity = 0.7, title = NULL, position = "bottomright")
    m
  })
  
  chosenVariable <- reactive({
    input$variable
  })
  
  output$PercentMap <- renderLeaflet({
    bins <- c(0, 25, 50, 75, 100)
    pal <- colorBin("YlOrRd", domain = mapVars[,chosenVariable()], bins = bins)
    labels <- sprintf("<strong>%s</strong><br/>%g %%", mapVars$Region.Name, mapVars[,chosenVariable()]
                      ) %>% lapply(htmltools::HTML)
    m <- leaflet(mapVars) %>%
      setView(-96, 38, 4) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      
      addPolygons(data = simpNoPR, fillColor = ~pal(mapVars[,chosenVariable()]), weight = 2, opacity = 1, 
                  color = "white", dashArray = "3", fillOpacity = 0.7, 
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", 
                                               fillOpacity = 0.7,bringToFront = TRUE),
                  label = labels, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                              textsize = "15px", direction = "auto")) %>% 
      addLegend(pal = pal, values = bins, opacity = 0.7, title = NULL, position = "bottomright")
    m
  })
  
}

library(shiny)
library(shinydashboard)
library(DBI)
library(odbc)
library(lubridate)
library(stringr)
library(leaflet)
library(shinyBS)
library(sf)
library(geosphere)

GetWSNZAzureConnection <- function() {
  dbConnect(
    odbc(),
    Driver = "SQL Server",
    Server = "heimatau.database.windows.net",
    Database = "WSNZ",
    Port = 1433,
    Uid = Sys.getenv("WSNZDBUSER"),
    Pwd = Sys.getenv("WSNZDBPASS")
  )
}
all_zones <- st_read("Shapefiles/nz_mainland_and_buffers.shp")
nz_mainland <- all_zones[all_zones$name == "Mainland", ]
zone_1km    <- all_zones[all_zones$name == "Buffer_1km", ]
zone_5km    <- all_zones[all_zones$name == "Buffer_5km", ]



fetchChoices <- function(con, tableName) {
  if (tableName == "Site") {
    return(
      dbGetQuery(
        con,
        "SELECT CONCAT(s.Description, ', ', s1.Description, ', ', s2.Description) AS Description
       FROM Site s
       JOIN Site2 s2 ON s.Site2ID = s2.Site2ID
       JOIN Site1 s1 ON s2.Site1ID = s1.Site1ID
        order by Description ASC"
      )$Description
    )
  } else if (tableName == "Activity") {
    return(
      dbGetQuery(
        con,
        "SELECT CONCAT(a.Description, ', ', a3.Description, ', ', a2.Description, ', ', a1.Description) AS Description
       FROM Activity a
       JOIN Activity3 a3 ON a.Activity3ID = a3.Activity3ID
       JOIN Activity2 a2 ON a2.Activity2ID = a3.Activity2ID
       JOIN Activity1 a1 ON a2.Activity1ID = a1.Activity1ID
        order by Description ASC"
      )$Description
    )
  } else {
    return(dbGetQuery(con,paste("SELECT Description FROM",tableName," order by Description ASC"))$Description)
  }
}

getTableID <- function(con, tableName, description) {
  if (is.null(description) || nchar(description) == 0) {
    description <- "Unknown"
  } else if (tableName == "Site" | tableName == "Activity") {
    description <- str_extract(description, "^[^,]*")
  }
  
  query <-
    sprintf("SELECT %sID FROM %s WHERE Description = ?", tableName, tableName)
  result <- tryCatch({
    dbGetQuery(con, query, params = list(description))
  }, error = function(e) {
    print(paste("Error in getTableID for", tableName, ":", e$message))
    return(NULL)
  })
  
  if (is.null(result) || nrow(result) == 0) {
    print(paste("No result found for Description =",description,"in table",tableName))
    return(NULL)
  } else {
    return(result[[1]])
  }
}




ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(tags$head(tags$style(
    HTML(
      '/* Custom styles for full height layout */
      .content-wrapper, .right-side { height: 100vh; display: flex; flex-direction: column; overflow: hidden; }
      .content { flex-grow: 1; display: flex; flex-direction: column; overflow: auto; }
      .tab-pane { flex-grow: 1; display: flex; flex-direction: column; overflow: hidden; }
      .tab-content { flex-grow: 1; display: flex; flex-direction: column; padding: 10px; box-sizing: border-box; overflow: auto; }
      .box.box-solid { flex-grow: 1; display: flex; flex-direction: column; overflow: hidden; }
      .form-content { flex-grow: 1; display: flex; flex-direction: column; overflow: hidden; width: 100%; }
            #maxDrowningID { font-size: 24px; font-weight: bold; } /* Custom style for larger text */'
    )
  )),
  
  fluidPage( #page that holds 3 big columns that contain fluidRows, Columns as                                                                                                                                                                                                                       well as other input fields
    textOutput("maxDrowningID"),
    tags$div(style = "height: 30px;"),
    fluidRow(
      
      
      column(3, # This is the larger "row column" that holds the first "column" of the form
             fluidRow(
               column(6, uiOutput("PrimaryDataSourceSelect")),
               column(6, textInput("InquestValue", "Inquest Number", width = "100%"))),
             
             fluidRow(
               column(5, checkboxInput("CoronialFinding", "Coroner Report", value = FALSE,width = "100%")),
               column(3, checkboxInput("Missing", "Missing", value = FALSE, width = "100%")), 
               column(4, checkboxInput("Presumed","Presumed", value = FALSE, width = "100%"))),
             
             
             fluidRow(
               column(6, 
                      tags$div(
                        tags$label("Date"),
                        tags$span(class = "glyphicon glyphicon-info-sign", id = "date_info", style = "cursor: pointer; margin-left: 5px;"),
                        bsTooltip("date_info", "Select fatality date, if unknown, use the earliest report date from the source", placement = "right", trigger = "hover"),
                        dateInput("date", NULL,  max = Sys.Date(), width = "100%"))), 
               column(6, selectInput("Time", "Time", choices = c("", 0:23), selected = "", width = "100%" ))),
             
             
             textInput("FamilyName", "Family Name", width = "100%"),
             textInput("GivenName", "Given Names", width = "100%"),
             
             fluidRow(
               column(6, 
                      tags$div(
                        tags$label("Date of Birth"),
                        tags$span(class = "glyphicon glyphicon-info-sign",id = "dob_info", style = "cursor: pointer; margin-left: 5px;"),
                        bsTooltip( "dob_info", "Enter the date of birth in format yyyy-mm-dd",placement = "right",trigger = "hover"),
                        dateInput(inputId = "dob",label = NULL, value ="", max = Sys.Date(), width = "100%"))),
               column(6, numericInput( "age", "Age", value = NULL, min = 0, width = "100%"))),
             
             fluidRow(
               column(6, uiOutput("EthnicitySelect")), 
               column(6, uiOutput("SubEthnicitySelect"))),
             
             fluidRow(
               column(6, uiOutput("SexSelect")), 
               column(6, uiOutput("ImmigrationStatusSelect"))),
             
             fluidRow(
               column(6, textInput("ResidenceAddress", "Residence Address", width = "100%")), 
               column(6, uiOutput("ResidentSelect"))),
             
             fluidRow(
               column(12, uiOutput("CountrySelect")))
      ),
      
      
      column(3, # This is the larger "row column" that holds the second "column" of the form
             fluidRow(
               column(12, uiOutput("LocationSelect"))),
             
             fluidRow(
               column(12, textInput("Location", "Location Description", width = "100%"))),
             
             fluidRow(
               column(6, numericInput( "Lat", "Latitude", value = NULL, max = 37, min = -48)), 
               column(6, numericInput("Lon", "Longitude", value = NULL))),
             
             tags$div(style = "height: 30px;"),
             leafletOutput("map", width = "100%", height = "300px"),
             tags$div(style = "height: 10px;"),
             
             textOutput("distanceText"),
             conditionalPanel(
               condition = "input.SiteSelect.includes(' Shore')",
               checkboxInput("calcDistance", "Calculate Distance from Coast", value = FALSE)
             ),
             uiOutput("SiteSelect"),
             uiOutput("ActivitySelect"),
      ),
      
      column(5,# This is the larger "row column" that holds the third "column" of the form
             fluidRow(
               column(3,numericInput( "NoFatalities", "No.Fatalities", value = 1, width = "100%")), 
               column(9, uiOutput("FatalityTypeSelect"))),
             
             fluidRow(
               column(12,
                      tags$label("Relevant Medical Event"),
                      tags$span( class = "glyphicon glyphicon-info-sign", id = "MedicalEvent", style = "cursor: pointer; margin-left: 5px;"),
                      bsTooltip("MedicalEvent", "Medical Event that was/must be a SIGNIFICANT factor in the fatality, otherwise leave blank", placement = "right", trigger = "hover"),
                      textInput("MedicalCondition", NULL , width = "100%"))),
             
             textAreaInput("Synopsis", "Synopsis", width = "100%", rows = 5),
             fluidRow(
               column(4, uiOutput("AlcoholSelect")),
               column(4, numericInput("BloodAlcohol", "Blood Alcohol", value = 0, width = "100%")),
               column(4, uiOutput("DrugsSelect"))),
             
             fluidRow(
               column(8, uiOutput("BuoySelect")),
               column(4, numericInput("BoatLength","Boat Length", value = 0, width = "100%" ))),
             
             fluidRow(
               column(4, uiOutput("HypothermiaSelect")),
               column(4, uiOutput("ResusSelect")),
               column(4, uiOutput("RescueSelect"))),
             
             fluidRow(
               column(4, uiOutput("PoolFenceSelect")),
               column(4, uiOutput("ChildSupervisionSelect")),
               column(4, uiOutput("PurposeSelect"))),
             
             fluidRow(
               column(4, checkboxInput("rip", "Rip", value = FALSE, width = "100%")),
               column(4, checkboxInput("Commercial","Commercial",value = FALSE, width = "100%" )),
               column(4, checkboxInput("BeachPatrolled","Beach Patrolled", value = FALSE, width = "100%"))),
             tags$div(style = "height: 20px;"),
             textOutput("Query"),
             bsModal("modalExample", "Business Rules Check", "continue", size = "big",
                     uiOutput("modalContent"),
                     actionButton("insert", "Go to Insert")
             ),
             actionButton("continue", "Continue"),
             actionButton("reset", "Reset")
             
             
      )
    )
  ))
)


server <- function(input, output, session) {
  values <- reactiveValues(
    rules_error = list(),
    rules_warning = list()
  )
  
  con <- GetWSNZAzureConnection()
  
  EthnicityQuery <- "SELECT 
           s.SubEthnicityID,
           s.EthnicityID,
           s.Description AS SubethnicityDescription,
           e.Description AS MainEthnicityDescription
           FROM 
           Subethnicity s
           JOIN 
           Ethnicity e ON s.EthnicityID = e.EthnicityID;"
  
  EthnicityData <- dbGetQuery(con, EthnicityQuery)
  
  EthnicityData <- rbind(EthnicityData, data.frame(
    EthnicityID = 0,
    SubEthnicityID = 0,
    SubethnicityDescription = "Unknown",
    MainEthnicityDescription = "Unknown"))
  #EthnicityData = EthnicityData[EthnicityData$MainEthnicityDescription != "Unknown",]
  MultiQuery <- "  WITH CountGreaterThanOne AS (
        SELECT [DrowningID], [FamilyName], [GivenNames], [NumberofFatalities], [Date]
        FROM Drowning
        WHERE [NumberofFatalities] > 1)
        
        SELECT c.[DrowningID], c.[FamilyName], c.[GivenNames], c.[Date], c.[NumberofFatalities], COUNT(t.[GivenNames]) AS ExistingRows
        FROM CountGreaterThanOne c
        LEFT JOIN Drowning t ON c.[Date] = t.[Date] AND t.[NumberofFatalities] = c.[NumberofFatalities]
        GROUP BY c.[DrowningID], c.[FamilyName], c.[GivenNames], c.[Date], c.[NumberofFatalities]
		    ORDER BY DrowningID DESC; "
  
  MultiData <- dbGetQuery(con, MultiQuery)
  
  id = dbGetQuery(con,"select top(1) DrowningID from Drowning order by DrowningID Desc")
  
  maxDrowningID <- reactiveVal(id + 1)
  
  
  output$maxDrowningID <- renderText({
    paste("Inserting DrowningID:", maxDrowningID())
  })
  
  
  
  output$PrimaryDataSourceSelect <- renderUI({
    selectInput("PrimaryDataSourceSelect","Primary Data Source",choices = fetchChoices(con, "PrimaryDataSource"), selected = "Unknown",width = '100%')
  })
  
  selectedprimaryDataSourceID <- reactive({
    getTableID(con, "PrimaryDataSource", input$PrimaryDataSourceSelect)
  })
  
  output$SexSelect <- renderUI({
    selectInput("SexSelect","Sex",choices = fetchChoices(con, "Sex"),selected = "Unknown",width = '100%')
  })
  
  selectedSexID <- reactive({
    getTableID(con, "Sex", input$SexSelect)
  })
  
  observeEvent(input$EthnicitySelect, {
    selectedEthnicity <- input$EthnicitySelect
    if (selectedEthnicity == "Unknown") {
      subEthnicityChoices <- EthnicityData$SubethnicityDescription
    } else {
      subEthnicityChoices <- EthnicityData$SubethnicityDescription[EthnicityData$MainEthnicityDescription == selectedEthnicity]
    }
    updateSelectInput(session, "SubEthnicitySelect", choices = subEthnicityChoices, selected = input$SubEthnicitySelect)
  })
  
  observeEvent(input$SubEthnicitySelect, {
    selectedSubEthnicity <- input$SubEthnicitySelect
    mainEthnicity <- EthnicityData$MainEthnicityDescription[EthnicityData$SubethnicityDescription == selectedSubEthnicity]
    updateSelectInput(session, "EthnicitySelect", selected = mainEthnicity)
  })
  
  selectedEthnicityID <- reactive({
    getTableID(con, "Ethnicity", input$EthnicitySelect)
  })
  
  selectedSubEthnicityID <- reactive({
    getTableID(con, "SubEthnicity", input$SubEthnicitySelect)
  })
  
  output$EthnicitySelect <- renderUI({
    selectInput("EthnicitySelect", "Ethnicity", 
                choices = EthnicityData$MainEthnicityDescription, 
                selected = "Unknown", width = '100%')
  })
  
  # Initial Sub-Ethnicity choices
  output$SubEthnicitySelect <- renderUI({
    selectInput("SubEthnicitySelect", "Sub-Ethnicity", 
                choices = EthnicityData$SubethnicityDescription, selected = "Unknown", width = '100%')
  })
  
  
  output$ImmigrationStatusSelect <- renderUI({
    selectInput("ImmigrationStatusSelect","Immigration Status", choices = fetchChoices(con, "ImmigrationStatus"), selected = "Unknown", width = '100%')
  })
  
  selectedImmigrationStatusID <- reactive({
    getTableID(con, "ImmigrationStatus", input$ImmigrationStatusSelect)
  })
  
  output$ResidentSelect <- renderUI({
    selectInput("ResidentSelect","Residence Region",choices = fetchChoices(con, "Region"),selected = "Unknown",width = '100%')
  })
  
  selectedResidenceID <- reactive({
    getTableID(con, "Region", input$ResidentSelect)
  })
  
  output$CountrySelect <- renderUI({
    selectInput("CountrySelect", "Residence Country",choices = fetchChoices(con, "Country"),selected = "New Zealand",width = '100%')
  })
  
  selectedCountryID <- reactive({
    getTableID(con, "Country", input$CountrySelect)
  })
  
  output$LocationSelect <- renderUI({
    selectInput("LocationSelect","Location",choices = fetchChoices(con, "Location"),selected = "Unknown", width = '100%')
  })
  
  selectedLocationID <- reactive({
    getTableID(con, "Location", input$LocationSelect)
  })
  
  output$SiteSelect <- renderUI({
    selectInput("SiteSelect","Site",choices = fetchChoices(con, "Site"),selected = "Unknown, Unknown, Unknown",width = '100%')
  })
  
  selectedSiteID <- reactive({
    getTableID(con, "Site", input$SiteSelect)
  })
  
  output$ActivitySelect <- renderUI({
    selectInput("ActivitySelect","Activity",choices = fetchChoices(con, "Activity"), selected = "Unknown, Unknown, Unknown, Unknown" ,width = '100%')
  })
  
  selectedActivityID <- reactive({
    getTableID(con, "Activity", input$ActivitySelect)
  })
  
  output$FatalityTypeSelect <- renderUI({
    selectInput("FatalityTypeSelect","Fatality Type",choices = fetchChoices(con, "FatalityType"),selected = "Unknown", width = '100%')
  })
  
  selectedFatalityTypeID <- reactive({
    getTableID(con, "FatalityType", input$FatalityTypeSelect)
  })
  
  output$BuoySelect <- renderUI({
    selectInput("BuoySelect","Buoyancy",choices = fetchChoices(con, "Buoyancy"),selected = "Not Applicable",width = '100%')
  })
  
  selectedBuoyID <- reactive({
    getTableID(con, "Buoyancy", input$BuoySelect)
  })
  
  output$AlcoholSelect <- renderUI({
    selectInput("AlcoholSelect","Alcohol Or Drugs",choices = fetchChoices(con, "AlcoholDrug"),selected = "Unknown",width = '100%')
  })
  
  selectedAlcoholID <- reactive({
    getTableID(con, "AlcoholDrug", input$AlcoholSelect)
  })
  
  output$DrugsSelect <- renderUI({
    selectInput("DrugsSelect","Drugs Status",choices = fetchChoices(con, "Drug"),selected = "Unknown",width = '100%')
  })
  
  selectedDrugsID <- reactive({
    getTableID(con, "Drug", input$DrugsSelect)
  })
  
  output$HypothermiaSelect <- renderUI({
    selectInput("HypothermiaSelect","Hypothermia Status",choices = fetchChoices(con, "Hypothermia"),selected = "Unknown",width = '100%')
  })
  
  selectedHypothermiaID <- reactive({
    getTableID(con, "Hypothermia", input$HypothermiaSelect)
  })
  
  output$ResusSelect <- renderUI({
    selectInput("ResusSelect","Resuscitation", choices = fetchChoices(con, "Resuscitation"), selected = "Unknown",width = '100%')
  })
  
  selectedResusID <- reactive({
    getTableID(con, "Resuscitation", input$ResusSelect)
  })
  
  output$RescueSelect <- renderUI({
    selectInput("RescueSelect","Rescue",choices = fetchChoices(con, "Rescue"),selected = "Unknown",width = '100%')
  })
  
  selectedRescueID <- reactive({
    getTableID(con, "Rescue", input$RescueSelect)
  })
  
  output$PoolFenceSelect <- renderUI({
    selectInput("PoolFenceSelect","Pool Fence",choices = fetchChoices(con, "PoolFenceType"),selected =  "Not Applicable", width = '100%')
  })
  
  selectedPoolFenceID <- reactive({
    getTableID(con, "PoolFenceType", input$PoolFenceSelect)
  })
  
  output$ChildSupervisionSelect <- renderUI({
    selectInput("ChildSupervisionSelect","Child Supervision",choices = fetchChoices(con, "ChildSupervision"),selected = "Not Applicable",width = '100%')
  })
  
  selectedChildSupervisionID <- reactive({
    getTableID(con, "ChildSupervision", input$ChildSupervisionSelect)
  })
  
  observe({
    
    if ( !is.na(input$date) && length((input$date))>0 && !is.na(input$dob) && length((input$dob))>0  ) {
      age <- floor(time_length(interval(input$dob, input$date), "years"))
      updateNumericInput(session, "age", value = age)
      
    }
  })
  
  observeEvent(c(input$Lat, input$Lon), {
    if (!is.na(input$Lat) && !is.na(input$Lon) && input$Lat != 0 && input$Lon != 0) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(lng = input$Lon, lat = input$Lat) %>%
        setView(lng = input$Lon, lat = input$Lat, zoom = 14) # Set zoom level to 14 or desired level
    }
  })
  
  
  
  
  
  output$PurposeSelect <- renderUI({
    selectInput("PurposeSelect","Purpose",choices = fetchChoices(con, "Purpose"),selected = "Unknown",width = '100%')
  })
  
  selectedPurposeID <- reactive({
    getTableID(con, "Purpose", input$PurposeSelect)
  })
  
  insertStatement <- reactiveVal("")
  
  generateInsertStatement <- reactive({
    InquestValue <- 
      ifelse(
        is.na(input$InquestValue) || input$InquestValue == "",  #if condition
        "NULL", sprintf("'%s'", gsub("'", "''", input$InquestValue)))  #resolutions
    
    FamilyNameValue <- 
      ifelse(
        is.na(input$FamilyName) || input$FamilyName == "",
        "NULL", sprintf("'%s'", gsub("'", "''", str_to_upper(input$FamilyName))))
    
    GivenNameValue <- 
      ifelse(
        is.na(input$GivenName) || input$GivenName == "",
        "NULL", sprintf("'%s'", gsub("'", "''", input$GivenName)))
    
    ResidenceAddressValue <- 
      ifelse(
        is.na(input$ResidenceAddress) || input$ResidenceAddress == "",
        "NULL", sprintf("'%s'", gsub("'", "''", input$ResidenceAddress)))
    
    BoatLength <-
      ifelse(
        is.na(input$BoatLength) || input$BoatLength == 0,
        "NULL", sprintf("%s", input$BoatLength))
    
    locationValue <- 
      ifelse(
        is.na(input$Location) || input$Location == "",
        "NULL", sprintf("'%s'", gsub("'", "''", input$Location)))
    
    SynopsisValue <- 
      ifelse(
        is.na(input$Synopsis) || input$Synopsis == "",
        "NULL", sprintf("'%s'", gsub("'", "''", input$Synopsis)))
    
    MedicalCondition <- 
      ifelse(
        is.na(input$MedicalCondition) || input$MedicalCondition == "",
        "NULL",sprintf("'%s'", gsub("'", "''", input$MedicalCondition)))
    
    noFatal <-
      ifelse(
        is.na(input$NoFatalities), 
        0, input$NoFatalities)
    
    lon <- 
      ifelse(
        is.na(input$Lon),
        0, input$Lon)
    
    lat <- 
      ifelse(
        is.na(input$Lat), 
        0, input$Lat)
    
    CoronialFinding <- 
      ifelse(
        input$CoronialFinding, 
        1, 0)
    
    Commercial <- 
      ifelse(
        input$Commercial, 
        1, 0)
    
    BloodAlcohol <-
      ifelse(
        is.na(input$BloodAlcohol) || input$BloodAlcohol == 0,
        "NULL", sprintf("%s", input$BloodAlcohol))
    
    MedicalCondition <-
      ifelse(
        is.na(input$MedicalCondition) || input$MedicalCondition == "",
        "NULL", sprintf("'%s'", input$MedicalCondition))
    
    Missing <- 
      ifelse(
        input$Missing, 
        1, 0)
    
    Presumed <- 
      ifelse(
        input$Presumed, 
        1, 0)
    
    BeachPatrolled <- 
      ifelse(
        input$BeachPatrolled, 
        1, 0)
    
    rip <- 
      ifelse(
        input$rip, 
        1, 0)
    
    dateValue <- 
      sprintf("'%s'", input$date)
    
    locationValue <-
      ifelse(
        is.na(input$Location) || input$Location == "",
        "NULL", sprintf("'%s'", input$Location))
    
    SynopsisValue <-
      ifelse(
        is.na(input$Synopsis) || input$Synopsis == "",
        "NULL", sprintf("'%s'", gsub("'", "''", input$Synopsis)))
    
    Time <-
      ifelse(
        is.na(input$Time) || input$Time == "",
        "NULL", sprintf("%s", input$Time))
    
    DOBValue = sprintf("'%s'", input$dob)
    
    AgeValue <- 
      ifelse(
        is.na(input$age), 
        "NULL", input$age)
    
    
    
    if (selectedActivityID() %in% c(36:42, 0)) {
      prev <- 0
    } else {
      prev <- 1
    }
    
    
    insertStatement(
      paste(
        "INSERT INTO Drowning VALUES (",
        maxDrowningID(),
        ", ",
        selectedprimaryDataSourceID(),
        ", ",
        CoronialFinding,
        ", ",
        dateValue,
        ", ",
        InquestValue,
        ", ",
        FamilyNameValue,
        ", ",
        GivenNameValue,
        ", ",
        DOBValue,
        ", ",
        AgeValue,
        ", ",
        selectedSexID(),
        ", ",
        selectedEthnicityID(),
        ", ",
        selectedSubEthnicityID(),
        ", ",
        selectedImmigrationStatusID(),
        ", ",
        ResidenceAddressValue,
        ", ",
        selectedResidenceID(),
        ", ",
        selectedCountryID(),
        ", ",
        locationValue,
        ", ",
        selectedLocationID(),
        ", ",
        SynopsisValue,
        ", ",
        selectedSiteID(),
        ", ",
        rip,
        ", ",
        selectedActivityID(),
        ", ",
        BoatLength,
        ", ",
        selectedFatalityTypeID(),
        ", ",
        noFatal,
        ", ",
        selectedBuoyID(),
        ", ",
        MedicalCondition,
        ", ",
        selectedHypothermiaID(),
        ", ",
        selectedAlcoholID(),
        ", ",
        BloodAlcohol,
        ", ",
        selectedDrugsID(),
        ", ",
        selectedResusID(),
        ", ",
        selectedRescueID(),
        ", ",
        BeachPatrolled,
        ", ",
        selectedPoolFenceID(),
        ", ",
        selectedChildSupervisionID(),
        ", ",
        Commercial,
        ", ",
        selectedPurposeID(),
        ", ",
        Time,
        ", ",
        lat,
        ", ",
        lon,
        ", ",
        Missing,
        ", ",
        Presumed,
        ", ",
        prev,
        
        ")"
        
      )
    )
  })
  
  observeEvent(input$SiteSelect, {
    if (!grepl("Offshore", input$SiteSelect, ignore.case = TRUE)) {
      updateCheckboxInput(session, "calcDistance", value = FALSE)
    }
  })
  
  
  distance_zone <- reactive({
    req(input$Lat, input$Lon, input$SiteSelect, input$calcDistance)
    
    # Only calculate if "Offshore" is in site and user checked the box
    if (!grepl(" Shore", input$SiteSelect, ignore.case = TRUE) || !isTRUE(input$calcDistance)) {
      return(NULL)
    }
    
    point <- st_sfc(st_point(c(input$Lon, input$Lat)), crs = 4326)
    
    if (st_within(point, nz_mainland, sparse = FALSE)) {
      return("Inside the main coastline of New Zealand.")
    } else if (st_within(point, zone_1km, sparse = FALSE)) {
      return("0–1 km from the New Zealand coastline.")
    } else if (st_within(point, zone_5km, sparse = FALSE)) {
      return("1–5 km from the New Zealand coastline.")
    } else {
      return("More than 5 km from the New Zealand coastline.")
    }
  })
  
  output$distanceText <- renderText({
    if (!is.na(input$Lat) && !is.na(input$Lon) && input$Lat != 0 && input$Lon != 0) {
      if (!grepl(" Shore", input$SiteSelect, ignore.case = TRUE)) {
        return("Distance from coast is only calculated for offshore sites.")
      } else if (isTRUE(input$calcDistance)) {
        return(distance_zone())
      } else {
        return("")  # Don't show anything unless checkbox is ticked
      }
    } else {
      return("")
    }
  })
  
  
  
  
  
  observeEvent(input$continue, { ## ALL BUSINESS RULES ARE (meant to be) HERE
    
    values$rules_error <- list()
    values$rules_warning <- list()
    
    ## 1: If we have DOB and date, age should be the same
    if(!is.na(input$date) && !is.na(input$date) && length(input$date)>0 && length(input$dob)>0){
      
      dob <- as.Date(input$dob)
      today <- as.Date(input$date)
      calculated_age <- as.period(interval(dob, today), unit = "years")$year
      
      if (is.na(input$age) || as.numeric(input$age) != calculated_age) {
        values$rules_error <- append(values$rules_error, "Age is inconsistent with the given Date of Birth")
      }
    }
    print("Successfully checked rule 1: Age and DOB consistency")
    
    ## 2: if the word rip or riptide is in the synopsis the rip box should be ticked
    if (!is.na(input$Synopsis)) {
      synopsis <- tolower(input$Synopsis)
      rip_pattern <- "\\brip\\b|\\bripti\\b"
      
      if (grepl(rip_pattern, synopsis, ignore.case = TRUE) && !isTRUE(input$rip)) {
        values$rules_warning <- append(
          values$rules_warning,
          "Rip/riptide found in synopsis, but rip not ticked as involved in incident"
        )
      }
    }
    print("Successfully checked rule 2: Rip/riptide in synopsis")
    ## 3: if selectedprimaryDataSourceID() returns 2, append a specific message
    if (input$PrimaryDataSourceSelect == "NZHIS") {
      values$rules_error <- append(values$rules_error, "PrimaryDataSource should not be NZHIS")
    }
    print("Successfully checked rule 3: PrimaryDataSource is not NZHIS")
    
    ## 4: if selectedprimaryDataSourceID() == 4 and InquestValue is an empty character string or NULL
    if (input$PrimaryDataSourceSelect == "Coroner" && (is.null(input$InquestValue) || input$InquestValue == "")) {
      values$rules_error <- append(values$rules_error, "PrimaryDataSource is Coroner, provide an inquest number")
    }
    
    print("Successfully checked rule 4: Coroner and Inquest number")
    
    
    ## 5: if dateValue is in the future
    ## 6: if date is over 3 months ago from today
    # Assume date is valid initially
    if (!is.null(input$date) && !is.na(input$date) && length(input$date) != 0) {
      
      # Convert input$date to a Date object safely
      date_input <- tryCatch(as.Date(input$date), error = function(e) NA)
      
      if (!is.na(date_input)) {
        # 7: If dateValue is in the future
        if (date_input > Sys.Date()) {
          values$rules_error <- append(values$rules_error, "Date is in the future")
        }
        
        # 8: If date is over 3 months ago from today
        if (date_input < Sys.Date() - months(3)) {
          values$rules_warning <- append(values$rules_warning, "Date is over 3 months old")
        }
      } else {
        values$rules_error <- append(values$rules_error, "Invalid date format")
      }
    } else {
      values$rules_error <- append(values$rules_error, "Date is missing")
    }
    
    print("Successfully checked rule 5/6: Date validity")
    ## 9: if either familyname or givenname is empty while the other one has an entry
    if ((input$FamilyName == "" && input$GivenName != "") || (input$FamilyName != "" && input$GivenName == "")) {
      values$rules_warning <- append(values$rules_warning, "One of the name fields is empty, while the other one is not")
    }
    print("Successfully checked rule 9: Name field consistency")
    # ## 10: if date of birth (input$dob) is within the last 6 months from today
    # if (!is.na(input$dob) && length(as.character.Date(input$dob)) && input$dob > Sys.Date() - months(6)) {
    #   values$rules_warning <- append(values$rules_warning, "Age of deceased is under 6 months, is this correct?")
    # } 
    print("Removed rule 10")
    
    ## 11: if input$age is negative
    if (!is.na(input$age) && input$age < 0) {
      values$rules_error <- append(values$rules_error, "Age is a negative value")
    }
    
    ## 12: if input$age is over 100
    else if (!is.na(input$age) && input$age > 100) {
      values$rules_warning <- append(values$rules_warning, "Age is over 100, are you sure?")
    }
    print("Successfully checked rule 11/12: Age")
    
    ## 13: if selectedSexID() == 0
    if (input$SexSelect == "Unknown") {
      values$rules_warning <- append(values$rules_warning, "Are you sure selected Sex is Unknown?")
    }
    print("Successfully checked rule 13: Sex selection")
    ## 14: if the words alcohol or drug are in the synopsis, AlcoholSelect should not be "Unknown" or "Not Involved"
    if (!is.na(input$Synopsis)) {
      synopsis <- tolower(input$Synopsis)
      
      # Check for alcohol mentions
      if (grepl("alcohol", synopsis)) {
        if (input$AlcoholSelect == "Unknown") {
          values$rules_warning <- append(
            values$rules_warning,
            "Alcohol mentioned in synopsis, but Alcohol/Drug field is 'Unknown'"
          )
        } else if (input$AlcoholSelect == "Not Involved") {
          values$rules_warning <- append(
            values$rules_warning,
            "Alcohol mentioned in synopsis, but Alcohol/Drug field is 'Not Involved'"
          )
        }else if (input$AlcoholSelect == "Drugs Involved") {
          values$rules_warning <- append(
            values$rules_warning,
            "Alcohol mentioned in synopsis, but Drug field is 'Drugs Involved'"
          )
        }
      }
      
      # Check for drug mentions
      if (grepl("drug", synopsis)) {
        if (input$AlcoholSelect == "Unknown") {
          values$rules_warning <- append(
            values$rules_warning,
            "Drug mentioned in synopsis, but Drug field is 'Unknown'"
          )
        } else if (input$AlcoholSelect == "Not Involved") {
          values$rules_warning <- append(
            values$rules_warning,
            "Drug mentioned in synopsis, but Drug field is 'Not Involved'"
          )
        }else if (input$AlcoholSelect == "Alcohol Involved") {
          values$rules_warning <- append(
            values$rules_warning,
            "Drug mentioned in synopsis, but Drug field is 'Alcohol Involved'"
          )
        }
      }
    }
    print("Successfully checked rule 14: Alcohol and Drug mentions in synopsis")
    
    
    ## 15: if input$SiteSelect is "Unknown, Unknown, Unknown" check that is correct
    if (input$SiteSelect == "Unknown, Unknown, Unknown") {
      values$rules_warning <- append(values$rules_warning, "Are you sure Site is unknown")
    }
    print("Successfully checked rule 15: Site selection as unknown")
    
    ## 16: if input$Location is "Unknown"
    if (is.na(input$Location)) {
      values$rules_warning <- append(values$rules_warning, "Are you sure Location Description is unknown")
      
    }
    print("Successfully checked rule 16: Location selection as unknown")
    
    if (input$LocationSelect == "Unknown") {
      values$rules_warning <- append(values$rules_warning, "Are you sure Location is unknown")
    }
    
    # ## 17: if input$ethnicity is "Unknown"
    # if (input$EthnicitySelect == "Unknown") {
    #   values$rules_errors <- append(values$rules_warning, "Ethnicity can not be unknown")
    # }
    # print("Successfully checked rule 17: Ethnicity selection as unknown")
    # 
    ## 18: if input$Activity is "Unknown, Unknown, Unknown"
    ## 19: if input$ActivitySelect is "Unknown, Unknown, Unknown" then the checkbox presumed should be ticked
    
    if (input$ActivitySelect == "Unknown, Unknown, Unknown, Unknown") {
      
      if(!input$Presumed){
        values$rules_warning <- append(values$rules_warning, "Are you sure Activity is unknown and presumed is not ticked?")
        
      }else{
        values$rules_warning <- append(values$rules_warning, "Are you sure Activity is unknown?")
        
      }
    }
    print("Successfully checked rule 18/19: Activity selection and presumed checkbox")
    
    
    ## 20: if input$boatlength is less than 0 or greater than 100
    if (!is.na(input$BoatLength)) {
      if (input$BoatLength < 0 || input$BoatLength > 100) {
        values$rules_warning <- append(values$rules_warning,
                                       "Boatlength is over 100 or less than 0, are you sure?")
      }
    }
    print("Successfully checked rule 20: Activity is unknown with presumed checkbox")
    
    ## 21: if input$noFatalities is less than or equal to 0
    if(is.na(input$NoFatalities)){
      values$rules_error <- append(values$rules_error, "Missing Number of Fatalities")
      
    }else if (input$NoFatalities <= 0) {
      values$rules_error <- append(values$rules_error, "Fatalities is 0 or less")
    }
    print("Successfully checked rule 21: Number of Fatalities")
    
    ## 22: if input$NoFatalities is > 1 then selectedFatalityTypeID must be 4, 5, 6, or 7
    if (!is.na(input$NoFatalities) && input$NoFatalities > 1 &&
        !(input$FatalityTypeSelect %in% c(
          "Multiple with Survivors",
          "Multiple with Bystanders",
          "Multiple with Survivors & Bystanders",
          "Multiple without Survivors",
          "Unknown"))) {
      
      values$rules_error <- append(values$rules_error,
                                   "Fatalities is >1 but Fatality Type is not multiple")
    }
    
    ## Check for single fatality
    if (!is.na(input$NoFatalities) && input$NoFatalities == 1 && 
        !(input$FatalityTypeSelect %in% c(
          "Single with Survivors",
          "Single with Bystanders",
          "Single with Survivors & Bystanders",
          "Single and Alone",
          "Unknown"))) {
      
      values$rules_error <- append(
        values$rules_error,
        "Fatalities is 1 but Fatality Type is not single"
      )
    }
    print("Successfully checked rule 22: Fatality type")
    
    
    ## 23: if the word "pool" (case insensitive) is in the synopsis
    if (!is.na(input$Synopsis)) {
      synopsis <- tolower(input$Synopsis)
      if (grepl("pool", synopsis)) {
        values$rules_warning <- append(values$rules_warning,
                                       "Was there a home pool involved in this incident?")
      }
    }
    print("Successfully checked rule 23: Pool")
    ## 24: if selectedSiteID() isnt 28, then pool fence should not be "Not Applicable"
    if (input$SiteSelect == "Home Pools, Artifical Water, Home Pools" && input$PoolFenceSelect == "Not Applicable") {
      values$rules_error <- append(values$rules_error, "Site is a pool, pool fence is marked as 'Not Applicable'")
    }
    
    ## 25: if selectedSiteID() is not 17, pool fence should be 'Not Applicable'
    if (input$SiteSelect != "Home Pools, Artifical Water, Home Pools" && input$PoolFenceSelect != "Not Applicable") {
      values$rules_error <- append(values$rules_error, "Site is not a pool, pool fence should be 'Not Applicable'")
    }
    print("Successfully checked rule 24/25: Home Pool")
    ## 26: if input$24hourtime is 0
    if (input$Time == 0) {
      values$rules_warning <- append(values$rules_warning, "Are you sure the time was at midnight?")
    }
    print("Successfully checked rule 26: Time")
    ## 27: if input$ActivitySelect is 40 or 44, then input$commercial should be ticked
    if (grepl("commercial", tolower(input$ActivitySelect)) && !input$Commercial) {
      values$rules_error <- append(values$rules_error,
                                   "Activity is commercial, but commercial is not ticked")
    }
    if (grepl("commercial", tolower(input$PurposeSelect)) && !input$Commercial) {
      values$rules_error <- append(values$rules_error,
                                   "Purpose is commercial, but commercial is not ticked")
    }    
    
    print("Successfully checked rule 27: Commercial")
    
    ## 28: If field AlcoholDrugID is 4 or 2, then Bloodalcohol should not be 0
    if (!is.na(input$BloodAlcohol)) {
      if (input$AlcoholSelect %in% c("Alcohol Involved", "Alcohol & Drugs Involved") &&
          input$BloodAlcohol == 0) {
        values$rules_error <- append(values$rules_error,
                                     "Alcohol indicated as involved, but Bloodalcohol is 0")
      }
    }
    print("Successfully checked rule 28: Blood Alcohol")
    
    
    ## 29: If AlcoholDrugID is 3 or 4 and DrugsID is 0 or 11, then say "Drugs involved but no drug selected"
    if (input$AlcoholSelect %in% c("Drugs Involved", "Alcohol & Drugs Involved")  && input$DrugsSelect %in% c("Unknown", "Not Applicable")) {
      values$rules_error <- append(values$rules_error, "Drugs involved but no drug selected")
    }
    print("Successfully checked rule 29: Drugs")
    
    ## 30: Distances
    if(grepl(" Shore", input$SiteSelect) && !is.na(input$Lat) && !is.na(input$Lon)){
      point <- st_sfc(st_point(c(input$Lon, input$Lat)), crs = 4326)
      
      if (st_within(point, nz_mainland, sparse = FALSE)) {
        statement = ("Inside the main coastline of New Zealand.")
      } else if (st_within(point, zone_1km, sparse = FALSE)) {
        statement = ("0–1 km from the New Zealand coastline.")
      } else if (st_within(point, zone_5km, sparse = FALSE)) {
        statement  =("1–5 km from the New Zealand coastline.")
      } else {
        statement = ("More than 5 km from the New Zealand coastline.")
      }
      
      if(grepl("0 - 1 Km from Shore", input$SiteSelect)){
        if(statement != "0–1 km from the New Zealand coastline."){
          values$rules_error <- append(values$rules_error, "Site is '0 - 1 Km from Shore' but coordinate is not.")
        }
      }else if(grepl("1 - 5 Km from Shore", input$SiteSelect)){
        if(statement != "1–5 km from the New Zealand coastline."){
          values$rules_error <- append(values$rules_error, "Site is '1 - 5 Km from Shore' but coordinate is not.")
        }
      }else if(grepl("5 \\+ Km from Shore", input$SiteSelect)){
        if(statement != "More than 5 km from the New Zealand coastline."){
          values$rules_error <- append(values$rules_error, "Site is '1 - 5 Km from Shore' but coordinate is not.")
        }
      }
    }
    print("Successfully checked rule 30: Distances & Sites")
    
    ## 31: If Bloodalcohol is not 0, then AlcoholDrugID should be 2 or 4
    if (!is.na(input$BloodAlcohol)) {
      if (input$BloodAlcohol != 0 &&
          !(input$AlcoholSelect %in% c("Alcohol Involved", "Alcohol & Drugs Involved"))) {
        values$rules_error <- append(values$rules_error,
                                     "Bloodalcohol is not 0 but Alcohol not indicated as involved")
      }
    }
    
    print("Successfully checked rule 31: Blood Alcohol")
    
    ## 32: If the word Boat is in the activity and boatlength is 0, are you sure?
    if (!is.na(input$BoatLength)) {
      if (grepl("Powered Boat", input$ActivitySelect) && input$BoatLength == 0) {
        values$rules_warning <- append(values$rules_warning,
                                       "Boat involved in activity, but no boat length provided")
      }
    }
    
    print("Successfully checked rule 32: Boat length")
    
    ## 33: Query the database for the Ethnicity ID based on the selected ethnicity
    query <- sprintf("SELECT EthnicityID FROM ethnicity WHERE Description = '%s'", input$EthnicitySelect)
    result <- dbGetQuery(con, query)
    
    if (nrow(result) == 0) {
      values$rules_error <- append(values$rules_error, "Selected Ethnicity not found in the database")
    } else {
      ethnicity_id <- result$EthnicityID
      if(ethnicity_id==0){
        values$rules_error <- append(values$rules_error, "Ethnicity can not be unknown")
        
      }
      # You can now use ethnicity_id for further processing or validation
    }
    print("Successfully checked rule 33: Ethnicity")
    
    ## 34: Multiple fatality checks
    
    if(!is.na(input$NoFatalities)){
      most_recent <- MultiData[1, ]
      number_of_fatalities <- most_recent$NumberofFatalities
      existing_rows <- most_recent$ExistingRows
      if (number_of_fatalities > existing_rows){
        values$rules_warning <- append(values$rules_warning, paste0("Most recent multiple fatality is ", number_of_fatalities, " but the count of records with a multiple fatality on that date is ", existing_rows, " this will be the ", existing_rows +1, " record entered if you add it to that group of fatalities"))
      }
      if (number_of_fatalities < existing_rows){
        values$rules_error <- append(values$rules_error, paste0("Most recent multiple fatality is ", number_of_fatalities, " but the count of records with a multiple fatality on that date is ", existing_rows, " this will be the ", existing_rows +1, " record entered if you add it to that group of fatalities"))
      }
      if (input$NoFatalities > 1){
        values$rules_warning <- append(values$rules_warning, "You are entering one entry of multiple fatalities")
      }}
    print("Successfully checked rule 24: Number of fatalities")
    
    # 35: unknown AlcoholDrugID should have NULL value not 0
    if (!is.na(input$BloodAlcohol)) {
      if (input$BloodAlcohol == 0 &&
          (input$AlcoholSelect == "Unknown" )) {
        values$rules_error <- append(
          values$rules_error,
          "Blood Alcohol should be NULL not 0 when AlcoholDrug is Unknown"
        )
      }
    }
    
    print("Successfully checked rule 35: DrugAlcohol")
    
    # Now, check if the point is also outside the Chatham Islands
    lat_min <- -48.5584319828943
    lat_max <- -32.7480126035835
    lon_min <- 164.282082584236
    lon_max <- 179.667833005821
    
    
    if (!is.na(input$Lat) & !is.na(input$Lon)) {
      # Check if the point is outside the defined boundary
      if (input$Lat < lat_min ||
          input$Lat > lat_max ||
          input$Lon < lon_min || input$Lon > lon_max) {
        # If true, the point is not within the boundaries of New Zealand or the Chatham Islands
        values$rules_warning <- append(values$rules_warning, "Check your coordinates.")
      }
    }else if (is.na(input$Lat) & is.na(input$Lon)){
      values$rules_warning <- append(values$rules_warning,"Missing coordinates.")
      
    }
    
    
    print("Successfully checked rule 36: Coordinates")
    
    
    output$modalContent <- renderUI({
      content <- list()
      
      if (length(values$rules_warning) > 0) {
        content <- c(content, lapply(values$rules_warning, function(warning) {
          p(style = "color: #FFD700;", warning)  # Darker yellow
        }))
      }
      
      if (length(values$rules_error) > 0) {
        content <- c(content, lapply(values$rules_error, function(error) {
          p(style = "color: red;", error)
        }))
      }
      
      if (length(content) == 0) {
        content <- p(style = "color: green;", "All business rules are satisfied.")
      }
      
      tagList(content)
    })
    
    
  })
  
  
  observeEvent(input$insert, {
    
    if (length(values$rules_error) > 0) {
      print("Rules errors detected, please fix before inserting")
      
      showModal(modalDialog(
        title = "Business Rules Check",
        "Rules errors detected, please fix before inserting.",
        easyClose = TRUE,
        footer = NULL
      ))
      
    } else {
      
      removeModal()
      
      print("Attempting Insert now, please wait . . .")
      
      tryCatch({
        
        print("Generating Connection ...")
        con <- GetWSNZAzureConnection()
        
        
        Sys.sleep(0.5) # allow for con to reestablish
        
        print("Connection established to: ")
        print(con@info$dbname)
        
        print("Generating Insert query")
        generateInsertStatement()
        
        insert_query <- insertStatement()
        
        insert_query <- gsub(",[[:space:]]*,", ",NULL,", insert_query)
        
        print(length(insert_query))
        print(insert_query)
        print(paste("Executing Insert Query: ", insert_query))  # Debug statement
        
        #dbExecute(con, insert_query)
        
        showNotification("Successfully inserted record", type = "message")
        
        # new_id <- dbGetQuery(con, "select top(1) DrowningID from Drowning order by DrowningID Desc")
        # Sys.sleep(5)
        #print("Insert successful")  # Debug statement
        
        session$reload()
        
      }, error = function(e) {
        
        print(paste("Error inserting record:", e$message))  # Debug statement
        Sys.sleep(5)
        showNotification("Insert not successful", type = "message")
        
        print(insertStatement())  # Debug statement
        
        
      })
    }
  })
  
  
  
  observeEvent(input$reset, {
    session$reload()
    
    insertStatement("Successfully Reset!") # Reset the insert statement
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 174.785,
              lat = -41.2865,
              zoom = 6) # Center on New Zealand
  })
  
  observeEvent(input$map_click, {
    click <- input$map_click
    updateNumericInput(session, "Lat", value = click$lat)
    updateNumericInput(session, "Lon", value = click$lng)
  })
  
  observeEvent(c(input$Lat, input$Lon), {
    if (!is.na(input$Lat) &&
        !is.na(input$Lon) && input$Lat != 0 && input$Lon != 0 && abs(input$Lat)<=90 && abs(input$Lon)<=180 ) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(lng = input$Lon, lat = input$Lat)
    }
  })
  

  
 
  
}


shinyApp(ui = ui, server = server)
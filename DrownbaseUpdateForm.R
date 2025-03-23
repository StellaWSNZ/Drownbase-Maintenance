library(shiny)
library(shinydashboard)
library(DBI)
library(odbc)
library(lubridate)
library(stringr)
library(shinyBS)
library(leaflet)
library(sf)
library(geosphere)


# Function to establish database connection
GetWSNZAzureConnection <- function() {
  dbConnect(
    odbc(),
    Driver = "SQL Server",
    Server = "heimatau.database.windows.net",
    Database = "wsnz",
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
          JOIN Site1 s1 ON s2.Site1ID = s1.Site1ID"
      )
    )
  } else if (tableName == "Activity") {
    return(
      dbGetQuery(
        con,
        "SELECT CONCAT(a.Description, ', ', a3.Description, ', ', a2.Description, ', ', a1.Description) AS Description
          FROM Activity a
          JOIN Activity3 a3 ON a.Activity3ID = a3.Activity3ID
          JOIN Activity2 a2 ON a2.Activity2ID = a3.Activity2ID
          JOIN Activity1 a1 ON a2.Activity1ID = a1.Activity1ID"
      )
    )
  } else{
    query <- sprintf("SELECT * FROM %s", tableName)
    data <- dbGetQuery(con, query)
    return(data)
  }
}

getTableID <- function(con, tableName, description) {
  if (is.null(description) || nchar(description) == 0) {
    description <- "Unknown"
  } else if (tableName == "Site" | tableName == "Activity") {
    description <- str_extract(description, "^[^,]*")
  }
  
  description = gsub("'", "''", description)
  query <-
    sprintf("SELECT %sID FROM %s WHERE Description = '%s'",
            tableName,
            tableName,
            description)
  data <- dbGetQuery(con, query)
  if (nrow(data) == 0) {
    return(NULL)
  }
  return(data[[1]])
}

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(tags$head(tags$style(
    HTML(
      "
        /* Set the height of the body container to allow for scrolling */
        .content-wrapper, .right-side {
          height: 100vh;
          overflow-y: auto; /* Enables vertical scrolling */
        }
        /* Remove any padding or margins that might cause extra space */
        body, .content-wrapper, .right-side {
          padding-bottom: 0px;
          margin-bottom: 0px;
        }
        /* Ensure the entire app layout uses the full height */
        .wrapper {
          height: 100vh;
          overflow: hidden; /* Prevent horizontal overflow */
        }
      "
    )
  )), fluidPage(
    textOutput("maxDrowningID"), fluidRow(tags$head(
      tags$style(HTML("
    .btn-primary {
      color: white !important; /* Force text color to white */
    }
  "))
    ),column(
      2, wellPanel(
        numericInput(
          "drowningID",
          "Enter DrowningID:",
          value = NA,
          min = 1,
          width = "100px"
        ),
        
        actionButton("submit", "Find DrowningID"),
        textOutput("drowningIDError"),
        textInput(
          "inquest",
          "Enter Inquest Number:",
          value = NA,
          width = "100px"
        ),
        actionButton("submit_inquest", "Find InquestNumber"),
        textOutput("inquestNumberError") ,
        uiOutput("actionButtons")
      )
    ), column(
      10, wellPanel(
        tableOutput("drowningData"),
        uiOutput("formUI"),
        textOutput("ErrorDisplay")
      )
    ))
  ))
)



fetchValidDrowningIDs <- function(con) {
  query <- "SELECT DISTINCT DrowningID FROM drowning"
  dbGetQuery(con, query)$DrowningID
}
fetchValidInquests <- function(con) {
  query <- "SELECT DISTINCT InquestNumber FROM drowning WHERE InquestNumber IS NOT NULL"
  dbGetQuery(con, query)$InquestNumber
}
server <- function(input, output, session) {
  con <- GetWSNZAzureConnection()
  validDrowningIDs <- fetchValidDrowningIDs(con)
  validInquests <- fetchValidInquests(con)
  values <- reactiveValues(rules_error = list(), rules_warning = list())
  
  
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
  
  EthnicityData <- rbind(
    EthnicityData,
    data.frame(
      EthnicityID = 0,
      SubEthnicityID = 0,
      SubethnicityDescription = "Unknown",
      MainEthnicityDescription = "Unknown"
    )
  )
  
  showForm <- reactiveVal(FALSE)
  forminitialised <- reactiveVal(FALSE)
  initialValues <- reactiveValues()
  selectedTimeValue <- reactiveVal(NULL)
  
  
  initialiseDefaults <- function(con, drowningID) {
    if (drowningID %in% validDrowningIDs) {
      data <- fetchDrowningData(con, drowningID)
      if (nrow(data) > 0) {
        initialValues$CoronialFinding <- as.logical(data$CoronialFinding[1])
        initialValues$Date <- data$Date[1]
        initialValues$dob <- data$DateOfBirth[1]
        initialValues$Age <- data$Age[1]
        
        initialValues$Presumed <- as.logical(data$Presumed[1])
        initialValues$Missing <- as.logical(data$Missing[1])
        initialValues$Rip <- as.logical(data$Rip[1])
        initialValues$Commercial <- as.logical(data$Commercial[1])
        initialValues$BeachPatrolled <-
          as.logical(data$BeachPatrolled[1])
        initialValues$PrimaryDataSource <- data$PrimaryDataSource[1]
        initialValues$Reg <- data$ResidenceID[1]
        
        initialValues$Ethnicity <- data$EthnicityID[1]
        initialValues$SubEthnicity <- data$SubEthnicityID[1]
        initialValues$Country <- data$CountryID[1]
        
        initialValues$Sex <- data$SexID[1]
        initialValues$Hypothermia <- data$HypothermiaID[1]
        initialValues$Resuscitation <- data$ResuscitationID[1]
        initialValues$Rescue <- data$RescueID[1]
        initialValues$PoolFence <- data$PoolFenceTypeID[1]
        initialValues$ChildSupervision <- data$ChildSupervisionID[1]
        initialValues$Purpose <- data$PurposeID[1]
        initialValues$AlcoholDrug <- data$AlcoholDrugID[1]
        initialValues$Drug <- data$DrugID[1]
        initialValues$FatalityType <- data$FatalityTypeID[1]
        initialValues$Buoy <- data$BuoyancyID[1]
        initialValues$ImmigrationStatus <- data$ImmigrationStatus[1]
        initialValues$Activity <- data$ActivityID[1]
        initialValues$Site <- data$SiteID[1]
        initialValues$Longitude  <- data$Longitude[1]
        initialValues$Latitude <- data$Latitude[1]
        initialValues$Prev <- data$Preventable[1]
        initialValues$LocationID <- data$LocationID[1]
        initialValues$Location <- data$Location[1]
        selectedTimeValue(data$TimeOfIncident[1])
        updateNumericInput(session, "Latitude", value = initialValues$Latitude)
        updateNumericInput(session, "Longitude", value = initialValues$Longitude)
        
        forminitialised(TRUE)
        output$noDataError <-
          renderText("")  # Clear any Previous error messAge
        output$actionButtons <- renderUI({
          tagList(
            actionButton("continue", "Check / Update", class = "btn-primary",style = "margin-top: 10px;"),
            actionButton("reset", "Reset Drowning Record",style = "margin-top: 10px;")
          )
        })
        
      } else {
        forminitialised(FALSE)
        showForm(FALSE)
        output$noDataError <-
          renderText("No data found for the provided DrowningID.")
        resetInputs()  # Ensure inputs are reset when no data is found
      }
    } else {
      forminitialised(FALSE)
      showForm(FALSE)
      output$noDataError <-
        renderText("Invalid DrowningID provided.")
      resetInputs()  # Ensure inputs are reset when invalid ID is provided
    }
  }
  
  observe({
    if (!is.null(input$dob) && !is.na(input$dob) && length(input$dob) > 0 &&
        !is.null(input$date) && !is.na(input$date) && length(input$date) > 0) {
      # Both Date of Birth and Date are provided
      calculated_Age <- floor(time_length(interval(input$dob, input$date), "years"))
      updateNumericInput(session, "Age", value = calculated_Age)
    } else {
      # Either Date of Birth or Date is NULL/NA; reset the Age input to initialValues
      updateNumericInput(session, "Age", value = initialValues$Age)
    }
  })
  
  
  observeEvent(input$reset, {
    resetInputs()
  })
  
  fetchDrowningData <- function(con, drowningID) {
    query <-
      sprintf("SELECT * FROM drowning WHERE DrowningID = %.0f", drowningID)
    dbGetQuery(con, query)
  }
  
  currentDrowningID <- reactiveVal(0)
  observeEvent(input$submit_inquest, {
    inquest <- input$inquest
    
    if (is.na(inquest) || !(inquest %in% validInquests)) {
      output$inquestNumberError <- renderText("Invalid InquestNumber provided.")  # Error message
      showForm(FALSE)
      forminitialised(FALSE)
    } else {
      query <- sprintf("SELECT DrowningID FROM drowning WHERE InquestNumber = '%s'", inquest)
      result <- dbGetQuery(con, query)
      
      if (nrow(result) == 0) {
        output$inquestNumberError <- renderText("No DrowningID found for the given InquestNumber.")  # Error message
        showForm(FALSE)
        forminitialised(FALSE)
      } else {
        drowningID <- result$DrowningID[1]
        currentDrowningID(drowningID)
        showForm(TRUE)
        initialiseDefaults(con, currentDrowningID())
        updateInputs()
        forminitialised(TRUE)
        output$inquestNumberError <- renderText("")  # Clear error message
        output$drowningIDError <- renderText("")  # Clear error message
        values$rules_error <- list()  # Reset rules error
        values$rules_warning <- list()  # Reset rules warning
        removeModal()
      }
    }
  })
  
  observeEvent(input$submit, {
    if (is.na(input$drowningID) || !(input$drowningID %in% validDrowningIDs)) {
      output$drowningIDError <- renderText("Invalid DrowningID provided.")  # Error message
      showForm(FALSE)
      forminitialised(FALSE)
      output$ErrorDisplay <- renderText("")  # Clear other error messages
      
    } else {
      currentDrowningID(input$drowningID)
      showForm(TRUE)
      initialiseDefaults(con, currentDrowningID())
      updateInputs()
      forminitialised(TRUE)  # Form initialized successfully
      output$drowningIDError <- renderText("")  # Clear error message
      output$inquestNumberError <- renderText("")  # Clear error message
      updateTextInput(session, "inquest", value = getValueUpdate(con, "InquestNumber"))
      values$rules_error <- list()  # Reset rules error
      values$rules_warning <- list()  # Reset rules warning
      removeModal()
    }
  })  
  resetInputs <- function() {
    if (currentDrowningID() > 0) {
      updateInputs()
    } else {
      showForm(FALSE)
    }
  }
  
  updateInputs <- function() {
    updateNumericInput(session, "drowningID", value = currentDrowningID())
    
    updateSelectInput(
      session,
      "PrimaryDataSourceSelect",
      selected = getSelectedDescription(
        con,
        "PrimaryDataSource",
        "PrimaryDataSourceID",
        currentDrowningID()
      )
    )
    
    updateCheckboxInput(session, "CoronialFinding", value = initialValues$CoronialFinding)
    updateDateInput(session, "date", value = getValueUpdate(con, "Date"))
    updateSelectInput(session, "Time", selected = getValueUpdate(con, "TimeOfIncident"))
    updateTextInput(session, "InquestNumber", value = getValueUpdate(con, "InquestNumber"))
    updateTextInput(session, "FamilyName", value = getValueUpdate(con, "FamilyName"))
    updateTextInput(session, "GivenNames", value = getValueUpdate(con, "GivenNames"))
    updateDateInput(session, "dob", value = getValueUpdate(con, "DateOfBirth"))
    if (is.null(initialValues$dob) ||
        is.na(initialValues$dob)) {
      updateNumericInput(session, "Age", value = initialValues$Age)
    } else {
      Age <-
        floor(time_length(interval(initialValues$dob, Sys.Date()), "years"))
      updateNumericInput(session, "Age", value = Age)
    }
    
    updateSelectInput(
      session,
      "EthnicitySelect",
      selected = getSelectedDescription(con, "Ethnicity", "EthnicityID", currentDrowningID())
    )
    
    updateSelectInput(
      session,
      "SubEthnicitySelect",
      selected = getSelectedDescription(
        con,
        "SubEthnicity",
        "SubEthnicityID",
        currentDrowningID()
      )
    )
    updateSelectInput(session,
                      "SexSelect",
                      selected = getSelectedDescription(con, "Sex", "SexID", currentDrowningID()))
    updateSelectInput(
      session,
      "ResidentSelect",
      selected = getSelectedDescription(con, "Region", "RegionID", currentDrowningID())
    )
    updateTextInput(session, "Residence", value = getValueUpdate(con, "Residence"))
    
    updateSelectInput(
      session,
      "LocationSelect",
      selected = getSelectedDescription(con, "Location", "LocationID", currentDrowningID())
    )
    updateTextInput(session, "Location", value = getValueUpdate(con, "Location"))
    updateNumericInput(session, "Latitude", value = getValueUpdate(con, "Latitude"))
    updateNumericInput(session, "Longitude", value = getValueUpdate(con, "Longitude"))
    
    updateNumericInput(session,
                       "NumberOfFatalities",
                       value = getValueUpdate(con, "NumberOfFatalities"))
    updateSelectInput(
      session,
      "FatalityTypeSelect",
      selected = getSelectedDescription(
        con,
        "FatalityType",
        "FatalityTypeID",
        currentDrowningID()
      )
    )
    updateNumericInput(session, "BoatLength", value = getValueUpdate(con, "BoatLength"))
    updateSelectInput(
      session,
      "BuoySelect",
      selected = getSelectedDescription(con, "Buoyancy", "BuoyancyID", currentDrowningID())
    )
    updateNumericInput(session, "BloodAlcohol", value = getValueUpdate(con, "BloodAlcohol"))
    updateSelectInput(
      session,
      "AlcoholDrugSelect",
      selected = getSelectedDescription(con, "AlcoholDrug", "AlcoholDrugID", currentDrowningID())
    )
    updateSelectInput(session,
                      "DrugSelect",
                      selected = getSelectedDescription(con, "Drug", "DrugID", currentDrowningID()))
    updateTextInput(session,
                    "MedicalCondition",
                    value = getValueUpdate(con, "MedicalCondition"))
    updateSelectInput(
      session,
      "HypothermiaSelect",
      selected = getSelectedDescription(con, "Hypothermia", "HypothermiaID", currentDrowningID())
    )
    updateSelectInput(
      session,
      "ResusSelect",
      selected = getSelectedDescription(
        con,
        "Resuscitation",
        "ResuscitationID",
        currentDrowningID()
      )
    )
    updateSelectInput(
      session,
      "RescueSelect",
      selected = getSelectedDescription(con, "Rescue", "RescueID", currentDrowningID())
    )
    updateSelectInput(
      session,
      "PoolFenceSelect",
      selected = getSelectedDescription(
        con,
        "PoolFenceType",
        "PoolFenceTypeID",
        currentDrowningID()
      )
    )
    updateSelectInput(
      session,
      "ChildSupervisionSelect",
      selected = getSelectedDescription(
        con,
        "ChildSupervision",
        "ChildSupervisionID",
        currentDrowningID()
      )
    )
    updateSelectInput(
      session,
      "PurposeSelect",
      selected = getSelectedDescription(con, "Purpose", "PurposeID", currentDrowningID())
    )
    updateCheckboxInput(session, "Missing", value = initialValues$Missing)
    updateCheckboxInput(session, "Presumed", value = initialValues$Presumed)
    updateCheckboxInput(session, "BeachPatrolled", value = initialValues$BeachPatrolled)
    updateCheckboxInput(session, "Rip", value = initialValues$Rip)
    updateCheckboxInput(session, "Commercial", value = initialValues$Commercial)
    
    updateTextInput(session, "Synopsis", value = getValueUpdate(con, "Synopsis"))
  }
  
  drowningData <- eventReactive(input$submit, {
    query <-
      sprintf("SELECT * FROM drowning WHERE DrowningID = %.0f",
              currentDrowningID())
    dbGetQuery(con, query)
  })
  
  
  
  observeEvent(input$Time, {
    selectedTimeValue(input$Time)
  })
  
  getDefaultValue <- function(con, column) {
    query <-
      sprintf("SELECT %s FROM drowning WHERE DrowningID = %.0f",
              column,
              currentDrowningID())
    data <- dbGetQuery(con, query)
    if (nrow(data) > 0) {
      return(data[[1]])
    } else {
      return(NA)
    }
  }
  
  getSelectedDescription <-
    function(con, tableName, tableID, drowningID) {
      if (tableName == "Activity") {
        data = dbGetQuery(
          con,
          paste(
            "SELECT CONCAT(a.Description, ', ', a3.Description, ', ', a2.Description, ', ', a1.Description) AS Description
          FROM Activity a
          JOIN Activity3 a3 ON a.Activity3ID = a3.Activity3ID
          JOIN Activity2 a2 ON a2.Activity2ID = a3.Activity2ID
          JOIN Activity1 a1 ON a2.Activity1ID = a1.Activity1ID
          WHERE a.Description = (SELECT Description FROM Activity WHERE ActivityID = (SELECT ActivityID FROM drowning WHERE DrowningID = ",
            drowningID,
            "))"
          )
        )
      } else if (tableName == "Site") {
        data = dbGetQuery(
          con,
          paste(
            "SELECT CONCAT(s.Description, ', ', s1.Description, ', ', s2.Description) AS Description
          FROM Site s
          JOIN Site2 s2 ON s.Site2ID = s2.Site2ID
          JOIN Site1 s1 ON s2.Site1ID = s1.Site1ID
          WHERE s.Description = (SELECT Description FROM Site WHERE SiteID = (SELECT SiteID FROM drowning WHERE DrowningID = ",
            drowningID,
            "))"
          )
        )
      } else if (tableName == "Region") {
        query = paste(
          "SELECT * FROM Region WHERE RegionID = (SELECT ResidenceID FROM Drowning WHERE DrowningID = ",
          drowningID,
          ")"
        )
        data <- dbGetQuery(con, query)
      } else {
        query <- sprintf(
          "SELECT Description FROM %s WHERE %sID = (SELECT %sID FROM drowning WHERE DrowningID = %.0f)",
          tableName,
          tableName,
          tableName,
          drowningID
        )
        data <- dbGetQuery(con, query)
        if (nrow(data) == 0) {
          return("Unknown")
        }
        return(data$Description[1])
      }
    }
  
  
  
  getValueUpdate <- function(con, column, default = NA) {
    variable_name <- paste0("default_", column)
    query <-
      sprintf("SELECT %s FROM Drowning WHERE DrowningID = %s",
              column,
              currentDrowningID())
    data <- dbGetQuery(con, query)
    
    if (nrow(data) == 0) {
      assign(variable_name, default, envir = .GlobalEnv)
      return(default)
    }
    
    assign(variable_name, data[[1]], envir = .GlobalEnv)
    return(data[[1]])
  }
  
  drowningPrimaryDataSourceID <- eventReactive(input$submit, {
    getSelectedDescription(con,
                           "PrimaryDataSource",
                           "PrimaryDataSourceID",
                           currentDrowningID())
  })
  
  output$formUI <- renderUI({
    if (!showForm())
      return(NULL)
    
    initialValues <- reactiveValues()
    
    fetchDrowningData <- function(con, drowningID) {
      query <-
        sprintf("SELECT * FROM drowning WHERE DrowningID = %.0f", drowningID)
      dbGetQuery(con, query)
    }
    
    fluidRow(
      column(
        3,
        # This is the larger "row column" that holds the first "column" of the form
        fluidRow(column(
          6, uiOutput("PrimaryDataSourceSelect")
        ), column(
          6,
          textInput("InquestNumber", "Inquest Number", width = "100%")
        )),
        
        fluidRow(
          column(
            5,
            checkboxInput(
              "CoronialFinding",
              "Coroner Report",
              value = FALSE,
              width = "100%"
            )
          ),
          column(
            3,
            checkboxInput("Missing", "Missing", value = FALSE, width = "100%")
          ),
          column(
            4,
            checkboxInput(
              "Presumed",
              "Presumed",
              value = FALSE,
              width = "100%"
            )
          )
        ),
        
        fluidRow(column(
          6,
          tags$div(
            tags$label("Date"),
            tags$span(
              class = "glyphicon glyphicon-info-sign",
              id = "date_info",
              style = "cursor: pointer; margin-left: 5px;"
            ),
            bsTooltip(
              "date_info",
              "Select fatality date, if unknown, use the earliest report date from the source",
              placement = "right",
              trigger = "hover"
            ),
            dateInput("date", NULL, max = Sys.Date(), width = "100%")
          )
        ), column(
          6,
          selectInput(
            "Time",
            "Time",
            choices = c("", 0:23),
            selected = "",
            width = "100%"
          )
        )),
        textInput("FamilyName", "Family Name", width = "100%"),
        textInput("GivenNames", "Given Names", width = "100%"),
        
        fluidRow(column(
          6,
          tags$div(
            tags$label("Date of Birth"),
            tags$span(
              class = "glyphicon glyphicon-info-sign",
              id = "dob_info",
              style = "cursor: pointer; margin-left: 5px;"
            ),
            bsTooltip(
              "dob_info",
              "Enter the date of birth in format yyyy-mm-dd",
              placement = "right",
              trigger = "hover"
            ),
            dateInput(
              inputId = "dob",
              label = NULL,
              value = "1960-01-01",
              max = Sys.Date(),
              width = "100%"
            )
          )
        ), column(
          6, numericInput(
            "Age",
            "Age",
            value = 0,
            min = 0,
            width = "100%"
          )
        )),
        
        fluidRow(column(6, uiOutput(
          "EthnicitySelect"
        )), column(6, uiOutput(
          "SubEthnicitySelect"
        ))),
        
        fluidRow(column(6, uiOutput("SexSelect")), column(
          6, uiOutput("ImmigrationStatusSelect")
        )),
        
        fluidRow(column(
          6,
          textInput("Residence", "Residence Address", width = "100%")
        ), column(6, uiOutput(
          "ResidentSelect"
        ))),
        
        fluidRow(column(12, uiOutput(
          "CountrySelect"
        )))
      ),
      
      
      column(
        3,
        # This is the larger "row column" that holds the second "column" of the form
        fluidRow(column(12, uiOutput(
          "LocationSelect"
        ))),
        
        fluidRow(column(
          12,
          textInput("Location", "Location Description", width = "100%")
        )),
        
        fluidRow(column(
          6,
          numericInput(
            "Latitude",
            "Latitude",
            value = 0,
            max = 37,
            min = -48
          )
        ), column(
          6,
          numericInput(
            "Longitude",
            "Longitude",
            value = 0,
            min = -178,
            max = 178
          )
        )),
        
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
      
      
      column(
        5,
        # This is the larger "row column" that holds the third "column" of the form
        fluidRow(column(
          3,
          numericInput(
            "NoFatalities",
            "No.Fatalities",
            value = 1,
            width = "100%"
          )
        ), column(9, uiOutput(
          "FatalityTypeSelect"
        ))),
        
        fluidRow(
          column(
            12,
            tags$label("Relevant Medical Event"),
            tags$span(
              class = "glyphicon glyphicon-info-sign",
              id = "MedicalEvent",
              style = "cursor: pointer; margin-left: 5px;"
            ),
            bsTooltip(
              "MedicalEvent",
              "Medical Event that was/must be a SIGNIFICANT factor in the fatality, otherwise leave blank",
              placement = "right",
              trigger = "hover"
            ),
            textInput("MedicalCondition", NULL , width = "100%")
          )
        ),
        
        textAreaInput("Synopsis", "Synopsis", width = "100%", rows = 8),
        fluidRow(
          column(4, uiOutput("AlcoholDrugSelect")),
          column(
            4,
            numericInput(
              "BloodAlcohol",
              "Blood Alcohol",
              value = 0,
              width = "100%"
            )
          ),
          column(4, uiOutput("DrugSelect"))
        ),
        
        fluidRow(column(8, uiOutput("BuoySelect")), column(
          4,
          numericInput(
            "BoatLength",
            "Boat Length",
            value = 0,
            width = "100%"
          )
        )),
        
        fluidRow(
          column(4, uiOutput("HypothermiaSelect")),
          column(4, uiOutput("ResusSelect")),
          column(4, uiOutput("RescueSelect"))
        ),
        
        fluidRow(
          column(4, uiOutput("PoolFenceSelect")),
          column(4, uiOutput("ChildSupervisionSelect")),
          column(4, uiOutput("PurposeSelect"))
        ),
        
        fluidRow(
          column(4, checkboxInput(
            "Rip", "Rip", value = FALSE, width = "100%"
          )),
          column(
            4,
            checkboxInput(
              "Commercial",
              "Commercial",
              value = FALSE,
              width = "100%"
            )
          ),
          column(
            4,
            checkboxInput(
              "BeachPatrolled",
              "Beach Patrolled",
              value = FALSE,
              width = "100%"
            )
          )
        ),
        tags$div(style = "height: 20px;"),
        textOutput("Query"),
        
        bsModal(
          "modalExample",
          "Business Rules Check",
          "continue",
          size = "big",
          uiOutput("modalContent"),
          actionButton("update", "Update", class = "btn-primary")
        ),
        
      )
      
    )
  })
  observeEvent(input$SiteSelect, {
    if (!grepl("Offshore", input$SiteSelect, ignore.case = TRUE)) {
      updateCheckboxInput(session, "calcDistance", value = FALSE)
    }
  })
  distance_zone <- reactive({
    req(input$Latitude, input$Longitude, input$SiteSelect, input$calcDistance)
    
    # Only calculate if "Offshore" is in site and user checked the box
    if (!grepl(" Shore", input$SiteSelect, ignore.case = TRUE) || !isTRUE(input$calcDistance)) {
      return(NULL)
    }
    
    point <- st_sfc(st_point(c(input$Longitude, input$Latitude)), crs = 4326)
    
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
    if (!is.na(input$Latitude) && !is.na(input$Longitude) && input$Latitude != 0 && input$Longitude != 0) {
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
  
  
  
  
  output$PrimaryDataSourceSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con,
                             "PrimaryDataSource",
                             "PrimaryDataSourceID",
                             currentDrowningID())
    selectInput(
      "PrimaryDataSourceSelect",
      "Primary Data Source",
      choices = fetchChoices(con, "PrimaryDataSource")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedPrimaryDataSourceID <- reactive({
    getTableID(con, "PrimaryDataSource", input$PrimaryDataSourceSelect)
  })
  
  output$SexSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con, "Sex", "SexID", currentDrowningID())
    selectInput(
      "SexSelect",
      "Sex",
      choices = fetchChoices(con, "Sex")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedSexID <- reactive({
    getTableID(con, "Sex", input$SexSelect)
  })
  
  output$EthnicitySelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con, "Ethnicity", "EthnicityID", currentDrowningID())
    selectInput(
      "EthnicitySelect",
      "Ethnicity",
      choices = fetchChoices(con, "Ethnicity")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedEthnicityID <- reactive({
    getTableID(con, "Ethnicity", input$EthnicitySelect)
  })
  
  output$SubEthnicitySelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con,
                             "SubEthnicity",
                             "SubEthnicityID",
                             currentDrowningID())
    selectInput(
      "SubEthnicitySelect",
      "Sub-Ethnicity",
      choices = fetchChoices(con, "SubEthnicity")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedSubEthnicityID <- reactive({
    getTableID(con, "SubEthnicity", input$SubEthnicitySelect)
  })
  
  output$CountrySelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con, "Country", "CountryID", currentDrowningID())
    selectInput(
      "CountrySelect",
      "Residence Country",
      choices = fetchChoices(con, "Country")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedCountryID <- reactive({
    getTableID(con, "Country", input$CountrySelect)
  })
  
  
  output$ImmigrationStatusSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con,
                             "ImmigrationStatus",
                             "ImmigrationStatusID",
                             currentDrowningID())
    selectInput(
      "ImmigrationStatusSelect",
      "Immigration Status",
      choices = fetchChoices(con, "ImmigrationStatus")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedImmigrationStatusID <- reactive({
    getTableID(con, "ImmigrationStatus", input$ImmigrationStatusSelect)
  })
  
  output$ResidentSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con, "Region", "RegionID", currentDrowningID())
    selectInput(
      "ResidentSelect",
      "Residence Region",
      choices = fetchChoices(con, "Region")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedResidenceID <- reactive({
    getTableID(con, "Region", input$ResidentSelect)
  })
  
  output$LocationSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con, "Location", "LocationID", currentDrowningID())
    selectInput(
      "LocationSelect",
      "Location",
      choices = fetchChoices(con, "Location")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedLocationID <- reactive({
    getTableID(con, "Location", input$LocationSelect)
  })
  
  output$SiteSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con, "Site", "SiteID", currentDrowningID())
    selectInput(
      "SiteSelect",
      "Site",
      choices = fetchChoices(con, "Site")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedSiteID <- reactive({
    getTableID(con, "Site", input$SiteSelect)
  })
  
  output$ActivitySelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con, "Activity", "ActivityID", currentDrowningID())
    selectInput(
      "ActivitySelect",
      "Activity",
      choices = fetchChoices(con, "Activity")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedActivityID <- reactive({
    getTableID(con, "Activity", input$ActivitySelect)
  })
  
  output$FatalityTypeSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con,
                             "FatalityType",
                             "FatalityTypeID",
                             currentDrowningID())
    selectInput(
      "FatalityTypeSelect",
      "FatalityType",
      choices = fetchChoices(con, "FatalityType")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedFatalityTypeID <- reactive({
    getTableID(con, "FatalityType", input$FatalityTypeSelect)
  })
  
  output$BuoySelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con, "Buoyancy", "BuoyancyID", currentDrowningID())
    selectInput(
      "BuoySelect",
      "Buoyancy",
      choices = fetchChoices(con, "Buoyancy")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedBuoyID <- reactive({
    getTableID(con, "Buoyancy", input$BuoySelect)
  })
  
  output$AlcoholDrugSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con, "AlcoholDrug", "AlcoholDrugID", currentDrowningID())
    selectInput(
      "AlcoholDrugSelect",
      "Alcohol/Drug",
      choices = fetchChoices(con, "AlcoholDrug")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedAlcoholDrugID <- reactive({
    getTableID(con, "AlcoholDrug", input$AlcoholDrugSelect)
  })
  
  output$DrugSelect <- renderUI({
    selectInput(
      "DrugSelect",
      "Drug",
      choices = fetchChoices(con, "Drug")$Description,
      selected = "Unknown",
      width = '100%'
    )
  })
  
  selectedDrugID <- reactive({
    getTableID(con, "Drug", input$DrugSelect)
  })
  output$HypothermiaSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con, "Hypothermia", "HypothermiaID", currentDrowningID())
    selectInput(
      "HypothermiaSelect",
      "Hypothermia",
      choices = fetchChoices(con, "Hypothermia")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedHypothermiaID <- reactive({
    getTableID(con, "Hypothermia", input$HypothermiaSelect)
  })
  
  output$ResusSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con,
                             "Resuscitation",
                             "ResuscitationID",
                             currentDrowningID())
    selectInput(
      "ResusSelect",
      "Resuscitation",
      choices = fetchChoices(con, "Resuscitation")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedResusID <- reactive({
    getTableID(con, "Resuscitation", input$ResusSelect)
  })
  
  output$RescueSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con, "Rescue", "RescueID", currentDrowningID())
    selectInput(
      "RescueSelect",
      "Rescue",
      choices = fetchChoices(con, "Rescue")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedRescueID <- reactive({
    getTableID(con, "Rescue", input$RescueSelect)
  })
  
  output$PoolFenceSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con,
                             "PoolFenceType",
                             "PoolFenceTypeID",
                             currentDrowningID())
    selectInput(
      "PoolFenceSelect",
      "Pool Fence",
      choices = fetchChoices(con, "PoolFenceType")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedPoolFenceID <- reactive({
    getTableID(con, "PoolFenceType", input$PoolFenceSelect)
  })
  
  output$ChildSupervisionSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con,
                             "ChildSupervision",
                             "ChildSupervisionID",
                             currentDrowningID())
    selectInput(
      "ChildSupervisionSelect",
      "Child Supervision",
      choices = fetchChoices(con, "ChildSupervision")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedChildSupervisionID <- reactive({
    getTableID(con, "ChildSupervision", input$ChildSupervisionSelect)
  })
  
  output$PurposeSelect <- renderUI({
    selectedDescription <-
      getSelectedDescription(con, "Purpose", "PurposeID", currentDrowningID())
    selectInput(
      "PurposeSelect",
      "Purpose",
      choices = fetchChoices(con, "Purpose")$Description,
      selected = selectedDescription,
      width = '100%'
    )
  })
  selectedPurposeID <- reactive({
    getTableID(con, "Purpose", input$PurposeSelect)
  })
  
  

  updateStatement <- reactiveVal("")
  generateUpdateStatement <- reactive({
    updateFields <- c()
    
    cat("Generating update statement\n")
    
    checkAndUpdate <- function(inputValue, dbValue, fieldName) {
      if (is.null(inputValue) ||
          is.na(inputValue) || length(inputValue) == 0) {
        return(NULL)
      }
      if (is.null(dbValue) ||
          is.na(dbValue) || length(dbValue) == 0) {
        # cat("DB value is NULL, NA, or of length zero for field:",
        #     fieldName,
        #     "\n")
        dbValue <- ""
      }
      if (inputValue != dbValue) {
        if (is.numeric(inputValue)) {
          # return(sprintf(
          #   "%s = %s",
          #   fieldName,
          #   ifelse(is.na(inputValue), "NULL", inputValue)
          # ))
          break
        }
        else {
          return(sprintf(
            "%s = '%s'",
            fieldName,
            ifelse(
              is.na(inputValue) || inputValue == "",
              "NULL",
              inputValue
            )
          ))
        }
      }
      return(NULL)
    }
    
    
    compareLogical <- function(inputVal, initVal, fieldName) {
      if (is.null(inputVal) ||
          is.null(initVal) ||
          length(inputVal) == 0 || length(initVal) == 0) {
        cat("Logical value is NULL or of length zero for field:",
            fieldName,
            "\n")
        return(NULL)
      }
      if (!is.logical(inputVal) || !is.logical(initVal)) {
        cat("Logical value comparison failed for:", fieldName, "\n")
        return(NULL)
      }
      if (inputVal != initVal) {
        return(sprintf("%s = %.0f", fieldName, as.integer(inputVal)))
      }
      return(NULL)
    }
    
    compareDate <- function(inputDate, initDate, fieldName) {
      if (is.null(inputDate)) {
        return(NULL)
      }
      
      # Convert dates to Date objects for accurate comparison
      inputDate <- as.Date(inputDate)
      initDate <- as.Date(initDate)
      
      # If initDate is NULL but inputDate is not, include in the update statement
      if (is.null(initDate) && !is.null(inputDate)) {
        return(sprintf("%s = '%s'", fieldName, inputDate))
      }
      
      # If the dates are different, include in the update statement
      if (!identical(inputDate, initDate)) {
        return(sprintf("%s = '%s'", fieldName, inputDate))
      }
      
      # If dates are the same, do not include in the update statement
      return(NULL)
    }
    
    
    updateFields <- c(
      updateFields,
      compareLogical(
        input$CoronialFinding,
        initialValues$CoronialFinding,
        "CoronialFinding"
      )
    )
    updateFields <-
      c(updateFields,
        compareLogical(input$Missing, initialValues$Missing, "Missing"))
    updateFields <-
      c(
        updateFields,
        compareLogical(input$Presumed, initialValues$Presumed, "Presumed")
      )
    updateFields <-
      c(updateFields,
        compareLogical(input$Rip, initialValues$Rip, "Rip"))
    updateFields <-
      c(
        updateFields,
        compareLogical(input$Commercial, initialValues$Commercial, "Commercial")
      )
    updateFields <-
      c(
        updateFields,
        compareLogical(
          input$BeachPatrolled,
          initialValues$BeachPatrolled,
          "BeachPatrolled"
        )
      )
    
    
    if (!is.null(selectedLocationID()) &&
        length(selectedLocationID()) > 0 &&
        as.numeric(selectedLocationID()) != initialValues$LocationID) {
      updateFields <-
        c(updateFields, sprintf("LocationID = %.0f", as.integer(selectedLocationID())))
    }
    if (!is.null(selectedPrimaryDataSourceID()) &&
        length(selectedPrimaryDataSourceID()) > 0 &&
        as.numeric(selectedPrimaryDataSourceID()) != initialValues$PrimaryDataSource) {
      updateFields <-
        c(updateFields,
          sprintf(
            "PrimaryDataSourceID = %.0f",
            as.integer(selectedPrimaryDataSourceID())
          ))
    }
    if (!is.null(selectedEthnicityID()) &&
        length(selectedEthnicityID()) > 0 &&
        as.numeric(selectedEthnicityID()) != initialValues$Ethnicity) {
      updateFields <-
        c(updateFields, sprintf("EthnicityID = %.0f", as.integer(selectedEthnicityID())))
    }
    if (!is.null(selectedSubEthnicityID()) &&
        length(selectedSubEthnicityID()) > 0 &&
        as.numeric(selectedSubEthnicityID()) != initialValues$SubEthnicity) {
      updateFields <-
        c(updateFields,
          sprintf(
            "SubEthnicityID = %.0f",
            as.integer(selectedSubEthnicityID())
          ))
    }
    
    if (!is.null(selectedCountryID()) &&
        length(selectedCountryID()) > 0 &&
        as.numeric(selectedCountryID()) != initialValues$Country) {
      updateFields <-
        c(updateFields, sprintf("CountryID = %.0f", as.integer(selectedCountryID())))
    }
    
    if (!is.null(selectedSexID()) &&
        length(selectedSexID()) > 0 &&
        as.numeric(selectedSexID()) != initialValues$Sex) {
      updateFields <-
        c(updateFields, sprintf("SexID = %.0f", as.integer(selectedSexID())))
    }
    if (!is.null(selectedHypothermiaID()) &&
        length(selectedHypothermiaID()) > 0 &&
        as.numeric(selectedHypothermiaID()) != initialValues$Hypothermia) {
      updateFields <-
        c(updateFields, sprintf(
          "HypothermiaID = %.0f",
          as.integer(selectedHypothermiaID())
        ))
    }
    
    if (!is.null(selectedResusID()) &&
        length(selectedResusID()) > 0 &&
        as.numeric(selectedResusID()) != initialValues$Resuscitation) {
      updateFields <-
        c(updateFields,
          sprintf("ResuscitationID = %.0f", as.integer(selectedResusID())))
    }
    if (!is.null(selectedRescueID()) &&
        length(selectedRescueID()) > 0 &&
        as.numeric(selectedRescueID()) != initialValues$Rescue) {
      updateFields <-
        c(updateFields, sprintf("RescueID = %.0f", as.integer(selectedRescueID())))
    }
    if (!is.null(selectedPoolFenceID()) &&
        length(selectedPoolFenceID()) > 0 &&
        as.numeric(selectedPoolFenceID()) != initialValues$PoolFence) {
      updateFields <-
        c(updateFields,
          sprintf(
            "PoolFenceTypeID = %.0f",
            as.integer(selectedPoolFenceID())
          ))
    }
    
    if (!is.null(selectedAlcoholDrugID()) &&
        length(selectedAlcoholDrugID()) > 0 &&
        as.numeric(selectedAlcoholDrugID()) != initialValues$AlcoholDrug) {
      updateFields <-
        c(updateFields, sprintf(
          "AlcoholDrugID = %.0f",
          as.integer(selectedAlcoholDrugID())
        ))
    }
    
    if (!is.null(selectedDrugID()) &&
        length(selectedDrugID()) > 0 &&
        as.numeric(selectedDrugID()) != initialValues$Drug) {
      updateFields <-
        c(updateFields, sprintf("DrugID = %.0f", as.integer(selectedDrugID())))
    }
    if (!is.na(initialValues$ChildSupervision) &&
        !is.na(selectedChildSupervisionID()) &&
        !is.null(selectedChildSupervisionID()) &&
        length(selectedChildSupervisionID()) > 0 &&
        as.numeric(selectedChildSupervisionID()) != initialValues$ChildSupervision) {
      updateFields <-
        c(updateFields,
          sprintf(
            "ChildSupervisionID = %.0f",
            as.integer(selectedChildSupervisionID())
          ))
    }
    
    if (!is.null(selectedPurposeID()) &&
        length(selectedPurposeID()) > 0 &&
        as.numeric(selectedPurposeID()) != initialValues$Purpose) {
      updateFields <-
        c(updateFields, sprintf("PurposeID = %.0f", as.integer(selectedPurposeID())))
    }
    if (!is.null(selectedFatalityTypeID()) &&
        length(selectedFatalityTypeID()) > 0 &&
        as.numeric(selectedFatalityTypeID()) != initialValues$FatalityType) {
      updateFields <-
        c(updateFields,
          sprintf(
            "FatalityTypeID = %.0f",
            as.integer(selectedFatalityTypeID())
          ))
    }
    AgeValue <-
      ifelse(is.na(input$Age), initialValues$Age, input$Age)
    DOBValue <- sprintf("'%s'", input$dob)
    if (!is.null(selectedBuoyID()) &&
        length(selectedBuoyID()) > 0 &&
        as.numeric(selectedBuoyID()) != initialValues$Buoy) {
      updateFields <-
        c(updateFields, sprintf("BuoyancyID = %.0f", as.integer(selectedBuoyID())))
    }
    if (!is.null(selectedImmigrationStatusID()) &&
        length(selectedImmigrationStatusID()) > 0 &&
        as.numeric(selectedImmigrationStatusID()) != initialValues$ImmigrationStatus) {
      updateFields <-
        c(updateFields,
          sprintf(
            "ImmigrationStatusID = %.0f",
            as.integer(selectedImmigrationStatusID())
          ))
    }
    
    if (!is.null(selectedActivityID()) &&
        length(selectedActivityID()) > 0 &&
        as.numeric(selectedActivityID()) != initialValues$Activity) {
      updateFields <-
        c(updateFields, sprintf("ActivityID = %.0f", as.integer(selectedActivityID())))
    }
    
    if (!is.null(selectedSiteID()) &&
        length(selectedSiteID()) > 0 &&
        as.numeric(selectedSiteID()) != initialValues$Site) {
      updateFields <-
        c(updateFields, sprintf("SiteID = %.0f", as.integer(selectedSiteID())))
    }
    dateUpdate <-
      compareDate(input$date, initialValues$Date, "Date")
    if (!is.null(dateUpdate)) {
      updateFields <- c(updateFields, dateUpdate)
    }
    if (!is.null(selectedResidenceID()) &&
        length(selectedResidenceID()) > 0 &&
        as.numeric(selectedResidenceID()) != initialValues$Reg) {
      updateFields <-
        c(updateFields, sprintf("ResidenceID = %.0f", as.integer(selectedResidenceID())))
    }
    
    
    
    # if (!is.null(input$Latitude) && !is.na(input$Latitude) &&
    #     !is.null(input$Longitude) && !is.na(input$Longitude) &&
    #     is.numeric(input$Latitude) && is.numeric(input$Longitude) &&
    #     input$Latitude >= -90 && input$Latitude <= 90 &&
    #     input$Longitude >= -180 && input$Longitude <= 180){
    #   if ((
    #     !is.null(input$Longitude) &&
    #     !is.na(input$Longitude) &&
    #     !is.null(initialValues$Longitude) &&
    #     !is.na(initialValues$Longitude) &&
    #     input$Longitude != initialValues$Longitude
    #   ) || (!is.na(input$Longitude))) {
    #     updateFields <-
    #       c(updateFields, sprintf("Longitude = %f", input$Longitude))
    #   }
    #   if ((
    #     !is.null(input$Latitude) &&
    #     !is.na(input$Latitude) &&
    #     !is.null(initialValues$Latitude) &&
    #     !is.na(initialValues$Latitude) &&
    #     input$Latitude != initialValues$Latitude
    #   ) || (!is.na(input$Latitude))) {
    #     updateFields <-
    #       c(updateFields, sprintf("Latitude = %f", input$Latitude))
    #   }
    #
    # }
    if (selectedActivityID() %in% c(36, 37, 38, 39, 40, 41, 42, 0)) {
      Prev <- 0
    } else {
      Prev <- 1
    }
    
    
    if (Prev != initialValues$Prev) {
      updateFields <-
        c(updateFields, sprintf("Preventable = %.0f", Prev))
    }
    if (!is.null(input$dob) && !is.na(input$dob) && length(input$dob) > 0) {
      dobUpdate <- compareDate(input$dob, initialValues$dob, "DateOfBirth")
      updateFields <- c(updateFields, dobUpdate)
    } else {
      # Handle NULL value for DateOfBirth correctly in SQL
      updateFields <- c(updateFields, "DateOfBirth = NULL")
    }
    
    
    if (!is.null(input$Time) && input$Time != '' &&
        ((
          !is.null(initialValues$Time) &&
          input$Time != initialValues$Time
        ) || (is.null(initialValues$Time) && !is.na(input$Time)))) {
      TimeValue <-
        ifelse(input$Time == " ", "NULL", as.numeric(input$Time))
      
      updateFields <- c(updateFields, sprintf("TimeOfIncident = %s", TimeValue))
    }
    
    
    
    inputFields <- c(
      "FamilyName",
      "GivenNames",
      "InquestNumber",
      "Location",
      "MedicalCondition",
      "Residence",
      "Synopsis",
      "Age",
      "NumberOfFatalities",
      "BloodAlcohol",
      "BoatLength",
      "Latitude",
      "Longitude"
    )
    
    for (field in inputFields) {
      inputValue <- input[[field]]
      inputValue <-
        gsub("'", "''", inputValue)  # Escape single quotes
      
      dbValue <- getValueUpdate(con, field)
      dbValue <-
        gsub("'", "''", dbValue)  # Escape single quotes
      
      fieldUpdate <- checkAndUpdate(inputValue, dbValue, field)
      
      
      if (!is.null(fieldUpdate) ) {
        updateFields <- c(updateFields, fieldUpdate)
      }
    }
    
    
    
    if (length(updateFields) > 0) {
      cat("Update fields:", updateFields, "\n")
      return(
        sprintf(
          "UPDATE drowning SET %s WHERE DrowningID = %.0f",
          paste(updateFields, collapse = ", "),
          currentDrowningID()
        )
      )
    } else {
      cat("No changes detected.\n")
      return("No changes detected.")
    }
    
    print(updateFields)
  })
  
  
  output$map <- renderLeaflet({
    # Check if initialValues$Latitude and initialValues$Longitude are not NULL
    if (!is.null(initialValues$Latitude) &&
        !is.null(initialValues$Longitude) &&
        is.numeric(initialValues$Latitude) &&
        is.numeric(initialValues$Longitude) &&
        !is.na(initialValues$Latitude) &&
        !is.na(initialValues$Longitude) &&
        initialValues$Latitude >= -90 &&
        initialValues$Latitude <= 90 &&
        initialValues$Longitude >= -180 &&
        initialValues$Longitude <= 180) {
      leaflet() %>%
        addTiles() %>%
        setView(
          lng = initialValues$Longitude,
          lat = initialValues$Latitude,
          zoom = 5
        ) %>%
        addMarkers(lng = initialValues$Longitude, lat = initialValues$Latitude)
      
    } else {
      # Render a default map view if either latitude or longitude is NULL or invalid
      leaflet() %>%
        addTiles() %>%
        setView(lng = 174.7633,
                lat = -36.8485,
                zoom = 5) # Default to Auckland, New Zealand
    }
  })
  
  observe({
    Latitude <- input$Latitude
    Longitude <- input$Longitude
    
    if (length(Latitude) == 1 &&
        length(Longitude) == 1 && !is.null(Latitude) &&
        !is.null(Longitude) &&
        !is.na(Latitude) && !is.na(Longitude) &&
        is.numeric(Latitude) && is.numeric(Longitude) &&
        Latitude >= -90 &&
        Latitude <= 90 && Longitude >= -180 && Longitude <= 180) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(lng = Longitude, lat = Latitude) %>%
        setView(lng = Longitude,
                lat = Latitude,
                zoom = 10)
    }
    
  })
  
  
  observe({
    if (is.null(input$Latitude) || is.null(input$Longitude)) {
      updateNumericInput(session, "Latitude", value = initialValues$Latitude)  # Default to some valid coordinate
      updateNumericInput(session, "Longitude", value = initialValues$Longitude)   # Default to some valid coordinate
    }
  })
  observeEvent(input$map_click, {
    click <- input$map_click
    updateNumericInput(session, "Latitude", value = click$lat)
    updateNumericInput(session, "Longitude", value = click$lng)
  })
  
  observeEvent(input$continue, {
    ## ALL BUSINESS RULES ARE (meant to be) HERE
    values$rules_error <- list()
    values$rules_warning <- list()
    
    # ## 1: If we have DOB and date, age should be the same
    if (!is.na(input$date) &&
        !is.na(input$date) && length(input$date) > 0 &&
        length(input$dob) > 0) {
      dob <- as.Date(input$dob)
      today <- as.Date(input$date)
      calculated_age <- as.period(interval(dob, today), unit = "years")$year
      
      if (is.na(input$Age) ||
          as.numeric(input$Age) != calculated_age) {
        values$rules_error <- append(values$rules_error,
                                     "Age is inconsistent with the given Date of Birth")
      }
      print("Successfully checked rule 1: Age and DOB consistency")
    }
    
    # ## 2: if the word rip or riptide is in the synopsis the rip box should be ticked
    if (!is.na(input$Synopsis)) {
      synopsis <- tolower(input$Synopsis)
      rip_pattern <- "\\brip\\b|\\bripti\\b"
      
      if (grepl(rip_pattern, synopsis, ignore.case = TRUE) &&
          !isTRUE(input$Rip)) {
        values$rules_warning <- append(
          values$rules_warning,
          "Rip/riptide found in synopsis, but rip not ticked as involved in incident"
        )
      }
      print("Successfully checked rule 2: Rip/riptide in synopsis")
    }
    
    ## 3: if selectedPrimaryDataSourceID() returns 2, append a specific message
    if (input$PrimaryDataSourceSelect == "NZHIS") {
      values$rules_error <- append(values$rules_error,
                                   "PrimaryDataSource should not be NZHIS")
    }
    print("Successfully checked rule 3: PrimaryDataSource is not NZHIS")
    
    # ## 4: if selectedPrimaryDataSourceID() == 4 and InquestValue is an empty character string or NULL
    if (input$PrimaryDataSourceSelect == "Coroner" &&
        (is.null(input$InquestNumber) ||
         input$InquestNumber == "")) {
      values$rules_error <- append(values$rules_error,
                                   "PrimaryDataSource is Coroner, provide an inquest number")
    } else if (input$PrimaryDataSourceSelect != "Coroner" &&
               !is.null(input$InquestNumber) &&
               input$InquestNumber != "") {
      values$rules_error <- append(
        values$rules_error,
        "InquestNumber is provided, but PrimaryDataSource is not Coroner"
      )
    }
    print("Successfully checked rule 4: Coroner and Inquest number")
    
    ## 6: if dateValue is in the future
    if (!is.null(input$date) &&
        !is.na(input$date) && length(input$date) != 0) {
      if (input$date > Sys.Date()) {
        values$rules_error <- append(values$rules_error, "Date is in the future")
      }
      print("Successfully checked rule 6: Date validity")
    } else {
      values$rules_error <- append(values$rules_error, "Date is not provided")
    }
    
    ## 8: if either familyname or givenname is empty while the other one has an entry
    if (!is.na(input$FamilyName) & !is.na(input$GivenNames))
      if ((input$FamilyName == "" &&
           input$GivenNames != "") ||
          (input$FamilyName != "" && input$GivenNames == "")) {
        values$rules_warning <- append(values$rules_warning,
                                       "One of the name fields is empty, while the other one is not")
      }
    print("Successfully checked rule 8: Name field consistency")
    
    
    # # 9: if date of birth (input$dob) is within the last 6 months from today
    # if (!is.na(input$dob) && length(input$dob) > 0) {
    #   if (as.Date(input$dob) > Sys.Date() - months(6)) {
    #     values$rules_warning <- append(values$rules_warning,
    #                                    "Age of deceased is under 6 months, is this correct?")
    #   }
    #   print("Successfully checked rule 9: DOB within last 6 months")
    # }
    print("Removed Rule 9")
    ## 10: if input$age is negative
    if (!is.na(input$Age)) {
      if (!is.na(input$Age) && input$Age < 0) {
        values$rules_error <- append(values$rules_error, "Age is a negative value")
      }
      print("Successfully checked rule 10: Age is non-negative")
    }
    
    ## 11: if input$age is over 100
    else if (!is.na(input$Age) && input$Age > 100) {
      values$rules_warning <- append(values$rules_warning, "Age is over 100, are you sure?")
    }
    print("Successfully checked rule 11: Age is below 100")
    
    ## 12: if selectedSexID() == 0
    if (input$SexSelect == "Unknown") {
      values$rules_warning <- append(values$rules_warning,
                                     "Are you sure selected Sex is Unknown?")
    }
    print("Successfully checked rule 12: Sex selection")
    
    ## 13: if the words alcohol or drug are in the synopsis, AlcoholSelect should not be "Unknown" or "Not Involved"
    if (!is.na(input$Synopsis)) {
      synopsis <- tolower(input$Synopsis)
      
      # Check for alcohol mentions
      if (grepl("alcohol", synopsis)) {
        if (input$AlcoholDrugSelect == "Unknown") {
          values$rules_warning <- append(
            values$rules_warning,
            "Alcohol mentioned in synopsis, but Alcohol/Drug field is 'Unknown'"
          )
        } else if (input$AlcoholDrugSelect == "Not Involved") {
          values$rules_warning <- append(
            values$rules_warning,
            "Alcohol mentioned in synopsis, but Alcohol/Drug field is 'Not Involved'"
          )
        } else if (input$AlcoholDrugSelect == "Drugs Involved") {
          values$rules_warning <- append(
            values$rules_warning,
            "Alcohol mentioned in synopsis, but Drug field is 'Drugs Involved'"
          )
        }
      }
      
      # Check for drug mentions
      if (grepl("drug", synopsis)) {
        if (input$AlcoholDrugSelect == "Unknown") {
          values$rules_warning <- append(
            values$rules_warning,
            "Drug mentioned in synopsis, but Drug field is 'Unknown'"
          )
        } else if (input$AlcoholDrugSelect == "Not Involved") {
          values$rules_warning <- append(
            values$rules_warning,
            "Drug mentioned in synopsis, but Drug field is 'Not Involved'"
          )
        } else if (input$AlcoholDrugSelect == "Alcohol Involved") {
          values$rules_warning <- append(
            values$rules_warning,
            "Drug mentioned in synopsis, but Drug field is 'Alcohol Involved'"
          )
        }
      }
      print("Successfully checked rule 13: Alcohol and Drug mentions in synopsis")
    }
    
    
    ## 14: if input$SiteSelect is "Unknown, Unknown, Unknown" check that is correct
    if (input$SiteSelect == "Unknown, Unknown, Unknown") {
      values$rules_warning <- append(values$rules_warning, "Are you sure Site is unknown")
    }
    print("Successfully checked rule 14: Site selection as unknown")
    
    ## 15: if input$Location is "Unknown"
    if (is.na(input$Location)) {
      values$rules_warning <- append(values$rules_warning,
                                     "Are you sure Location Description is unknown")
    }
    if (input$LocationSelect == "Unknown") {
      values$rules_warning <- append(values$rules_warning, "Are you sure Location is unknown")
    }
    print("Successfully checked rule 15: Location selection as unknown")
    
    ## 16: if input$ethnicity is "Unknown"
    if (input$EthnicitySelect == "Unknown") {
      values$rules_warning <- append(values$rules_warning, "Are you sure Ethnicity is unknown")
    }
    print("Successfully checked rule 16: Ethnicity selection as unknown")
    
    ## 17: if input$Activity is "Unknown, Unknown, Unknown"
    if (input$ActivitySelect == "Unknown, Unknown, Unknown, Unknown") {
      if (!input$Presumed) {
        values$rules_warning <- append(
          values$rules_warning,
          "Are you sure Activity is unknown and presumed is not ticked?"
        )
      } else {
        values$rules_warning <- append(values$rules_warning,
                                       "Are you sure Activity is unknown?")
      }
    }
    print("Successfully checked rule 17: Activity selection and presumed checkbox")
    
    
    ## 18: if input$ActivitySelect is "Unknown, Unknown, Unknown" then the checkbox presumed should be ticked
    if (input$ActivitySelect == "Unknown, Unknown, Unknown, Unknown" &&
        !input$Presumed) {
      values$rules_warning <- append(
        values$rules_warning,
        "Activity is 'Unknown', but presumed checkbox is not ticked"
      )
    }
    print("Successfully checked rule 18: Activity is unknown with presumed checkbox")
    
    ## 19: if input$boatlength is less than 0 or greater than 100
    if (!is.na(input$BoatLength)) {
      if (input$BoatLength < 0 || input$BoatLength > 100) {
        values$rules_warning <- append(values$rules_warning,
                                       "Boatlength is over 100 or less than 0, are you sure?")
      }
    }
    print("Successfully checked rule 19: Boat length within bounds")
    
    ## 20: if input$noFatalities is less than or equal to 0
    if (is.na(input$NoFatalities)) {
      values$rules_error <- append(values$rules_error, "Missing Number of Fatalities")
    } else if (input$NoFatalities <= 0) {
      values$rules_error <- append(values$rules_error, "Fatalities is 0 or less")
    }
    print("Successfully checked rule 20: Fatalities count")
    
    ## 21: if input$NoFatalities is > 1 then selectedFatalityTypeID must be 4, 5, 6, or 7
    if (!is.na(input$NoFatalities) && input$NoFatalities > 1 &&
        !(
          input$FatalityTypeSelect %in% c(
            "Multiple with Survivors",
            "Multiple with Bystanders",
            "Multiple with Survivors & Bystanders",
            "Multiple without Survivors",
            "Unknown"
          )
        )) {
      values$rules_error <- append(values$rules_error,
                                   "Fatalities is >1 but Fatality Type is not multiple")
    }
    print("Successfully checked rule 21: Fatality type for multiple fatalities")
    
    ## Check for single fatality
    if (!is.na(input$NoFatalities) && input$NoFatalities == 1 &&
        !(
          input$FatalityTypeSelect %in% c(
            "Single with Survivors",
            "Single with Bystanders",
            "Single with Survivors & Bystanders",
            "Single and Alone",
            "Unknown"
          )
        )) {
      values$rules_error <- append(values$rules_error,
                                   "Fatalities is 1 but Fatality Type is not single")
    }
    print("Successfully checked rule 21: Fatality type for single fatality")
    
    
    
    
    print("Rule 22 Removed")
    ## 22: if the word "pool" (case insensitive) is in the synopsis
    # if (!is.na(input$Synopsis)) {
    #   synopsis <- tolower(input$Synopsis)
    #   if (grepl("pool", synopsis)) {
    #     values$rules_warning <- append(values$rules_warning,
    #                                    "Was there a home pool involved in this incident?")
    #   }
    # }
    
    
    ## 23: if selectedSiteID() isnt 28, then pool fence should not be "Not Applicable"
    if (input$SiteSelect == "Home Pools, Artifical Water, Home Pools" &&
        input$PoolFenceSelect == "Not Applicable") {
      values$rules_error <- append(values$rules_error,
                                   "Site is a pool, pool fence is marked as 'Not Applicable'")
    }
    print("Successfully checked rule 23: Pool fence for pool sites")
    
    ## 24: if selectedSiteID() is not 17, pool fence should be 'Not Applicable'
    if (input$SiteSelect != "Home Pools, Artifical Water, Home Pools" &&
        input$PoolFenceSelect != "Not Applicable") {
      values$rules_error <- append(values$rules_error,
                                   "Site is not a pool, pool fence should be 'Not Applicable'")
    }
    print("Successfully checked rule 24: Pool fence for non-pool sites")
    
    ## 25: if input$24hourtime is 0
    if (!is.na(selectedTimeValue())&& !is.null(selectedTimeValue()) && selectedTimeValue() == 0 ) {
      values$rules_warning <- append(values$rules_warning,
                                     "Are you sure the time was at midnight?")
    }
    print("Successfully checked rule 25: Midnight time check")
    
    ## 26: if input$ActivitySelect is 40 or 44, then input$commercial should be ticked
    if (grepl("commercial", tolower(input$ActivitySelect)) && !input$Commercial) {
      values$rules_error <- append(values$rules_error,
                                   "Activity is commercial, but commercial is not ticked")
    }
    if (grepl("commercial", tolower(input$PurposeSelect)) && !input$Commercial) {
      values$rules_error <- append(values$rules_error,
                                   "Purpose is commercial, but commercial is not ticked")
    }
    print("Successfully checked rule 26: Commercial activity and purpose")
    
    ## 27: If field AlcoholDrugID is 4 or 2, then Bloodalcohol should not be 0
    if (!is.na(input$BloodAlcohol)) {
      if (input$AlcoholDrugSelect %in% c("Alcohol Involved", "Alcohol & Drugs Involved") &&
          input$BloodAlcohol == 0) {
        values$rules_error <- append(values$rules_error,
                                     "Alcohol indicated as involved, but Bloodalcohol is 0")
      }
    }
    print("Successfully checked rule 27: Blood alcohol for AlcoholDrug involvement")
    
    ## 28: If AlcoholDrugID is 3 or 4 and DrugsID is 0 or 11, then say "Drugs involved but no drug selected"
    if (input$AlcoholDrugSelect %in% c("Drugs Involved", "Alcohol & Drugs Involved")  &&
        input$DrugSelect %in% c("Unknown", "Unspecified")) {
      values$rules_error <- append(values$rules_error, "Drugs involved but no drug selected")
    }
    print("Successfully checked rule 28: Drug selection when drugs involved")
    # input$Longitude, input$Latitude
    if(grepl(" Shore", input$SiteSelect) && !is.na(input$Latitude) && !is.na(input$Longitude)){
      point <- st_sfc(st_point(c(input$Longitude, input$Latitude)), crs = 4326)
      
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
    print(values$rules_error)
    print("Successfully checked rule 29: Distances & Sites")
    

    ## 30: If Bloodalcohol is not 0, then AlcoholDrugID should be 2 or 4
    if (!is.na(input$BloodAlcohol)) {
      if (input$BloodAlcohol != 0 &&
          !(input$AlcoholDrugSelect %in% c("Alcohol Involved", "Alcohol & Drugs Involved"))) {
        values$rules_error <- append(
          values$rules_error,
          "Bloodalcohol is not 0 but Alcohol not indicated as involved"
        )
      }
    }
    print("Successfully checked rule 30: Blood alcohol non-zero when AlcoholDrug indicated")
    
    
    # # EXTRA unknown AlcoholDrugID should have NULL value not 0
    # if (!is.na(input$BloodAlcohol)) {
    #   if (input$BloodAlcohol == 0 &&
    #       (selectedAlcoholDrugID() == 0)) {
    #     values$rules_error <- append(
    #       values$rules_error,
    #       "Blood Alcohol should be NULL not 0 when AcoholDrug is Unknown"
    #     )
    #   }
    # }
    
    # TODO
    # if(is.na(input$BoatLength)){
    #   if(selectedActivityID() %in% c(1:4) & input$BoatLength == 0){
    #    values$rules_error <- append(
    #      values$rules_error,
    #      "Boat length should be NULL not 0 when activity is boating "
    #    )
    #   }
    # }
    
    
    ## 31: If the word Boat is in the activity and boatlength is 0, are you sure?
    if (!is.na(input$BoatLength)) {
      if (grepl("Boat", input$ActivitySelect) &&
          input$BoatLength == 0) {
        values$rules_warning <- append(values$rules_warning,
                                       "Boat involved in activity, but no boat length provided")
      }
    }
    print("Successfully checked rule 31: Boat activity and boat length provided")
    
    # ## 32: If residence region is "Overseas", immigration status cannot be NZ Born
    # if (input$ResidentSelect == "Overseas" &&
    #     selectedImmigrationStatusID() == 7) {
    #   values$rules_error <- append(values$rules_error,
    #                                "Residence is Overseas, but immigration status is NZ born")
    # }
    # print("Successfully checked rule 32: Overseas residence and immigration status")
    
    ## 33: Query the database for the Ethnicity ID based on the selected ethnicity
    query <- sprintf(
      "SELECT EthnicityID FROM ethnicity WHERE Description = '%s'",
      input$EthnicitySelect
    )
    
    result <- dbGetQuery(con, query)
    
    if (nrow(result) == 0) {
      values$rules_warning <- append(values$rules_warning,
                                     "Selected ethnicity not found in the database")
    } else {
      ethnicity_id <- result$EthnicityID
      # You can now use ethnicity_id for further processing or validation
    }
    print("Successfully checked rule 33: Ethnicity ID retrieval from database")
    
    # ## 34: check ethnicity ID's match between ethnicity and sub ethnicity
    # query <- sprintf(
    #   "SELECT EthnicityID FROM ethnicity WHERE Description = '%s'",
    #   input$EthnicitySelect
    # )
    # result <- dbGetQuery(con, query)
    #
    # if (nrow(result) == 0) {
    #   values$rules_warning <- append(values$rules_warning,
    #                                  "Selected ethnicity not found in the database")
    # } else {
    #   ethnicity_id <- result$EthnicityID
    #   # You can now use ethnicity_id for further processing or validation
    # }
    # print("Successfully checked rule 34: Ethnicity and sub-ethnicity ID match")
    
    lat_min <- -48.5584319828943
    lat_max <- -32.7480126035835
    lon_min <- 164.282082584236
    lon_max <- 179.667833005821
    
    if (!is.na(input$Latitude) & !is.na(input$Longitude)) {
      # Check if the point is outside the defined boundary
      if (input$Latitude < lat_min ||
          input$Latitude > lat_max ||
          input$Longitude < lon_min || input$Longitude > lon_max) {
        # If true, the point is not within the boundaries of New Zealand or the Chatham Islands
        values$rules_warning <- append(values$rules_warning, "Check your coordinates.")
      }
      print("Successfully checked coordinates against New Zealand boundaries")
    } else if (is.na(input$Latitude) & is.na(input$Longitude)) {
      values$rules_warning <- append(values$rules_warning, "Missing coordinates.")
      print("Successfully checked rule 35: Coordinates presence")
    }
    
    # 36: unknown AlcoholDrugID should have NULL value not 0
    if (!is.na(input$BloodAlcohol)) {
      if (input$BloodAlcohol == 0 &&
          (input$AlcoholDrugSelect == "Unknown")) {
        values$rules_error <- append(
          values$rules_error,
          "Blood Alcohol should be NULL not 0 when AlcoholDrug is Unknown"
        )
      }
    }
    print("Successfully checked rule 36: Blood Alcohol consistency with AlcoholDrug status")
    
    
    output$modalContent <- renderUI({
      print("Rendering modalContent")
      
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
    
    
    observeEvent(input$update, {
      if (length(values$rules_error) > 0) {
        print("Rules errors detected, please fix before inserting")
        
        showModal(
          modalDialog(
            title = "Business Rules Check",
            "Rules errors detected, please fix before inserting.",
            easyClose = TRUE,
            footer = NULL
          )
        )
        
      } else {
        removeModal()
        print("Attempting Update now, please wait . . .")
        
        tryCatch({
          print("Generating Connection ...")
          con <- GetWSNZAzureConnection()
          
          
          Sys.sleep(0.5) # allow for con to reestablish
          
          print("Connection established to: ")
          print(con@info$dbname)
          
          print("Generating Update query")
          
          update_query <- generateUpdateStatement()
          
          #print(length(update_query))
          print(update_query)
          #print(paste("Executing Update Query: ", update_query))  # Debug statement
          
          #dbExecute(con, update_query)
          
          showNotification("Successfully updated record", type = "message")
          
          
          print("Update successful")  # Debug statement
          Sys.sleep(5)
          session$reload()
          
        }, error = function(e) {
          stop(paste("Error:", e$message))  # Debug statement
          Sys.sleep(5)
          
          
          print(update_query())  # Debug statement
          
          
        })
      }
    })
    
  })
}


shinyApp(ui = ui, server = server)
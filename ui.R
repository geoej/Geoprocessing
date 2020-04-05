#######################################
###### ui.R web app for PF map  #######
#######################################

library(shiny)
shinyUI(pageWithSidebar(
        
  headerPanel("Shiny PF map"),   #1
  
  sidebarPanel(                  #2
    h5("Input data"),
          checkboxInput(inputId = "datafile", label = "Load csv data", value = FALSE),
          conditionalPanel(
            condition = "input.datafile",
          fileInput('dat', 'Choose data File',
                    accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
          checkboxInput(inputId = "gridfile", label = "Load grid data", value = FALSE), 
          conditionalPanel(
            condition = "input.gridfile",
          fileInput('grd', 'Choose grid File',
                    accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
          #tags$hr(),
          checkboxInput(inputId = "csvoptions", label = "CSV file loading options", value = FALSE), 
          conditionalPanel(
            condition = "input.csvoptions", 
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', 'Separator',
                       c(Comma=',',
                         Semicolon=';',
                         Tab='\t'),
                       'Comma'),
          radioButtons('quote', 'Quote',
                       c(None='',
                         'Double Quote'='"',
                         'Single Quote'="'"),
                       'Double Quote')        
          ),
    br(), 
    h5("IDW parameter"),
          numericInput("idp", "IDW power", 1, 5, 1),
    br(),
    actionButton("idwbutton", "Update Map"),
    br(),
    h5("Variogram parameters"),
    h6("Model Parameters"),
          numericInput("sill", "Sill", 1, 1000,100),
          numericInput("nugget", "Nugget", 1, 1000, 100), 
          selectInput("models", "Models", choices= c("Nugget" = "Nug",
                                                     "Exponential" = "Exp",
                                                     "Matern" = "Mat", 
                                                     "Spherical" = "Sph",
                                                     "Circular" = "Cir"
                                                     )),
          numericInput("range", "Range", 1, 1000, 100),           
    actionButton("okrigbutton", "Update Map"),
    br(),
    br(),
    h6("            2014, Ebrahim Jahanshiri")
          ),
  mainPanel(
    tabsetPanel(
      tabPanel("Exploratory data analysis",plotOutput("ESDAPlots")),
      tabPanel("IDW Map", plotOutput("MapIdwE1")),
      tabPanel("Simple Kirgged Map", plotOutput("MapOKE1")) 
        ), 
    htmlOutput("selectUIdatX"),
    htmlOutput("selectUIdatY"),
    htmlOutput("selectUIdatZ")
            )))
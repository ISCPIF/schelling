library(shiny)

shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  titlePanel("Schelling Viz"),  
  sidebarPanel(
    fileInput('file1', 'CSV File at the cell level',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    checkboxInput('header', 'Header', FALSE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
    br(), 
    fileInput('file2', 'CSV File at the aggregated level',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    checkboxInput('header', 'Header', FALSE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ',')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Density Grids",
               
               fluidRow(
                 h3("Spatial Distribution of Density Grids"),
                 column(6,
                        sliderInput("ngrid", label = "Grid ID",
                                    min = 0, max = 2033, value = 0, step = 1, animate=T))
               ),
               plotOutput("map_density"),
               tableOutput("indicesgrid"),
               h3("Explore Density Grids' Features"),
               dataTableOutput("gridresults"),
               "Summary :",
               tableOutput("summarygrids")),
      tabPanel("Schelling Map",

              fluidRow(
                h3("Model Parameterization"),
                tableOutput("paramtable"),
                h3("Spatial Distributions"),
               column(6,
                      sliderInput("step", label = "Simulation Step",
                           min = 0, max = 99, value = 0, step = 1, animate=T)),
               column(6,
                      selectInput("var", label = "Quantity to map",
                                  choices = c("Density" = "totalPop",
                                              "% Greens" = "pctgreens",
                                              "% Reds" = "pctreds",
                                              "% Unsatisfied"="pctunsatisfied"
                                              ), selected = "pctgreens"))
             ),
             plotOutput("map_cell"),
            h3("Segregation measures"),
            tableOutput("measurestable"))

  
    
))
))

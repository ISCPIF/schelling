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
            tableOutput("measurestable")),
      tabPanel("Selection of Indicators",
             h3("Measures correlation"),
             plotOutput("plotindexes"),
             h3("Redundant measures :"),
             "Entropy (with dissimilarity), IsolationGreen (with ExposureGreenToRed),
             IsolationRed (with ExposureRedToGreen), DeltaGreenRed (with dissimilarity),
             ExposureGreenRed (with ExposureRedToGreen and DeltaRedGreen), DeltaRedGreen (with dissimilarity)",
             h3("Selected measures' correlation :"),
             plotOutput("plotindexes2"),
             "Correlations are computed over 23550 runs of a schelling model.
             Parameters are set with a Sobol suite"
             ),
      tabPanel("Sensitivity Analysis",
               h3("Sensitivity of segregation measures to parameter values"),
               fluidRow(column(6,
                      selectInput("index", label = "Segregation Index",
                                  choices = c("Moran's I" = "moran",
                                              "Dissimilarity" = "dissimilarity",
                                              "Exposure of Reds to Greens" = "exposureRedGreen",
                                              "Entropy"="entropy",
                                              "Isolation of Reds" ="isolationRedGreen",
                                              "Share of Unsatisfied"="unsatisfiedRatio"
                                  ), selected = "moran")),
                      column(6,
                             selectInput("param", label = "Sensitivity to Parameter",
                                         choices = c("Tolerance Level" = "ToleranceLevel",
                                                     "Vacancy Rate" = "VacancyRate"
                                         ), selected = "ToleranceLevel")),
                      column(6,
                             sliderInput("bins", label = "Number of bins for the parameter",
                                         min = 2, max = 100, value = 10, step = 1))
      ),
              
#                plotOutput("sensitivity1"),
plotOutput("sensitivity2"),
tableOutput("test"),
dataTableOutput("sensitivity")
      )
  
    
))
))

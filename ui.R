library(shiny) #import shiny
college = read.csv("college.csv")
shinyUI(fluidPage(tags$head(tags$style(type="text/css", "#school table {font-family: Lucida Sans Unicode, Lucida Grande, Sans-Serif; text-align: left; font-size:12px; line-height:100%;}"
                                       )), #style for page
                      
                  #set layouts
                  titlePanel("College Finder Tool"),
                  sidebarLayout(
                    sidebarPanel(
                      conditionalPanel(condition = "input.condTabs == 'College Database'",
      
      helpText("To filter your results, please enter information for the following categories:"),                                 
      br(),
      h5("Institution Characteristics"),
      selectInput("Type",
                  label = 'Institution Type',
                  c("All", unique(as.character(college$Institution.Type)))),
      conditionalPanel(
                   condition = "input.Type == 'Public'",
                  checkboxInput("Outstate", label = 'Out of State Tuition?', value = FALSE)),
      selectInput("Campus",
                  label = 'Campus Type',
                  c("Any", sort(unique(as.character(college$Campus))))),        
      br(),
      h5("Size of school"),
      sliderInput("Enrollment",
                  label = 'Number of students',
                  min = 0, max = 100000, value = c(0, 100000), step = 1000)
      
      
                      
      
      ),
      #set conditions for panels and show what appears in each tab
      conditionalPanel(condition = "input.condTabs == 'Weights'", h5('Weigh the importance of the following variables:'),
      numericInput("Academics", label = 'How important is it to find schools where you are most likely to be admitted?', value = 0, 
                   min = 0, max = 100,
                   step = 5),
      numericInput("Cost", label = 'How important is it to find schools that are within your budget?', value = 0, 
                   min = 0, max = 100, step = 5),
      numericInput("Scholarship", label = 'How important is it to receive a scholarship from the school?', value = 0,
                   min = 0, max = 100, step = 5),
      br(),
      conditionalPanel(condition = "input.Cost > 0 | input.Academics > 0", 
      h5('Enter the following information')),
      conditionalPanel(condition = "input.Cost > 0",
      sliderInput("FundsAvailable", label = 'Please enter how much you are willing to spend per year on college tuition and costs.  Refer to FAFSA return, if necessary.', value = 0, min = 0, max = 80000, step = 1000)),
      conditionalPanel(condition = "input.Academics > 0",
      sliderInput("userGPA", label = 'Please enter your GPA', min = 0, max = 4.00, value = 2, step = .01),
      sliderInput("userSAT", label = 'Please enter your SAT Score', min = 800, max = 2400, value = 800, step = 5)),
      br(),
      h5('Enter the state for which you are looking for schools'),
      selectInput("State",
                  label = 'State',
                  c("USA", sort(unique(as.character(college$State)))))              
      
      )),
  
  
    
    mainPanel(tabsetPanel(id = "condTabs", type = "tab",
                          tabPanel("Weights", fluidRow(column(3, uiOutput("value")))),
                          tabPanel("College Database", dataTableOutput(outputId="school") )
     
      
                          ))
  )))

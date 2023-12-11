# Load the required libraries
library(shiny)
library(ggplot2)

df <- read.csv("data.csv")


# Define UI
ui <- navbarPage(
  title = "Interactive App",
  tabPanel("Intro", 
           fluidPage(
             titlePanel("Drug Deaths and Crimes"),
             mainPanel(
               p("Drugs and Crimes have been commonly linked together throughout recent history. From the 1971 ", a(" War on Drugs", href = "https://www.history.com/topics/crime/the-war-on-drugs"), " to recent days where some types of drugs are beginning to be ", a("legalized", href = "https://lcb.wa.gov/education/know-the-law-cannabis")),
               br(),
               p("What does this mean for us as residents of Washington State and commuters to the greater Seattle Area? Has drug use really influenced crimes? Better yet, have the legalization of drugs in Washington state correlate to the amount of crimes committed, or the number of drug overdoses in our state?"),
               br(),
               br(),
               p("Let's find out through this interactive tool!")
             )
           )
  ),
  
  tabPanel("Drugs", 
           fluidPage(
             
             titlePanel("Scatterplot Page"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("drugType", "Select Drug Type:",
                             choices =df$drug_type ,
                             selected = "Drug A")
               ),
               
               mainPanel(
                 plotOutput("scatterplot")
               )
             )
           )
  ),
  tabPanel("Overdose Histogram",
           fluidPage(
             titlePanel("Overdose Histogram"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("drugType", "Select Drug Type:",
                             choices =df$drug_type ,
                             selected = "Drug A")
               ),
               mainPanel(
                 plotOutput("histogram")
               )
             )
           )
  ),
  tabPanel("Analysis",
           fluidPage(
             titlePanel("Drug Overdose Analysis"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("analysisYear", "Select Year:",
                             choices = unique(df$year),
                             selected = max(df$year)),
                 radioButtons("sortOrder", "Sort Order:",
                              choices = c("Ascending", "Descending"),
                              selected = "Descending")
               ),
               mainPanel(
                 plotOutput("analysisBarChart")
               )
             )
           )
  ),
  
  tabPanel("Scatter", 
           titlePanel("Drug and Crime Scatterplot"),
           sidebarLayout(
             sidebarPanel(
               radioButtons("selectedDrug", "Select Drug Type:",
                            choices = unique(df$drug_type),
                            selected = unique(df$drug_type)[1])
             ),
             mainPanel(
               plotOutput("scatterplot2")
             )
           )),
  tabPanel("Summary", 
           fluidPage(
             titlePanel("Drug Deaths and Crimes"),
             mainPanel(
               p("Drugs and Crimes have been commonly linked together throughout recent history. From the 1971 ", a(" War on Drugs", href = "https://www.history.com/topics/crime/the-war-on-drugs"), " to recent days where some types of drugs are beginning to be ", a("legalized", href = "https://lcb.wa.gov/education/know-the-law-cannabis")),
               br(),
               p("What does this mean for us as residents of Washington State and commuters to the greater Seattle Area? Has drug use really influenced crimes? Better yet, have the legalization of drugs in Washington state correlate to the amount of crimes committed, or the number of drug overdoses in our state?"),
               br(),
               br(),
               p("Let's find out through this interactive tool!")
             )
           )
  ),
)

# Define server
server <- function(input, output) {
  
  data <- read.csv("data.csv")
  
  filteredData <- reactive({
    data[data$drug_type == input$drugType, ]
  })
  
  output$scatterplot <- renderPlot({
    ggplot(filteredData(), aes(x = year, y = count, size = crimes)) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1)) +
      geom_vline(xintercept = 2012, linetype = "dashed", color = "red") +
      scale_size_continuous(name = "Number of Crimes", guide = "legend") + 
      labs(title = paste("Scatterplot for", input$drugType),
           x = "Year", y = "Overdoses Per Year")
  })
  
  filteredDataHist <- reactive ({
    data[data$drug_type == input$drugType, ]
  })
  
  output$histogram <- renderPlot({
    ggplot(filteredDataHist(), aes(x = year, y = count)) +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1)) +
      geom_vline(xintercept = 2012, linetype = "dashed", color = "red") +
      
      geom_bar(stat = "identity", fill = "black") +
      labs(title = paste("Overdose Histogram for", input$drugType),
           x = "Year", y = "Overdoses")
  }) 
  
  filteredDataAnalysis <- reactive({
    data[data$drug_type != "any_drug" & data$year == input$analysisYear, ]
  })
  
  output$analysisBarChart <- renderPlot({
    if (input$sortOrder == "Ascending") {
      ggplot(filteredDataAnalysis(), aes(x = reorder(drug_type, count), y = count)) +
        geom_bar(stat = "identity", fill = "green") +
        coord_flip()+ 
        labs(title = paste("Drug Overdose Analysis for Year", input$analysisYear),
             x = "Drug Type", y = "Total Overdoses")
    } else {
      ggplot(filteredDataAnalysis(), aes(x = reorder(drug_type, -count), y = count)) +
        geom_bar(stat = "identity", fill = "green") +
        coord_flip()+ 
        labs(title = paste("Drug Overdose Analysis for Year", input$analysisYear),
             x = "Drug Type", y = "Total Overdoses")
    }
  })
  
  selected_opacity <- reactive({
    ifelse(df$drug_type == input$selectedDrug, 1, 0.3)
  })
  
  output$scatterplot2 <- renderPlot({
    ggplot(df, aes(x = year, y = count, size = crimes, color = drug_type, alpha = selected_opacity())) +
      geom_point() +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1)) +
      labs(title = paste("Scatterplot for", input$selectedDrug),
           x = "Year", y = "Count") +
      scale_size_continuous(name = "Number of Crimes", guide = "legend") +
      scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
      theme_minimal() +
      theme(legend.position = "right")
  })
}
shinyApp(ui = ui, server = server)
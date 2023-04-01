# Load required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)
data= read.table("bands.data",sep=',')
data[data == '?'] ="NA"


data= setNames(data, c("timestamp","cylinder","customer","job_number","grain_screened"
                       ,"ink_color","proof_on_ctd_ink","blade_mfg","cylinder_division","paper_type","ink_type"," direct_steam",
                       "solvent_type","solvent_type","press_type","press","unit_number","cylinder_size","location","plating_tank",
                       "proof_cut","viscosity","caliper","ink_temperatur","humifity","roughness","blade_pressure",
                       "varnish_pct"," press_speed","ink_pct","solvent_pct","ESA_Voltage","ESA_Amperage","wax","hardener","roller_durometer","urrent_density","anode_space_ratio",
                       "chrome_content","band_type"
))
View(data)
#data cleaning
colSums(is.na(data))
colnames(data) <- make.unique(names(data))
data <- data[!duplicated(data), ]

data <- data                                              # Duplicate data frame
for(i in 1:ncol(data)) {                                   # Replace NA in all columns
  data[ , i][is.na(data[ , i])] <- mean(data[ , i], na.rm = TRUE)
}

colSums(is.na(data))
str(data)
data=type.convert(data)


ui <- fluidPage(
  titlePanel("20BDS0008 DASHBOARD"),
  sidebarLayout(
    sidebarPanel(
      # Add input controls such as selectInput, textInput, etc.
      selectInput("plot_var", "Choose a variable to plot:",
                  choices = c("viscosity", "caliper", "roughness", "press_speed")),
      selectInput("ink_color", "Choose an ink color:",
                  choices = unique(data$ink_color)),
      selectInput("paper_type", "Choose a paper type:",
                  choices = unique(data$paper_type))
    ),
    mainPanel(
      tabsetPanel(
        # Add each plot to a separate tab using tabPanel()
        tabPanel("Plot 1", plotOutput("plot1")),
        tabPanel("Plot 2", plotOutput("plot2")),
        tabPanel("Plot 3", plotOutput("plot3")),
        tabPanel("Plot 4", plotOutput("plot4"))
      )
    )
  ),
  # Set some basic colors for the dashboard
  theme = shinytheme("cerulean")
)

server <- function(input, output) {
  # Create a reactive subset of the data based on the user's input
  filtered_data <- reactive({
    data %>% 
      filter(ink_color == input$ink_color) %>%
      filter(paper_type == input$paper_type) %>%
      select(timestamp, input$plot_var)
  })
  
  # Create plot 1
  output$plot1 <- renderPlot({
    ggplot(filtered_data(), aes(x = timestamp, y = !!sym(input$plot_var))) +
      geom_point(color = "blue") +
      labs(x = "Timestamp", y = input$plot_var) +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray"),
            axis.line = element_line(color = "black"),
            text = element_text(color = "black"))
  })
  
  # Create plot 2
  output$plot2 <- renderPlot({
    ggplot(filtered_data(), aes(x = timestamp, y = !!sym(input$plot_var))) +
      geom_line(color = "red") +
      labs(x = "Timestamp", y = input$plot_var) +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray"),
            axis.line = element_line(color = "black"),
            text = element_text(color = "black"))
  })
  
  # Create plot 3
  output$plot3 <- renderPlot({
    ggplot(filtered_data(), aes(x = !!sym(input$plot_var))) +
      geom_histogram(color = "green", fill = "lightgreen") +
      labs(x = input$plot_var, y = "Count") +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray"),
            axis.line = element_line(color = "black"),
            text = element_text(color = "black"))
  })
  
  # Create plot 4
  output$plot4 <- renderPlot({
    ggplot(filtered_data(), aes(x = !!sym(input$plot_var))) +
      geom_density(color = "purple", fill = "violet") +
      labs(x = input$plot_var, y = "Density") +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray"),
            axis.line = element_line(color = "black"),
            text = element_text(color = "black"))
  })
}

# Run the dashboard using runApp()
shinyApp(ui = ui, server = server)

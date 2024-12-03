library(shiny)
library(dplyr)
library(ggplot2)

data <- as.data.frame(HairEyeColor)

ui <- fluidPage(
    titlePanel("Hair and Eye Color of Students"),
    
    h3("This app is designed to examine the hair and eye color of students at the University of Delaware."),
    "Using the filters on left side, filter by hair color, eye color, and gender. The corresponding number of students who have these characteristics will be shown in the bar graph and table on the right hand side.",
# The title Panel and above text provide useful information for the user of the app on how to use the app and what the resulting graph and table represent based on their selections. 
   
   sidebarLayout(
        sidebarPanel(
            selectInput("hair", "Select Hair Color:",
                        choices = c("All", "Black", "Brown", "Red", "Blond"),
                        multiple = TRUE, selected = "All"),
            selectInput("eye", "Select Eye Color:",
                        choices = c("All", "Brown", "Blue", "Hazel", "Green"),
                        multiple = TRUE, selected = "All"),
            selectInput("sex", "Select Gender:",
                        choices = c("All", "Male", "Female"),
                        multiple = TRUE, selected = "All"),
            br(),
            em("Remove a selected option using the backspace key when unwanted option is selected, indicated by blue highlight."),
# The select input feature in the side bar allows the user to filter the dataset by hair colour, eye colour and gender. This feature directly contributes to the user's experience by allowing them to select traits of interest to see how many students have the characteristics they select. 
# This was a feature from Assignment 3.
     ),

      mainPanel(
          tabsetPanel(
            tabPanel("Bar Graph",
              plotOutput("barPlot"),
              textOutput("summaryText")),
            tabPanel("Data Table",
                    downloadButton("downloadData", "Download Data Table"),
                    tableOutput("dataTable"))
          )
# The main panel has two tabs, one tab for the bar graph and another for the data table. The main panel will display a bar graph and summary text to give the user both visual and text feedback based on their selections in the side panel. 
# The second tab for the data table shows a representation of the same data selected in the side panel but in table format. The data table also has a download button, allowing the user to save their selections. 
# Creating two tabs contributes to the user's experience because it provides them options for how they would like to see their data and selected characteristics. 
# The two tabs feature is new in Assignment 4. 
        )
     )
)

server <- function(input, output) {
  filtered_data <- reactive({
    filtered <- data
    if("All" %in% input$hair == FALSE) {
     filtered <- filtered[filtered$Hair %in% input$hair, ]
    }
    if("All" %in% input$eye == FALSE) {
      filtered <- filtered[filtered$Eye %in% input$eye, ]
   }
    if("All" %in% input$sex == FALSE) {
      filtered <- filtered[filtered$Sex %in% input$sex, ]
   }
    filtered <- filtered %>%
      rename(`Number of Students` = Freq)
    return(filtered)
})
# This reactive filtered data dataset has to be included in the code for the app so that the app will filter the data based on what the user selects the input as in the side panel.
# The If("All"... expressions are necessary because they check if the "all" option is selected in the side panel, and if it is not selected, then the dataset is filtered to include only what the user selected the input to be. 
# While not directly impacting the user's experience of the app, this is necessary because it allows the app to work correctly and filter the data according to what the user selects the input to be. 

    output$barPlot <- renderPlot({
       data_filtered <- filtered_data()
# This output bar plot function creates a bar graph from the filtered dataset. 
       
    summary_data <- data_filtered %>%
      group_by(Hair, Eye) %>% 
      summarise(total_students = sum(`Number of Students`), .groups = "drop")
# The data used to create the plot has to be grouped by hair and eye color and summarized so that the total number of students can be calculated and plotted on the bar graph. 
    
       ggplot(summary_data, aes(x = Hair, y = total_students, fill = Eye)) +
         geom_bar(stat = "identity", position = "dodge") + 
         labs(title = "Number of Students with Selected Hair and Eye Color",
              x = "Hair Color", y = "Number of Students") + 
         theme_minimal() +
         theme(plot.title = element_text(face = "bold")) +
         scale_fill_manual(values = c("Brown" = "chocolate4", "Blue" = "slategray2", "Hazel" = "lightgoldenrod4", "Green" = "darkolivegreen"))
    })
# This ggplot function is creating a bar graph to represent the dataset based on what the user selected the input to be. This directly contributes to the user experience because it allows the user to visualize the number of students who have each hair colour, eye colour and gender that they selected. 
# This was a feature in Assignment 3. 
    
    output$summaryText <- renderText({
      data_filtered <- filtered_data()
      total_students <- sum(data_filtered$`Number of Students`)
      paste("Total number of students selected:", total_students)
    })
# This output summary text function creates a sentence that changes based on the total number of students who have the selected characteristics the user chose. This also directly contributes to the user experience because it allows the user to easily see the total number of students with the traits they have selected.
# This was a feature in Assignment 3.
    
    output$dataTable <- renderTable({
      filtered_data()
    }, rownames = FALSE)
# The output data table is created based on what the user selects in the side panel and changes based on their input. This also directly contributes to the user experience and allows the user to see the raw data of how many students have their selected characteristics.
# Providing both a data table and graph is useful for the user because it allows them to see the exact numbers representing their selected inputs in the table and the graph allows the user to easily interpret the data. 
# The data table feature is new in Assignment 4. 
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("filtered_data.csv")
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE)
      }
    )
# The download data function allows the user to download the data table they created as a csv file and open it in excel. This also directly contributes to the user's experience as it allows them to save and download the raw data and the corresponding number of students for each selection. 
# The download button is new in Assignment 4. 
}


shinyApp(ui = ui, server = server)

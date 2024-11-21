library(shiny)
library(dplyr)
library(ggplot2)
data <- as.data.frame(HairEyeColor)

ui <- fluidPage(
    titlePanel("Hair and Eye Color of Students"),
    
    h3("This app is designed to examine the hair and eye color of students at the University of Delaware."),
    "Using the filters on left side, filter by hair color, eye color, and gender. The corresponding number of students who have these characteristics will be shown in the bar graph on the right side.",
# The title Panel and above text provide useful information for the user of the app on how to use the app and what the resulting graph represents from their selections. 
   
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
            em("Remove a selected option using the backspace key when unwanted option is selected, indicated by blue highlight.")
        ),
# The select input feature in the side bar allows the user to filter the dataset by hair colour, eye colour and gender. This feature directly contributes to the user's experience by allowing them to select traits of interest to see how many students have the traits they select. 
        mainPanel(
          plotOutput("barPlot"),
          textOutput("summaryText")
# The main panel will display a bar graph and a summary text to show the user both visual and text feedback based on their selections in the side panel. 
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
      summarise(total_students = sum(Freq), .groups = "drop")
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
 
    output$summaryText <- renderText({
      data_filtered <- filtered_data()
      total_students <- sum(data_filtered$Freq)
      paste("Total number of students selected:", total_students)
    })
}
# This output summary text function creates a sentence that changes based on the total number of students who have the selected characteristics the user chose. This also directly contributes to the user experience because it allows the user to easily see the total number of students with the traits they have selected.


shinyApp(ui = ui, server = server)

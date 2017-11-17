# UI for looking at learnr-related submitted events
library(shiny)
library(submittr)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Event browser"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         textInput("professor", "Grader ID"),
         textInput("password", "Password"),
         textOutput("validlogin"),
         actionButton("connect", "Login"),
         textInput("table_name", "Name of event table", value = "higgins"),
         textOutput("tablestate", inline = TRUE)

      ),

      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("all_events")
      )
   )
)

server <- function(input, output, session) {
   state <- reactiveValues()
   state$validated <- FALSE
   state$dbconnection <- "invalid"
   state$Event_table <- NULL
   observeEvent(input$connect, {
     cat("pressed login button\n")
     if (input$password == "demonstration") state$validated <- TRUE
     else state$validated <- FALSE
   })
   output$validlogin <- renderText({ state$validated
     # paste(input$password, "produces valid login", as.character(state$validated))
     ifelse(state$validated, "Access authorized", "Please log in")
   })
   output$tablestate <- renderText({
     if (is.null(state$Event_table)) "No such table found."
     else paste("Event table read in ", Sys.time())
   })
   observe({
     if (state$validated) {
       submittr:::establish_db_connection() # puts connection in submittr:::db_info

       cat("Connection is", capture.output(submittr:::db_info$this_connection))
       if (TRUE){ #dbExistsTable(state$dbconnection, input$table_name)) {
         state$Event_table <- dbReadTable(submittr:::db_info$this_connection, input$table_name)
         dbDisconnect(submittr:::db_info$this_connection)
       } else {
         state$Event_table <- NULL
       }
     }
   })
   output$all_events <- renderTable({
     if (!is.null(state$Event_table))
       state$Event_table %>% mutate(timestamp = as.character(timestamp))
     else data_frame("status" = "No table yet available.")
   })
}

# Run the application
shinyApp(ui = ui, server = server)


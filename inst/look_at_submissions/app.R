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
         textInput("user", "Grader ID"),
         textInput("password", "Password"),
         textOutput("validlogin"),
         actionButton("connect", "Login"),
         textInput("table_name", "Name of event table", value = "higgins"),
         textOutput("tablestate", inline = TRUE),
         hr(),
         p("Selecting none means all will be used."),
         selectizeInput("documents", "Select tutorials", c(All = ""), multiple = TRUE),
         selectizeInput("items", "Select items:", c(All = ""), multiple = TRUE),
         selectizeInput("groups", "Select student group:", c(All = ""), multiple = TRUE),
         selectizeInput("users", "Select students:", c(All = ""), multiple = TRUE),
         dateRangeInput("dates", "In interval:", start = "2017-01-01")

         ),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Counts", tableOutput("event_count")),
          tabPanel("Raw events", tableOutput("all_events")),
          type = "tabs", id = "main_panel")
      )
   )
)

# Convenience function for constructing menus
with_all <- function(x) {
  c(All = "", unique(x))
}
get_students <- function(groups) {
  if (length(groups) == 0) unique(state$user) # students get access only to themselves
  else {
    Tmp <-
      submittr:::db_info$Password_table %>%
      filter(group %in% groups) %>% select(user)
    unique(Tmp$user)
  }
}

server <- function(input, output, session) {
  # TO DO: Find a way to pass the parameters into establish_db_connection()
  submittr:::establish_db_connection() # puts connection in submittr:::db_info
  cat("Connection is", capture.output(submittr:::db_info$this_connection))
  state <- reactiveValues()
  state$user <- ""
  state$validated <- FALSE
  state$Event_table <- NULL
  state$groups <- ""
  observeEvent(input$connect, {
    ind <- which(input$user == submittr:::db_info$Password_table$user)
    if (length(ind) > 0 && input$password == submittr:::db_info$Password_table$passwd[ind]) {
      state$user <- input$user
      state$validated <- TRUE
      if (submittr:::db_info$Password_table$role[ind] == "prof") {
        if (submittr:::db_info$Password_table$group == "ALL") {
          # access to all the groups
          state$groups = unique(submittr:::db_info$Password_table$group)
        } else {
          # just the specifically authorized groups
          state$groups <- strsplit(submittr:::db_info$Password_table$group[ind], ":")
        }
      } else {
        # but students get access only to themselves
        state$groups = NULL
      }


    }
  })
  observeEvent(state$Event_table, {
    # update the filter choices
    if (! is.null(state$Event_table)) {
      updateSelectizeInput(session, "documents", choices = with_all(state$Event_table$tutorial_id))
      updateSelectizeInput(session, "items", choices = with_all(state$Event_table$label))
      # To do ....
      # narrow the following so that only the groups for the logged-in instructor will show
      # or, better, eliminate the rows in the password and event tables at the very
      # beginning of the login.
      if (length(state$groups) == 1) {
        updateSelectizeInput(session, "groups", choices = state$groups, selected = state$groups)
      } else {
        updateSelectizeInput(session, "groups", choices = with_all(state$groups))
      }
    }
  })
  observe({
    if (state$validated) {
      if (is.null(state$groups)) choices <- c(state$user)
      else choices <- with_all(get_students(state$groups))
      updateSelectizeInput(session, "users", choices = choices)
    }
  })
  output$validlogin <- renderText({ state$validated
    ifelse(state$validated, "Access authorized", "Please log in")
  })
  output$tablestate <- renderText({
    if (is.null(state$Event_table)) "No such table found."
    else paste("Event table read in ", Sys.time())
  })
  observe({
    if (state$validated) {
      if (dbExistsTable(submittr:::db_info$this_connection, input$table_name)) {
        state$Event_table <- dbReadTable(submittr:::db_info$this_connection, input$table_name)
        dbDisconnect(submittr:::db_info$this_connection)
      } else {
        state$Event_table <- NULL
      }
    }
  })
  output$all_events <- renderTable({
    if (!is.null(state$Event_table)) {
      state$Event_table %>%
        mutate(timestamp = as.character(timestamp)) %>%
        select(timestamp, user_id, type, label)
    } else {
      data_frame("status" = "No event table yet available.")
    }
  })
  output$event_count <- renderTable({
    if (!is.null(state$Event_table)) {
      state$Event_table %>%
        group_by(login_id, tutorial_id, label) %>%
        summarize(count = n(), latest = as.character(max(timestamp)))
    } else {
      data_frame("status" = "No event table yet available.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)


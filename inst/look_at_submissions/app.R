# UI for looking at learnr-related submitted events
library(shiny)
library(submittr)

# Convenience function for constructing menus
with_all <- function(x) {
  c(All = "", unique(x))
}
# detect when the input item has the implicit all selected instead of any
# of the actual choices
implicit_all_selected <- function(x) is.null(x) || "" %in% x

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Event browser"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         actionButton("Auto_login", "Quick login"), # DELETE THIS AFTER DEBUGGING
         textInput("user", "Grader ID"),
         textInput("password", "Password"),
         textOutput("validlogin"),
         actionButton("connect", "Login"),
         textInput("table_name", "Name of event table", value = "higgins"),
         textOutput("tablestate", inline = TRUE),
         hr(),
         selectizeInput("documents", "Select documents", with_all(NULL), multiple = TRUE),
         selectizeInput("items", "Select items:", with_all(NULL), multiple = TRUE),
         selectizeInput("groups", "Select student group:", with_all(NULL), multiple = TRUE),
         selectizeInput("submitters", "Select students:", with_all(NULL), multiple = TRUE),
         dateRangeInput("dates", "In interval:", start = "2017-01-01")

         ),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Counts",
                   selectizeInput("breakdown_vars", "Break down by:",
                                  choices = c("None" = "", students = "login_id",
                                              documents = "tutorial_id",
                                              items = "label",
                                              type = "type",
                                              "is correct" = "correct"),
                                  multiple = TRUE),
                   tableOutput("event_count")
                   ),
          tabPanel("Questions",
                   checkboxInput("questions_by_student", 'Breakdown by student', FALSE),
                   tableOutput("question_display")
                   ),
          tabPanel("Code",
                   checkboxInput("code_by_student", 'Breakdown by student', FALSE),
                   htmlOutput("code_display")
                   ),
          tabPanel("Raw events",
                   tableOutput("all_events")
                   ),
          type = "tabs", id = "main_panel")
      )
   )
)



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
  state$permitted_groups <- ""
  state$selected_submitters <- NULL
  state$selected_documents <- NULL
  state$selected_items <- NULL
  state$selected_groups <- NULL

  # Shortcut for loging in while in development
  observeEvent(input$Auto_login, {
    state$validated <- TRUE
    # all student groups permitted
    state$permitted_groups <- unique(submittr:::db_info$Password_table$group)
    state$user <- "Danny"
    updateTextInput(session, "user", value = state$user)
  })

  # handle login events
  observeEvent(input$connect, {
    ind <- which(input$user == submittr:::db_info$Password_table$user)
    if (length(ind) > 0 && input$password == submittr:::db_info$Password_table$passwd[ind]) {
      state$user <- input$user
      state$validated <- TRUE
      # state$permitted_groups is the permitted set of groups from which the prof can choose
      if (submittr:::db_info$Password_table$role[ind] == "prof") {
        if (submittr:::db_info$Password_table$group[ind] == "ALL") {
          cat("ALL the groups are available.\n")
          # access to all the groups
          state$permitted_groups <- unique(submittr:::db_info$Password_table$group)
        } else {
          # just the specifically authorized groups
          cat("Only specifically authorized groups.")
          state$permitted_groups <- strsplit(submittr:::db_info$Password_table$group[ind], ":")
        }
      } else {
        state$permitted_groups <- NULL
      }
    }
  })
  observe({
    if (is.null(state$permitted_groups)) # students get access only to themselves
      state$permitted_submitters <- input$user
    else
      state$permitted_submitters <-
        unique(submittr:::db_info$Password_table %>%
                 filter(group %in% state$permitted_groups) %>%
                 .$user)
  })
  # connect and disconnect from the database
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

  # get the selected_ components
  selected_documents <- reactive({
    cat("Setting selected documents\n")
    if (implicit_all_selected(input$documents))
      unique(state$Event_table$tutorial_id)
    else
      input$documents
  })
  selected_items <- reactive({
    cat("Setting selected ITEMS\n")
    if (implicit_all_selected(input$items))
      unique(state$Event_table %>%
                 filter(tutorial_id %in% selected_documents()) %>%
                 .$label)
    else input$items
  })
  selected_groups <- reactive({
    cat("Setting selected GROUPS\n")
    if (implicit_all_selected(input$groups))
      state$permitted_groups
    else
      input$groups
  })
  selected_submitters <- reactive({
    cat("Setting selected SUBMITTERS\n")
    if (implicit_all_selected(input$submitters))
      state$permitted_submitters
    else
      input$submitters
  })
  # Setting menu choices

  # initialize the choice of documents and items when the event table is read in.
  observeEvent(state$Event_table, {
    state$permitted_documents <- unique(state$Event_table$tutorial_id)
    state$permitted_items <- unique(state$Event_table$label)
  })
  observeEvent(state$permitted_documents, {
    updateSelectizeInput(session, "documents", choices = with_all(state$permitted_documents))
  })
  # Give a choice of only those items in the permitted documents.
  observe({
    if (is.null(state$Event_table)) return()
    choices <-
      if (implicit_all_selected(input$documents)) {
        # no documents explicitly selected, so use all documents and all items in them
        with_all(state$Event_table %>%
                   filter(tutorial_id %in% state$permitted_documents) %>%
                   .$label)
      } else {
        unique(state$Event_table %>%
                 filter(tutorial_id %in% input$documents) %>%
                 .$label)
      }
    updateSelectizeInput(session, "items", choices = with_all(choices))
  })

  # Set menu choice of groups
  observeEvent(state$permitted_groups, {
      if (length(state$permitted_groups) == 1) {
        updateSelectizeInput(session, "groups",
                             choices = state$permitted_groups, selected = state$permitted_groups)
      } else {
        updateSelectizeInput(session, "groups",
                             choices = with_all(state$permitted_groups))
      }
    })


  # set the menu choice of submitters
  observe({
    if (length(state$permitted_submitters) == 1)
      updateSelectizeInput(session, "submitters",
                           choices = state$permitted_submitters, selected = state$permitted_submitters)
    else
      updateSelectizeInput(session, "submitters",
                           choices = with_all(state$permitted_submitters))
  })




  output$validlogin <- renderText({ state$validated
    ifelse(state$validated, "Access authorized", "Please log in")
  })

  output$tablestate <- renderText({
    if (is.null(state$Event_table)) "No such table found."
    else paste("Event table read in ", Sys.time())
  })

  # get all relevant submissions, given the document, item, student choices
  relevant_submissions <- reactive({
    Res <-
      if (is.null(state$Event_table)) {
        NULL
    } else {
      state$Event_table %>%
        filter(login_id %in% selected_submitters()) %>%
        filter(tutorial_id %in% selected_documents()) %>%
        filter(label %in% selected_items())
      ## NEED TO ADD IN DATE LIMITS
    }

    Res
  })

  output$all_events <- renderTable({
    Tmp <- relevant_submissions()
    if (is.null(Tmp)) data_frame("status" = "No event table yet available.")
    else {
      Tmp %>%
        mutate(timestamp = as.character(timestamp)) %>%
        select(timestamp, login_id, type, label, data)
    }
  })
  output$event_count <- renderTable({
    Tmp <- relevant_submissions()
    if (is.null(Tmp)) data_frame("status" = "No event table yet available.")
    else {
      if ("login_id" %in% input$breakdown_vars) Tmp <- Tmp %>% group_by(login_id, add = TRUE)
      if ("tutorial_id" %in% input$breakdown_vars) Tmp <- Tmp %>% group_by(tutorial_id, add = TRUE)
      if ("label" %in% input$breakdown_vars) Tmp <- Tmp %>% group_by(label, add = TRUE)
      if ("type" %in% input$breakdown_vars) Tmp <- Tmp %>% group_by(type, add = TRUE)
      if ("correct" %in% input$breakdown_vars) Tmp <- Tmp %>% group_by(correct, add = TRUE)
      Tmp %>%
        summarize(count = n(), latest = as.character(max(timestamp)))
    }
  })
  output$question_display <- renderTable({
    Tmp <- relevant_submissions()
    if (is.null(Tmp)) data_frame("status" = "No event table yet available.")
    # pull out just the questions
    Tmp <- Tmp %>%
      filter(type == "question_submission")
    # extract out the contents of the <data> field
    data_fields <- Tmp$data
    question <- correct <- answers <- character(nrow(Tmp))
    for (k in 1:nrow(Tmp)) {
      raw <- fromJSON(data_fields[k])
      answers[k] <- paste(paste0("(", raw$answers, ")"), collapse = "::")
      question[k] <- raw$question
      correct[k] <- raw$correct
    }
    Tmp$answers <- answers
    Tmp$correct <- correct
    Tmp$question <- question
    Tmp <- Tmp %>% group_by(label, answers, correct)
    if (input$questions_by_student) Tmp <- Tmp %>% group_by(login_id, add = TRUE)

    Tmp %>%
      summarise(count = n(), latest = as.character(max(timestamp))) %>%
      arrange(label, answers)
  })
  output$code_display <- renderText({
    Tmp <- relevant_submissions()
    if (is.null(Tmp)) data_frame("status" = "No event table yet available.")
    # pull out just the questions
    Tmp <- Tmp %>%
      filter(type == "exercise_submission")
    # extract out the contents of the <data> field
    data_fields <- Tmp$data
    checked <- message <- correct <- code <- character(nrow(Tmp))
    for (k in 1:nrow(Tmp)) {
      raw <- fromJSON(data_fields[k])
      code[k] <- HTML(paste(HTML("<code>"),
                            gsub("([^ \t\r\n])[ \r\t\n]+$", "\\1", raw$code),  # remove trailing spaces
                            HTML("</code>")))
      if (raw$checked) {
        message[k] <- raw$feedback$message
        correct[k] <- raw$feedback$correct
      } else {
        message[k] <- "not checked"
        correct[k] <- NA
      }
    }
    Tmp$message <- message
    Tmp$correct <- correct
    Tmp$code <- code
    Tmp <- Tmp %>% group_by(label, correct, message, code)
    if (input$code_by_student) Tmp <- Tmp %>% group_by(login_id, add = TRUE)

    Tmp %>%
      summarise(count = n()) %>%
      arrange(label, correct, message) %>%
      knitr::kable(., format = "html", escape = TRUE) -> foo
    goo <- gsub("&lt;code&gt;", "<pre>", foo)
    hoo <- gsub("&lt;/code&gt;", "</pre>", goo)


    hoo
  })
}

# Run the application
shinyApp(ui = ui, server = server)


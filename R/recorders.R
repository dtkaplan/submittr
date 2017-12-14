#' Submit events via cat
#'
#' A submitted event consists of a list with several text elements
#'
#' @rdname recorders
#' @export
submit_via_cat <- function(tutorial_id,
                           tutorial_version,
                           user_id,
                           event, data) {
  cat(tutorial_id, " (", tutorial_version, "): ", user_id , "\n", sep = "")
  cat("logged in as", get_user()$user, "\n")
  cat("event: ", event, "\n", sep = "")
  cat(capture.output(data), "\n")
}
#' Submit events to a text file in JSON format
#'
#' @param fname text string giving the name of the file for storage
#' @rdname recorders
#' @export
create_submit_to_file <- function(fname) {
  function(tutorial_id,
           tutorial_version,
           user_id,
           event, data) {
    str <-
      jsonlite::toJSON(
        event_to_df(tutorial_id, tutorial_version, user_id, event, data))

    cat(str, "\n\n\n")
    cat(str, "\n", file = fname, append = TRUE)
  }
}

#' @rdname recorders
#' @param table_name string giving the name of the table in your database to use for storing submissions
#' @param password_table_name string giving the name of the table in your database containing
#' student (and grader) login credentials
#' @param restart For development purposes only. If `TRUE` empties the submission table.
#' @export
create_submit_to_db <- function(table_name = "higgins",
                                password_table_name = "passwords",
                                restart = FALSE) {
  establish_db_connection(table_name = table_name,
                          password_table_name = "passwords",
                          restart = FALSE)

  # return the actual recorder function
  # learnr will call this function
  function(tutorial_id = "blank",
           tutorial_version = "blank",
           user_id = "blank",
           event = "empty event", data = list(label = "empty")) {
    event <- event_to_df(tutorial_id, tutorial_version, user_id, event, data)

    # Which events to discard: exercises where "run" rather than "submit" was pressed.
    if (event$type == "exercise_submission" &&
        "checked" %in% names(data) &&
        (! data$checked)) {
      return
    } else {
      dbWriteTable(get_this_connection(),
                   submittr:::db_info$this_connection_info$table_name,
                   event, append = TRUE, row.names = FALSE)
    }
  }

}


db_info <- new.env()
assign("this_driver",  NULL, envir = submittr:::db_info)
assign("this_connection", NULL, envir = submittr:::db_info)
assign("this_connection_info", NULL, envir = submittr:::db_info)


#' @export
get_this_connection <- function() {
  if (is.null(submittr:::db_info$this_driver)) stop("dbDriver never established.")
  if (is.null(submittr:::db_info$this_connection)) stop("Connection never established.")
  if (is.null(submittr:::db_info$this_connection_info)) stop("No info for db connection ever established.")
  res <- try(dbGetInfo(submittr:::db_info$this_connection))
  if (inherits(res, "try-error")) {
    res <- try(dbGetInfo(submittr:::db_info$this_driver))
    if (inherits(res, "try-error"))
      assign("this_driver",  dbDriver("PostgreSQL"), envir = submittr:::db_info)
    assign("this_connection", with(this_connection_info,
                             dbConnect(this_driver,
                                       dbname = dbname,
                                       host = host,
                                       port = port,
                                       user = user,
                                       password = password
                                  )),
           envir = submittr:::db_info)
  }

  return(submittr:::db_info$this_connection)
}

# Establish a connection to the database
#
# There needs to be a file in the working directory, dbkeys.yaml, that contains
# the database login information.


establish_db_connection <- function(table_name = "higgins",
                                    password_table_name = "passwords",
                                    restart = FALSE) {
  keys <- yaml::yaml.load_file("dbkeys.yml")
  library("DBI")
  library("RPostgreSQL")
  assign("this_driver", dbDriver("PostgreSQL"), envir = submittr:::db_info)
  assign("this_connection", dbConnect(submittr:::db_info$this_driver,
                   dbname = keys$dbname,
                   host = keys$host,
                   port = keys$port,
                   user = keys$user,
                   password = keys$password),
         envir = submittr:::db_info)
  assign("this_connection_info",  list(dbname = keys$dbname, host = keys$host,
                               port = keys$port, user = keys$user, password = keys$password,
                               table_name = table_name),
         envir = submittr:::db_info)
  PW <- if (length(password_table_name) > 0)
    dbReadTable(submittr:::db_info$this_connection, password_table_name)
  else data.table(a = 1, b = 2)[NULL,] # Flag that no password is required.

  assign("Password_table",
         PW,
         envir = submittr:::db_info)

  df_format <- event_to_df()[NULL,] # an empty table in the right format

  if (restart) { # wipe out any existing table by that name}
    dbRemoveTable(con, table_name)
  }

  # see if table exists. If not, create it
  if ( ! dbExistsTable(submittr:::db_info$this_connection, table_name))
    dbWriteTable(submittr:::db_info$this_connection, table_name, df_format, row.names = FALSE)

  dbGetInfo(submittr:::db_info$this_connection) # throws error if the connection isn't valid

}

#' @export
event_to_df <- function(tutorial_id = "blank",
                        tutorial_version = "blank",
                        user_id = "setup",
                        event = "setup_table",
                        data = list(label = "bogus", what ="Setting up the table")){
  # don't store the output from the command. It can be of arbitrary size.
  data["output"] <- NULL
  this_user <- submittr::get_user()
  if ( ! "label" %in% names(data)) data$label <- "unlabeled"
  cat("Data were:", capture.output(data), "\n")
  correct <-
    if ("correct" %in% names(data)) data$correct
    else if ("feedback" %in% names(data)) data$feedback$correct
    else "na"
  if (is.null(correct)) correct <- "na"
  cat("correct is:", capture.output(correct), "\n")
  dplyr::data_frame(
    timestamp = Sys.time(),
    tutorial_id = tutorial_id,
    tutorial_version = tutorial_version,
    user_id = user_id,
    login_id = this_user$user,
    login_group = this_user$group,
    type = event,
    label = data$label,
    correct = correct,
    data = jsonlite::toJSON(data)
  )
}

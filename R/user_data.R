#' Access user data
#'
#' Set and get user information in a way that's accessible to the data logger.
#'
#' @param user Character string user ID
#' @param role Character string role in the system.
#' @param group Character string, gives the "group", which is to say the class
#' that the user belongs to.
#'
#' For communicating account information from Shiny to learnr.

#' @rdname user_data
#' @export
set_user <- function(user, role, group) {
  assign("info", list(user = user, role = role, group = group),
         envir = submittr:::user_info)
}

#' @rdname user_data
#' @export
get_user <- function() {
  if ("info" %in% names(submittr:::user_info)) {
    return(get("info", envir = submittr:::user_info))
  } else {
    return("")
  }
}
#' @rdname user_data
#' @export
clear_user <- function() {
  assign("info", list(user = "*cleared*", role = "*missing*", group = "*missing*"), envir = submittr:::user_info)
}

user_info <- new.env()
clear_user()

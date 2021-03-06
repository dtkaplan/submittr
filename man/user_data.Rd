% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_data.R
\name{set_user}
\alias{set_user}
\alias{get_user}
\alias{clear_user}
\title{Access user data}
\usage{
set_user(user, role, group)

get_user()

clear_user()
}
\arguments{
\item{user}{Character string user ID}

\item{role}{Character string role in the system.}

\item{group}{Character string, gives the "group", which is to say the class
that the user belongs to.

For communicating account information from Shiny to learnr.}
}
\description{
Set and get user information in a way that's accessible to the data logger.
}

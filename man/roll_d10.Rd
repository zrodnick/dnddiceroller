% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dice_Roller.R
\name{roll_d10}
\alias{roll_d10}
\title{Roll d10}
\usage{
roll_d10(dice = 1, bonus = 0, each = FALSE)
}
\arguments{
\item{dice}{The number of dice to be rolled. Defaults to 1}

\item{bonus}{Any bonus amount to be added or subtracted from your roll. Defaults to 0}

\item{each}{Specifies if the bonus should be added to each individual die roll. If FALSE, the bonus will be added to the total rolls. Defaults to FALSE.}
}
\description{
This function allows you to roll 10 sided dice.
}
\examples{
roll_d10()
}


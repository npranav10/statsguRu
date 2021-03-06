% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bowling_Dismissals.R
\name{dispBowlerDismissals}
\alias{dispBowlerDismissals}
\title{Plots a pie chart of Dismissal Type Summary of a Bowler.}
\usage{
dispBowlerDismissals(data, plot_title = "Distribution of Dismissals")
}
\arguments{
\item{data}{Output of statsguRu::split_DismissalSummary_Bowling().}
}
\value{
Plots a pie chart of Dismissal Type Summary of a Bowler.
}
\description{
This function takes in output of splitDismissalSummary and plots the Dismissal Type Summary of a Bowler as a pie chart.
}
\examples{
ashwin_DismissalSummary_Bowling = statsguRu::get_DismissalSummary_Bowling(PlayerID = 26421, MatchType = 1)
ashwin_DismissalSummary_Bowling = statsguRu::split_DismissalSummary_Bowling(data = ashwin_DismissalSummary_Bowling, MatchType = 1)
statsguRu::dispBowlerDismissals(data = ashwin_DismissalSummary_Bowling)
}
\keyword{Bowling}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bowling.R
\name{dispWktsTakenByOpposition}
\alias{dispWktsTakenByOpposition}
\title{Displays Barplot of Players's Wickets by List of Oppostion teams}
\usage{
dispWktsTakenByOpposition(data, plot_title = "Wickets taken against a Country")
}
\arguments{
\item{data}{Output of statsguRu::split_Summary_Bowling()}
}
\value{
Plots a player's sum of wickets taken against every opposition team.
}
\description{
This function takes in "Bowling Summary" dataframe modified after splitBowlingSummary and plots a player's sum of wickets taken against every opposition team.
}
\examples{
ashwin_BowlingSummary = statsguRu::get_Summary_Bowling(PlayerID = 26421, MatchType = 1)
ashwin_BowlingSummary = statsguRu::split_Summary_Bowling(data = ashwin_BowlingSummary, MatchType = 1)
statsguRu::dispWktsTakenByOpposition(data = ashwin_BowlingSummary)
}
\keyword{Bowling}

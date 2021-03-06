% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batting_dismissals.r
\name{dispBattingAveByBowlerType}
\alias{dispBattingAveByBowlerType}
\title{Plots a bar chart of Player's Batting average vs type and style of Bowlers}
\usage{
dispBattingAveByBowlerType(data, plot_title = "Batting Average by Bowler-Type")
}
\arguments{
\item{data}{Output of statsguRu::split_DismissalSummary_Batting().}
}
\value{
Plots a pie chart of Dismissal Summary vs type and style of Bowlers
}
\description{
This function takes in output of splitDismissalSummary()  and Plots a bar chart of Player's Batting averagevs type and style of Bowlers.
}
\examples{
sachin_DismissalSummary_Batting = statsguRu::get_DismissalSummary_Batting(PlayerID = 35320, MatchType = 2)
sachin_DismissalSummary_Batting = statsguRu::split_DismissalSummary_Batting(sachin_DismissalSummary_Batting)
statsguRu::dispBattingAveByBowlerType(data = sachin_DismissalSummary_Batting)
}
\keyword{Batting}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OuterJoinMerge.R
\name{OuterJoinMerge}
\alias{OuterJoinMerge}
\title{Returns all possible combinations of two dataframes}
\usage{
OuterJoinMerge(df_a, df_b)
}
\arguments{
\item{df_a}{The first dataframe}

\item{df_b}{The second dataframe}
}
\value{
A dataframe with all combinations
}
\description{
Returns all possible combinations of two dataframes
}
\examples{

df_a = data.frame(matrix(seq(1,20),nrow = 5, ncol = 4))
df_b = data.frame(matrix(seq(21,40),nrow = 5, ncol = 4))
joined_df = OuterJoinMerge(df_a, df_b)
}
\author{
Tasos Grivas <tasos@openriskcalculator.com>
}

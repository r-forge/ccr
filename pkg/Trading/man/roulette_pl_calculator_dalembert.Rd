% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roulette_pl_calculator_dalembert.R
\name{roulette_pl_calculator_dalembert}
\alias{roulette_pl_calculator_dalembert}
\title{Roulette P&L betting based on the D'Alembert Betting System}
\usage{
roulette_pl_calculator_dalembert(
  bet_minimum,
  bet_maximum,
  initial_capital,
  simulations_num,
  trials_per_sim
)
}
\arguments{
\item{bet_minimum}{The minimum betting amount that the casino allows}

\item{bet_maximum}{The maximum betting amount that the casino allows}

\item{initial_capital}{The initial capital to be used}

\item{simulations_num}{The number of simulations to be run}

\item{trials_per_sim}{The number of trials in each simulation}
}
\value{
A list containing the minimum, the maximum and the final balance for each simulation. Also the P&L graph for the last simulation will be plotted.
}
\description{
Calculates the potential profit or loss when someone is betting in the roulette based on the D'Alembert Betting System
}
\examples{

# This software is covered by GPL license and provided strictly for educational 
# reasons (no actual investment/betting decisions should be taken based on this)
# On top of these, the below example contains a tiny number of simulations and
# trials just to pass CRAN tests - the user would have to highly increase both
# variables when running these.
pl_results = roulette_pl_calculator_dalembert(bet_minimum = 0.1 , bet_maximum = 3276.8,
initial_capital = 20000, simulations_num = 100, trials_per_sim = 100)
summary(pl_results$min_capital)
summary(pl_results$max_capital)
summary(pl_results$final_capital)

}
\references{
https://en.wikipedia.org/wiki/Roulette#Betting_strategies_and_tactics
}
\author{
Tasos Grivas <tasos@openriskcalculator.com>
}

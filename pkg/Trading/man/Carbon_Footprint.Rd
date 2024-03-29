% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cf.R
\name{Carbon_Footprint}
\alias{Carbon_Footprint}
\title{Carbon Footprint}
\usage{
Carbon_Footprint(portfolio_exposure, emissions_capitalization_data)
}
\arguments{
\item{portfolio_exposure}{The exposure per issuer in the portfolio}

\item{emissions_capitalization_data}{The capitalization and the Scope 1 & 2 GHG emissions per issuer}
}
\value{
Total carbon emissions for a portfolio normalized by the market value of the portfolio, expressed in tons CO2e / $M invested.
}
\description{
Returns the Total carbon emissions for a portfolio normalized by the market value of the portfolio,
expressed in tons CO2e / $M invested.Scope 1 and Scope 2 GHG emissions are allocated to investors based on an equity
}
\examples{
 portfolio_exposure     = data.table::data.table(Issuers = c('A','B','C'),
 exposures = c(100, 200, 50))
 emissions_capitalization_data = data.table::data.table(Issuers = c('A','B','C'), 
 emissions = c(1000, 5000, 6000),  Capitalization = c(20000, 10000, 30000))
 Carbon_Footprint(portfolio_exposure, emissions_capitalization_data)
}
\references{
https://www.tcfdhub.org/Downloads/pdfs/E09%20-%20Carbon%20footprinting%20-%20metrics.pdf
}
\author{
Tasos Grivas <tasos@openriskcalculator.com>
}

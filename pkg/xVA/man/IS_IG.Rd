% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IS_IG.R
\name{IS_IG}
\alias{IS_IG}
\title{Checks if Credit rating is Investment Grade}
\usage{
IS_IG(credit_rating)
}
\arguments{
\item{credit_rating}{The Credit Rating to be checked}
}
\value{
TRUE if Rating is Investment Grade
}
\description{
Checks if the credit rating is investment grade or not (if not rating not recognised will be unrated)
}
\examples{

TRUE == IS_IG('AAA')

}
\references{
https://en.wikipedia.org/wiki/Credit_rating
}
\author{
Tasos Grivas <tasos@openriskcalculator.com>
}

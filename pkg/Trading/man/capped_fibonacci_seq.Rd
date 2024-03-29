% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capped_fibonacci_seq.R
\name{capped_fibonacci_seq}
\alias{capped_fibonacci_seq}
\title{Fibonacci sequence up to a specified maximum number}
\usage{
capped_fibonacci_seq(max_number)
}
\arguments{
\item{max_number}{The maximum number up to which the sequence should be generated}
}
\value{
A vector containing the Fibonacci sequence
}
\description{
Generates the Fibonacci sequence up to a specified maximum number
}
\examples{

fibonacci_seq = capped_fibonacci_seq(max_number = 6000)
}
\references{
https://en.wikipedia.org/wiki/Fibonacci_number
}
\author{
Tasos Grivas <tasos@openriskcalculator.com>
}

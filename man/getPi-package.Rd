\name{getPi-package}
\alias{getPi-package}
\alias{getPi-markovChain}
\docType{package}
\title{
\packageTitle{getPi}
}
\description{
\packageDescription{getPi}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{getPi}
\packageIndices{getPi}
Use function getPi to obtain the Steady-State probability vector of a Markov chain model using a transition probability matrix P. Matrix P must only have reccurent states of the Markov chain model.
}
\author{
\packageAuthor{getPi}

Maintainer: \packageMaintainer{getPi}
}
\references{
Cinlar, E. (2013). Introduction to stochastic processes. Courier Corporation.
}

\keyword{ package }
\seealso{
\code{\link[matlib:matlib-package]{matlib}}
}
\examples{
getPi(matrix(c(0.8,0.5,0.2,0.5),2,2))

}

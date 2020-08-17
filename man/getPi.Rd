\name{getPi}
\alias{getPi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Pi
}
\description{
This function return the Steady-State probability vector of a Markov Chain Model with transition probability Matrix P. P must only have reccurent states.
}
\usage{getPi(P)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{\code{A square matrix in which every row sum up to 1.}}
}
\details{
A Markov chain model with reccurent states and an aperiodic irreducible transition probability matrix P have a Steady-State vector b such that b=b.P. The function getPi return b for a given matrix P.
}
\value{
%%  ~Describe the value returned
  \item{pi_vector}{A vector with the size equals to the dimension of P.}
%%  \item{comp2 }{Description of 'comp2'}
}
\references{
Cinlar, E. (2013). Introduction to stochastic processes. Courier Corporation.
}
\author{
MSc. Felipe Domingues Simoes
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{matlib}}
}
\examples{
getPi(matrix(c(0.8,0.5,0.2,0.5),2,2))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
 \keyword{ Markov chain }
 \keyword{ steady state vector }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

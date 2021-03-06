% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Glicko2Functions.R
\name{GetFxHelper}
\alias{GetFxHelper}
\title{Get value of helper function f(x).}
\usage{
GetFxHelper(x, sigma, phi, Delta, nu, tau)
}
\arguments{
\item{x}{[\code{numeric}]\cr
A vector of the quantity x.}

\item{sigma}{[\code{numeric(1)}]\cr
A numeric value of the rating volatility for a given player on the
Glicko-2 scale.}

\item{phi}{[\code{numeric}]\cr
A numeric value of the rating deviation on the Glicko-2 scale.}

\item{Delta}{[\code{numeric}]\cr
A numeric value of quantity Delta.}

\item{nu}{[\code{numeric}]\cr
A numeric value of the quantity nu.}

\item{tau}{[\code{numeric(1)}]\cr
A numeric value of the system constant tau.}
}
\value{
[\code{numeric}] the numeric value of the helper function.
}
\description{
Get value of helper function f(x).
}

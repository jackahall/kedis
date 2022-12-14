% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rse.R
\name{rse.kd_cv}
\alias{rse.kd_cv}
\title{Relative Squared Error of kd_cv}
\usage{
\method{rse}{kd_cv}(actual, predicted = join_cv_predictions(actual)$predicted)
}
\arguments{
\item{actual}{a fitted kd_cv object}

\item{predicted}{the predicted rates, default is from kd_cv}
}
\description{
Relative Squared Error of kd_cv
}

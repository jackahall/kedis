% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_index.r
\name{make_index.kd_data}
\alias{make_index.kd_data}
\title{Make index for kedis cross validation}
\usage{
\method{make_index}{kd_data}(data, k_out, k_in = NULL, seed = NULL, ...)
}
\arguments{
\item{data}{a kd_data object}

\item{k_out}{number of outer loops}

\item{k_in}{number of inner loops (NULL for non-nested cross-validation, default)}

\item{seed}{seed}

\item{...}{additional parameters}
}
\value{
a kd_index object
}
\description{
Make index for kedis cross validation
}

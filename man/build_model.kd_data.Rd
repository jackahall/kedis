% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_model.R
\name{build_model.kd_data}
\alias{build_model.kd_data}
\title{build_model}
\usage{
\method{build_model}{kd_data}(
  data,
  layers_cov = NULL,
  layers_xy = NULL,
  link = "identity",
  optimizer = NULL,
  loss = NULL,
  metrics = NULL,
  ...,
  seed = NULL,
  clear_session = TRUE
)
}
\arguments{
\item{data}{kd_data}

\item{layers_cov}{list of layers for covariate training. If NULL, pass through with no additional layers.}

\item{layers_xy}{list of layers for xy training, otherwise if TRUE, pass through with no additional layers. If NULL, do not include xy layers. Default NULL. If NULL, output_xy will be the same as output_disag}

\item{link}{link function to be used. default "identity", can be one of "logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse".}

\item{optimizer}{keras optimizer}

\item{loss}{keras loss, if included model will compile}

\item{metrics}{keras metrics}

\item{...}{other parameters to pass to keras::compile}

\item{seed}{seed, default NULL}

\item{clear_session}{default TRUE, clears keras session before running}
}
\value{
a kd_model object
}
\description{
build_model
}

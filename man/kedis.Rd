% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kedis.R
\name{kedis}
\alias{kedis}
\title{Build a kedis model}
\usage{
kedis(
  formula,
  shape,
  rasters,
  family = poisson(link = "log"),
  layers_cov = NULL,
  layers_xy = NULL,
  loss = NULL,
  optimizer = keras::optimizer_adam(),
  metrics = NULL,
  x.center = TRUE,
  x.scale = TRUE,
  y.center = TRUE,
  y.scale = TRUE
)
}
\arguments{
\item{formula}{on object of class "formula". To include a variable with the special "pop(x)", which specifies the population raster and "agg(x)", which specifies the aggregation variable.}

\item{shape}{SpatVector of shapefile, or address to .shp file, which includes the response variable and geometries of the aggregation areas.}

\item{rasters}{SpatRaster of covariates, or address of .tif or folder of .tifs. Should include the population raster and all covariates used in the model.}

\item{family}{a descriptionn of the error distribution and link function ot be used in the model. Kedis currently only supports a "poisson" family.}

\item{layers_cov}{list of "keras" layers for covariate training. If NULL, pass through with no additional layers.}

\item{layers_xy}{list of "keras" layers for xy training, otherwise if NULL, pass through with no additional layers.}

\item{loss}{Manually specify a "keras" loss function to use. If NULL, will use the default loss of the family.}

\item{optimizer}{The "keras" optimiser to use.}

\item{metrics}{a vector of "keras" metrics.}

\item{x.center}{Center the x values before model fitting. Default TRUE.}

\item{x.scale}{Scale the x values before model fitting. Default TRUE.}

\item{y.center}{Center the y values before model fitting. Default TRUE.}

\item{y.scale}{Scale the y values before model fitting. Default TRUE.}
}
\value{
A kd_model object.
}
\description{
Build a kedis model
}

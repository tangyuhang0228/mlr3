% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_prediction_data.R
\name{as_prediction_data}
\alias{as_prediction_data}
\alias{as_prediction_data.Prediction}
\alias{as_prediction_data.PredictionData}
\alias{as_prediction_data.list}
\title{PredictionData}
\usage{
as_prediction_data(x, task, row_ids = task$row_ids, check = TRUE, ...)

\method{as_prediction_data}{Prediction}(x, task, row_ids = task$row_ids, check = TRUE, ...)

\method{as_prediction_data}{PredictionData}(x, task, row_ids = task$row_ids, check = TRUE, ...)

\method{as_prediction_data}{list}(x, task, row_ids = task$row_ids, check = TRUE, ...)
}
\arguments{
\item{x}{(\code{any})\cr
Object to convert.}

\item{task}{(\link{Task}).}

\item{row_ids}{(\code{integer()}).}

\item{check}{(\code{logical(1)})\cr
Perform argument checks and type conversions?}

\item{...}{(\code{any})\cr
Additional arguments.}
}
\value{
\link{PredictionData}.
}
\description{
Convert object to a \link{PredictionData} or a list of \link{PredictionData}.
}

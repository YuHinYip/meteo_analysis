% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meteo_functions.R
\name{best_linear_model_in_this_analysis}
\alias{best_linear_model_in_this_analysis}
\title{Find suitable linear model}
\usage{
best_linear_model_in_this_analysis(dataset)
}
\arguments{
\item{dataset}{data frame}
}
\value{
a linear model to explain \code{dataset}
}
\description{
Instead of doing manual again, if you are using the same data, it should have the same best linear model to explain \code{dataset}. In case of you want to remove other variables or interaction, it have to do it manually
}
\details{
expected the data is the combination of two weather data, to have the right formula, if using the same kind of data
}

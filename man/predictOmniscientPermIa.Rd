% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictOmniscientPermIa.r
\name{predictOmniscientPermIa}
\alias{predictOmniscientPermIa}
\title{Predictions for OMNISCIENT model using permuted interaction terms}
\usage{
predictOmniscientPermIa(
  model,
  data,
  sim,
  permProd = TRUE,
  permProdRule = NULL,
  b0 = NA,
  b1 = NA,
  b2 = NA,
  b11 = NA,
  b12 = NA,
  mu1 = NA,
  mu2 = NA,
  sigma1 = NA,
  sigma2 = NA,
  rho = NA,
  ...
)
}
\arguments{
\item{model}{Omniscient model object (of class "function") representing a bivariate \code{\link[enmSdmPredImport]{logistic}} or bivariate \code{\link[enmSdmPredImport]{gaussian}} function.}

\item{data}{Data frame.}

\item{sim}{Raster layer object used to standardize output.}

\item{permProd}{Logical, if \code{TRUE} (default) then permute product features using variables T1 and T2 according to \code{permProdRule}. If \code{FALSE} then predict as normal (no permutation).}

\item{permProdRule}{Character or logical. Rule for how permutation of product features is applied. \code{'before'} ==> Permute individual variable values then calculate product. \code{'after'} ==> Calculate product then permute across these values. Ignored if permProd is \code{NULL}.}

\item{b0}{Numeric, parameters for \code{\link[enmSdmPredImport]{logistic}} or \code{\link[enmSdmPredImport]{logisticShift}} function specified in the \code{response} argument (above). Logistic intercept. Default is \code{NA}.}

\item{b1}{Numeric, parameters for \code{\link[enmSdmPredImport]{logistic}} or \code{\link[enmSdmPredImport]{logisticShift}} function specified in the \code{response} argument (above). Logistic slope. Default is \code{NA}.}

\item{b2}{Numeric, parameters for \code{\link[enmSdmPredImport]{logistic}} or \code{\link[enmSdmPredImport]{logisticShift}} function specified in the \code{response} argument (above). Logistic slope. Default is c.}

\item{b11}{Numeric, parameters for \code{\link[enmSdmPredImport]{logistic}} or \code{\link[enmSdmPredImport]{logisticShift}} function specified in the \code{response} argument (above). Left-right shift along variable. Default is \code{NA}.}

\item{b12}{Numeric, parameters for \code{\link[enmSdmPredImport]{logistic}} or \code{\link[enmSdmPredImport]{logisticShift}} function specified in the \code{response} argument (above). Interaction term. Default is \code{NA}.}

\item{mu1}{Numeric, parameters for \code{\link[enmSdmPredImport]{gaussian}} function specified in the \code{response} argument (above). Mean of variable. Default is \code{NA}.}

\item{mu2}{Numeric, parameters for \code{\link[enmSdmPredImport]{gaussian}} function specified in the \code{response} argument (above). Mean of variable. Default is \code{NA}.}

\item{sigma1}{Numeric, parameters for \code{\link[enmSdmPredImport]{gaussian}} function specified in the \code{response} argument (above). Standard deviation of variable. Default is \code{NA}.}

\item{sigma2}{Numeric, parameters for \code{\link[enmSdmPredImport]{gaussian}} function specified in the \code{response} argument (above). Standard deviation of variable. Default is \code{NA}.}

\item{rho}{Numeric, parameters for \code{\link[enmSdmPredImport]{gaussian}} function specified in the \code{response} argument (above). Covariance term. Default is \code{NA}.}

\item{...}{Arguments to pass to model function(s).}
}
\value{
Numeric vector.
export
}
\description{
Predictions for OMNISCIENT model using permuted interaction terms.
}

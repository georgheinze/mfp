\name{predict.mfp}
\alias{predict.mfp}
\title{
Predict method for mfp fits
}
\description{
Obtains predictions from an "mfp" object
}
\usage{
predict.mfp(object, newdata, type = c("link", "response", "lp", "risk", "expected", "terms"), terms, ref = NULL, seq = NULL, se.fit = FALSE, dispersion = NULL, na.action = na.pass, collapse, safe = FALSE, ...)
}

\arguments{
  \item{object}{
    an "mfp" object.
}
  \item{newdata}{
    optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted linear predictors are used.
}
  \item{type}{
    the type of prediction required. The default is on the scale of the linear predictors; the alternative "response" is on the scale of the response variable. The "terms" option returns fitted values of each term on the linear predictor scale. This is useful for visualizing the effects for fractional polynomials.
}
  \item{terms}{
    A character vector specifiying the variables for which to return fitted values. Only relevant if \code{type = "terms"}.
}
  \item{ref}{
    the reference value realtive to which contrasts should be computed. Defaults to the mean value of the variable.
}
  \item{seq}{
    a numeric vector specifiying the input values for which contrasts should be computed.
}
  \item{se.fit}{
    logical switch indicating if standard errors are required.
}
  \item{dispersion}{
    the dispersion of the GLM fit to be assumed in computing the standard errors. If omitted, that returned by summary applied to the object is used.
}
  \item{na.action}{
    function determining what should be done with missing values in newdata. The default is to predict NA.
}
  \item{collapse}{
    optional vector of subject identifiers. If specified, the output will contain one entry per subject rather than one entry per observation.
}
  \item{safe}{
        
}
  \item{\dots}{
    further arguments used by \code{predict.glm} or \code{predict.coxph}.
}
}
\details{

}
\value{
If \code{type = "terms"}, a list with term predictions relative to the reference value. Otherwise, a prediction object based on \code{predict.glm} or \code{predict.coxph}.
}
\references{

}
\author{

}
\note{

}

\seealso{
\code{\link{predict.glm}}, \code{\link{predict.coxph}}
}

\name{mfp.object}
\alias{mfp.object}
\title{Multiple Fractional Polynomial Model Object} 
\description{
Objects returned by fitting fractional polynomial model objects.

These are objects representing fitted \code{mfp} models. 
Class \code{mfp} inherits from either glm or coxph depending on the type of model fitted.
}

\value{
In addition to the standard glm/coxph components the following components are 
included in a \code{mfp} object.

 \item{x}{
	the final FP transformations that are contained in the design matrix x. 
        The predictor "z" with 4 df would have corresponding columns "z.1" and 
        "z.2" in x.}
 \item{powers}{
	a matrix containing the best FP powers for each predictor. If a predictor
	has less than two powers a NA will fill the appropriate cell of the
	matrix.}
 \item{pvalues}{
	a matrix containing the P-values from the closed tests. Briefly p.null is
	the P-value for the test of inclusion (see mfp), p.lin corresponds to
	the test of nonlinearity and p.FP the test of simplification. The best m=1
	power (power2) and best m=2 powers (power4.1 and power4.2) are also given.}
 \item{scale}{
	all predictors are shifted and rescaled before being power transformed if
	nonpositive values are encountered or the range of the predictor is 
	reasonably large. If x' would be used instead of x where
		x' = (x+a)/b
	the parameters a (shift) and b (scale) are contained in the matrix scale.}
 \item{df.initial}{
	a vector containing the degrees of freedom allocated to each predictor.}
 \item{df.final}{
	a vector containing the degrees of freedom of each predictor at
	convergence of the backfitting algorithm.}
 \item{dev}{
	the deviance of the final model.}
 \item{dev.lin}{
	the deviance of the model that has every predictor included with 1 df
	(i.e. linear).}
 \item{dev.null}{
	the deviance of the null model.}
 \item{fptable}{
	the table of the final fp transformations.}
 \item{formula}{
	the proposed formula for a call of glm/coxph.}
 \item{fit}{
	the fitted glm/coxph model using the proposed formula. This component can be used for prediction, etc.}
}
  
\seealso{
	mfp, glm.object}

\keyword{ classes }

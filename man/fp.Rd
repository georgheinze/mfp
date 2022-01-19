\name{fp}
\alias{fp}

\title{Fractional Polynomial Transformation}

\description{This function defines a fractional polynomial object for a quantitative input variable x. 
}
   
\usage{fp(x, df = 4, select = NA, alpha = NA, scale=TRUE)}

\arguments{
 \item{x}{
	quantitative input variable.}
 \item{df}{
	degrees of freedom of the FP model. 
	df=4: FP model with maximum permitted degree m=2 (default),
	df=2: FP model with maximum permitted degree m=1,
	df=1: Linear FP model.}
 \item{select}{
	sets the variable selection level for the input variable.}
 \item{alpha}{
	sets the FP selection level for the input variable.}
 \item{scale}{
	use pre-transformation scaling to avoid numerical problems (default=TRUE).}
}

\examples{
\dontrun{
	fp(x, df = 4, select = 0.05, scale = FALSE)
}
}


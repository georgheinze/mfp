\name{mfp}
\alias{mfp}
\title{Fit a Multiple Fractional Polynomial Model} 
\description{
Selects the multiple fractional polynomial (MFP) model which best predicts 
the outcome. The model may be a generalized linear model or a proportional
hazards (Cox) model.
}
\usage{
	mfp(formula, data, family = gaussian, method = c("efron", "breslow"),
	    subset = NULL, na.action = na.omit, init = NULL, alpha=0.05, 
	    select = 1, maxits = 20, keep = NULL, rescale = FALSE, 
	    verbose = FALSE, x = TRUE, y = TRUE)
}

\arguments{
 \item{formula}{
	a formula object, with the response of the left of a ~ operator, and
	the terms, separated by + operators, on the right. Fractional
	polynomial terms are indicated by fp.  If a Cox PH model is required
	then the outcome should be specified using the Surv() notation used
	by coxph.}
 \item{data}{
	a data frame containing the variables occurring in the formula. If this
	is missing, the variables should be on the search list.}
 \item{family}{
	a family object - a list of functions and expressions for defining the
	link and variance functions, initialization and iterative weights.
	Families supported are gaussian, binomial, poisson, Gamma,
	inverse.gaussian and quasi.  Additionally Cox models are specified
	using "cox".}
 \item{method}{
	a character string specifying the method for tie handling. This argument is 
	used for Cox models only and has no effect for other model families.
        See 'coxph' for details.}
 \item{subset}{
	expression saying which subset of the rows of the data should be used
	in the fit.  All observations are included by default.}
 \item{na.action}{
	function to filter missing data. This is applied to the model.frame
	after any subset argument has been used. The default (with na.fail) is
	to create an error if any missing values are found.}
 \item{init}{
	vector of initial values of the iteration (in Cox models only).}
 \item{alpha}{
	sets the FP selection level for all predictors.  Values for individual
	predictors may be changed via the fp function in the formula.}
 \item{select}{
	sets the variable selection level for all predictors.  Values for
	individual predictors may be changed via the fp function in the formula.}
 \item{maxits}{
	maximum number of iterations for the backfitting stage.}
 \item{keep}{
	keep one or more variables in the model. The selection level for these variables will be set to 1.}
 \item{rescale}{
	logical; uses re-scaling to show the parameters for covariates on their original scale (default TRUE). Should only be used if no non-linear terms are selected.}
 \item{verbose}{
	logical; run in verbose mode (default FALSE).}
 \item{x}{
	logical; return the design matrix in the model object?}
 \item{y}{
	logical; return the response in the model object?}
}
\value{
	an object of class \code{mfp} is returned which either inherits from both glm
	and lm or coxph.}
\details{
	The estimation algorithm processes the predictors in turn. Initially, 
	mfp silently arranges the predictors in order of increasing P-value
	(i.e. of decreasing statistical significance) for omitting each predictor
	from the model comprising all the predictors with each term linear. The
	aim is to model relatively important variables before unimportant ones.

	At the initial cycle, the best-fitting FP function for the first predictor
	is determined, with all the other variables assumed linear. The FP
	selection  procedure is described below. The functional form (but NOT the
	estimated regression coefficients) for this predictor is kept, and the 
	process is repeated for the other predictors in turn. The first iteration
	concludes when all the variables have been processed in this way. The next
	cycle is similar, except that the functional forms from the initial cycle
	are retained for all variables excepting the one currently being processed.

	A variable whose functional form is prespecified to be linear (i.e. to 
	have 1 df) is tested only for exclusion within the above procedure when 
	its nominal P-value (selection level) according to select() is less than 1.

	Updating of FP functions and candidate variables continues until the functions
	and variables included in the overall model do not change (convergence).
	Convergence is usually achieved within 1-4 cycles.

	\emph{Model Selection}

	mfp uses a form of backward elimination. It start from a most complex 
	permitted FP model and attempt to simplify it by reducing the df. The 
	selection algorithm is inspired by the so-called "closed test procedure",
	a sequence of tests in each of which the "familywise error rate" or
	P-value is maintained at a prespecified nominal value such as 0.05. 

	The "closed test" algorithm for choosing an FP model with maximum 
	permitted degree m=2 (4 df) for a single continuous predictor, x, is as
	follows:

	1. Inclusion: test the FP in x for possible omission of x (4 df test,
	   significance level determined by select). If x is significant,
	   continue, otherwise drop x from the model.

	2. Non-linearity: test the FP in x against a straight line in x (3 df
	   test, significance level determined by alpha). If significant,
	   continue, otherwise the chosen model is a straight line.

	3. Simplification: test the FP with m=2 (4 df) against the best FP with
	   m=1 (2 df) (2 df test at alpha level). If significant, choose m=2,
	   otherwise choose m=1.

	All significance tests are carried out using an approximate P-value
	calculation based on a difference in deviances (-2 x log likelihood) 
	having a chi-squared or F distribution, depending on the regression in 
	use. Therefore, each of the tests in the procedure maintains a 
	significance level only approximately equal to select. The algorithm is 
	thus not truly a closed procedure. However, for a given significance level
	it does provide some protection against over-fitting, that is against
	choosing over-complex MFP models.}
\section{Side Effects}{
	details are produced on the screen regarding the progress of the 
	backfitting routine. At completion of the algorithm a table is displayed
	showing the final powers selected for each variable along with other
	details.}
\section{Known Bugs}{   
	glm models should not be specified without an intercept term as the
	software does not yet allow for that possibility.}
\references{
	Ambler G, Royston P (2001) Fractional polynomial model selection procedures: 
		investigation of Type I error rate. \emph{Journal of Statistical Simulation 
		and Computation} 69: 89--108.
		
	Benner A (2005) mfp: Multivariable fractional polynomials. \emph{R News} 5(2): 20--23.
	
	Royston P, Altman D (1994) Regression using fractional polynomials
		of continuous covariates. \emph{Appl Stat.} 3: 429--467.
		
	Sauerbrei W, Royston P (1999) Building multivariable prognostic and diagnostic models: 
		transformation of the predictors by using fractional polynomials. \emph{Journal 
		of the Royal Statistical Society (Series A)} 162: 71--94.
	}
    
\seealso{
	mfp.object, fp, glm}

\examples{
	data(GBSG)
	f <- mfp(Surv(rfst, cens) ~ fp(age, df = 4, select = 0.05)
                 + fp(prm, df = 4, select = 0.05), family = cox, data = GBSG)
	print(f)
	survfit(f$fit)  # use proposed coxph model fit for survival curve estimation
}

\author{ Gareth Ambler and Axel Benner, updated by Georg Heinze  }

\keyword{ models }

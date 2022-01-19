\name{bodyfat}
\alias{bodyfat}
\title{ percentage of body fat determined by underwater weighing }
\usage{data(bodyfat)}
\description{
  A data frame containing the estimates of the percentage of body fat 
  determined by underwater weighing and various body circumference 
  measurements for 252 men.
  
  Source: Roger W. Johnson (1996), ''Fitting Percentage of Body Fat to 
  Simple Body Measurements'', Journal of Statistics Education. 
  Original data are from K. Penrose, A. Nelson, and A. Fisher (1985), 
  ''Generalized Body Composition Prediction Equation for Men Using 
  Simple Measurement Techniques'' (abstract), Medicine and Science in 
  Sports and Exercise, 17(2), 189. 
  
  Data were supplied by Dr. A. Garth Fisher, Human Performance Research 
  Center, Brigham Young University, who gave permission to freely distribute 
  the data and use them for non-commercial purposes. 
  Note, however, that there seem to be a few errors.  
  For instance, body densities for cases 48, 76, and 96 seem to have one digit 
  in error in the two body fat percentage values.  
  Also note a man (case 42) over 200 pounds in weight who is less than 3 feet tall 
  (the height should presumably be 69.5 inches, not 29.5 inches).  
  Percent body fat estimates are truncated to zero when negative (case 182).
}
\format{
  This data frame contains the observations of 252 men:
  \describe{
    \item{case}{Case number.}
    \item{brozek}{Percent body fat using Brozek's equation: 457/Density - 414.2}
    \item{siri}{Percent body fat using Siri's equation: 495/Density - 450}
    \item{density}{Density determined from underwater weighing (gm/cm**3).}
    \item{age}{Age (years).}
    \item{weight}{Weight (lbs).}
    \item{height}{Height (inches).}
    \item{neck}{Neck circumference (cm).}
    \item{chest}{Chest circumference (cm).}
    \item{abdomen}{Abdomen circumference (cm) ''at the umbilicus and level 
                   with the iliac crest''.}
    \item{hip}{Hip circumference (cm).}
    \item{thigh}{Thigh circumference (cm).}
    \item{knee}{Knee circumference (cm).}
    \item{ankle}{Ankle circumference (cm).}
    \item{biceps}{Biceps (extended) circumference (cm).}
    \item{forearm}{Forearm circumference (cm).}
    \item{wrist}{Wrist circumference (cm) ''distal to the styloid processes''.}
  }
}
\source{
  e.g. \url{http://lib.stat.cmu.edu/datasets/bodyfat}
}
\references{
  R.W. Johnson (1996). Fitting percentage of body fat to simple body measurements.
  \emph{Journal of Statistics Education [Online]}, \bold{4(1)}.

  K.W. Penrose, A.G. Nelson, A.G. Fisher (1985). Generalized body composition prediction
           equation for men using simple measurement techniques. \emph{Medicine and Science 
           in Sports and Exercise}, \bold{17}, 189.

  P. Royston, W. Sauerbrei (2004). Improving the robustness of fractional polynomial models 
  by preliminary covariate transformation. Submitted.
}
\examples{
    data(bodyfat)
    bodyfat$height[bodyfat$case==42] <- 69.5   # apparent error
    bodyfat <- bodyfat[-which(bodyfat$case==39),]  # cp. Royston $\amp$ Sauerbrei, 2004
    mfp(siri ~ fp(age, df = 4, select = 0.1) + fp(weight, df = 4, select = 0.1)
                 + fp(height, df = 4, select = 0.1), family = gaussian, data = bodyfat)
}
\keyword{datasets}

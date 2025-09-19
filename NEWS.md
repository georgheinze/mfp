# mfp 1.5.5

* Fixed a NOTE (link to included package missing) and removed an irrelevant argument.

# mfp 1.5.4

* Fixed a bug where variables with nested names were incorrectly excluded from the model.

# mfp 1.5.3

* Added term predictions including first and second derivatives.
* Rewrote vignette as an .Rmd file and added a terms prediction example.
* Corrected the use of offsets in glm models.
* Changed rescale default: Now, by default `rescale = FALSE'.

# mfp 1.5.2

* Minor corrections (Vignette float options error; function namespace).

# mfp 1.5.1

* Minor corrections (Vignette keywords updated).

# mfp 1.5.0

* New maintainer; 'vignettes' directory added.

# mfp 1.4.9

* Minor corrections in fp.rescale.

# mfp 1.4.8

* Call of final fitted glm/coxph model corrected (parent frame was missing).
* plot.mfp (temporarily) removed.
* Correction of rescaling procedure of log trafos in fp.rescale.

# mfp 1.4.7

* summary.mfp added to avoid problems calling summary directly on mfp object.  

# mfp 1.4.6

* Minor corrections.

# mfp 1.4.5

* Minor corrections of component 'fit'. 

# mfp 1.4.4

* predict.mfp added.

# mfp 1.4.3

* Corrected in object$fit: Use quadratic in ln(X) when powers=(0,0).

# mfp 1.4.2

* Minor corrections to use 'subset' properly and to allow to ignore 'data'.

# mfp 1.4.0

* Data set 'GBSG' from the JRSS-A paper replaced by its Stata version.
* 'rescale' argument added to function call.
* 'keep' argument added to function call.
* Verbose output now shows also transformation functions of covariates.

# mfp 1.3.2

* Scaling of variables is added to model fit used for output component 'fit'.

# mfp 1.3.1

* Output corrected for FP2 with equal powers.
* mfp now works even without any FP call.
* New argument 'method' to select method for tie handling in Cox models.
* Vignette updated.

# mfp 1.3.0

* Changes in the primary ordering of variables.
* Scaling changed.
* Likelihood ratio tests are now used for all selections.
* Plot of functional form corrected.
* Print function corrected.
* Value of mfp fit has new component `fit'.

# mfp 1.2.2

* plot.mfp changed: new argument `var'.
* New dataset `bodyfat' to be used as an example for linear regression.
* Vignette included.

# mfp 1.2.1

* Original port.










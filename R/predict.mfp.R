predict.mfp <- function(object, newdata, type = c("link", "response", "lp", "risk", "expected", "terms"), terms = NULL, ref = NULL, seq = NULL, se.fit = FALSE, 
	dispersion = NULL, na.action = na.pass, collapse, safe = FALSE, ...) 
{

type <- match.arg(type)
if(object$family$family == "Cox"){ # for cox models 'link' should be 'lp'
  if(type == "link") type <- "lp"
}

if(type != "terms"){
  
  if(!is.null(terms)) stop("terms should only be specified if type = 'terms'")
  
  if(object$family$family=="Cox") {
    if (is.null(object$terms)) terms = names(object$assign)
    if (!missing(newdata)) 
      if (!missing(collapse)) 
        res <- getFromNamespace("predict.coxph", "survival")(object$fit, newdata = newdata, type = type, se.fit = se.fit, terms = terms, collapse = collapse, safe = safe, ...) 
    else
      res <- getFromNamespace("predict.coxph", "survival")(object$fit, newdata = newdata, type = type, se.fit = se.fit, terms = terms, safe = safe, ...) 
    else
      if (!missing(collapse)) 
        res <- getFromNamespace("predict.coxph", "survival")(object$fit, type = type, se.fit = se.fit, terms = terms, collapse = collapse, safe = safe, ...) 
    else
      res <- getFromNamespace("predict.coxph", "survival")(object$fit, type = type, se.fit = se.fit, terms = terms, safe = safe, ...) 
  } else {
    if (!missing(newdata)) 
      res <- predict.glm(object$fit, newdata = newdata, type = type, se.fit = se.fit, dispersion = dispersion, terms = terms, na.action = na.action, ...)
    else 
      res <- predict.glm(object$fit, type = type, se.fit = se.fit, dispersion = dispersion, terms = terms, na.action = na.action, ...)
  }
  
  return(res)
}

if(type == "terms"){
  
  
  all.terms <- colnames(object$X)
  
  # exclude factor variables
  bool.fact <- !apply(object$X, 2, function(x) length(unique(x)) == 2 && all(sort(unique(x)) == c(0, 1)))
  all.terms <- all.terms[bool.fact] 
  
  # only keep selected variables
  bool.selected <- sapply(all.terms, function(x) any(grepl(x, names(object$coefficients))))
  all.terms <- all.terms[bool.selected] 
  
  # check that only "valid" variables are used
  if(!all(terms %in% all.terms)) stop("Invalid terms. Note that term prediction for factor variables is not yet supported.")
  
  
  # if no terms specified do for all "valid" variables, i.e. excluding factor variables
  if(is.null(terms)) terms <- all.terms
  
  # set default for seq (range) and ref (mean)
  if(is.null(seq)) seq <- lapply(terms, function(variable){
    seq(min(object$X[, variable], na.rm = TRUE),
        max(object$X[, variable], na.rm = TRUE),
        length.out = 100)
  })
  if(is.null(ref)) ref <- lapply(terms, function(variable){
    mean(object$X[, variable], na.rm = TRUE) # use mean value as default reference
  })
  
  # if there is only 1 term and seq and ref are not given as lists transform them to a list
  if(length(terms) != length(seq) | length(terms) != length(ref)){
    stop("seq and ref should be lists of the same length as terms.")
  }
  
  
  # loop over all variables
  res.list <- Map(function(variable, seq.int, ref.int){
    
    # get indices by matching variable name and coefficient names
    indices <- which(grepl(paste0(variable, "\\."), names(object$coefficients)))
    
    # extract coefs and vcov from fit
    vcov <- vcov(object)[indices, indices]
    coefs <- summary(object)$coefficients[indices]
    
    # get fp functions
    strs <- unlist(strsplit(object$trafo[variable, ], ")\\+")) # extract transformations as strings (the closing bracket is included to deal with cases where there is a + within an fp transformation)
    fp.list <- lapply(1:length(strs), function(i){
      if(i < length(strs)) strs[i] <- paste0(strs[i], ")") # the closing bracket that is lost in the stringsplit needs to be added again (except for the last string)
      eval(parse(text = paste("function(", variable, "){", strs[i], "}"))) # create functions
    })
    
    # compute contrast
    x0 <- sapply(fp.list, function(f) f(seq.int) - f(ref.int))
    contrast <- x0 %*% coefs 
    
    # similarly for first and second derivative
    contrast.d1 <- sapply(fp.list, function(f){
      sapply(seq.int, function(x){
        numDeriv::grad(f, x) - numDeriv::grad(f, ref.int)
      })
    })  %*% coefs
    
    contrast.d2 <- sapply(fp.list, function(f){
      sapply(seq.int, function(x){
        numDeriv::hessian(f, x) - numDeriv::hessian(f, ref.int)
      })
    })  %*% coefs
    
    variance <- sapply(1:length(seq.int), function(X) x0[X,, drop=F] %*% vcov %*% t(x0[X,,drop=F]))
    stderr <- sqrt(variance)
    res <- data.frame(variable=seq.int, contrast=contrast, stderr=stderr, ref=ref.int, x0=x0, contrast.d1, contrast.d2)
    
    return(res)
    
    
  }, terms, seq, ref) |> setNames(terms)
  
  return(res.list)
}


}



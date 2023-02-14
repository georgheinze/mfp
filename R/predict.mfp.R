predict.mfp <- function(object, newdata, type = c("link", "response", "lp", "risk", "expected", "terms"), terms, ref = NULL, seq = NULL, se.fit = FALSE, 
	dispersion = NULL, na.action = na.pass, collapse, safe = FALSE, ...) 
{
# for glm: type in c("lp", "risk", "expected", "terms")
# for coxph: type in c("lp", "risk", "expected", "terms")
type <- match.arg(type)

if(type != "terms"){
  
  if(object$family$family=="Cox") {
    if (is.null(object$terms)) terms = names(object$assign)
    if (!missing(newdata)) 
      if (!missing(collapse)) 
        getFromNamespace("predict.coxph", "survival")(object$fit, newdata = newdata, type = type, se.fit = se.fit, terms = terms, collapse = collapse, safe = safe, ...) 
    else
      getFromNamespace("predict.coxph", "survival")(object$fit, newdata = newdata, type = type, se.fit = se.fit, terms = terms, safe = safe, ...) 
    else
      if (!missing(collapse)) 
        getFromNamespace("predict.coxph", "survival")(object$fit, type = type, se.fit = se.fit, terms = terms, collapse = collapse, safe = safe, ...) 
    else
      getFromNamespace("predict.coxph", "survival")(object$fit, type = type, se.fit = se.fit, terms = terms, safe = safe, ...) 
  } else {
    if (!missing(newdata)) 
      predict.glm(object$fit, newdata = newdata, type = type, se.fit = se.fit, dispersion = dispersion, terms = terms, na.action = na.action, ...)
    else 
      predict.glm(object$fit, type = type, se.fit = se.fit, dispersion = dispersion, terms = terms, na.action = na.action, ...)
  }
}

if(type == "terms"){
  
  # loop over all variables and bind together if necessary
  res.list <- lapply(terms, function(variable){
    
    # set default for seq and ref
    if(is.null(seq)) seq <- seq(min(object$fit$data[, variable], na.rm = TRUE),
                                max(object$fit$data[, variable], na.rm = TRUE),
                                length.out = 100)
    if(is.null(ref)) ref <- mean(object$fit$data[, variable], na.rm = TRUE) # use mean value as default reference
    
    # get indices by matching variable name and coefficient names
    indices <- which(grepl(paste0(variable, "\\."), names(object$coefficients)))
    
    # extract coefs and vcov from fit
    vcov <- vcov(object)[indices, indices]
    coefs <- summary(object)$coefficients[indices]
    
    # get fp functions
    strs <- unlist(strsplit(object$trafo[variable, ], "\\+")) # extract transformations as strings
    fp.list <- lapply(1:length(strs), function(i){
      eval(parse(text = paste("function(", variable, "){", strs[i], "}"))) # create functions
    })
    
    # compute contrast
    x0 <- sapply(fp.list, function(f) f(seq) - f(ref))
    contrast <- x0 %*% coefs 
    
    variance <- sapply(1:length(seq), function(X) x0[X,, drop=F] %*% vcov %*% t(x0[X,,drop=F]))
    stderr <- sqrt(variance)
    res <- data.frame(variable=seq, contrast=contrast, stderr=stderr, ref=ref, x0=x0)
    colnames(res)[1] <- variable
    
    return(res)
    
    
  }) |> setNames(terms)
  
  return(res.list)
}


}



#' Compute critical effect size values for a range of different objects
#' @description Compute critical effect size values for objects of classes htest (Student's t-test, correlation test), lm, or rma
#' @param x an object of class htest (available for t.test, cor.test), lm (a linear regression model), or rma (a meta-analytic linear model fitted with the "metafor" package)
#' @param ... other arguments passed for the specific method, depending on the class of the object x
#' @return an object of class critvalue
#' @export
critical <- function(x, ...){
  UseMethod("critical")
}


#' Compute critical effect size values for t-test and correlation tests
#' @description Compute critical effect size values for objects of classes htest computed with t.test or cor.test
#' @param x an object of class htest (available for t.test, cor.test)
#' @param ... Additional arguments (currently unused).
#' @return an object of class critvalue
#' @export
critical.htest <- function(x, ...){
  # all implemented subtype of htest objects
  mtds <- c("Two Sample", "One Sample", "correlation", "Paired")
  
  if(all(!grepl(paste0(mtds, collapse = "|"), x$method))){
    stop(
      sprintf("method %s of class %s not implemented yet!",
              x$method, class(x))
    )
  } else {
    D <- insight::get_data(x)
    
    if(is.null(D)){
      stop("insight::get_data(x) returning NULL. Are you using the formula syntax (y ~ x) with the data = argument? This syntax is not supported yet. See vignette('formula-syntax', package = 'criticalESvalue'). Try to call the function without the 'data = ' argument")
    }
    
    conf.level <- attributes(x$conf.int)$conf.level
    
    # hypothesis <- ifelse(x$alternative == "two.sided", "2t", "1t")
    hypothesis <- x$alternative
    
    alpha <- .get_alpha(conf.level, hypothesis)
    df <- x$parameter
    tc <- stats::qt(alpha, df)
    t <- x$statistic
    se <- x$stderr
    b <- x$estimate
    method <- x$method
  }
  
  if(grepl("correlation", method)){
    n <- df + 2
    r <- x$estimate
    cc <- critical_cor(r = r, n = n, conf.level = conf.level, hypothesis = hypothesis)
    dc <- bc <- cc$rc
    d <- b
    class(x) <- c("critvalue", "ctest", "htest")
  }else{ # if t-test
    if(grepl("Two Sample", method)){
      n <- tapply(D$x, D$y, length)
      if(grepl("Welch", method)){
        ss <- tapply(D$x, D$y, stats::sd)
        mm <- tapply(D$x, D$y, mean)
        tt <- critical_t2s(m1 = mm[1], m2 = mm[2],
                           sd1 = ss[1], sd2 = ss[2], 
                           n1 = n[1], n2 = n[2],
                           se = se,
                           hypothesis = hypothesis,
                           var.equal = FALSE)
      }else{
        tt <- critical_t2s(t = t, se = se, n1 = n[1], n2 = n[2], var.equal = TRUE,
                           conf.level = conf.level, hypothesis = hypothesis)
      }
    } else if(grepl("One Sample", method)){
      n <- nrow(D)
      tt <- critical_t1s(t = t, se = se, n = n, hypothesis = hypothesis, conf.level = conf.level)
    } else if(grepl("Paired", method)){
      n <- length(D$x[D$y == 1]) # n is not nrow(D)
      r12 <- stats::cor(D$x[D$y == 1], D$x[D$y == 2])
      tt <- critical_t2sp(t = t, 
                          se = se, 
                          r12 = r12,
                          n = n,
                          hypothesis = hypothesis, 
                          conf.level = conf.level)
      x$dz <- tt$dz
      x$dzc <- tt$dzc
      x$gz <- tt$gz
      x$gzc <- tt$gzc
    }

    d <- tt$d
    bc <- tt$bc
    dc <- tt$dc
    x$g <- unname(tt$g)
    x$gc <- tt$gc
    class(x) <- c("critvalue", "ttest", "htest")
    
  }
  
  # common elements
  x$d <- unname(d)
  x$bc <- unname(bc)
  x$dc <- unname(dc)
  
  return(x)
}


#' Compute critical effect size values for linear regression models
#' @description Compute critical effect size values for linear model coefficients of objects of class lm
#' @param x an object of class lm
#' @param conf.level the confidence interval level, needed to compute the smallest significant coefficient (default is 0.95, equaling a critical alpha = 0.05)
#' @param test which test should be used. `t` for a standard t-test (the default) and `z` for a z test.
#' @param ... Additional arguments (currently unused).
#' @return an object of class critvalue
#' @export
critical.lm <- function(x, conf.level = 0.95, test = "t", ...){
  
  # always two.sided
  hypothesis <- "two.sided"
  alpha <- .get_alpha(conf.level, hypothesis)
  df <- x$df.residual
  
  seb <- sqrt(diag(stats::vcov(x))) # standard error coefficients
  ll <- critical_coef(seb, df = df, conf.level = conf.level, hypothesis = hypothesis, test = test)
  d <- NA
  dc <- NA
  x$d <- NA
  x$dc <- NA
  x$bc <- unname(ll$bc)
  class(x) <- c("critvalue", "lm")
  return(x)
}


#' Compute critical effect size values for meta-analytic linear models
#' @description Compute critical effect size values for coefficients of objects of class rma (fitted with the "metafor" package)
#' @param x an object of class rma
#' @param conf.level the confidence interval level, needed to compute the smallest significant coefficient (default is 0.95, equaling a critical alpha = 0.05)
#' @param ... Additional arguments (currently unused).
#' @return an object of class critvalue
#' @export
critical.rma <- function(x, conf.level = 0.95, ...){
  if(inherits(x, "rma.uni")){
    hypothesis <- "two.sided"
    se <- x$se
    df <- x$dfs
    test <- x$test
    
    # only z and t supported
    test <- match.arg(test, choices = c("t", "z"))
    ll <- critical_coef(se, df = df, conf.level = conf.level, hypothesis = hypothesis, test = test)
    d <- NA
    dc <- NA
    x$d <- NA
    x$dc <- NA
    x$bc <- unname(ll$bc)
    class(x) <- c("critvalue", "rma.uni", "rma")
  } else{
    stop(paste("class", class(x)[1], "not supported yet!"))
  }
  return(x)
}

#' @export
print.critvalue <- function(x, digits = getOption("digits"), ...){
  NextMethod(x, ...)
  x <- .round_list(x, digits)
  if(inherits(x, "ttest")){
    cat("|== Effect Size and Critical Value ==|", "\n")
    if(x$alternative == "two.sided"){
      cat("d =", x$d, "dc = \u00b1", abs(x$dc), "bc = \u00b1", abs(x$bc),"\n")
      cat("g =", x$g, "gc = \u00b1", abs(x$gc), "\n")
    } else if(x$alternative == "greater"){
      cat("d =", x$d, "dc =", abs(x$dc), "bc =", abs(x$bc),"\n")
      cat("g =", x$g, "gc =", abs(x$gc), "\n")
    }else{
      cat("d =", x$d, "dc =", -abs(x$dc), "bc =", -abs(x$bc),"\n")
      cat("g =", x$g, "gc =", -abs(x$gc), "\n")
    }
    
    if(grepl("Paired", x$method)){
      if(x$alternative == "two.sided"){
        cat("dz =", x$dz, "|dzc| =", abs(x$dzc), "\n")
        cat("gz =", x$gz, "|gzc| =", abs(x$gzc), "\n")
      } else if(x$alternative == "greater"){
        cat("dz =", x$dz, "dzc =", abs(x$dzc), "\n")
        cat("gz =", x$gz, "gzc =", abs(x$gzc), "\n")
      } else{
        cat("dz =", x$dz, "dzc =", -abs(x$dzc), "\n")
        cat("gz =", x$gz, "gzc =", -abs(x$gzc), "\n")
      }
    }
    
  }else if(inherits(x, "ctest")){
    cat("|== Critical Value ==|", "\n")
    if(x$alternative == "two.sided"){
      cat("|rc| =", abs(x$dc),"\n\n")
    } else if(x$alternative == "greater"){
      cat("rc =", abs(x$dc),"\n\n")
    } else{
      cat("rc =", -abs(x$dc),"\n\n")
    }
    
  } else if(inherits(x, "lm")){
    cat("\nCritical |Coefficients| \n\n")
    bc <- x$bc
    names(bc) <- names(stats::coef(x))
    print(abs(bc), ...)
    cat("\n")
  } else if(inherits(x, "rma.uni")){
    crit <- data.frame(x$b)
    crit[, 1] <- x$bc
    if(x$int.only){
      rownames(crit) <- ""
    }
    names(crit) <- "|critical estimate|"
    cat(utils::capture.output(print(crit)), sep = "\n")
  }
  invisible(x)
}

#' @export
summary.critvalue <- function(object, ...) {
  NextMethod(object, ...)
  if (inherits(object, "lm")) {
    # taken from https://github.com/cran/lm.beta/blob/master/R/summary.lm.beta.R
    object2 <- object
    attr(object2, "class") <- "lm"
    object.summary <- summary(object2, ...)
    object.summary$coefficients <- cbind(
      object.summary$coefficients[, 1, drop = FALSE],
      `|Critical Estimate|` = abs(object$bc),
      object.summary$coefficients[, -1, drop = FALSE]
    )
    class(object.summary) <- c("summary.critvalue", class(object.summary))
  } else if (inherits(object, "rma.uni")) {
    object.summary <- object
  }
  object.summary
}
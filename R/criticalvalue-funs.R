#' crit_from_t_t2s
#' @description
#' This function allows to calculate the cohen's d and the critical d given the t-value for a two samples t-test, sample size of the two groups and the standard error, specifying the confidence level of the interval, the direction of the hypothesis and the variance parameter.
#' 
#' @param t the t value.
#' @param n1 a number corresponding to the sample size of group 1.
#' @param n2 a number corresponding to the sample size of group 2.
#' @param se a number corresponding to the standard error. 
#' @param conf.level confidence level of the interval.
#' @param hypothesis a character string indicating the alternative hypothesis ("less", "greater" or "two.tailed").
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal.
#'
#' @return the output returns a `d` which is the Cohen's d, the critical d which is the minimum value for which to get a significant result with a given sample, the `bc` is the numerator of the formula from which the d is calculated, `df` are the degrees of freedom and `se` is the standard error.

crit_from_t_t2s <- function(t = NULL, n1, n2, se = NULL,
                            conf.level, 
                            hypothesis,
                            var.equal = FALSE){
  if(!var.equal){
    warning("When var.equal = FALSE the critical value calculated from t assume sd1 = sd2!")
  }
  alpha <- .get_alpha(conf.level, hypothesis)
  df <- n1 + n2 - 2
  tc <- abs(stats::qt(alpha, df))
  if(is.null(t)){
    warning("When t is NULL, d cannot be computed, returning NA")
    d <- NA
  }else{
    d <- t * sqrt(1/n1 + 1/n2)
  }
  dc <- tc * sqrt(1/n1 + 1/n2)
  if(is.null(se)){
    warning("When se = NULL bc cannot be computed, returning NA!")
    bc <- NA
  }else{
    bc <- (tc * se)
  }
  out <- list(d = d, dc = dc, bc = bc, se = se, df = df)
  return(out)
}

#' crit_from_data_t2s
#' @description
#' This function allows to calculate the cohen's d and the critical d given the mean of the two groups, the standard deviation of the means and sample size of the two groups, specifying the confidence level of the interval, the direction of the hypothesis, the standard error, degrees of freedom and the variance parameter.
#' 
#' @param m1 a number representing the mean of group 1.
#' @param m2 a number representing the mean of group 2.
#' @param sd1 a number representing the standard deviation of group 1.
#' @param sd2 a number representing the standard deviation of group 2.
#' @param n1 a number corresponding to the sample size of group 1.
#' @param n2 a number corresponding to the sample size of group 2.
#' @param conf.level confidence level of the interval.
#' @param hypothesis a character string indicating the alternative hypothesis ("less", "greater" or "two.tailed").
#' @param se a number corresponding to the standard error.
#' @param df degrees of freedom.
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal.
#'
#' @return the output returns a `d` which is the Cohen's d, the critical d which is the minimum value for which to get a significant result with a given sample size, the `bc` is the numerator of the formula from which the d is calculated, `df` are the degrees of freedom, and `se` is the standard error.
#'
crit_from_data_t2s <- function(m1, m2, 
                               sd1, sd2, 
                               n1, n2, 
                               conf.level,
                               hypothesis,
                               se = NULL,
                               df = NULL,
                               var.equal = FALSE){
  alpha <- .get_alpha(conf.level, hypothesis)
  b <- m1 - m2
  if(!var.equal){ # welch
    se1 <- sd1 / sqrt(n1)
    se2 <- sd2 / sqrt(n2)
    if(is.null(se)) se <- sqrt(se1^2 + se2^2)
    # average sd
    s <- sqrt((sd1^2 + sd2^2)/2)
    if(is.null(df)) df <- se^4/(se1^4/(n1-1) + se2^4/(n2-1))
  }else{ # standard
    # pooled sd
    s <- sqrt((sd1^2 * (n1 - 1) + sd2^2 * (n2 - 1)) / (n1 + n2 - 2))
    if(is.null(se)) se <- s * sqrt(1/n1 + 1/n2)
    if(is.null(df)) df <- n1 + n2 - 2
  }
  tc <- abs(stats::qt(alpha, df))
  d <- b / s
  bc <- tc * se
  dc <- tc * sqrt(1/n1 + 1/n2)
  out <- list(d = d, dc = dc, bc = bc, se = se, df = df)
  return(out)
}


#' crit_from_t_t1s
#' @description
#' This function allows to calculate the cohen's d and the critical d for a one samples t-test given the t-value, sample size, specifying the confidence level of the interval and the direction of the hypothesis.
#' 
#' @param t the t value.
#' @param n a number corresponding to the sample size.
#' @param se a number corresponding to the standard error.
#' @param conf.level confidence level of the interval.
#' @param hypothesis a character string indicating the alternative hypothesis ("less", "greater" or "two.tailed").
#'
#' @return the output returns a `d` which is the cohen's d, the critical d which is the minimum value for which to get a significant result with a given sample, the `bc` is the numerator of the formula from which the d is calculated, `df` are the degrees of freedom, and `se` is the standard error.
#'
crit_from_t_t1s <- function(t = NULL, n, se = NULL,
                            conf.level,
                            hypothesis){
  alpha <- .get_alpha(conf.level, hypothesis)
  df <- n - 1
  tc <- abs(stats::qt(alpha, df))
  dc <- tc * sqrt(1/n)
  if(is.null(t)){
    warning("When t is NULL, d cannot be computed, returning NA")
    d <- NA
  }else{
    d <- t * sqrt(1/n)
  }
  if(is.null(se)){
    warning("When se = NULL bc cannot be computed, returning NA!")
    bc <- NA
  }else{
    bc <- tc * se
  }
  out <- list(d = d, dc = dc, bc = bc, se = se, df = df)
  return(out)
}

#' crit_from_data_t1s
#' @description
#' This function allows to calculate the cohen's d and the critical d for a one samples t-test given the mean, the variance and the sample numerosity, specifying the standard error, degrees of freedom, confidence level of the interval and the direction of the hypothesis.
#' 
#' @param m mean of the sample.
#' @param s a number corresponding to the sample standard deviatio
#' @param n a number corresponding to the sample size.
#' @param se a number corresponding to the standard error.
#' @param df degrees of freedom.
#' @param conf.level confidence level of the interval.
#' @param hypothesis a character string indicating the alternative hypothesis ("less", "greater" or "two.tailed").
#'
#' @return the output returns a `d` which is the cohen's d, the critical d which is the minimum value for which to get a significant result with a given sample, the `bc` is the numerator of the formula from which the d is calculated `df` are the degrees of freedom, and `se` is the standard error.
#'
crit_from_data_t1s <- function(m, s, n, se = NULL, df = NULL, 
                               conf.level, hypothesis){
  alpha <- .get_alpha(conf.level, hypothesis)
  if(is.null(df)) df <- n - 1
  tc <- abs(stats::qt(alpha, df))
  if(is.null(se)) se <- s / sqrt(n)
  d <- m / s
  dc <- tc * sqrt(1/n)
  bc <- tc * se
  out <- list(d = d, dc = dc, bc = bc, se = se, df = df)
  return(out)
}

#' crit_from_t_t2sp
#' @description
#' This function allows to calculate the standardized cohen's d, the critical standardized cohen's d, cohen's d and critical cohen's d given for a paired t test, given the t-value, the sample size, the standard error and the correlation between the two variables, specifying the confidence level of the interval and the direction of the hypothesis.
#' 
#' @param t the t value.
#' @param n a number corresponding to the sample size.
#' @param se a number corresponding to the standard error.
#' @param r12 a number corresponding to the correlation between variable 1 and variable 2.
#' @param hypothesis a character string indicating the alternative hypothesis ("less", "greater" or "two.tailed").
#' @param conf.level confidence level of the interval.
#'
#' @return the output returns a `dz` which is the critical Cohen's d standartized on the standard deviation of the differences, the `dzc` is the critical standardized d using the pooled standard deviation, the `d` is the Cohen's d (pooled standard deviation), the `bc` is the numerator of the formula from which the d is calculated, `df` are the degrees of freedom, and `se` is the standard error.
#'
crit_from_t_t2sp <- function(t = NULL, n, se = NULL, r12 = NULL, hypothesis, conf.level){
  alpha <- .get_alpha(conf.level, hypothesis)
  df <- n - 1
  tc <- abs(stats::qt(alpha, df))
  
  # d on differences
  dzc <- tc * sqrt(1/n)
  
  if(is.null(r12)) r12 <- 0
  dc <- dzc * sqrt(2 * (1 - r12))
  
  if(is.null(t)){
    warning("when t is NULL, dz and d cannot be computed, returning NA")
    dz <- NA
    d <- NA
  }else{
    dz <- t * sqrt(1/n)
    d <- dz * sqrt(2 * (1 - r12)) 
  }
  
  if(is.null(se)){
    warning("When se is NULL, bc cannot be computed, returning NA")
    bc <- NA
  }else{
    bc <- tc * se
  }
  
  out <- list(dz = dz, dzc = dzc, d = d, dc = dc, bc = bc, se = se, df = df)
  return(out)
}

#' crit_from_data_t2sp
#' @description
#' This function allows to calculate the standardized cohen's d, the critical standardized cohen's d, cohen's d, critical cohen's d and the numerator of formula from which to compute the cohen's d  given the mean of the two groups, the standard deviation of the means, correlation between the two variables and sample size, specifying the degrees of freedom, the confidence level of the interval and the direction of the hypothesis.
#' 
#' @param m1 a number representing the mean of condition 1 (e.g., pre scores). If `m2` is `NULL`, `m1` is the mean of differences between condition 1 and condition 2.
#' @param m2 a number representing the mean of condition 2 (e.g., post scores). Default to `NULL`. 
#' @param sd1 a number representing the standard deviation of condition 1. If `sd2` is `NULL`, `sd1` is the standard deviation of differences between condition 1 and condition 2.
#' @param sd2 a number representing the standard deviation of condition 2. Default to `NULL`. 
#' @param r12 a number corresponding to the correlation between the two conditions (e.g., pre-post correlation).
#' @param se standard error of the mean of differences.
#' @param n a number corresponding to the sample size.
#' @param df degrees of freedom.
#' @param conf.level confidence level of the interval.
#' @param hypothesis a character string indicating the alternative hypothesis ("less", "greater" or "two.tailed").
#'
#'  @return the output returns a `dz` which is the critical Cohen's d standartized on the standard deviation of the differences, the `dzc` is the critical standardized d using the pooled standard deviation, the `d` is the Cohen's d (pooled standard deviation), the `bc` is the numerator of the formula from which the d is calculated, `df` are the degrees of freedom, and `se` is the standard error.
#'

crit_from_data_t2sp <- function(m1, m2 = NULL, # if m2 = NULL, m1 is the mean of differences
                                sd1, sd2 = NULL, # if sd2 = NULL, sd1 is the standard deviation of differences
                                r12 = NULL, 
                                n,
                                se = NULL, # standard error of differences
                                df = NULL,
                                conf.level,
                                hypothesis){
  alpha <- .get_alpha(conf.level, hypothesis)
  df <- n - 1
  tc <- abs(stats::qt(alpha, df))
  
  if(!is.null(m2) & !is.null(sd2) & is.null(r12)){
    warning("when m2 and sd2 are provided and r12 is NULL dz and dzc and cannot be computed, returning NA")
  }
  
  if(is.null(r12)) r12 <- 0
  
  if(is.null(m2)){ # m1 is the average of differences
    b <- m1
  }else{
    # for paired (n1 = n2) the average of differences is the difference of the means
    b <- m1 - m2
  }
  
  if(is.null(sd2)){ # sd1 is the standard deviation of differences
    sdiff <- sd1
    sp <- sdiff / sqrt(2 * (1 - r12))
  }else{
    sdiff <- sqrt(sd1^2 + sd2^2 - 2*r12*sd1*sd2)
    sp <- sqrt((sd1^2 + sd2^2) / 2)
  }
  
  se <- sdiff / sqrt(n)
  
  dz <- b / sdiff
  dzc <- tc * sqrt(1 / n)
  d <- b / sp
  dc <- dzc * sqrt(2 * (1 - r12))
  bc <- tc * se
  out <- list(dz = dz, dzc = dzc, d = d, dc = dc, bc = bc, se = se, df = df)
  return(out)
}

# CRITICAL FUNS -----------------------------------------------------------

#' critical_t1s
#' @description
#' The function allows to calculate cohen's d and critical d for a one-sample t-test.
#' 
#' @param m a number representing the mean of the group.
#' @param s Variance
#' @param t the t value.
#' @param n a number corresponding to the sample size.
#' @param se a number corresponding to the standard error.
#' @param hypothesis a character string indicating the alternative hypothesis ("less", "greater" or "two.tailed").
#' @param conf.level the confidence level to set the confidence interval, default is set to 0.95.
#'
#' @return the output returns a `d` which is the Cohen's d, the critical d which is the minimum value for which to get a significant result with a given sample, the `bc` is the numerator of the formula from which the d is calculated, `df` are the degrees of freedom and `se` is the standard error, then it also gives the `g` and `gc` which are respectively `d` and `dc` with Hedfer's Correction for small samples.
#' @export
#'
#' @examples
#' # critical value from summary statistics
#' m <- 0.5
#' s <- 1
#' n <- 30
#' critical_t1s(m = m, s = s, n = n)
#' # critical value from the t statistic
#' se <- s / sqrt(n)
#' t <- m / se
#' critical_t1s(t = t, n = n, se = se) # se only required for calculating bc

critical_t1s <- function(m = NULL, s = NULL, t = NULL,
                         n, se = NULL,
                         hypothesis = c("two.sided", "greater", "less"),
                         conf.level = 0.95){
  hypothesis <- match.arg(hypothesis)
  if(!is.null(m)){
    out <- crit_from_data_t1s(m = m, s = s, n = n, conf.level = conf.level, hypothesis = hypothesis)
  }else{
    out <- crit_from_t_t1s(t = t, n = n, se = se, conf.level = conf.level, hypothesis = hypothesis)
  }
  
  # Hedges's Correction
  
  J <- .get_J(out$df)
  
  if(!is.na(out$d)){
    out$g <- J * out$d
  }
  if(!is.na(out$dc)){
    out$gc <- J * out$dc
  }
  out <- lapply(out, unname)
  return(out)
}

#' critical_t2s
#' @description
#' The function allows to calculate cohen's d and critical d for a two samples t-test.
#'
#' @param m1 a number representing the mean of group 1.
#' @param m2 a number representing the mean of group 2.
#' @param t the t value.
#' @param sd1 a number representing the standard deviation of group 1.
#' @param sd2 a number representing the standard deviation of group 2.
#' @param n1 a number corresponding to the sample size of group 1.
#' @param n2 a number corresponding to the sample size of group 2.
#' @param se a number corresponding to the standard error.
#' @param df degrees of freedom.
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal.
#' @param hypothesis a character string indicating the alternative hypothesis ("less", "greater" or "two.tailed").
#' @param conf.level the confidence level to set the confidence interval, default is set to 0.95.
#'
#' @return the output returns a `d` which is the Cohen's d, the critical d (`dc`) which is the minimum value for which to get a significant result with a given sample, the `bc` is the numerator of the formula from which the d is calculated, `se` which is the standard error, `df` are the degrees of freedom, then it also gives the `g` and `gc` which are respectively `d` and `dc` with Hedges's correction for small samples.
#' @export
#'
#' @examples
#' # critical value from summary statistics
#' m1 <- 0.5
#' m2 <- 1.0
#' sd1 <- 1
#' sd2 <- 1.5
#' n1 <- 30
#' n2 <- 35
#' critical_t2s(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2)
#' # critical value from the t statistic
#' se <- sqrt(sd1^2 / n1 + sd2^2 / n2)
#' t <- (m1 - m2) / se
#' critical_t2s(t = t, n1 = n1, n2 = n2, se = se) # se only required for calculating bc

critical_t2s <- function(m1 = NULL, m2 = NULL, t = NULL,
                         sd1 = NULL, sd2 = NULL,
                         n1, n2, se = NULL,
                         df = NULL,
                         var.equal = FALSE,
                         hypothesis = c("two.sided", "greater", "less"),
                         conf.level = 0.95){
  hypothesis <- match.arg(hypothesis)
  if(!is.null(m1) | !is.null(m2)){
    out <- crit_from_data_t2s(m1 = m1, m2 = m2, 
                              sd1 = sd1, sd2 = sd2, 
                              n1 = n1, n2 = n2, 
                              conf.level = conf.level,
                              hypothesis = hypothesis,
                              se = se,
                              df = df,
                              var.equal = var.equal)
  }else{
    out <- crit_from_t_t2s(t = t, 
                    n1 = n1, n2 = n2, 
                    se = se, 
                    conf.level = conf.level, 
                    hypothesis = hypothesis, 
                    var.equal = var.equal)
  }
  # Hedges's Correction
  J <- .get_J(out$df)
  
  if(!is.na(out$d)){
    out$g <- J * out$d
  }
  if(!is.na(out$dc)){
    out$gc <- J * out$dc
  }
  out <- lapply(out, unname)
  return(out)
}


#' critical_t2sp
#' @description
#' The function allows to calculate the standardized cohen's d, the critical standardized cohen's d, the cohen's d and the critical d for a paired two samples t-test.
#'
#' @param m1 a number representing the mean of group 1.
#' @param m2 a number representing the mean of group 2.
#' @param t the t value.
#' @param sd1 a number representing the standard deviation of group 1.
#' @param sd2 a number representing the standard deviation of group 2.
#' @param r12 a number corresponding to the correlation between variable 1 and variable 2.
#' @param n a number corresponding to the sample size.
#' @param se a number corresponding to the standard error.
#' @param hypothesis a character string indicating the alternative hypothesis ("less", "greater" or "two.tailed").
#' @param conf.level the confidence level to set the confidence interval, default is set to 0.95.
#'
#' @return the output returns a `dz` which is the critical Cohen's d standartized on the standard deviation of the differences, the `dzc` is the critical standardized d using the pooled standard deviation, the `d` is the Cohen's d, the `dc` is the minimum value for which to get a significant result with a given sample, the `bc` is the numerator of the formula from which the d is calculated, `se` is the standard error, `df` are the degrees of freedom,the `g` and `gc` are respectively `d` and `dc` with Hedges's Correction for small samples and `gz` and `gzc` are the standardized ones.  
#' @export
#'
#' @examples
#' # critical value from summary statistics
#' m1 <- 10
#' m2 <- 15
#' sd1 <- 5
#' sd2 <- 4.25
#' n <- 30
#' critical_t2sp(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n = n)
#' # critical value from the t statistic
#' se <- sqrt((sd1^2 + sd2^2) / n)
#' t <- (m1 - m2) / se
#' critical_t2sp(t = t, n = n, se = se) # se only required for calculating bc

critical_t2sp <- function(m1 = NULL, m2 = NULL, t = NULL,
                          sd1 = NULL, sd2 = NULL, r12 = NULL,
                          n, se = NULL,
                          hypothesis = c("two.sided", "greater", "less"),
                          conf.level = 0.95){
  hypothesis <- match.arg(hypothesis)
  if(!is.null(m1)){
    out <- crit_from_data_t2sp(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, r12 = r12, n = n, conf.level = conf.level, hypothesis = hypothesis)
  }else{
    out <- crit_from_t_t2sp(t = t, n = n, se = se, r12 = r12, hypothesis = hypothesis, conf.level = conf.level)
  }
  # Hedges's Correction
  
  J <- .get_J(out$df)
  if(!is.na(out$d)){
    out$g <- J * out$d
  }
  if(!is.na(out$dc)){
    out$gc <- J * out$dc
  }
  if(!is.na(out$dz)){
    out$gz <- J * out$dz
  }
  if(!is.na(out$dzc)){
    out$gzc <- J * out$dzc
  }
  out <- lapply(out, unname)
  return(out)
}

#' critical_cor
#' @description
#' This function allows to calculate the critical correlation value. When using `test = "t"`, the critical value is calculated assuming a Student t distribution with \eqn{n - 2} degrees of freedom and the standard error calculated using the raw correlation coefficient. When `test = "z"`, the critical value is calculated assimong a standard Normal distribution and the standard error is calculated applying the Fisher's z transformation.
#' 
#' @param r a number corresponding to the correlation coefficient.
#' @param n a number corresponding to the sample size.
#' @param conf.level the confidence level to set the confidence interval, default is set to 0.95.
#' @param hypothesis a character string indicating the alternative hypothesis ("less", "greater" or "two.tailed").
#' @param test a parameter to specify which test to apply, either "t" for a t-test or "z" for a z-test.
#' @return `rc` is the critical correlation value, `rzc` is the Fisher's z transformed critical correlation, `df` are the degrees of freedom, `se_r` is the standard error of the observed correlation, `se_rc` is the standard error of the critical correlation, `se_rzc` is the standard error of the Fisher's z transformed critical correlation and `test` is the statistical test (either t or z).
#' @export
#'
#' @examples
#' # critical value from r and sample size
#' r <- .25
#' n <- 30
#' critical_cor(r = r, n = n, test = "t")
#' critical_cor(r = r, n = n, test = "z") # using Fisher's z transformation
#' 
critical_cor <- function(r = NULL, n, 
                         conf.level = 0.95, 
                         hypothesis = c("two.sided", "greater", "less"), 
                         test = c("t", "z")){
  df <- n - 2
  test <- match.arg(test, test)
  hypothesis <- match.arg(hypothesis)
  
  alpha <- .get_alpha(conf.level, hypothesis)
  
  if(test == "t"){
    tc <- abs(stats::qt(alpha, df))
    rc <- tc / sqrt(n - 2 + tc^2)
    se_rc <- sqrt((1 - rc^2)/(n - 2))
    if(!is.null(r)){
      se_r <- sqrt((1 - r^2)/(n - 2))
    }else{
      se_r <- NA
    }
    rzc <- NA
    se_rzc <- NA
  }else{
    # F(r) is the Fisher's z transformed correlation
    zc <- abs(stats::qnorm(alpha))
    rc <- tanh(zc / sqrt(n - 3)) # critical r
    rzc <- atanh(rc) # critical z = F(r)
    se_rzc <- 1 / sqrt(n - 3) # standard error in z units
    se_rc <- sqrt((1 - rc^2)/(n - 2)) # standard error in r units
    if(!is.null(r)){
      se_r <- se_rc
    }else{
      se_r <- sqrt((1 - r^2)/(n - 2))
    }
  }
  out <- list(rc = rc, rzc = rzc, df = df, se_r = se_r, se_rc = se_rc, se_rzc = se_rzc, test = test)
  out <- lapply(out, unname)
  
  return(out)
}

#' critical_coef
#' @description
#' This function allows to calculate the critical beta.
#' 
#' @param seb a numeric vector of standard error of the regression coefficients.
#' @param n a number corresponding to the sample size.
#' @param p number of parameters.
#' @param df degrees of freedom.
#' @param conf.level the confidence level to set the confidence interval, default is set to 0.95.
#' @param hypothesis a character string indicating the alternative hypothesis ("less", "greater" or "two.tailed").
#' @param test a parameter to specify which test to apply, either "t" for a t-test or "z" for a z-test.
#'
#' @return the output returns the critical beta and the test used (either t or z).
#' @export
#'
#' @examples
#' # critical value from sample size and standard errors of the parameters
#' n <- 170
#' p = 3
#' seb <- c(25,.25,.10)
#' # seb is an hypothetical vector of standard errors from summary() of an lm() model 
#' critical_coef(seb = seb, n = n, p = p)
#' # using a fitted model
#' fit <- lm(mpg ~ disp + hp, data = mtcars)
#' fits <- summary(fit)
#' seb <- fits$coefficients[, "Std. Error"]
#' critical_coef(seb = seb, n = nrow(fit$model), p = ncol(fit$model) - 1)

critical_coef <- function(seb, n = NULL, p = NULL,df = NULL,
                             conf.level = 0.95,
                             hypothesis = c("two.sided", "greater", "less"),
                             test = c("t", "z")){
  if(is.null(df) & (is.null(p) & is.null(n))){
    stop("df or p and n need cannot be NULL")
  }
  
  test <- match.arg(test, test)
  hypothesis <- match.arg(hypothesis)
  alpha <- .get_alpha(conf.level, hypothesis)
  
  if(is.null(df)){
    df <- n - p - 1
  }
  
  if(test == "t"){
    qc <- abs(stats::qt(alpha, df))
  }else{
    qc <- abs(stats::qnorm(alpha))
  }
  
  bc <- qc * seb
  out <- list(bc = bc, test = test)
  return(out)
}
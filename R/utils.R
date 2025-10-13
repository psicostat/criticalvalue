.p_value <- function(q,
                 df = NULL,
                 test = c("t", "z"),
                 conf.level = 0.95,
                 hypothesis = c("1t", "2t")){
  hypothesis <- match.arg(hypothesis)
  test <- match.arg(test)
  # two tails
  if(test == "t"){
    p <- 2 * stats::pt(-abs(q), df)
  }else{
    p <- 2 * stats::pnorm(-abs(q))
  }
  if(hypothesis == "1t"){
    p <- p / 2
  }
  return(p)
}

.get_alpha <- function(conf.level = 0.95,
                       hypothesis = "two.sided"){
  hypothesis <- if(hypothesis == "two.sided") "2t" else "1t"
  alpha <- 1 - conf.level
  if(hypothesis == "2t"){
    alpha <- alpha / 2
  }
  return(alpha)
}

.get_J <- function(df){
  # taken from https://github.com/easystats/effectsize/blob/a579a5093549be2edc264e2d1da95726ff4f4369/R/cohens_d.R#L315
  exp(lgamma(df/2) - log(sqrt(df/2)) - lgamma((df - 1)/2))
}

.round_list <- function(x, digits = getOption("digits")){
  out <- lapply(x, function(el){
    if(is.numeric(el)){
      round(el, digits)
    } else{
      el
    }
  })
  class(out) <- class(x)
  out
}

.hyp_sign <- function(x, hypothesis){
  x <- abs(x)
  ifelse(hypothesis %in% c("two.sided", "greater"), x, -x) 
}

.sim_t_data <- function(m1, m2 = NULL, 
                       sd1, sd2 = NULL, 
                       n1, n2 = NULL,
                       type = c("1s", "2s", "2sp"),
                       r12 = 0,
                       empirical = TRUE){
  type <- match.arg(type)
  
  if(type == "1s"){
    y <- MASS::mvrnorm(n1, m1, Sigma = sd1^2, empirical = empirical)
    data <- data.frame(y = y[, 1])
  } else if(type == "2s"){
    y1 <- MASS::mvrnorm(n1, m1, Sigma = sd1^2, empirical = empirical)
    y2 <- MASS::mvrnorm(n2, m2, Sigma = sd2^2, empirical = empirical)
    data <- data.frame(y = c(y1, y2), x = rep(0:1, c(n1, n2)))
  } else{
    R <- matrix(c(1, r12, r12, 1), nrow = 2)
    S <- diag(c(sd1, sd2)) %*% R %*% diag(c(sd1, sd2))
    Y <- MASS::mvrnorm(n1, c(m1, m2), Sigma = S, empirical = empirical)
    data <- data.frame(y = c(Y[, 1], Y[, 2]), x = rep(0:1, each = n1))
  }
  
  return(data)
  
}

.get_s <- function(sd1, sd2, n1 = NULL, n2 = NULL, var.equal = TRUE){
  if(var.equal){
    if(is.null(n1) | is.null(n2)){
      stop("When pooled = TRUE, sample size need to be provided!")
    }
    s <- sqrt((sd1^2 * (n1 - 1) + sd2^2 * (n2 - 1)) / (n1 + n2 - 2))
  } else{
    s <- sqrt(0.5*(sd1^2 + sd2^2))
  }
  return(s)
}
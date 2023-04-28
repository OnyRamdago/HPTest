#' Hypothesis Testing Function: myttest
#' The constructor will take at least three arguments x, y, and alpha and conducts hypothesis testing
#' @param x vector
#' @param y vector
#' @param alpha double
#' @param paired boolean
#'
#' @importFrom stats t.test
#' @return a list object containing a dataframe of x and y, the alpha value, p-value and confidence interval
#' @export
#'
#' @examples set.seed(21); x <-rnorm(30,5,2); set.seed(23); y<- rnorm(40, 3,2); paired=FALSE
#' alpha <- 0.05; obj <- myttest(x = x, y = y, paired=FALSE, alpha = 0.05)
#'
myttest = function(x, y, alpha, paired=FALSE){

  #Conduct ttest
  if (paired) { #If data is paired, find the difference and conduct paired t-test
    diff <- x - y
    tt <- t.test(diff, mu = 0, alternative = "two.sided", conf.level = 1 - alpha)
    test_type <- "Paired"
  } else { #check variances and choose the type of test performed
    tt <- stats::var.test(x, y, alternative = "two.sided", conf.level = 1 - alpha)
    test_type <- ifelse(tt$method == "F test", "Welch", "T-test")
  }
  #Reject or fail to reject the hypothesis
  conclusion <- ifelse(tt$p.value < alpha, "Y", "N")

  # Find out maximum length
  max_length <- max(c(length(x), length(y)))
  # Create data frame with unequal vectors
  df <- data.frame(x = c(x,
                         rep(NA, max_length - length(x))),
                   y = c(y,
                         rep(NA, max_length - length(y))))


  #Create named list object
  obj = list(data = data.frame(df), alpha=alpha, pval= tt$p.value, conf_int=tt$conf.int, test_type=test_type, concl=conclusion, diff=diff)

  #Name the class Rttest
  class(obj) = "Rttest"

  obj
}

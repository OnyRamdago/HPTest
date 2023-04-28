#' Rttest Print Function
#' @export
#' @export print.Rttest
#' @param obj Object of type Rttest, from myttest constructor
#'
#' @return The confidence interval and type of test performed
#'
#'
#' @examples set.seed(21); x <-rnorm(30,5,2); set.seed(23); y<- rnorm(40, 3,2); paired=FALSE
#' alpha <- 0.05; obj <- myttest(x = x, y = y, paired=FALSE, alpha = 0.05); print(obj)
#'
print.Rttest = function(obj) {
  print(paste("The value of (1 - alpha) * 100 is"))
  print((1-obj$alpha)*1000)
  cf = (1-obj$alpha)*1000
  ci1 = obj$conf_int[1]
  ci2 = obj$conf_int[2]
  ttype = obj$test_type
  cat(sprintf("The confidence interval of mu_x and mu_y at %..0f%% confidence level is [%.3f, %.3f]\n for a %s test",
              cf, ci1, ci2, ttype))
}

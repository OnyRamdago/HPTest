#' Boxplots for myttest
#' @export
#' @param obj Object of type Rttest, from myttest constructor
#'
#' @return Boxplot of differences if data is paired; Side by side boxplot if data is unpaired
#'
#'
#' @examples set.seed(21); x <-rnorm(30,5,2); set.seed(23); y<- rnorm(40, 3,2); paired=FALSE
#' alpha <- 0.05; obj <- myttest(x = x, y = y, paired=FALSE, alpha = 0.05); class(obj); plot(obj)
#'
plot <- function(obj) {

  #Check if the object is paired or not
  if (obj$test_type == "Paired") {
    #get the difference and create a boxplot of differences
    diff <- obj$data$x - obj$data$y
    graphics::boxplot(diff, xlab = "Difference", ylab = "Data", main = "Boxplot of differences")
    ci <- obj$conf_int
    graphics::text(1, ci[1] + (ci[2] - ci[1])/2, paste0("CI = [", round(ci[1], 2), ", ", round(ci[2], 2), "]"), pos = 1)
  } else {
    #Create a side by side boxplot of x and y
    graphics::boxplot(obj$data$x, obj$data$y, main = "Boxplot of x and y", names = c("x", "y"))
  }
}

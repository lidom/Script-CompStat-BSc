## Figure 2.9
set.seed(321)

n <- 50

## true function
true_f <- function(x) {
  4 + 0.025 * x + 2* sin(10 + x * 2 * pi / 110)
}

## generate data points for plotting true function
xx     <- seq(0, 100, length.out = 150)
fxx    <- true_f(xx)

## generate training data (x, y)
x_vec  <- runif(n, min = 0, max = 100)
e_vec  <- rnorm(n)
y_vec  <- true_f(x_vec) + e_vec

## fit a simple linear regression model
lm_fit      <- lm(y_vec ~ x_vec)

## fit two smoothing spline models with different degrees of freedom
ss_fit_df5  <- smooth.spline(x_vec, y_vec, df = 5)
ss_fit_df20 <- smooth.spline(x_vec, y_vec, df = 20)


## plot the true function, the training data, and the fitted models
plot(x    = xx, 
     y    = fxx, 
     type = "l", lwd  = 2,
     ylim = range(y_vec), 
     ylab = "Y", xlab = "X")
points(x = x_vec, y = y_vec)
abline(lm_fit, col = "darkorange", lwd = 2)
lines(predict(ss_fit_df5, x=xx), col = "darkblue", lwd = 2)
lines(predict(ss_fit_df20, x=xx), col = "darkgreen", lwd = 2)

## adding a legend
legend("topleft", 
       legend = c("True function", 
       "Linear model", "Smoothing spline (df = 5)", 
       "Smoothing spline (df = 20)"), 
       col    = c("black", "darkorange", "darkblue", "darkgreen"), 
       lty    = c(1, 1, 1, 1), 
       lwd    = c(2, 2, 2, 2))

## compute training MSE
(MSE_train_lm      <- mean((y_vec - predict(lm_fit, newdata = data.frame("x_vec" = x_vec)))^2))
(MSE_train_ss_df5  <- mean((y_vec - predict(ss_fit_df5,  x=x_vec)$y)^2))
(MSE_train_ss_df20 <- mean((y_vec - predict(ss_fit_df20, x=x_vec)$y)^2))


## compute test MSE

## generate test data (x, y)
x_test_vec  <- runif(n, min = 0, max = 100)
e_test_vec  <- rnorm(n)
y_test_vec  <- true_f(x_test_vec) + e_test_vec

(MSE_test_lm      <- mean((y_test_vec - predict(lm_fit, newdata = data.frame("x_vec" = x_test_vec)))^2))
(MSE_test_ss_df5  <- mean((y_test_vec - predict(ss_fit_df5,  x=x_test_vec)$y)^2))
(MSE_test_ss_df20 <- mean((y_test_vec - predict(ss_fit_df20, x=x_test_vec)$y)^2))




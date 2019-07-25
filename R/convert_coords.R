
lag1 <- function(x) {
    c(NA, x[-length(x)])
}
lead1 <- function(x) {
    c(x[-1], NA)
}
distance <- function(x, y){
    sqrt(((x - lead1(x)) ^ 2) + ((y - lead1(y)) ^ 2))
}
angle <- function(x, y){
    atan2(lead1(x) - x, lead1(y) - y)
}
heading <- function(x, y) {
    theta <- atan2(lead1(x) - x, lead1(y) - y)
    ifelse(theta < 0, (2 * pi) + theta, theta)
}
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}


# ## Checking functions
# x <- c(0, 1, 1, 2, 2, 1, 0)
# y <- c(0, 1, 2, 2, 0, 0, 1)
#
# distance(x, y)
# rad2deg(angle(x, y))
# rad2deg(heading(x, y))
#
# plot(x, y, col = "white")
# lines(x, y, lty = 3)
# text(x, y, seq(x))


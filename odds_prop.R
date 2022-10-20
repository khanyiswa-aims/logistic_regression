pregMice <- matrix(c(  0,     15,   1, 281,
                       62.5,  17,   0, 225,
                       125,    22,   7, 283,
                       250,    38,  59, 202,
                       500,   144, 132,   9),
                   ncol = 4, byrow = TRUE)
colnames(pregMice) <- c("concentration", "dead", "malformed", "normal")
(pregMice <- as.data.frame(pregMice))
library(VGAM)
fit <- vglm(cbind(pregMice$dead, pregMice$malformed, pregMice$normal) ~
              pregMice$concentration, family = cumulative(parallel = T))
summary(fit)
library(brant)
brant('fit')
exp(confint(fit))
coef(fit)
newx <- seq(0, 5.5, length = 500)
binomial.logit.fit <- function(x, a, b) {
  ex <- exp(a + b*x)
  ex/(1 + ex)
}

pctA <- binomial.logit.fit(newx, coef(fit)[1], coef(fit)[3])
#pctB <-binomial (newx, coef(fit)[1] + coef(fit)[2], coef(fit)[3])
xrange <- range(0, 5.5)
yrange <- range(0, 1)
par(new = T)
plot(newx, pctB, type = "l", lwd = 3, col = "green", xlim = xrange, ylim = yrange,
     xlab = "", ylab = "")
plot(newx, pctB, type = "l", lwd = 3, col = "yellow", xlim = xrange, ylim = yrange,
     xlab = "", ylab = "")

legend("topleft", legend = c("Drug A", "Drug B", 'Drug C'),
       col = c("red", "green"), lty = c(1, 1), pch = c(19, 19), lwd = 3)



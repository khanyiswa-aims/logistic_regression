y <- c(3, 4, 5, 6, 6, 7, 8, 8, 9, 10, 10)
drug <- c(rep("A", 3), rep("B", 8))
(direct <- data.frame(y, drug))
t.test(y ~ drug, data = direct, var.equal = T)
dumA <- c(rep(1, 3), rep(0, 8)) # Assigning dumbA variables
dumB <- c(rep(0, 3), rep(1, 8)) # Assigning dumbA variables
(FC <- data.frame(y, dumA, dumB))
FCmodfun <- function(dA, dB, muA, rho) muA*dA + muA*rho*dB
fit <- nls(y ~ FCmodfun(dumA, dumB, meanA, relPot), data = FC,
           start = list(meanA = 1, relPot = 1))
summary(fit)
FCmodfunR <- function(dA, dB, muA) muA*dA + muA*dB
fitR <- nls(y ~ FCmodfunR(dumA, dumB, meanA), data = FC,
            start = list(meanA = 1))
anova(fitR, fit)
LRCI <- confint(fit)[2, ]
pe <- coef(fit)[2]
se <- sqrt(vcov(fit)[2,2])
WCI <- pe + c(-1, 1)*qt(0.975, 9)*se
rbind(WCI, LRCI)

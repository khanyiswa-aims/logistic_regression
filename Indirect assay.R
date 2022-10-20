dose<- rep(c(1, 2, 4, 8, 16, 32), 2)
L2dose<- log2(dose)
dumB<- c(rep(0, 6), rep(1, 6))
dumB.L2dose <- dumB*L2dose
nn<- rep(20, 12)
yy<- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
pct<- yy/nn
(budworms<- data.frame(dose, L2dose, dumB, dumB.L2dose, yy, nn, pct))
budwormsA<- subset(budworms, dumB == 0)
budwormsB<- subset(budworms, dumB == 1)
xrange <- range(0, 5.5); yrange <- range(0, 1)
plot(budwormsA$L2dose, budwormsA$pct, pch = 19, cex = 1.5, col = "red",
     xlab = "log2(dose)", ylab = "percent mortality",
     ylim = yrange, xlim = xrange); par(new = T)
plot(budwormsB$L2dose, budwormsB$pct, pch = 19, cex = 1.5, col = "green",
     xlab = "", ylab = "", ylim = yrange, xlim = xrange)
legend("topleft", legend = c("Drug A", "Drug B"),
       col = c("blue", "purple"), lty = c(1, 1), pch = c(19, 19), lwd = 3)

#Verify (via testing) that the drug A and B logistic curves are parallel here (report the TS and p-value)
(budworms <- data.frame(dose, L2dose, dumB, dumB.L2dose, yy, nn, pct))
fit <- glm(pct ~ L2dose + dumB + dumB.L2dose, family = binomial, weight = nn,
           data = budworms)
summary(fit)
fit2 <- glm(pct ~ L2dose + dumB, family = binomial, weight = nn, data = budworms)
summary(fit2)
#library(car)
anova(fit2, fit, test = "LRT")
coef(fit2)
coef(fit2)[2]
newx <- seq(0, 5.5, length = 500)
binomial.logit.fit <- function(x, a, b) {
  ex <- exp(a + b*x)
  ex/(1 + ex)
}

pctA <- binomial.logit.fit(newx, coef(fit2)[1], coef(fit2)[2])
pctB <- binomial.logit.fit(newx, coef(fit2)[1] + coef(fit2)[3], coef(fit2)[2])
xrange <- range(0, 5.5)
yrange <- range(0, 1)

plot(newx, pctA, type = "l", lwd = 3, col = "red", xlim = xrange, ylim = yrange,
     xlab = "log2(dose)", ylab = "percent mortality")
par(new = T)
plot(newx, pctB, type = "l", lwd = 3, col = "green", xlim = xrange, ylim = yrange,
     xlab = "", ylab = "")
legend("topleft", legend = c("Drug A", "Drug B"),
       col = c("red", "green"), lty = c(1, 1), pch = c(19, 19), lwd = 3)
(LD50A <- -coef(fit2)[1]/coef(fit2)[2])
(LD50B <- -(coef(fit2)[1] + coef(fit2)[3])/coef(fit2)[2])
points(LD50A, 0.5, pch = 19, col = "red", cex = 2)
points(LD50B, 0.5, pch = 19, col = "green", cex = 2)

(RPlogscale <- LD50B/LD50A)
(RP <- 2^RPlogscale)
coef(fit2)[3]

voltage<- read.csv(file.choose())


attach(voltage)
summary(voltage)
library(splines)

# Voltage drop vs. time

plot(time, drop, bg='red', pch=23, cex=2)

# Fitting a  spline model bs to generate basis matrix for a polynomial spline

spline.lm <- lm(drop ~ bs(time, knots=c(6.5,13)))
lines(time, predict(spline.lm), lwd=2, col='yellow')
summary(spline.lm)

plot(spline.lm,which = 1,pch = 16)

quadline(spline.lm)
# Fit a reduced cubic model: important: this model is contained

##cubic uses polynomial predictors,poly()
cubic.lm <- lm(drop ~ poly(time, 3))
lines(time, predict(cubic.lm), lwd=2, lty=2, col='green')
library(fgac)
print(ftest(spline.lm, cubic.lm))






op <- par(mfrow = c(2,1), mgp = c(2,.8,0), mar = .1+c(3,3,3,1))
n <- 9
x <- 1:n
y <- rnorm(n)
plot(x, y, main = paste("spline[fun](.) through", n, "points"))
lines(spline(x, y))
lines(spline(x, y, n = 201), col = 2)


y <- (x-6)^2
plot(x, y, main = "spline(.) -- 3 methods")
lines(spline(x, y, n = 201), col = 2)
lines(spline(x, y, n = 201, method = "natural"), col = 3)
lines(spline(x, y, n = 201, method = "periodic"), col = 4)
legend(6,25, c("fmm","natural","periodic"), col=2:4, lty=1)


?par(mfrow = c(2,1), mgp = c(2,.8,0), mar = .1+c(3,3,3,1))

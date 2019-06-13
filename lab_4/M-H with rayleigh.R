# Metropolis-Hastings sampling from Rayleigh Density
f <- function(x, sig) { return((x / sig^2) * exp(-x^2 / (2*sig^2)))}
T <- 100000; x <- numeric(T); u <- runif(T)
sig <- 4
# generate initial value from chi-square with 1 df
x[1] <- rchisq(1, df=1); Rej <- 0
# Sampling loop
for (t in 2:T) { xt <- x[t-1];
xcand <- rchisq(1, df = xt)
num <- f(xcand, sig) * dchisq(xt, df = xcand)
den <- f(xt, sig) * dchisq(xcand, df = xt)
if (u[t] <= num/den) x[t] <- xcand
else {x[t] <- xt; Rej <- Rej+1}} # proposal x.cand is rejected
RejR <- Rej/T; AccepR <- 1-RejR
cat("Acceptance Rate",AccepR,"nn")
# posterior summary
samp <- 5001:10000; xsamp <- x[samp]
summary(xsamp)
# plot samples 5001-10000
plot(samp, xsamp, type="l", main="Plot of Samples", ylab="x",xlab="Iteration")
# kernel plot
den.x <- density(xsamp) # returns the density data
plot(den.x)

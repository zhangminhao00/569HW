# Pareto
set.seed(569)
n = 50000
data = sort(rpareto(n,shape=3,scale=10))

p_values = seq(1/(n+1), n/(n+1), by=1/(n+1))
theoretical_Q = -log(1-p_values)
empirical_Q = quantile(data, probs=p_values)

plot(theoretical_Q, empirical_Q,
     main=c("Exponential QQ-plot","\nPareto"),
     xlab="-log(1-p)",
     ylab="Q(p)")

e=numeric(n)
K=1:(n-1)
e[n-K] = cumsum(data[n-K+1])/K - data[n-K]

plot(data[K],e[K],xlab=bquote(X["n-k,n"]), ylab=bquote(e["k,n"]),
     main=c("Mean excess plot","\nPareto"))



# Weibull (tau>1)
set.seed(569)
n = 50000
data = sort(rweibull(n,shape=3,scale=10))

p_values = seq(1/(n+1), n/(n+1), by=1/(n+1))
theoretical_Q = -log(1-p_values)
empirical_Q = quantile(data, probs=p_values)

plot(theoretical_Q, empirical_Q,
     main=c("Exponential QQ-plot","\nWeibull (tau>1)"),
     xlab="-log(1-p)",
     ylab="Q(p)")

e=numeric(n)
K=1:(n-1)
e[n-K] = cumsum(data[n-K+1])/K - data[n-K]

plot(data[K],e[K],xlab=bquote(X["n-k,n"]), ylab=bquote(e["k,n"]),
     main=c("Mean excess plot","\nWeibull (tau>1)"))

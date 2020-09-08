set.seed(569)
sample_size = c(1,2,5,100) 
n_samples = 2500 
means=c()
std=c()
variance=c()

par(mfrow=c(1,4))
for (i in c(1:4)){
simulated_number_accident = rexp(sample_size[i]*n_samples,rate=0.5)
simulated_matrix = matrix(simulated_number_accident, n_samples)
sample_means = rowMeans(simulated_matrix)
means=c(means,mean(sample_means))
std=c(std,sd(sample_means))
variance=c(variance,sd(sample_means)^2)


main=c("Means of Exp(rate=0.5) \n sample size:\n",sample_size[i])
hist_exp = hist(sample_means,xlab="means",
                main=main,
                ylim=c(0,850))
xfit_exp = seq(min(sample_means),max(sample_means),length=100)
yfit_exp = dnorm(xfit_exp,mean=mean(sample_means),sd=sd(sample_means))
yfit_exp_scaled = yfit_exp*diff( hist_exp$mids[1:2])*length(sample_means)
lines(xfit_exp, yfit_exp_scaled, col="blue", lwd=2)}

for (i in c(1:4)){
cat("\n\n When whe sample size is",sample_size[i],"\n the mean is", means[i],
    "\n the standard deviation is",std[i],"\n the variance is",variance[i])}


















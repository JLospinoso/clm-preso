source("experiment-utils.R")

levels <- seq(from=-0.5, to=0, by=.01)
results <- sapply(levels, experiment)
results
save(results, file="exper.Rda")
pdf("experiment_plot.pdf", width = 12, height=8)
plot(results[2,]~levels, type="l", ylim=c(0,1), col="red", lty=2, 
     xlab="Coefficient", ylab="Power/Type-1 Error", cex.lab=2.1, cex.axis=1.4)
lines(results[3,]~levels, col="red")
lines(results[4,]~levels, lty=2)
lines(results[5,]~levels)
dev.off()

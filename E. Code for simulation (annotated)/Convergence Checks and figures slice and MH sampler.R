load("E. Code for simulation (annotated)/MH Sampler/resultsK4c2dateMH.Rdata") 

#average convergence over 500 datasets

plot.ts(sapply(1:2250, function(i){mean(res[i,1,])}))
plot.ts(sapply(1:2250, function(i){mean(res[i,2,])}))
plot.ts(sapply(1:2250, function(i){mean(res[i,3,])}))
plot.ts(sapply(1:2250, function(i){mean(res[i,4,])}))
plot.ts(sapply(1:2250, function(i){mean(res[i,5,])}))
plot.ts(sapply(1:2250, function(i){mean(res[i,6,])}))
plot.ts(sapply(1:2250, function(i){mean(res[i,7,])}))
plot.ts(sapply(1:2250, function(i){mean(res[i,8,])}))
plot.ts(sapply(1:2250, function(i){mean(res[i,9,])}))

plot.ts(sapply(1:20000, function(i){mean(res[i,1,])}))
plot.ts(sapply(1:20000, function(i){mean(res[i,2,])}))
plot.ts(sapply(1:20000, function(i){mean(res[i,3,])}))
plot.ts(sapply(1:20000, function(i){mean(res[i,4,])}))
plot.ts(sapply(1:20000, function(i){mean(res[i,5,])}))
plot.ts(sapply(1:20000, function(i){mean(res[i,6,])}))
plot.ts(sapply(1:20000, function(i){mean(res[i,7,])}))
plot.ts(sapply(1:20000, function(i){mean(res[i,8,])}))
plot.ts(sapply(1:20000, function(i){mean(res[i,9,])}))

plot.ts(sapply(1:40000, function(i){mean(res[i,1,])}))
plot.ts(sapply(1:40000, function(i){mean(res[i,2,])}))
plot.ts(sapply(1:40000, function(i){mean(res[i,3,])}))
plot.ts(sapply(1:40000, function(i){mean(res[i,4,])}))
plot.ts(sapply(1:40000, function(i){mean(res[i,5,])}))
plot.ts(sapply(1:40000, function(i){mean(res[i,6,])}))
plot.ts(sapply(1:40000, function(i){mean(res[i,7,])}))
plot.ts(sapply(1:40000, function(i){mean(res[i,8,])}))
plot.ts(sapply(1:40000, function(i){mean(res[i,9,])}))

plot.ts(sapply(1:80000, function(i){mean(res[i,1,])}))
plot.ts(sapply(1:80000, function(i){mean(res[i,2,])}))
plot.ts(sapply(1:80000, function(i){mean(res[i,3,])}))
plot.ts(sapply(1:80000, function(i){mean(res[i,4,])}))
plot.ts(sapply(1:80000, function(i){mean(res[i,5,])}))
plot.ts(sapply(1:80000, function(i){mean(res[i,6,])}))
plot.ts(sapply(1:80000, function(i){mean(res[i,7,])}))
plot.ts(sapply(1:80000, function(i){mean(res[i,8,])}))
plot.ts(sapply(1:80000, function(i){mean(res[i,9,])}))

#convergence for last 100 datasets

for(i in 400:500){
  plot.ts(res[,1,i])
}

for(i in 400:500){
  plot.ts(res[,2,i])
}

for(i in 400:500){
  plot.ts(res[,3,i])
}

for(i in 400:500){
  plot.ts(res[,4,i])
}

for(i in 400:500){
  plot.ts(res[,5,i])
}

for(i in 400:500){
  plot.ts(res[,6,i])
}

for(i in 400:500){
  plot.ts(res[,7,i])
}

for(i in 400:500){
  plot.ts(res[,8,i])
}

for(i in 400:500){
  plot.ts(res[,9,i])
}

#ergodic means/running means plot

require(dlm)
for(i in 400:500){
em <- ergMean(res[,1,i], m = 1)
plot(ts(em, start=1), xlab="Iteration", main="Ergodic means")
}

for(i in 400:500){
  em <- ergMean(res[,2,i], m = 1)
  plot(ts(em, start=1), xlab="Iteration", main="Ergodic means")
}

for(i in 400:500){
  em <- ergMean(res[,3,i], m = 1)
  plot(ts(em, start=1), xlab="Iteration", main="Ergodic means")
}

for(i in 400:500){
  em <- ergMean(res[,4,i], m = 1)
  plot(ts(em, start=1), xlab="Iteration", main="Ergodic means")
}

for(i in 400:500){
  em <- ergMean(res[,5,i], m = 1)
  plot(ts(em, start=1), xlab="Iteration", main="Ergodic means")
}

for(i in 400:500){
  em <- ergMean(res[,6,i], m = 1)
  plot(ts(em, start=1), xlab="Iteration", main="Ergodic means")
}

#create plots for paper


library(extrafont)
font_install("fontcm")
loadfonts()
pdf("F. Figures/Convergence_acceptable.pdf", family="CM Roman")
op <- par(cex.lab=14/10, cex.axis=14/10, pin=c(5.5,0.5), mfrow=c(3,1))
load("E. Code for simulation (annotated)/results/20000 its/resultsF3b.Rdata")
plot.ts(sapply(1:20000, function(i){mean(res[i,2,])}), ylab="Estimated Value", xlab="Iterations")
par(op)
dev.off()


library(extrafont)
font_install("fontcm")
loadfonts()
pdf("F. Figures/Convergence_good.pdf", family="CM Roman")
op <- par(cex.lab=14/10, cex.axis=14/10, pin=c(5.5,0.5), mfrow=c(3,1))
load("E. Code for simulation (annotated)/results/20000 its/resultsH1c.Rdata")
plot.ts(sapply(1:20000, function(i){mean(res[i,2,])}), ylab="Estimated Value", xlab="Iterations")
par(op)
dev.off()

library(extrafont)
font_install("fontcm")
loadfonts()
pdf("F. Figures/Convergence_bad.pdf", family="CM Roman")
op <- par(cex.lab=14/10, cex.axis=14/10, pin=c(5.5,0.5), mfrow=c(3,1))
load("E. Code for simulation (annotated)/results/Not converged 26-01-2015/resultsH3a.Rdata")
plot.ts(sapply(1:20000, function(i){mean(res[i,2,])}), ylab="Estimated Value", xlab="Iterations")
par(op)
dev.off()





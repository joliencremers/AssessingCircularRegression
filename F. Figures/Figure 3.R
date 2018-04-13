library(extrafont)
font_install("fontcm")
loadfonts()

pdf("F. Figures/Convergence.pdf",
    family="CM Roman", height=6, width=10)

postscript("F. Figures/Convergence.eps",
           horizontal = FALSE, onefile = FALSE,
           paper = "special", height=6, width=10)

op=par(mfrow=c(3,1), mai=c(0.6,0.6,0.01,0.1), cex.lab=18/12, cex.axis=16/12)

load("E. Code for simulation (annotated)/MH Sampler/results/Not converged 26-01-2015/resultsH3a.Rdata")

plot.ts(sapply(1:20000, function(i){mean(res[i,2,])}),
        ylab="Estimated Value", xaxt='n', xlab="")

load("E. Code for simulation (annotated)/MH Sampler/results/20000 its/resultsF3b.Rdata")

plot.ts(sapply(1:20000, function(i){mean(res[i,2,])}),
        ylab="Estimated Value", xlab="")

load("E. Code for simulation (annotated)/MH Sampler/results/20000 its/resultsG3c.Rdata")

plot.ts(sapply(1:20000, function(i){mean(res[i,2,])}),
        ylab="Estimated Value", xaxt='n', xlab="Iterations")

par(op) 
dev.off()


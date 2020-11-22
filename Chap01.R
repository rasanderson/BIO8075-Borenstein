# Chap01. How a meta-analysis works
library(metafor)
dat <- dat.cannon2006[,1:6] # keep only variables we really need
# ep1t = number of events in the high dose group for end points 1: coronary
#        death or non-fatal mycardial infarction
# ep1c = number of events in the standard dose group for end point 1: coronary
#        death or non-fatal mycardial infarction
# nt   = number of patients in the high dose group
# nc   = number of patients in the standard dose group
dat <- escalc(measure="RR", ai=ep1t, n1i=nt, ci=ep1c, n2i=nc, data=dat, slab=trial)
dat # yi observed effect sizes; vi corresponding variances

# meta analysis fixed-effect
res <- rma(yi, vi, data=dat, method="DL")
res

predict(res, transf=exp, digits=2) # summary effect size (risk ratio)

dat$weights <- paste0(round(weights(res)), "%")   # weights in % (rounded)
dat$pvals   <- round(summary(dat)$pval, digits=3) # p-values of the individual trials

### Figure 1.1

par(mar=c(4,4,2,2))

forest(res, xlim=c(-1,2), atransf=exp, at=log(c(2/3, 1, 3/2)),
       header=TRUE, top=2, mlab="Summary", efac=c(0,1,3),
       ilab=data.frame(dat$weights, dat$pvals), ilab.xpos=c(0.8,1.2), ilab.pos=2)
text(0.8, -1, "100%", pos=2)
text(1.2, -1, formatC(res$pval, format="f", digits=5), pos=2)
text(0.8,  6, "Weight",  pos=2, font=2)
text(1.2,  6, "P-Value", pos=2, font=2)

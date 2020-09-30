library(rmeta)
library(survival)

glmer_ci_dk <- function (model1,tail)
{
lower <- exp(coef(summary(model1))[,1] + qnorm(.025)*coef(summary(model1))[,2])
upper <- exp(coef(summary(model1))[,1] + qnorm(.975)*coef(summary(model1))[,2])
cbind(exp(coef(summary(model1))[,1]),lower, upper, coef(summary(model1))[,c(4)])
}

glmer_ci_dk_fp <- function(model,bsize=0.5,zro=1,thiscolor='black',lo=0.1,hi=3,tcks=c(0.1,0.5,1,2,3)) {
	forestplot(matrix(c(rownames(glmer_ci_dk(model))[2:nrow(glmer_ci_dk(model))])),glmer_ci_dk(model)[2:nrow(glmer_ci_dk(model)),1],glmer_ci_dk(model)[2:nrow(glmer_ci_dk(model)),2],glmer_ci_dk(model)[2:nrow(glmer_ci_dk(model)),3],
	           boxsize=bsize,zero=zro,col=meta.colors(box=thiscolor,lines= thiscolor,zero= thiscolor,text= thiscolor),clip=c(lo,hi),xticks=tcks,xlab='Odds Ratio')
}

cox_dk_fp <- function(model,bsize=0.5,zro=1,thiscolor='black',lo=0.1,hi=3,tcks=c(0.1,0.5,1,2,3),lbl=NA,ttle) {
  thiscox <- summary(model)$conf.int
  if (length(lbl)>1) {
    labels <- matrix(lbl,ncol=1) 
  } else if (length(lbl)==1) {
    labels <- matrix(c(rownames(thiscox)))
  }
  forestplot(labels,mean=(thiscox[,1]),lower=thiscox[,3],upper=thiscox[,4],
             boxsize=bsize,zero=zro,col=meta.colors(box=thiscolor,lines= thiscolor,zero= thiscolor,text= thiscolor),clip=c(lo,hi),xticks=tcks,xlab='Hazard Ratio')
  title(ttle)
}

glmer_ci_dk_fp_log <- function(model,bsize=0.5,zro=log2(1),thiscolor='black',lo=log2(0.5),hi=log2(8),tcks=log2(c(0.5,1,2,4,8))) {
  forestplot(matrix(c(rownames(glmer_ci_dk(model))[2:nrow(glmer_ci_dk(model))])),log2(glmer_ci_dk(model)[2:nrow(glmer_ci_dk(model)),1]),log2(glmer_ci_dk(model)[2:nrow(glmer_ci_dk(model)),2]),log2(glmer_ci_dk(model)[2:nrow(glmer_ci_dk(model)),3]),
             boxsize=bsize,zero=zro,col=meta.colors(box=thiscolor,lines= thiscolor,zero= thiscolor,text= thiscolor),clip=c(lo,hi),xticks=tcks,xlab='Odds Ratio')
}

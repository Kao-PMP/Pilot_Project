library(rmeta)
library(survival)

glmer_ci_dk <- function (model1,tail,ordig=2,pdig=3)
{
  or <- round(exp(coef(summary(model1))[,1]),ordig)
  lower <- round(exp(coef(summary(model1))[,1] + qnorm(.025)*coef(summary(model1))[,2]),ordig)
  upper <- round(exp(coef(summary(model1))[,1] + qnorm(.975)*coef(summary(model1))[,2]),ordig)
  p <- round(coef(summary(model1))[,c(4)],pdig)
  cbind(or,lower, upper, p)
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


clean_cox <- function(x,hrdig=2,pdig=3) {
    ### Uses a coxph object
    hrs <- round(summary(x)$conf.int[,c(1,3,4)],hrdig)
    ps <-  round(summary(x)$coefficients[,5],pdig)
    if (is.vector(hrs)) {
      tbl <- c(hrs,ps)
      cat(names(x$coefficients),"\tHR: ",tbl[[1]], " [",tbl[[2]],"-",tbl[[3]],"]"," p.value=",tbl[[4]],sep="")
      }
    if (is.matrix(hrs)) {
      tbl <- cbind(hrs,ps)
      colnames(tbl) <- c("HR","lowCI","hiCI","p.value")
      print(tbl)
    }
    cat('\n\n','Log likelihood test:','\n')
    print(summary(x)$logtest)
    cat('ZPH test (Check proportional hazards assumption):','\n')
    print(cox.zph(x))
    cat('---------------------------')
}

clean_cox_hronly <- function(x,hrdig=2,pdig=3) {
    ### Uses a coxph object
    hrs <- round(summary(x)$conf.int[,c(1,3,4)],hrdig)
    ps <-  round(summary(x)$coefficients[,5],pdig)
    if (is.vector(hrs)) {
        tbl <- c(hrs,ps)
        cat(names(x$coefficients),"\tHR: ",tbl[[1]], " [",tbl[[2]],"-",tbl[[3]],"]"," p.value=",tbl[[4]],sep="")
    }
    if (is.matrix(hrs)) {
        tbl <- cbind(hrs,ps)
        colnames(tbl) <- c("HR","lowCI","hiCI","p.value")
        print(tbl)
    }
    cat('\n---------------------------')
}


clean_logit <- function(x,ordig=2,pdig=3) {
  ## Assumes a glm binomial object
  thisresult <- paste(names(x$coefficients),": OR ",round(exp(x$coefficients),ordig)," 95% CI [",
        round(exp(confint(x)[,1]),ordig),"-",round(exp(confint(x)[,2]),ordig),"], p=",round(summary(x)$coefficients[,4],pdig),sep="")
  print(thisresult,quote=F)
}

clean_speedglm <- function(x,ordig=2,pdig=3){
  ## Assumes x is a speedglm binomial object
  thisglm_out <- summary(x)$coefficients
  thisglm_out$OR <- round(exp(thisglm_out$Estimate),ordig)
  thisglm_out <- cbind(thisglm_out,round(exp(confint(x)),ordig))
  thisglm_out$pvalue <- round(as.numeric(as.character(summary(x)$coefficients[,4])),pdig)
  print(thisglm_out[,c("OR","2.5 %","97.5 %","pvalue")])
}

speedglm_ci_dk_fp <- function(x,bsize=0.5,zro=1,thiscolor='black',lo=0.1,hi=3,tcks=c(0.1,0.5,1,2,3)) {
  ## Assumes x is a speedglm object
  model <- clean_speedglm(x)
  forestplot(matrix(c(rownames(model)[2:nrow(model)])),model[2:nrow(model),1],model[2:nrow(model),2],model[2:nrow(model),3],
             boxsize=bsize,zero=zro,col=meta.colors(box=thiscolor,lines= thiscolor,zero= thiscolor,text= thiscolor),clip=c(lo,hi),xticks=tcks,xlab='Odds Ratio')
}

speedglm_ci_dk_fp_log <- function(x,bsize=0.5,zro=log2(1),thiscolor='black',low=0.5,high=8,tks=c(0.5,1,2,4,8)) {
  model <- clean_speedglm(x)
  tcks <- log2(tks)
  lo <- log2(low)
  hi <- log2(high)
  forestplot(matrix(c(rownames(model)[2:nrow(model)])),log2(model[2:nrow(model),1]),log2(model[2:nrow(model),2]),log2(model[2:nrow(model),3]),
             boxsize=bsize,zero=zro,col=meta.colors(box=thiscolor,lines= thiscolor,zero= thiscolor,text= thiscolor),clip=c(lo,hi),xticks=tcks,xlab='Odds Ratio')
}


## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

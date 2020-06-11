library(reshape2)
library(poLCA)
library(plotrix)

poLCA_iterate <- function (f,dat,maxclass=10,reps=10) {
  lca_error <- matrix(nrow=maxclass-1,ncol=6,dimnames=list(c(2:maxclass),c('AIC','BIC','X2','G2','cAIC','aBIC')))
  for (i in 2:maxclass) {
    this_iter <- poLCA(f,dat,nclass=i,nrep=reps,na.rm=FALSE,verbose=FALSE)
    write(this_iter$aic)
    lca_error[as.character(i),'AIC'] <- this_iter$aic
    lca_error[as.character(i),'BIC'] <- this_iter$bic
    lca_error[as.character(i),'X2'] <- this_iter$Chisq
    lca_error[as.character(i),'G2'] <- this_iter$Gsq
    lca_error[as.character(i),'cAIC'] <- -2*this_iter$llik+this_iter$npar*log(this_iter$N)+1
    lca_error[as.character(i),'aBIC'] <- -2*this_iter$llik+this_iter$npar*log((this_iter$N+2)/24)
    cat("Completed",i,"class analysis.\n")
  }
  print(lca_error)
  par(mfrow=c(3,2))
  plot(x=2:maxclass,y=lca_error[as.character(2:maxclass),'AIC'],col='blue',xlab='Latent classes',
       type='b',pch=1,ylab='AIC',main='AIC')
  plot(x=2:maxclass,y=lca_error[as.character(2:maxclass),'BIC'],col='blue',xlab='Latent classes',
       type='b',pch=1,ylab='BIC',main='BIC')
  plot(x=2:maxclass,y=lca_error[as.character(2:maxclass),'X2'],col='blue',xlab='Latent classes',
             type='b',pch=1,ylab='X2',main='X2')
  plot(x=2:maxclass,y=lca_error[as.character(2:maxclass),'G2'],col='blue',xlab='Latent classes',
       type='b',pch=1,ylab='G2',main='G2')
  plot(x=2:maxclass,y=lca_error[as.character(2:maxclass),'cAIC'],col='blue',xlab='Latent classes',
       type='b',pch=1,ylab='cAIC',main='cAIC')
  plot(x=2:maxclass,y=lca_error[as.character(2:maxclass),'aBIC'],col='blue',xlab='Latent classes',
       type='b',pch=1,ylab='aBIC',main='aBIC')
  return(lca_error)
}

## Function to export a table that will support LC application in SQL using 'lca_application.sql'

write_poLCA_coeff_tab <- function(df,pl,mname) {
  library(reshape2)
  problist <- names(df$probs)
  full_list <- data.frame(latent_class = NA, brief_name = NA, variable=NA,value=NA,stringsAsFactors=F)
  thispop <- as.data.frame(cbind(df$P))
  colnames(thispop)[1] <- 'share'
  for (i in 1:length(problist)) {
    thisslot <- df['probs'][[1]]
    thismatrix <- thisslot[[i]]
    for (j in 1:ncol(thismatrix)) {
      colnames(thismatrix)[j] <- as.character(j)
    }
    thisdf <- as.data.frame(thismatrix)
    thisdf$brief_name=problist[i]
    for (k in 1:nrow(thisdf)) {
      thisdf[k,'latent_class'] <- paste("Class",LETTERS[k])
      thispop[k,'latent_class'] <- paste("Class",LETTERS[k])
    }
    full_list <- rbind(full_list,melt(thisdf,id.vars=c('latent_class','brief_name')))
  }
  full_list <- full_list[2:nrow(full_list),]
  names(full_list)[3] <- 'var.value'
  names(full_list)[4] <- 'coeff'
  full_list$model_name <- mname
  thispop$model_name <- mname
  full_list <- merge(full_list,pl[,c('brief_name','phenotype')],by='brief_name',all.x=T)
  write.csv(full_list,paste(mname,'_lca_coeff.csv',sep=''),row.names = F)
  write.csv(thispop,paste(mname,'_popshare.csv',sep=''),row.names = F)
}

## Function to return modal class probability using poLCA model (df1) and data used to make model (dat)

poLCA_find_modal_prob <- function (df1,dat) {
  df <- cbind(df1$posterior,cbind(df1$predclass))
  endcol <- ncol(df)
  modal_prob <-vector()
  colnames(df)[endcol] <- 'modal_class'
  for (h in 1:(endcol-1)){
    colnames(df)[h] <- paste("Class",h,sep="_")
  }
  for (i in 1:nrow(df)) {
    thismodalclass <- df[i,endcol]
    modal_prob <- c(modal_prob,df[i,thismodalclass])
  }
  df <- cbind(df,cbind(modal_prob))
  x <- cbind(dat,df)
  return(x)
}

plot_LCA_outcomes <- function(time,cens,cens_val=1,lca_class,mname,outc_name,xlo=0,xhi=60,ylo=0,yhi=1) {
  if (dir.exists(mname)==F) {dir.create(mname)}
  pdf(paste(mname,"/LCA Model ",modelname," ",outc_name," according to class",".pdf",sep=""))
  plot(survfit(Surv(as.numeric(time)/30,cens==cens_val)~as.factor(lca_class),conf.type="none"),col=c(1:14),xlim=c(xlo,xhi),ylim=c(ylo,yhi))
  title(main=paste('Primary outcome according to LCA model:',modelname),xlab='Time since enrollment (months)',ylab='Proportion event free')
  dev.off()
}


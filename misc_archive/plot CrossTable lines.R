plot.CrossTable <- function (frm,dwn,across,ptype="both",plotrow=1,colrs=c("red"),icn=15,ymn=0,ymx=1,xlb="X-axis",ylb="Y-axis",mn = "Graph title") {
	plot(CrossTable(frm[,dwn],frm[,across],prop.t=FALSE,prop.chisq=FALSE,prop.c=FALSE)$prop.row[plotrow,],col=colrs,ylim=c(ymn,ymx),pch=icn,type=ptype,xlab=xlb,ylab=ylb,main=mn)
	}
	
points.CrossTable <- function (frm,dwn,across,ptype="both",plotcol=2,colrs=c("blue"),icn=16) {
	points(CrossTable(frm[,dwn],frm[,across],prop.t=FALSE,prop.chisq=FALSE,prop.c=FALSE)$prop.row[,plotcol],col=colrs,pch=icn,type=ptype)
	}
	

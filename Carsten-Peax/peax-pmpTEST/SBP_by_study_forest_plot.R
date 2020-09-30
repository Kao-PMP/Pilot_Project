pdf("~/Forest plot mortality vs. 10 mm Hg SBP by study.pdf")
forestplot(matrix(c("All studies",studies)),mean=study_dth_hrs[,"HR"],lower=study_dth_hrs[,"Lower CI"],upper=study_dth_hrs[,"Upper CI"],
           zero=1,xticks=c(0.75,0.8,0.9,1,1.1),boxsize = 0.5,col=meta.colors(all.elements="black"),xlab="Hazard ratio/10 mm Hg SBP")
dev.off()
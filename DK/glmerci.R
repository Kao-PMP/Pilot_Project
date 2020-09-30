glmer_ci_dk <- function (model1,tail)
{
lower <- exp(coef(summary(model1))[,1] + qnorm(.025)*coef(summary(model1))[,2])
upper <- exp(coef(summary(model1))[,1] + qnorm(.975)*coef(summary(model1))[,2])
a <- cbind(exp(coef(summary(model1))[,1]),lower, upper, coef(summary(model1))[,c(4)])
colnames(a) <- c('OR','Low_CI','Upper_CI','pval')
round(a,3)[2:nrow(a),]
}
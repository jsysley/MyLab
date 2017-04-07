#dir_path <- "F:/git/trial/R/Pre/WDBC"
dir_path <- "/Users/jsysley/Documents/git/MyLab/Pre/WDBC"
wdbc <- read.table(paste(dir_path,'/wdbc.data.txt',sep=""),sep = ",",
                   stringsAsFactors = FALSE,fileEncoding = 'GBK')
#查看数据大概结构
dim(wdbc)
str(wdbc)
#画出各数据的密度曲线
if(!require(ggplot2))install.packages("ggplot2")
require(ggplot2)

Plot_A_Density <- function(name,use_data,mode='normal')
{
    use <- use_data[,name]
    if (mode=='normal')
    {
        plot(density(use),xlab=name,main="density line")
    }else{
        ggplot(use_data, aes(eval(parse(text=name))))+geom_density(fill='lightblue')+xlab(name)
    }
}
#查看所有数据密度曲线
lapply(colnames(wdbc),Plot_A_Density,use_data=wdbc,mode='normal1')
#查看变量间先关系数
if(!require(GGally))install.packages("GGally")
library(GGally)
ggpairs(economics[,c(2,4:6)])
#对所有数据取对数
wdbc_log <- wdbc
wdbc_log[wdbc_log==0] <- 0.001#后续去对数方便
wdbc_log[,3:32] <- wdbc_log[,3:32]
#LDA
library(MASS)
out <- lda(V2~.,wdbc_log[,-1])
names(out)#查看对象的内容，还可以用attributes(out)
attributes(out)
plot(out)#返回的结果对象可以先plot
layout(matrix(c(1,2,3,3),nrow = 2,byrow = FALSE))
out_predict <- predict(out)
# renames(out_predict)
LDA1 <- out_predict$x#判别得分
hist(LDA1[wdbc_log$V2=='B'],xlim=c(-4,6),main="",xlab='GroupB',col='blue')
hist(LDA1[wdbc_log$V2=='M'],xlim=c(-4,6),main="",xlab='GroupM',col='blue')
#画密度曲线
plot(density(LDA1[wdbc$V2=='B']),xlim = c(-4,6))
lines(density(LDA1[wdbc$V2=='M']))#加线，也可以用lines
#或者ggplot
ggplot(data=NULL)+geom_density(aes(LDA1[wdbc_log$V2=='B']),fill='lightblue')+
    geom_density(aes(LDA1[wdbc_log$V2=="M"]),fill='steelblue')+
    scale_colour_manual("Group",values = c("GroupB" = "lightblue","GroupM" = "steelblue"))
#混淆矩阵
A <- table(wdbc_log$V2,out_predict$class)
(sum(A)-sum(diag(A)))/sum(A)#误判率

####Example Continued
#Estimate the prior pi1,pi2
index1 <- which(wdbc_log[,"V2"]=="B")
index2 <- which(wdbc_log[,"V2"]=="M")
pi1 <- length(index1)/nrow(wdbc_log)
pi2 <- length(index2)/nrow(wdbc_log)
#Estimate the mu1,mu2
mu1 <- matrix(colMeans(wdbc_log[index1,3:32]),ncol=1)
mu2 <- matrix(colMeans(wdbc_log[index2,3:32]),ncol=1)

ss1 <- cov(wdbc_log[index1,3:32])*nrow(wdbc_log)
ss2 <- cov(wdbc_log[index2,3:32])*nrow(wdbc_log)
ssx <- ss1+ss2
#Estimate the coefficients
b <- ssx%*%(mu1-mu2)
b0 <- -1/2*(t(mu1)%*%solve(ss1)%*%mu1 - t(mu2)%*%solve(ss2)%*%mu2) + log(pi1) + log(pi2)

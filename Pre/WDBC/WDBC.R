#dir_path <- "F:/git/trial/R/Pre/WDBC"
dir_path <- "/Users/jsysley/Documents/git/MyLab/Pre/WDBC"
wdbc <- read.table(paste(dir_path,'/wdbc.data.txt',sep=""),sep = ",",
                   stringsAsFactors = FALSE,fileEncoding = 'GBK')
colnames(wdbc)[3:32] <- c("radius.mv","texture.mv","peri.mv","area.mv",
                          "smooth.mv","comp.mv","scav.mv","ncav.mv",
                          "symt.mv","fracd.mv","radius.sd","texture.sd",
                          "peri.sd","area.sd","smooth.sd","ncav.sd",
                          "comp.sd","scav.sd","symt.sd","fracd.sd",
                          "radius.ev","texture.ev","peri.ev","area.ev",
                          "smooth.ev","comp.ev","scav.ev","ncav.ev",
                          "symt.ev","fracd.ev.")
                           
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

ss1 <- cov(wdbc_log[index1,3:32])*length(index1)
ss2 <- cov(wdbc_log[index2,3:32])*length(index2)
sigma_x <- (ss1+ss2)/nrow(wdbc_log)
#Estimate the coefficients
b <- solve(sigma_x)%*%(mu1-mu2)
b0 <- -1/2*(t(mu1)%*%solve(ss1)%*%mu1 - t(mu2)%*%solve(ss2)%*%mu2) + log(pi1) - log(pi2)

#########LDA via Multiple Regression
y = vector(length = nrow(wdbc_log))
y[wdbc_log[,"V2"]=="M"] = 1
lm_fun <- function(data,y,local)
{
    temp <- summary(lm(y~data[,local]))
    res <- vector(length=3)
    res[1] <- temp$coefficients[2,1]#coef
    res[2] <- temp$coefficients[2,2]#sd
    res[3] <- res[1]/res[2]#z-ratio
    names(res) = c("Coeff","S.E.","Ratio")
    res = list(res)
    names(res) = colnames(data)[local]
    return(res)
}
res <- sapply(3:32,lm_fun,data=wdbc_log,y=y)
graph <- Reduce(rbind,res)
graph <- data.frame(graph,row.names = colnames(wdbc_log)[3:32])
graph$name <- rownames(graph)
res
###
order_index <- order(graph$Ratio)
graph$new_name <- factor(graph$name,levels = graph$name[order_index])
ggplot(graph,aes(x=new_name,y=Ratio))+
    geom_bar(stat="identity") + coord_flip()

###
new_data <- cbind(y,wdbc[,-c(1,2)])
fm <- glm(y~.,family = binomial,data=new_data)
fm_step <- step(fm,direction="backward")
temp_step <- summary(fm_step)
res_step = temp_step$coefficients[,c(1,2)]
res_step <- cbind(res_step,res_step[,1]/res_step[,2])
res_step <- data.frame(res_step)
colnames(res_step) <- c("Coeff","S.E.","Ratio")
res_step

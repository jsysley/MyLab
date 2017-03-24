dir_path <- "/Users/jsysley/Desktop/多元统计/Pre/The_Nutritional_of_food"
raw_data <- read.table(paste(dir_path,"/food.data.txt",sep=""),header = TRUE,stringsAsFactors = FALSE)
#######################################################
#######################################################
#仅用前七列数据
use_data <- raw_data[,1:7]
#查看数据大概结构
dim(use_data)
str(use_data)
#画出各数据的密度曲线
if(!require(ggplot2))install.packages("ggplot2")
if(!require(reshape))install.packages("reshape")
require(ggplot2)
require(reshape)
#数据格式转换
Plot_Density <- function(use_data)
{
    id <- 1:nrow(use_data)
    long_data <- cbind(use_data,id)
    long_data <- melt(long_data,id="id")
    return(long_data)
}

Plot_A_Density <- function(name,use_data)
{
    use <- use_data[,name]
    plot(density(use),xlab=name,main="density line")
}
#查看所有数据密度曲线
long_data <- Plot_Density(use_data)
#所有密度曲线
ggplot(long_data,aes(x=value,colour=variable))+geom_density()+
    xlim(0,xend)+ylim(0,yend)
#单独密度曲线
lapply(colnames(use_data),Plot_A_Density,use_data=use_data)
#箱线图
ggplot(long_data,aes(x=variable,y=value,colour=variable))+geom_boxplot(varwidth = TRUE)
#删除一个变量（量纲问题）
ggplot(long_data[long_data[,2]!='Food.energy.calories',],aes(x=variable,y=value,colour=variable))+geom_boxplot(varwidth = TRUE)
ggplot(long_data[!long_data[,2]%in%c('Food.energy.calories','weight.grams'),],aes(x=variable,y=value,colour=variable))+geom_boxplot(varwidth = TRUE)
#查看各个变量分布值情况
if(!require(Hmisc))install.packages("Hmisc")
require(Hmisc)
describe(use_data)
#查看缺失值情况
if(!require(mice))install.packages("mice")
require(mice)
md.pattern(use_data)
#######################################################
#######################################################
#除以wight.grams
use_data_div <- apply(use_data,2,function(x,y)return(x/use_data[,"weight.grams"]))
#移除wight.grams变量
use_data_div <- use_data_div[,-which("weight.grams"==colnames(use_data_div))]
#标准化
use_data_div_ss <- scale(use_data_div)
#主成分分析
result <- princomp(use_data_div_ss)
summary(result)
result$loadings
result$scores#得分矩阵
plot(result,type="l")

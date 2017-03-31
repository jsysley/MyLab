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
#查看所有数据密度曲线
lapply(colnames(wdbc),Plot_A_Density,use_data=wdbc,mode='normal1')
#查看变量间先关系数
if(!require(GGally))install.packages("GGally")
library(GGally)
ggpairs(economics[,c(2,4:6)])
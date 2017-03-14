#######################
###读入数据
data <- data <- read.csv("/Users/jsysley/Desktop/ana/data.csv",header=TRUE,
                         stringsAsFactors = FALSE,fileEncoding = "GBK")
###去掉第一列序号
data <- data[,-1]
###转换列名
names <- colnames(data)
special <- strsplit(names[4],split="[.]")[[1]][2:3]
name_4 <- paste(special,collapse = "")
names_change <- unlist(lapply(strsplit(names,split = "[.]"),"[",2))
colnames(data) <- names_change
colnames(data)[4] <- name_4
####
###去除转义符
names_str <- c("结算日期","居间","建平仓","成交时间","商品","操作人")

CutOut <- function(x)return(unlist(strsplit(x,split = "\t")))
temp <- as.data.frame(apply(data[,names_str],2,CutOut),stringsAsFactors = FALSE)
data[,names_str] <- temp
###交易账号转换为字符串
data[,"交易账号"] <- as.character(data[,"交易账号"])
####查看时间跨度
unique(data[,"结算日期"])
length(unique(data[,"结算日期"]))
###按照时间顺序重新排列样本
data <- data[order(data[,"结算日期"]),]
###对平仓价，实际盈亏处理
index <- which(data[,c("平仓价","实际盈亏")]==" -",arr.ind = TRUE)
data[,c("平仓价","实际盈亏")][index]="0"
data[,c("平仓价","实际盈亏")] <- as.data.frame(apply(data[,c("平仓价","实际盈亏")],2,as.numeric))
####几个重要指标
team <- unique(data[,"居间"])
creat_cover <- unique(data[,"建平仓"])
buy_sell <- unique(data[,"买卖方向"])
deal_type <- unique(data[,"成交类型"])
handle_type <- unique(data[,"操作类型"])
commodity <- unique(data[,"商品"])
###对商品的分析
time <- unique(data[,1])###以时间分类求均值

###以日期分解，求target在每个月的均值
All_Mean <- function(data,commodity,target)
{
    all_mean <- list()
    for(i in 1:length(commodity))
    {
        index <- which(data[,"商品"]==commodity[i])
        data_temp <- data[index,]
        time_mean<- tapply(data_temp[,target], data_temp[,"结算日期"], mean)
        # a <- barplot(time_mean,ylab = commodity[i],xlab = "Time",xaxt="n")
        # axis(side=1,at=a,tick = 0.1,labels=names(time_mean),las=2,cex.axis = 0.5) 
        time_mean <- list(time_mean)
        names(time_mean) <- commodity[i]
        all_mean <- c(all_mean,time_mean)
    }
    return(all_mean)
}
###
#对all_mean的时间补齐，没有信息补0
All_time <- function(x,time)
{
    a <- rep(0,length(time))
    index <- match(names(x),time)
    a[index] <- x
    names(a) <- time
    return(a)
}

###得到各个商品在每个月的平均成交金额
money_mean <- All_Mean(data,commodity,"成交金额")
money_mean <- lapply(money_mean, All_time,time=time)

###得到得到各个商品在每个月的平均建仓价
creat_mean <- All_Mean(data,commodity,"建仓价")
creat_mean <- lapply(creat_mean, All_time,time=time)

###得到得到各个商品在每个月的平均持仓价
hold_mean <- All_Mean(data,commodity,"持仓价")
hold_mean <- lapply(hold_mean, All_time,time=time)

###得到得到各个商品在每个月的平均平仓价
cover_mean <- All_Mean(data,commodity,"持仓价")
cover_mean <- lapply(cover_mean, All_time,time=time)

###得到各个商品在每个月的平均盈利
profit_mean <- All_Mean(data,commodity,"平仓盈亏")
profit_mean <- lapply(profit_mean, All_time,time=time)

###成交类型

   
    
    
    
    
    
    

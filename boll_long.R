#布林道计算，abglen为周期参数
AverageFC <- function(close,index,avglen=20)
{
    #index <- which(date==colnames(close))
    if(index < avglen) return(close[,index])#当没有足够的周期数的数据
    start <- index-avglen+1
    end <- index
    res <- mean(as.numeric(close[,start:end]))
    return(res)
}

#布林道计算，sdlen为周期参数
StandardDev <- function(close,index,sdlen=12)
{
    #index <- which(date==colnames(close))
    if(index < sdlen) return(close[,index])#当没有足够的周期数的数据
    start <- index-sdlen+1
    end <- index
    res <- sd(as.numeric(close[,start:end]))
    return(res)
}

#布林道所有指标的计算,sdev为boll通道倍数参数
Boll <- function(close,index,avglen=20,sdlen=12,sdev=2)
{
    avg_val <- AverageFC(close=close,index=index,avglen=avglen)#中轨
    sd_mult <- StandardDev(close=close,index=index,sdlen=sdlen)*sdev#通道距离
    disp_top <- avg_val + sd_mult#通道高点
    disp_bottom <- avg_val - sd_mult#通道低点
    res <- c(avg_val,sd_mult,disp_top,disp_bottom)
    names(res) <- c("avg_val","sd_mult","disp_top","disp_bottom")
    return(res)
}

#返回带宽
StdMidRatio <- function(close,index,avglen=20,sdlen=12,sdev=2)
{
    res <- Boll(close=close,index=index,avglen=avglen,sdlen=sdlen,sdev=sdev)
    avg_val <- res[which("avg_val"==names(res))]
    sd_mult <- res[which("sd_mult"==names(res))]
    disp_top <- res[which("disp_top"==names(res))]
    disp_bottom <- res[which("disp_bottom"==names(res))]
    std_mid_ratio <- (disp_top-disp_bottom)/avg_val#计算带宽
    return(std_mid_ratio)
}

##计算TrendIndex,n为trend_index的周期函数
TrendIndex <- function(close,index,avglen=20,sdlen=12,sdev=2,n=20)
{
    #index <- which(date==colnames(close))
    std_mid_ratio <- StdMidRatio(close=close,index=index,avglen=avglen,sdlen=sdlen,sdev=sdev)
    start <- index-n+1
    end <- index
    #计算今天开始前n天的std
    use_date <- colnames(close)[start:end]
    use_index <- start:end
    res <- lapply(use_index,StdMidRatio,close=close,avglen=avglen,sdlen=sdlen,sdev=sdev)
    names(res) <- use_date
    avg_std_n <- mean(unlist(res))
    
    use_date <- colnames(close)[(start-1):(end-1)]
    use_index <- (start-1):(end-1)
    res <- lapply(use_index,StdMidRatio,close=close,avglen=avglen,sdlen=sdlen,sdev=sdev)
    names(res) <- use_date
    avg_std_n_1 <- mean(unlist(res))
    
    trend_index <- avg_std_n-avg_std_n_1
    return(trend_index)
}

#计算TrendIndex的上下轨
Trend_UD <- function(close,index,avglen=20,sdlen=12,sdev=2,n=20)
{
    #找到今天的日期
    #index <- which(date==colnames(close))
    std_mid_ratio <- StdMidRatio(close=close,index=index,avglen=avglen,sdlen=sdlen,sdev=sdev)
    start <- index-n+1
    end <- index
    #计算前n天的trend_index
    use_date <- colnames(close)[(start-1):(end-1)]
    use_index <- (start-1):(end-1)
    trend_all <- lapply(use_index,TrendIndex,close=close,avglen=avglen,sdlen=sdlen,sdev=sdev,n=n)
    names(trend_all) <- use_date
    trend_all <- unlist(trend_all)
    upper <- max(trend_all)
    lower <- min(trend_all)
    res <- c(upper,lower)
    names(res) <- c("upper","lower")
    return(res)
}

#计算ATR
ATR <- function(close,high_today,low_today,index,avglen=20,sdlen=12,sdev=2,n=20)
{
    #index <- which(date==colnames(close))
    std_mid_ratio <- StdMidRatio(close=close,index=index,avglen=avglen,sdlen=sdlen,sdev=sdev)
    start <- index-n+1
    end <- index
    a <- max(high_today,close[,(end-1)])-min(low_today,close[,(end-1)])#当前ATR
    return(a)
}

#计算ATR的平均波动
ATR_N <- function(close,high_today,low_today,index,avglen=20,sdlen=12,sdev=2,n=20)
{
    #index <- which(date==colnames(close))
    std_mid_ratio <- StdMidRatio(close=close,index=index,avglen=avglen,sdlen=sdlen,sdev=sdev)
    start <- index-n+1
    end <- index
    
    use_date <- colnames(close)[start:end]
    use_index <- start :end
    res <- lapply(use_index,ATR,close=close,high_today=high_today,low_today=low_today,
                  avglen=avglen,sdlen=sdlen,sdev=sdev)
    names(res) <- use_date
    ATR_N <- mean(unlist(res))
    return(ATR_N)
}

#每天指标总结
Indicator <- function(close,high,low,date,avglen=20,sdlen=12,sdev=2,n=20)
{
    #平移布林道通道
    index <- which(date==colnames(close))
    close_today <- close[,index]
    high_today <- high[,index]
    low_today <- low[,index]
    res <- Boll(close=close,index=index,avglen=avglen,sdlen=sdlen,sdev=sdev)
    avg_val <- res[which("avg_val"==names(res))]#
    sd_mult <- res[which("sd_mult"==names(res))]
    disp_top <- res[which("disp_top"==names(res))]
    disp_bottom <- res[which("disp_bottom"==names(res))]
    #计算trend_index
    trend_index <- TrendIndex(close=close,index=index,avglen=avglen,
                             sdlen=sdlen,sdev=sdev,n=n)
    res <- Trend_UD(close=close,index=index,avglen=avglen,
                    sdlen=sdlen,sdev=sdev,n=n)
    upper <- res[which("upper"==names(res))]#
    lower <- res[which("lower"==names(res))]#
    #计算ATR
    atr <- ATR(close=close,high_today=high_today,low_today=low_today,
               index=index,avglen=avglen,sdlen=sdlen,sdev=sdev,n=n)
    atr_n <- ATR_N(close=close,high_today=high_today,low_today=low_today,
                   index=index,avglen=avglen,sdlen=sdlen,sdev=sdev,n=n)
    result <- c(avg_val,sd_mult,disp_top,disp_bottom,trend_index,
                upper,lower,atr,atr_n,close_today,high_today,low_today)
    names(result) <- c("avg_val","sd_mult","disp_top","disp_bottom",
                       "trend_index","upper","lower","atr","atr_n",
                       "close_today","high_today","low_today")
    return(result)
}



#开仓函数
Open_Position_Long <- function(trend_index,upper,high_today,disp_top)
{
    if((trend_index > upper)&&(trend_index > 0)&&(high_today > disp_top))
        return("Open")
    else return("Open_Not")
}

#ATR离场
Long_Cover_ATR <- function(close,close_today,date,buy_time,atr_n,trend_index,lower)
{
    cover <- NULL#记录要平仓
    for(i in buy_time)
    {
        open_price <- close[, which(colnames(close)==i)]#拿到开仓价
        if(((open_price - close_today) > atr_n)&&(trend_index<lower))
            cover <- c(cover,i)
    }
    return(cover)
}

#趋势转变离场
Long_Cover_Trend <- function(close,close_today,low_today,date,buy_time,trend_index,lower,disp_bottom)
{
    cover <- NULL#记录要平仓
    for(i in buy_time)
    {
        if((trend_index < lower)&& (low_today < disp_bottom))
            cover <- c(cover,i)
    }
    return(cover)
}

#累计盈亏离场
Long_Cover_Profit <- function(close,close_today,buy_time,intimeprofit,highestprofit,ratio)
{
    cover <- NULL#记录要平仓
    for(i in 1:length(buy_time))
    {
        if((intimeprofit[i]<ratio*highestprofit[i])&&(highestprofit[i]>0))
            cover <- c(cover,buy_time[i])
    }
    return(cover)
}

#价格触及中轨离场
Long_Cover_Middle <- function(close,close_today,high_today,buy_time,avg_val)
{
    cover <- NULL#记录要平仓
    for(i in 1:length(buy_time))
    {
        if((close_today < avg_val)&&(high_today < avg_val))
            cover <- c(cover,buy_time[i])
    }
    return(cover)
}

#单轮止损
Long_Cover_Shot <- function(intimeprofit,buy_time,tolerance)
{
    cover <- NULL#记录要平仓
    for(i in 1:length(buy_time))
    {
        if(intimeprofit[i] <= -tolerance)
            cover <- c(cover,buy_time[i])
    }
    return(cover)
}

#加仓
Long_Open_Add <- function(trend_index,close,close_today,buy_time,atr_n,upper)
{
    for(i in buy_time)
    {
        open_price <- close[,which(i==colnames(close))]
        if((trend_index > upper) && (trend_index > 0) && (close_today> open_price+ atr_n) )
            return("Add")
    }
    return("Add_Not")
}

##主函数,date为今天日期
Main_Long <- function(close,high,low,tolerance,ratio,avglen=20,sdlen=12,sdev=2,n=20)
{
    #定义一些全局量
    highestprofit_long <- NULL#本次交易的最高累计利润
    intimeprofit_long <- NULL#实时利润
    #交易记录表初始化
    transaction_long <- NULL
    #购买时间的记录
    buy_time_long <- NULL
    #所有利润
    all_profit <- 0
    
    #############################对每一天开始运算
    for(day in 50:ncol(close))
    {
        # if(day==368)browser()
        date <- colnames(close)[day]
        print(paste("Day",day,date,"Holding_Long",length(buy_time_long),sep="-"))
        ###处理当天指标
        indicator <- Indicator(close=close,high=high,low=low,date=date,avglen=avglen,
                               sdlen=sdlen,sdev=sdev,n=n)
        avg_val <-indicator[which("avg_val"==names(indicator))]#
        sd_mult <-indicator[which("sd_mult"==names(indicator))]
        disp_top <-indicator[which("disp_top"==names(indicator))]
        disp_bottom <-indicator[which("disp_bottom"==names(indicator))]
        trend_index <-indicator[which("trend_index"==names(indicator))]
        upper <-indicator[which("upper"==names(indicator))]
        lower <-indicator[which("lower"==names(indicator))]
        atr <-indicator[which("atr"==names(indicator))]
        atr_n <-indicator[which("atr_n"==names(indicator))]
        #今天的价格
        close_today <- indicator[which("close_today"==names(indicator))]
        low_today <- indicator[which("low_today"==names(indicator))]
        high_today <- indicator[which("high_today"==names(indicator))]
        #计算当前的利润
        if(length(buy_time_long)!=0)
        {
            intimeprofit_long <- (close_today-close[,buy_time_long])
            names(intimeprofit_long) <- names(highestprofit_long)
            highestprofit_long <- apply(rbind(highestprofit_long,intimeprofit_long),2,max)
        }
        #############################处理离场
        #ATR离场
        if(length(buy_time_long) > 0)
        {
            long_cover_atr <- Long_Cover_ATR(close=close,close_today=close_today,
                    date=date,buy_time=buy_time_long,atr_n=atr_n,trend_index=trend_index,lower=lower)
            if(length(long_cover_atr)!=0)#有离场
            {
                print("Long_Cover:ATR Wave")
                open_time <- long_cover_atr
                open_price <- as.numeric(close[,match(long_cover_atr,colnames(close))])
                cover_price <- rep(close_today,length(open_time))
                cover_time <- rep(date,length(open_time))
                cover_reason <- rep("Long:ATR Wave",length(open_time))
                return <- cover_price-open_price
                re_rate <- return/open_price
                res <- cbind(open_time,open_price,cover_time,cover_price,
                             return,re_rate,cover_reason)
                transaction_long <- rbind(transaction_long,res)
                #处理持仓记录
                cut_index <- match(long_cover_atr,buy_time_long)
                buy_time_long <- buy_time_long[-cut_index]
                intimeprofit_long <- intimeprofit_long[-cut_index]
                highestprofit_long <- highestprofit_long[-cut_index]
                #####
                all_profit <- sum(all_profit,return)
                print(res)
                print(all_profit)
            }
        }
        
        #趋势转变离场
        if(length(buy_time_long) > 0)
        {
            long_cover_trend <- Long_Cover_Trend(close=close,close_today=close_today,
                                       low_today=low_today,date=date,buy_time=buy_time_long,
                                       trend_index=trend_index,lower=lower,disp_bottom=disp_bottom)
            if(length(long_cover_trend)!=0)#有离场
            {
                print("Long_Cover:Trend Changed")
                open_time <- long_cover_trend
                open_price <- as.numeric(close[,match(long_cover_trend,colnames(close))])
                cover_price <- rep(close_today,length(open_time))
                cover_time <- rep(date,length(open_time))
                cover_reason <- rep("Long:Trend Changed",length(open_time))
                return <- cover_price-open_price
                re_rate <- return/open_price
                res <- cbind(open_time,open_price,cover_time,cover_price,
                             return,re_rate,cover_reason)
                transaction_long <- rbind(transaction_long,res)
                #处理持仓记录
                cut_index <- match(long_cover_trend,buy_time_long)
                buy_time_long <- buy_time_long[-cut_index]
                intimeprofit_long <- intimeprofit_long[-cut_index]
                highestprofit_long <- highestprofit_long[-cut_index]
                #####
                all_profit <- sum(all_profit,return)
                print(res)
                print(all_profit)
            }
        }
        
        #累计盈亏离场
        if(length(buy_time_long) > 0)
        {
            long_cover_profit <- Long_Cover_Profit(close=close,close_today=close_today,
                                         buy_time=buy_time_long,intimeprofit=intimeprofit_long,
                                         highestprofit=highestprofit_long,ratio=ratio)
            if(length(long_cover_profit)!=0)#有离场
            {
                print("Long_Cover:Profit Down")
                open_time <- long_cover_profit
                open_price <- as.numeric(close[,match(long_cover_profit,colnames(close))])
                cover_price <- rep(close_today,length(open_time))
                cover_time <- rep(date,length(open_time))
                cover_reason <- rep("Long:Profit Down",length(open_time))
                return <- cover_price-open_price
                re_rate <- return/open_price
                res <- cbind(open_time,open_price,cover_time,cover_price,
                             return,re_rate,cover_reason)
                transaction_long <- rbind(transaction_long,res)
                #处理持仓记录
                cut_index <- match(long_cover_profit,buy_time_long)
                buy_time_long <- buy_time_long[-cut_index]
                intimeprofit_long <- intimeprofit_long[-cut_index]
                highestprofit_long <- highestprofit_long[-cut_index]
                #####
                all_profit <- sum(all_profit,return)
                print(res)
                print(all_profit)
            }
        }
        
        #价格触及中规离场
        if(length(buy_time_long) > 0)
        {
            long_cover_middle <- Long_Cover_Middle(close=close,close_today=close_today,
                high_today=high_today,buy_time=buy_time_long,avg_val=avg_val)
            if(length(long_cover_middle)!=0)#有离场
            {
                print("Long_Cover:Reach Middle")
                open_time <- long_cover_middle
                open_price <- as.numeric(close[,match(long_cover_middle,colnames(close))])
                cover_price <- rep(close_today,length(open_time))
                cover_time <- rep(date,length(open_time))
                cover_reason <- rep("Long:Reach Middle",length(open_time))
                return <- cover_price-open_price
                re_rate <- return/open_price
                res <- cbind(open_time,open_price,cover_time,cover_price,
                             return,re_rate,cover_reason)
                transaction_long <- rbind(transaction_long,res)
                #处理持仓记录
                cut_index <- match(long_cover_middle,buy_time_long)
                buy_time_long <- buy_time_long[-cut_index]
                intimeprofit_long <- intimeprofit_long[-cut_index]
                highestprofit_long <- highestprofit_long[-cut_index]
                #####
                all_profit <- sum(all_profit,return)
                print(res)
                print(all_profit)
            }
        }
        
        #单轮止损
        if(length(buy_time_long) > 0)
        {
            long_cover_shot <- Long_Cover_Shot(intimeprofit=intimeprofit_long,
                buy_time=buy_time_long,tolerance=tolerance)
            if(length(long_cover_shot)!=0)#有离场
            {
                print("Long_Cover:Reach Tolerance")
                open_time <- long_cover_shot
                open_price <- as.numeric(close[,match(long_cover_shot,colnames(close))])
                cover_price <- rep(close_today,length(open_time))
                cover_time <- rep(date,length(open_time))
                cover_reason <- rep("Long:Reach Tolerance",length(open_time))
                return <- cover_price-open_price
                re_rate <- return/open_price
                res <- cbind(open_time,open_price,cover_time,cover_price,
                             return,re_rate,cover_reason)
                transaction_long <- rbind(transaction_long,res)
                #处理持仓记录
                cut_index <- match(long_cover_shot,buy_time_long)
                buy_time_long <- buy_time_long[-cut_index]
                intimeprofit_long <- intimeprofit_long[-cut_index]
                highestprofit_long <- highestprofit_long[-cut_index]
                #####
                all_profit <- sum(all_profit,return)
                print(res)
                print(all_profit)
            }
        }
        
        #判断加仓
        if(length(buy_time_long) > 0)
        {
            long_add_signal <- Long_Open_Add(trend_index=trend_index,close_today=close_today,
                                  buy_time=buy_time_long,close=close,atr_n=atr_n,upper = upper)
            if(long_add_signal=="Add")#有加仓
            {
                print("Long Add")
                buy_time_long <- c(buy_time_long,date)
                intimeprofit_long <- c(intimeprofit_long,0)
                names(intimeprofit_long) <- buy_time_long
                highestprofit_long <- c(highestprofit_long,0)
                names(highestprofit_long) <- buy_time_long
            }
        }
        
        #判断开仓
        long_open_signal <- Open_Position_Long(trend_index=trend_index,
                                         upper=upper,high_today=high_today,
                                         disp_top=disp_top)
        if(long_open_signal=="Open") 
        {
            print("Long_Open")
            buy_time_long <- c(buy_time_long,date)
            intimeprofit_long <- c(intimeprofit_long,0)
            names(intimeprofit_long) <- buy_time_long
            highestprofit_long <- c(highestprofit_long,0)
            names(highestprofit_long) <- buy_time_long
        }
    }
    
    return(transaction_long)
}

trade_long <- Main_Long(close=Close,high=High,low=Low,tolerance=100,ratio=0.618,avglen=5,sdlen=5,sdev=2,n=20)

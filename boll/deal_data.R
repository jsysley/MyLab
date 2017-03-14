path <- "F:\\工作\\立信\\cta_boll"
# boll_upper <- read.csv(paste(path,"\\data\\Bollinger_UpperBand.csv",sep=""),header=TRUE)
# boll_middle <- read.csv(paste(path,"\\data\\Bollinger_MiddleBand.csv",sep=""),header=TRUE)
# boll_lower <- read.csv(paste(path,"\\data\\Bollinger_UpperBand.csv",sep=""),header=TRUE)
# atr <- read.csv(paste(path,"\\data\\ATR.csv",sep=""),header=TRUE)
close <- read.csv(paste(path,"\\data\\close.csv",sep=""),header = TRUE)
high <- read.csv(paste(path,"\\data\\high.csv",sep=""),header = TRUE)
low <- read.csv(paste(path,"\\data\\low.csv",sep=""),header = TRUE)


# atr_index <- which(colnames(boll_upper)[1]==colnames(atr))
# atr <- atr[,atr_index:ncol(atr)]
# close_index <- which(colnames(boll_upper)[1]==colnames(close))
# close <- close[,close_index:ncol(close)]

data <- cbind(t(close),t(boll_upper),t(boll_middle),t(boll_lower),t(atr))
colnames(data) <- c("close","boll_upper","boll_middle","boll_lower","atr")

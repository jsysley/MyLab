output_file <- "F:\\工作\\立信\\cta_boll\\data"
raw_data <- read.csv("F:\\工作\\立信\\cta_boll\\ICE.SHFE.rb.HOT 1 分.csv",header=TRUE,stringsAsFactors = FALSE)
col_name <- paste(raw_data[,1],raw_data[,2],sep="_")

Close <- raw_data[,"X.Close."]
Close <- rbind(Close)
colnames(Close) <- col_name
Close <- data.frame(Close)
write.csv(Close,file=paste(output_file,"\\close.csv",sep=""),row.names = FALSE)

Open <- raw_data[,"X.Open."]
Open <- rbind(Open)
colnames(Open) <- col_name
Open <- data.frame(Open)
write.csv(Open,file=paste(output_file,"\\open.csv",sep=""),row.names = FALSE)

High <- raw_data[,"X.High."]
High <- rbind(High)
colnames(High) <- col_name
High <- data.frame(High)
write.csv(High,file=paste(output_file,"\\high.csv",sep=""),row.names = FALSE)

Low <- raw_data[,"X.Low."]
Low <- rbind(Low)
colnames(Low) <- col_name
Low <- data.frame(Low)
write.csv(Low,file=paste(output_file,"\\low.csv",sep=""),row.names = FALSE)

Volume <- raw_data[,"X.Volume."]
Volume <- rbind(Volume)
colnames(Volume) <- col_name
Volume <- data.frame(Volume)
write.csv(Volume,file=paste(output_file,"\\volume.csv",sep=""),row.names = FALSE)

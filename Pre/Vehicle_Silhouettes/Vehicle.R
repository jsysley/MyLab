dir_path <- "F:/git/trial/R/Pre/Vehicle_Silhouettes"
vehicle <- read.table(paste(dir_path,"/vehicle3.txt",sep=""),
                      stringsAsFactors = FALSE,header = TRUE)
###check the structure of the data
dim(vehicle)
str(vehicle)
#Plot
if(!require(ggplot2))install.packages("ggplot2")
require(ggplot2)
#bar charts
Plot_A_Sta <- function(name,vehicle)
{
    ggplot(vehicle, aes(eval(parse(text=name))))+
        geom_bar(aes(fill=factor(eval(parse(text=name)))))+
        xlab(name)+guides(fill=guide_legend(title=name))
}
lapply(colnames(vehicle),Plot_A_Sta,vehicle=vehicle)
#Boxplot
Plot_A_Box <- function(name,vehicle)
{
    # vehicle[name] <- as.numeric(vehicle[name])
    ggplot(vehicle, aes(eval(parse(text=name)),eval(parse(text=name))))+
        geom_boxplot(fill="lightblue",varwidth = TRUE,outlier.colour = "red")+
        xlab(name)+ylab("Value")
}
lapply(colnames(vehicle),Plot_A_Box,vehicle=vehicle)

#add the class name
vehicle_augment <- vehicle
vehicle_augment["class"] <- factor(vehicle[,"class"],levels = c(1,2,3,4),
                                labels = c("Opel_Manta_car","Saab_9000_car","double_decker_bus","Chevrolet_van"))
table(vehicle_augment["class"])

#build the tree
if(!require(rpart))install.packages("rpart")
require(rpart)
if(!require(rpart.plot))install.packages("rpart.plot")
require(rpart.plot)
trees <- rpart(class~.,vehicle_augment)
attributes(trees)
rpart.plot(trees)
###another way to plot
plot(trees,uniform=T, branch=1, margin=0.1, main="Classification Tree")
text(trees,use.n=T, col="blue")
#reshow the graph in the book
plotcp(trees)

#calculate the resubstition error and the confusion matrix
if(!require(rpart.plot))install.packages("rpart.plot")
library(caret)
confusionMatrix(predict(trees,vehicle_augment,type="class"),vehicle_augment[,"class"])

#prune the tree according to the rulw 1-sSE
printcp(trees)
##minimize the cross validaion variation(xerror)
##choose the seven line ,0.42+0.21 = 0.636,between 3line and 4line
##choose cp=0.07
trees2 <- prune(trees,cp=0.07)
rpart.plot(trees2)
snip.rpart(trees)#动态剪枝
#check the confusion matrix
confusionMatrix(predict(trees2,vehicle_augment,type="class"),vehicle_augment[,"class"])

#10 fold cv results of different size trees
all_index <- sample(1:nrow(vehicle),replace=FALSE)

cv <- vector(length = 10)
for(j in 1:10)
{
    for(i in 1:10)
    {
        valid_index <- all_index[(84*(i-1)+1):(84*i)]
        train_index <- all_index[-valid_index]
        ct <- rpart(class~.,vehicle_augment[train_index,],maxdepth=j)
        res <- predict(ct,vehicle_augment[valid_index,],type="class")
        cv[j] <- cv[j] + sum(vehicle_augment[valid_index,"class"]!=res)
    }
}
###Plot the 10-fold validation error of different max depths
temp <- data.frame(x=1:10,y=cv)
ggplot(data=temp,aes(x,y)) + geom_point(colour="red",shape=1,size=3) + geom_line(colour="lightblue") +
    xlab("Max Depth") + ylab("10-Val Error")
rm(temp)



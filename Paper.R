##################标准例子
str(iris)
library(e1071)
x <- iris[which(iris[,5] %in% c("setosa","versicolor")),1:4]
y <- iris[which(iris[,5] %in% c("setosa","versicolor")),5]
y <- factor(y)#去冗余因子
sv <- svm(x,y)
summary(sv)
###预测
pred <- predict(sv,x)
table(pred,y)
sum(pred==y)/length(y)
##################
EM_SVM <- function(x,y,alpha=1,mu=1)
{
    k <- ncol(x)#变量个数
    n <- nrow(x)#样本数
    sigma_lower <- apply(x,2,sd)
    if(class(x)!="matrix")#数据格式
    {
        x <- as.matrix(x,ncol=k)
    }
    beta <- matrix(runif(k,0,1),ncol=1)#初始化beta0向量
    y_num <- matrix(rep(1,length(y)),ncol=1)
    y_num[which(levels(y)[2]==y),] <- -1#对y做-1,1的转换
    #E-Step
    for(i in 1:n)#对每个样本迭代
    {
        lamda_lower_inv <- 1/abs(1 - y_num[i,,drop=FALSE] %*% x[i,,drop=FALSE] %*% beta)
        lamda_upper_inv <- diag(rep(lamda_lower_inv[1],n))
        #求omega_lower_inv
        omega_lower_inv <- vector(length = k)
        for(j in 1:k)
        {
            if(beta[j]!=0)
            {
                omega_lower_inv[j] <- alpha*abs(beta[j]**(alpha-2))*((mu*sigma_lower[j])**(2-alpha))
            }else{
                omega_lower_inv[j] <- Inf
            }
        }
        omega_upper_inv <- diag(omega_lower_inv)
        #M-Step
        sigma_upper <- diag(sigma_lower)
        beta <- (mu**(-2)*solve(sigma_upper)%*%omega_upper_inv + t(x)%*%lamda_upper_inv%*%x)%*%t(x)%*%(matrix(rep(1,n),ncol=1)+1)
    }
    
}

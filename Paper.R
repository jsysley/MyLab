##################标准例子
str(iris)
library(e1071)
x <- iris[which(iris[,5] %in% c("setosa","versicolor")),1:4]
y <- iris[which(iris[,5] %in% c("setosa","versicolor")),5]
y <- factor(y)#去冗余因子
sv <- svm(x,y)
summary(sv)
sv$coefs#支持向量
###预测
pred <- predict(sv,x)
table(pred,y)
sum(pred==y)/length(y)
##################
EM_SVM <- function(x,y,alpha=1,mu=1)
{
    k <- ncol(x)#变量个数
    n <- nrow(x)#样本数
    #初始化sigma_lower
    sigma_lower <- c(1,apply(x,2,sd))
    names(sigma_lower)[1] <- "intecept"
    if(class(x)!="matrix")#数据格式
    {
        x <- as.matrix(x,ncol=k)
    }
    #处理截距项，补齐数据
    x <- cbind(rep(1,n),x)
    colnames(x)[1] <- "intercept"
    #随机初始化beta的初始值
    set.seed(1)
    beta <- matrix(runif(k+1,0,1),ncol=1)#初始化beta0向量,第一个元素的截距项
    y_num <- matrix(rep(1,length(y)),ncol=1)
    y_num[which(levels(y)[2]==y),] <- -1#对y做-1,1的转换,将第二类转换为-1
    #####临时变量
    temp <- matrix(nrow=n,ncol=k+1)
    colnames(temp) <- colnames(x)
    #####
    #E-Step
    for(i in 1:n)#对每个样本迭代
    {
        # browser()
        lamda_lower_inv <- 1/abs(1 - y_num[i,,drop=FALSE] %*% x[i,,drop=FALSE] %*% beta)
        lamda_upper_inv <- diag(rep(lamda_lower_inv[1],n))
        #求omega_lower_inv
        omega_lower_inv <- vector(length = k)
        for(j in 1:k+1)
        {
            if(alpha<2)
            {
                if(beta[j]!=0)
                {
                    omega_lower_inv[j] <- alpha*abs(beta[j])**(alpha-2)*((mu*sigma_lower[j])**(2-alpha))
                }else{
                    omega_lower_inv[j] <- Inf
                }
            }else if(alpha==2){
                omega_lower_inv[j] <- 1
            }
        }
        omega_upper_inv <- diag(omega_lower_inv)
        #M-Step
        sigma_upper <- diag(sigma_lower)
        beta <- (mu**(-2)*solve(sigma_upper)%*%omega_upper_inv + t(x)%*%lamda_upper_inv%*%x)%*%t(x)%*%(matrix(rep(1,n),ncol=1)+1)
        temp[i,] <- beta
    }
    return(beta)
}

Plot_Beta <- function(temp)
{
    k <- ncol(temp)
    par(mfrow=c(floor(sqrt(k)),ceiling(sqrt(k))))
    lapply(1:k,function(local) plot(x=1:nrow(temp),y=temp[,local],type = "l",
           xlab="Value",ylab=colnames(temp)[local]))
}
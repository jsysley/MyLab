##################标准例子
str(iris)
library(e1071)
x <- iris[which(iris[,5] %in% c("setosa","versicolor")),1:4]
y <- iris[which(iris[,5] %in% c("setosa","versicolor")),5]
y <- factor(y)#去冗余因子
sv <- svm(x,y,kernei="linear")
summary(sv)
sv$coefs#支持向量
###预测
pred <- predict(sv,x)
table(pred,y)
sum(pred==y)/length(y)

##################论文例子
spam <- read.table("F:/研究生/研一下/多元统计/DataSets/spambase.txt",header=TRUE)
spam <- spam[,c(1:57,59)]
x <- spam[,1:57]
y <- spam[,58]
####################################################
library(MASS)
EM_SVM <- function(x,y,alpha = 1,mu = 1,iter = 1000)
{
    ##############################数据预处理
    if(class(x) != "matrix")
    {
        x <- as.matrix(x,ncol = k)
    }
    #处理截距项，补齐数据
    x <- cbind(rep(1,n),x)
    colnames(x)[1] <- "intercept"
    k <- ncol(x)#变量个数
    n <- nrow(x)#样本数
    ##############################初始化
    #初始化sigma_lower
    sigma_lower <- c(1,apply(x,2,sd))
    names(sigma_lower)[1] <- "intecept"
    sigma_upper <- diag(sigma_lower^2)
    #随机初始化beta的初始值，第一个元素的截距项
    set.seed(1)
    beta <- matrix(runif(k + 1,0,1),ncol = 1)
    #对y做-1,1的转换,将第二类转换为-1
    y_num <- matrix(rep(1,length(y)),ncol = 1)
    y_num[which(levels(y)[2] == y),] <- -1
    #####临时变量
    temp <- matrix(nrow = iter,ncol = k + 1)
    colnames(temp) <- colnames(x)
    ##############################iterations
    for(step in 1:iter)
    {
        #####E-Step
        #lambda_lower_inv
        lambda_lower_inv <- vector(length = n)
        for(i in 1:n)
        {
            lambda_lower_inv[i] <- 1/abs(1 - y_num[i,,drop=FALSE] %*% x[i,,drop=FALSE] %*% beta)
        }
        #lambda_upper_inv
        lambda_upper_inv <- diag(lambda_lower_inv)
        #omega_lower_inv
        omega_lower_inv <- vector(length = k)
        for(j in 1:(k + 1))
        {
            if(alpha < 2)
            {
                if(beta[j] != 0)
                {
                    omega_lower_inv[j] <- alpha * (abs(beta[j]) ** (alpha - 2)) * ((mu * sigma_lower[j]) ** (2 - alpha))
                }else{
                    omega_lower_inv[j] <- 9999
                }
            }else if(alpha == 2){
                omega_lower_inv[j] <- 1
            }
        }
        #omega_upper_inv
        omega_upper_inv <- diag(omega_lower_inv)
        
        #####M-Step
        beta <- ginv((mu**(-2)) * solve(sigma_upper) %*% omega_upper_inv + t(x) %*% lambda_upper_inv %*% x) %*% t(x) %*% (matrix(rep(1,n),ncol=1) + lambda_lower_inv)
        #####record
        temp[step,] <- beta
    }
    
    res <- list(beta,temp)
    return(res)
}

Plot_Beta <- function(temp)
{
    k <- ncol(temp)
    par(mfrow=c(floor(sqrt(k)),ceiling(sqrt(k))))
    lapply(1:k,function(local) plot(x=1:nrow(temp),y=temp[,local],type = "l",
                                    xlab="Value",ylab=colnames(temp)[local]))
}

Predict <- function(x,beta)
{
    n <- nrow(x)
    if(class(x) != "matrix")
    {
        x <- as.matrix(x,ncol = (nrow(beta) - 1))
    }
    x <- cbind(rep(1,n),x)
    colnames(x)[1] <- "intercept"
    pre <- x %*% beta
    return(pre)
}

res <- EM_SVM(x,y,alpha=1,mu=2,iter=1000)
decision <- Predict(x,res[[1]])
Plot_Beta(res[[2]])


####################################################
ECME_SVM <- function(x,y,alpha = 1,mu = 1,amu = 0.1,bmu = 0.1,iter = 100)
{
    k <- ncol(x)#变量个数
    n <- nrow(x)#样本数
    #初始化sigma_lower
    sigma_lower <- c(1,apply(x,2,sd))
    names(sigma_lower)[1] <- "intecept"
    sigma_upper <- diag(sigma_lower^2)
    if(class(x) != "matrix")#数据格式
    {
        x <- as.matrix(x,ncol = k)
    }
    #处理截距项，补齐数据
    x <- cbind(rep(1,n),x)
    colnames(x)[1] <- "intercept"
    #随机初始化beta的初始值
    set.seed(1)
    beta <- matrix(runif(k + 1,0,1),ncol = 1)#初始化beta0向量,第一个元素的截距项
    y_num <- matrix(rep(1,length(y)),ncol = 1)
    y_num[which(levels(y)[2] == y),] <- -1#对y做-1,1的转换,将第二类转换为-1
    #####临时变量
    temp <- matrix(nrow = iter,ncol = k + 1)
    colnames(temp) <- colnames(x)
    #####iterations
    for(step in 1:iter)
    {
        #####E-Step
        #lambda_lower_inv
        lambda_lower_inv <- vector(length = n)
        for(i in 1:n)
        {
            lambda_lower_inv[i] <- 1/abs(1 - y_num[i,,drop=FALSE] %*% x[i,,drop=FALSE] %*% beta)
        }
        #lambda_upper_inv
        lambda_upper_inv <- diag(lambda_lower_inv)
        #omega_lower_inv
        omega_lower_inv <- vector(length = k)
        for(j in 1:(k + 1))
        {
            if(alpha < 2)
            {
                if(beta[j] != 0)
                {
                    omega_lower_inv[j] <- alpha * (abs(beta[j]) ** (alpha - 2)) * ((mu * sigma_lower[j]) ** (2 - alpha))
                }else{
                    omega_lower_inv[j] <- 9999
                }
            }else if(alpha == 2){
                omega_lower_inv[j] <- 1
            }
        }
        #omega_upper_inv
        omega_upper_inv <- diag(omega_lower_inv)
        
        #####M-Step
        beta <- ginv((mu**(-2)) * solve(sigma_upper) %*% omega_upper_inv + t(x) %*% lambda_upper_inv %*% x) %*% t(x) %*% (matrix(rep(1,n),ncol=1) + lambda_lower_inv)
        #####CME-Step
        mu_inv_alpha <- (bmu + sum((abs(beta / sigma_lower))**alpha)) / (k / alpha + amu - 1)
        mu <- mu_inv_alpha ** (- 1 / alpha)
        #####record
        temp[step,] <- beta
    }
    res <- list(beta,temp)
    return(res)
}

res2 <- ECME_SVM(x,y,alpha = 1,mu = 1,amu = 0.1,bmu = 0.1,iter = 1000)
decision <- Predict(x,res2[[1]])
Plot_Beta(res2[[2]])

####################################################
###定义Inverse Gaussian distribution generator
distr <- udig(mu = 1,lambda = 1)
gen <- pinvd.new(distr)
x <- ur(gen,1)

library(MASS)
MCMC_SVM <- function(x,y,alpha = 1,mu = 1,iter = 1000)###alpha=1
{
    k <- ncol(x)#变量个数
    n <- nrow(x)#样本数
    #初始化sigma_lower
    sigma_lower <- c(1,apply(x,2,sd))
    names(sigma_lower)[1] <- "intecept"
    sigma_upper <- diag(sigma_lower^2)
    #初始化
    #lambda_lower_inv
    lambda_lower_inv <- vector(length = n)
    for(i in 1:n)
    {
        lambda_lower_inv[i] <- 1/abs(1 - y_num[i,,drop=FALSE] %*% x[i,,drop=FALSE] %*% beta)
    }
    #lambda_upper_inv
    lambda_upper_inv <- diag(lambda_lower_inv)
    #omega_lower_inv
    omega_lower_inv <- vector(length = k)
    for(j in 1:(k + 1))
    {
        if(alpha < 2)
        {
            if(beta[j] != 0)
            {
                omega_lower_inv[j] <- alpha * (abs(beta[j]) ** (alpha - 2)) * ((mu * sigma_lower[j]) ** (2 - alpha))
            }else{
                omega_lower_inv[j] <- 9999
            }
        }else if(alpha == 2){
            omega_lower_inv[j] <- 1
        }
    }
    #omega_upper_inv
    omega_upper_inv <- diag(omega_lower_inv)
    
    if(class(x)!="matrix")#数据格式
    {
        x <- as.matrix(x,ncol=k)
    }
    #处理截距项，补齐数据
    x <- cbind(rep(1,n),x)
    colnames(x)[1] <- "intercept"
    #随机初始化beta的初始值
    set.seed(1)
    beta <- matrix(runif(k + 1,0,1),ncol = 1)#初始化beta0向量,第一个元素的截距项
    y_num <- matrix(rep(1,length(y)),ncol = 1)
    y_num[which(levels(y)[2] == y),] <- -1#对y做-1,1的转换,将第二类转换为-1
    #####临时变量
    temp <- matrix(nrow = iter,ncol = k + 1)
    colnames(temp) <- colnames(x)
    for(step in 1:iter)
    {
        #####step 1
        #b_upper_inv
        b_upper_inv <- ((mu ** (-2)) * solve(sigma_upper) %*% omega_upper_inv) + (t(x) %*% lambda_upper_inv %*% x)
        #b_lower
        b_lower <- solve(b_upper_inv) %*% t(x) %*% (matrix(rep(1,n),ncol=1) + lambda_lower_inv)
        set.seed(1)
        beta <- mvrnorm(n=1, b_lower, solve(b_upper_inv))
        #####step 2
        for(i in 1:n)
        {
            dis_mu <- 1/abs(1 - y_num[i,,drop=FALSE] %*% x[i,,drop=FALSE] %*% beta)
            distr <- udig(mu = dis_mu,lambda = 1)
            gen <- pinvd.new(distr)
            lambda_lower_inv[i] <- ur(gen,1)
        }
        lambda_upper_inv <- diag(lambda_lower_inv)
        #####step 3
        for(j in 1:(k + 1))
        {
            dis_mu <- mu * sigma_lower[j] * (abs(beta[j])**(-1))
            distr <- udig(mu = dis_mu,lambda = 1)
            gen <- pinvd.new(distr)
            omega_lower_inv[i] <- ur(gen,1)
        }
        omega_upper_inv <- diag(omega_lower_inv)
    }
    
    
}




#### M-H algorithm ####
M_H <- function(s)
{
    x <- vector(length = s)
    x[1] <- 0
    # uniform variable: u  
    set.seed(1)
    u <- runif(s)
    sd = 1
    df = 5  
    # sample the student distrubution with 5 freedom
    for(i in 2:s)
    {
        y = rnorm(1,mean = x[i - 1],sd = std)
        p_accept = dt(y,df) * pnorm(x[i-1],y,sd) / pnorm(x[i-1],y,sd) / (dt(x[i-1],df))
        if ((u[i] <= p_accept))  
        {  
            x[i] = y
        }  
        else{ 
            x[i] = x[i-1] }  
    }
    return(x)
}
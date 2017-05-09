##################Iris example
str(iris)
library(e1071)
x1 <- iris[which(iris[,5] %in% c("setosa","versicolor")),1:4]
y1 <- iris[which(iris[,5] %in% c("setosa","versicolor")),5]
y1 <- factor(y1) 
sv <- svm(x1,y1,kernei="linear")
summary(sv)
sv$coefs
### Predict
pred <- predict(sv,x1)
table(pred,y1)
sum(pred == y1)/length(y1)

##################Paprt examples
spam <- read.table("F:/研究生/研一下/多元统计/DataSets/spambase.txt",header=TRUE)
spam <- spam[,c(1:57,59)]
x2 <- spam[,1:57]
y2 <- spam[,58]
y2 <- factor(y2)
sv <- svm(x2,y2,kernei="linear")
####################################################
Plot_Beta <- function(temp)
{
    k <- ncol(temp)
    par(mfrow = c(floor(sqrt(k)),ceiling(sqrt(k))))
    lapply(1:k,function(local) plot(x = 1:nrow(temp),y = temp[,local],type = "l",
                                    xlab = "Value",ylab = colnames(temp)[local]))
}
Plot_Beta_One <- function(local,temp)
{
    plot(x = 1:nrow(temp),y = temp[,local],type = "l",
                                    xlab = "Value",ylab = colnames(temp)[local])
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
####################################################
library(MASS)
EM_SVM <- function(x,y,alpha = 1,mu = 1,iter = 1000)
{
    ############################## Pre Dealing
    if(class(x) != "matrix")
    {
        x <- as.matrix(x,ncol = k)
    }
    
    # Change the label into {-1,1};the second type is -1
    y_num <- matrix(rep(1,length(y)),ncol = 1)
    y_num[which(levels(y)[2] == y),] <- -1
    
    k <- ncol(x) # The number of the variables                               
    n <- nrow(x) # The number of the samples  
    
    sigma_lower <- c(1,apply(x,2,sd))
    names(sigma_lower)[1] <- "intecept"
    sigma_upper <- diag(sigma_lower^2)
    
    x <- cbind(rep(1,n),x) # Add the intercept                     
    colnames(x)[1] <- "intercept"
    
    set.seed(1) # Initialize beta 
    beta <- matrix(runif(k + 1,0,1),ncol = 1) # The first element is the intercept
    
    temp <- matrix(nrow = iter,ncol = k + 1) # Temporary variables
    colnames(temp) <- colnames(x)
    
    ############################## Iterations
    lambda_lower_inv <- vector(length = n)
    omega_lower_inv <- vector(length = k)
    for(step in 1:iter)
    {
        print(paste("Step: ",step,sep=""))
        ##### E-Step
        for(i in 1:n) # lambda_lower_inv
        {
            lambda_lower_inv[i] <- 1/abs(1 - y_num[i,,drop=FALSE] %*% x[i,,drop=FALSE] %*% beta)
        }
        
        lambda_upper_inv <- diag(lambda_lower_inv) # lambda_upper_inv
        
        for(j in 1:(k + 1)) # omega_lower_inv
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
        
        omega_upper_inv <- diag(omega_lower_inv) # omega_upper_inv
        
        ##### M-Step
        e <- tryCatch(temp1 <- ginv(sigma_upper), 
                      error=function(e){return("Error")})
        if(length(e) == 1)
        {
            which(temp1==Inf,arr.ind = TRUE)
        }
        
        temp2 <- (mu**(-2)) * temp1 %*% omega_upper_inv + t(x) %*% lambda_upper_inv %*% x
        
        e <- tryCatch(temp3 <- ginv(temp2), 
                      error=function(e){return("Error")})
        if(length(e) == 1)
        {
            temp2[which(temp2==Inf,arr.ind = TRUE)] = 999
            temp2[which(temp2==-Inf,arr.ind = TRUE)] = -999
            temp2[which(is.na(temp2),arr.ind = TRUE)] = 0.001
            temp3 <- ginv(temp2)
        }
        beta <- temp3 %*% t(x) %*% (matrix(rep(1,n),ncol=1) + lambda_lower_inv) 
        
        ##### Record
        temp[step,] <- beta
    }
    res <- list(beta,temp)
    return(res)
}
####################################################
ECME_SVM <- function(x,y,alpha = 1,mu = 1,amu = 0.1,bmu = 0.1,iter = 100)
{
    ############################## Pre Dealing
    if(class(x) != "matrix")
    {
        x <- as.matrix(x,ncol = k)
    }
    
    # Change the label into {-1,1};the second type is -1
    y_num <- matrix(rep(1,length(y)),ncol = 1)
    y_num[which(levels(y)[2] == y),] <- -1
    
    k <- ncol(x) # The number of the variables                               
    n <- nrow(x) # The number of the samples  
    
    sigma_lower <- c(1,apply(x,2,sd))
    names(sigma_lower)[1] <- "intecept"
    sigma_upper <- diag(sigma_lower^2)
    
    x <- cbind(rep(1,n),x) # Add the intercept                     
    colnames(x)[1] <- "intercept"
    
    set.seed(1) # Initialize beta
    beta <- matrix(runif(k + 1,0,1),ncol = 1) # The first element is the intercept
    
    temp <- matrix(nrow = iter,ncol = k + 1) # Temporary variables
    colnames(temp) <- colnames(x)
    
    ##############################Iterations
    lambda_lower_inv <- vector(length = n)
    omega_lower_inv <- vector(length = k)
    for(step in 1:iter)
    {
        print(paste("Step: ",step,sep=""))
        ##### E-Step
        for(i in 1:n) # lambda_lower_inv
        {
            lambda_lower_inv[i] <- 1/abs(1 - y_num[i,,drop=FALSE] %*% x[i,,drop=FALSE] %*% beta)
        }
        
        lambda_upper_inv <- diag(lambda_lower_inv) # lambda_upper_inv
        
        for(j in 1:(k + 1)) # omega_lower_inv
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
        
        omega_upper_inv <- diag(omega_lower_inv) # omega_upper_inv
        
        ##### M-Step
        # beta <- ginv((mu**(-2)) * ginv(sigma_upper) %*% omega_upper_inv + t(x) %*% lambda_upper_inv %*% x) %*% t(x) %*% (matrix(rep(1,n),ncol=1) + lambda_lower_inv)
        e <- tryCatch(temp1 <- ginv(sigma_upper), 
                      error=function(e){return("Error")})
        if(length(e) == 1)
        {
            sigma_upper[which(sigma_upper==Inf,arr.ind = TRUE)] = 999
            sigma_upper[which(sigma_upper==-Inf,arr.ind = TRUE)] = -999
            sigma_upper[which(is.na(sigma_upper),arr.ind = TRUE)] = 0.001
            temp1 <- ginv(sigma_upper)
        }
        
        temp2 <- (mu**(-2)) * temp1 %*% omega_upper_inv + t(x) %*% lambda_upper_inv %*% x
        
        e <- tryCatch(temp3 <- ginv(temp2), 
                      error=function(e){return("Error")})
        if(length(e) == 1)
        {
            temp2[which(temp2==Inf,arr.ind = TRUE)] = 999
            temp2[which(temp2==-Inf,arr.ind = TRUE)] = -999
            temp2[which(is.na(temp2),arr.ind = TRUE)] = 0.001
            temp3 <- ginv(temp2)
        }
        beta <- temp3 %*% t(x) %*% (matrix(rep(1,n),ncol=1) + lambda_lower_inv) 
        ##### CME-Step
        mu_inv_alpha <- (bmu + sum((abs(beta / sigma_lower)) ** alpha)) / (k / alpha + amu - 1)
        mu <- mu_inv_alpha ** (- 1 / alpha)
        
        ##### Record
        temp[step,] <- beta
    }
    res <- list(beta,temp)
    return(res)
}
####################################################
MCMC_SVM <- function(x,y,alpha = 1,mu = 1,iter = 1000)#alpha=1
{
    ############################## Pre Dealing
    if(class(x) != "matrix")
    {
        x <- as.matrix(x,ncol = k)
    }
    # Change the label into {-1,1};the second type is -1
    y_num <- matrix(rep(1,length(y)),ncol = 1)
    y_num[which(levels(y)[2] == y),] <- -1
    
    k <- ncol(x) # The number of the variables                               
    n <- nrow(x) # The number of the samples  
    
    sigma_lower <- c(1,apply(x,2,sd))
    names(sigma_lower)[1] <- "intecept"
    sigma_upper <- diag(sigma_lower^2)
    
    x <- cbind(rep(1,n),x) # Add the intercept                     
    colnames(x)[1] <- "intercept"
    
    set.seed(1) # Initialize beta
    beta <- matrix(runif(k + 1,0,1),ncol = 1) # The first element is the intercept
    
    lambda_lower_inv <- vector(length = n) # Initialize lambda_lower_inv
    for(i in 1:n)
    {
        lambda_lower_inv[i] <- 1/abs(1 - y_num[i,,drop=FALSE] %*% x[i,,drop=FALSE] %*% beta)
    }
    
    lambda_upper_inv <- diag(lambda_lower_inv) # Initialize lambda_upper_inv
    
    omega_lower_inv <- vector(length = k) # Initialize omega_lower_inv
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
    
    omega_upper_inv <- diag(omega_lower_inv) # Initialize omega_upper_inv
    
    temp <- matrix(nrow = iter,ncol = k + 1)# Temporary variables
    colnames(temp) <- colnames(x)
    
    ############################## Iterations
    for(step in 1:iter)
    {
        print(paste("Step: ",step,sep=""))
        ##### Step 1
        # b_upper_inv
        e <- tryCatch(temp1 <- ginv(sigma_upper), 
                      error=function(e){return("Error")})
        if(length(e) == 1)
        {
            sigma_upper[which(sigma_upper==Inf,arr.ind = TRUE)] = 999
            sigma_upper[which(sigma_upper==-Inf,arr.ind = TRUE)] = -999
            sigma_upper[which(is.na(sigma_upper),arr.ind = TRUE)] = 0.001
            temp1 <- ginv(sigma_upper)
        }
        b_upper_inv <- ((mu ** (-2)) * temp1 %*% omega_upper_inv) + (t(x) %*% lambda_upper_inv %*% x)
        # b_lower
        e <- tryCatch(temp2 <- ginv(b_upper_inv), 
                      error=function(e){return("Error")})
        if(length(e) == 1)
        {
            b_upper_inv[which(b_upper_inv==Inf,arr.ind = TRUE)] = 999
            b_upper_inv[which(b_upper_inv==-Inf,arr.ind = TRUE)] = -999
            b_upper_inv[which(is.na(b_upper_inv),arr.ind = TRUE)] = 0.001
            temp2 <- ginv(b_upper_inv)
        }
        b_lower <- temp2 %*% t(x) %*% (matrix(rep(1,n),ncol=1) + lambda_lower_inv)
        
        set.seed(1) # beta
        beta <- mvrnorm(n=1, b_lower, temp2)
        
        ##### Step 2
        for(i in 1:n)
        {
            dis_mu <- 1/abs(1 - y_num[i,,drop=FALSE] %*% x[i,,drop=FALSE] %*% beta)
            distr <- udig(mu = dis_mu,lambda = 1)
            gen <- pinvd.new(distr)
            lambda_lower_inv[i] <- ur(gen,1)
        }
        lambda_upper_inv <- diag(lambda_lower_inv)
        
        ##### Step 3
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

####################################################Spam
res <- EM_SVM(x2,y2,alpha = 1,mu = 2,iter = 300)
decision <- Predict(x2,res[[1]])
dev.new()
lapply(1:ncol(res[[2]]),Plot_Beta_One,temp = res[[2]])


res2 <- ECME_SVM(x2,y2,alpha = 1,mu = 1,amu = 0.1,bmu = 0.1,iter = 300)
decision <- Predict(x2,res2[[1]])
dev.new()
lapply(1:ncol(res[[2]]),Plot_Beta_One,temp = res[[2]])


res3 <- MCMC_SVM(x2,y2,mu = 1,iter = 300)
decision <- Predict(x2,res2[[1]])
dev.new()
lapply(1:ncol(res[[2]]),Plot_Beta_One,temp = res[[2]])
####################################################Iris
res <- EM_SVM(x,y,alpha = 1,mu = 2,iter = 1000)
decision <- Predict(x,res[[1]])
label <- sign(decision)
label[which(label == -1)] <- levels(y)[2]
label[which(label == 1)] <- levels(y)[1]
sum(label == y)/nrow(x1)
Plot_Beta(res[[2]])

res2 <- ECME_SVM(x,y,alpha = 1,mu = 1,amu = 0.1,bmu = 0.1,iter = 1000)
decision <- Predict(x,res2[[1]])
Plot_Beta(res2[[2]])

res3 <- MCMC_SVM(x,y,mu = 1,iter = 1000)
decision <- Predict(x,res2[[1]])
Plot_Beta(res2[[2]])
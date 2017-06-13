##################Iris example
str(iris)
library(e1071)
x1 <- iris[which(iris[,5] %in% c("setosa","versicolor")),1:4]
y1 <- iris[which(iris[,5] %in% c("setosa","versicolor")),5]
y1 <- factor(y1) 
sv <- svm(x1,y1,kernel = "linear")
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
sv <- svm(x2,y2,kernel = "linear")

##################Simulation examples
library(MASS)
set.seed(8)
x31 <- mvrnorm(250,c(10,10),matrix(c(1,0,0,1),ncol = 2))
set.seed(7)
x32 <- mvrnorm(250,c(-10,-10),matrix(c(1,0,0,1),ncol = 2))
x3 <- rbind(x32,x31)
be <- matrix(c(1,1),ncol = 1)
y3 <- factor(sign(x3 %*% be + 2),levels = c(1,-1),labels = c("P","N"))

####################################################
Plot_beta0 <- function(temp)
{
    k <- ncol(temp)
    par(mfrow = c(floor(sqrt(k)),ceiling(sqrt(k))))
    lapply(1:k,function(local) plot(x = 1:nrow(temp),y = temp[,local],type = "l",
                                    xlab = "Value",ylab = colnames(temp)[local]))
}

Plot_beta0_One <- function(local,temp)
{
    plot(x = 1:nrow(temp),y = temp[,local],type = "l",
                                    xlab = "Value",ylab = colnames(temp)[local])
}

Predict <- function(x,beta0)
{
    n <- nrow(x)
    if(class(x) != "matrix")
    {
        x <- as.matrix(x,ncol = (nrow(beta0) - 1))
    }
    x <- cbind(rep(1,n),x)
    # colnames(x)[1] <- "intercept"
    pre <- x %*% beta0
    return(pre)
}

####################################################
library(MASS)
EM_SVM <- function(x,y,alpha = 1,mu = 0.1,iter = 8000)
{
    ############################## Pre Dealing
    k <- ncol(x) # The number of the variables                               
    n <- nrow(x) # The number of the samples  
    if(class(x) != "matrix")
    {
        x <- as.matrix(x, ncol = k)
    }
    # Change the label into {-1,1};the second type is -1
    y_num <- matrix(rep(1, length(y)), ncol = 1)
    y_num[which(levels(y)[2] == y),] <- -1
    # Initialize
    sigma_lower <- c(1, apply(x, 2, sd)) # The first elements is interception
    names(sigma_lower)[1] <- "intercept"
    sigma_upper <- diag(sigma_lower^2)
    
    x <- cbind(rep(1,n),x) # Add the intercept     
    tryCatch({colnames(x)[1] <- "intercept"},error = function(e){
        colnames(x) <<- c("intercept",paste("x",1:k,sep = ""))
    })
    
    set.seed(1) # Initialize beta0 
    beta0 <- matrix(runif(k + 1,0,1),ncol = 1) # The first element is the intercept
    
    temp <- matrix(nrow = iter,ncol = k + 1) # Temporary variables
    colnames(temp) <- colnames(x)
    
    ############################## Iterations
    omega_lower_inv <- vector(length = k)
    for(step in 1:iter)
    {
        print(paste("Step: ", step, sep = ""))
        ##### E-Step
        lambda_lower_inv <- array(1 / abs(1 - (x %*% beta0) * y_num))
    
        lambda_upper_inv <- diag(lambda_lower_inv) # lambda_upper_inv
        
        for(j in 1:(k + 1)) # omega_lower_inv
        {
            if(alpha < 2)
            {
                if(beta0[j] != 0)
                {
                    omega_lower_inv[j] <- alpha * (abs(beta0[j]) ** (alpha - 2)) * ((mu * sigma_lower[j]) ** (2 - alpha))
                }else
                {
                    omega_lower_inv[j] <- 99999
                }
            }else if(alpha == 2)
            {
                omega_lower_inv[j] <- 1
            }
        }
        
        omega_upper_inv <- diag(omega_lower_inv) # omega_upper_inv
        
        ##### M-Step
        e <- tryCatch(temp1 <- ginv(sigma_upper), error = function(e){
            sigma_upper[which(sigma_upper == Inf,arr.ind = TRUE)] = 99999
            sigma_upper[which(sigma_upper == -Inf,arr.ind = TRUE)] = -99999
            sigma_upper[which(is.na(sigma_upper),arr.ind = TRUE)] = 0.00001
            temp1 <<- ginv(sigma_upper)
            })
        
        temp2 <- (mu ** (-2)) * temp1 %*% omega_upper_inv + t(x) %*% lambda_upper_inv %*% x
        
        e <- tryCatch(temp3 <- ginv(temp2), error = function(e){
            temp2[which(temp2 == Inf,arr.ind = TRUE)] <- 99999
            temp2[which(temp2 == -Inf,arr.ind = TRUE)] <- -99999
            temp2[which(is.na(temp2),arr.ind = TRUE)] <- 0.00001
            temp3 <<- ginv(temp2)
            })
        
        beta0 <- temp3 %*% t(x) %*% (matrix(rep(1,n),ncol = 1) + matrix(lambda_lower_inv,ncol = 1)) 
        
        ##### Record
        temp[step,] <- beta0
    }
    res <- list(beta0,temp)
    return(res)
}

####################################################
ECME_SVM <- function(x,y,alpha = 1,mu = 0.1,amu = 0.1,bmu = 0.1,iter = 100)
{
    ############################## Pre Dealing
    k <- ncol(x) # The number of the variables                               
    n <- nrow(x) # The number of the samples  
    if(class(x) != "matrix")
    {
        x <- as.matrix(x, ncol = k)
    }
    # Change the label into {-1,1};the second type is -1
    y_num <- matrix(rep(1, length(y)), ncol = 1)
    y_num[which(levels(y)[2] == y),] <- -1
    # Initialize
    sigma_lower <- c(1, apply(x, 2, sd)) # The first elements is interception
    names(sigma_lower)[1] <- "intercept"
    sigma_upper <- diag(sigma_lower^2)
    
    x <- cbind(rep(1,n),x) # Add the intercept
    tryCatch({colnames(x)[1] <- "intercept"},error = function(e){
        colnames(x) <<- c("intercept",paste("x",1:k,sep = ""))
    })
    
    
    set.seed(1) # Initialize beta0 
    beta0 <- matrix(runif(k + 1,0,1),ncol = 1) # The first element is the intercept
    
    temp <- matrix(nrow = iter,ncol = k + 1) # Temporary variables
    colnames(temp) <- colnames(x)
    
    ##############################Iterations
    omega_lower_inv <- vector(length = k)
    for(step in 1:iter)
    {
        print(paste("Step: ",step,sep = ""))
        ##### E-Step
        lambda_lower_inv <- array(1/abs(1 - (x %*% beta0) * y_num))
        
        lambda_upper_inv <- diag(lambda_lower_inv) # lambda_upper_inv
        
        for(j in 1:(k + 1)) # omega_lower_inv
        {
            if(alpha < 2)
            {
                if(beta0[j] != 0)
                {
                    omega_lower_inv[j] <- alpha * (abs(beta0[j]) ** (alpha - 2)) * ((mu * sigma_lower[j]) ** (2 - alpha))
                }else
                {
                    omega_lower_inv[j] <- 99999
                }
            }else if(alpha == 2)
            {
                omega_lower_inv[j] <- 1
            }
        }
        
        omega_upper_inv <- diag(omega_lower_inv) # omega_upper_inv
        
        ##### M-Step
        e <- tryCatch(temp1 <- ginv(sigma_upper), error = function(e){
            sigma_upper[which(sigma_upper == Inf,arr.ind = TRUE)] = 99999
            sigma_upper[which(sigma_upper == -Inf,arr.ind = TRUE)] = -99999
            sigma_upper[which(is.na(sigma_upper),arr.ind = TRUE)] = 0.00001
            temp1 <<- ginv(sigma_upper)
        })
        
        temp2 <- (mu**(-2)) * temp1 %*% omega_upper_inv + t(x) %*% lambda_upper_inv %*% x
        
        e <- tryCatch(temp3 <- ginv(temp2), error = function(e){
            temp2[which(temp2 == Inf,arr.ind = TRUE)] <- 99999
            temp2[which(temp2 == -Inf,arr.ind = TRUE)] <- -99999
            temp2[which(is.na(temp2),arr.ind = TRUE)] <- 0.00001
            temp3 <<- ginv(temp2)
        })
        
        beta0 <- temp3 %*% t(x) %*% (matrix(rep(1,n),ncol = 1) + matrix(lambda_lower_inv,ncol = 1)) 
        
        ##### CME-Step
        mu_inv_alpha <- (bmu + sum((abs(beta0 / sigma_lower)) ** alpha)) / (k / alpha + amu - 1)
        mu <- mu_inv_alpha ** (- 1 / alpha)
        
        ##### Record
        temp[step,] <- beta0
    }
    res <- list(beta0,temp)
    return(res)
}

####################################################
library(Runuran)
library(MASS)
f <- function(x)
{
    temp1 <- (x / gamma(1 + 1 / (x + 0.01))) ** k
    temp2 <- sum(abs(beta0 / (mu * sigma_lower)) ** x)
    res <- temp1 * exp(- temp2)
    return(res)
}

MCMC_SVM <- function(x,y,alpha = 1,mu = 0.1,amu = 0.1,bmu = 0.1,iter = 1000)#alpha=1
{
    ############################## Pre Dealing
    k <- ncol(x) # The number of the variables                               
    n <- nrow(x) # The number of the samples  
    if(class(x) != "matrix")
    {
        x <- as.matrix(x, ncol = k)
    }
    # Change the label into {-1,1};the second type is -1
    y_num <- matrix(rep(1, length(y)), ncol = 1)
    y_num[which(levels(y)[2] == y),] <- -1
    # Initialize
    sigma_lower <- c(1, apply(x, 2, sd)) # The first elements is interception
    names(sigma_lower)[1] <- "intercept"
    sigma_upper <- diag(sigma_lower^2)
    
    x <- cbind(rep(1,n),x) # Add the intercept
    tryCatch({colnames(x)[1] <- "intercept"},error = function(e){
        colnames(x) <<- c("intercept",paste("x",1:k,sep = ""))
    })
    
    set.seed(1) # Initialize beta0 
    beta0 <- matrix(runif(k + 1,0,1),ncol = 1) # The first element is the intercept
    
    lambda_lower_inv <- array(1/abs(1 - (x %*% beta0) * y_num)) # lambda_upper_inv
    
    lambda_upper_inv <- diag(lambda_lower_inv) # lambda_upper_inv
    
    omega_lower_inv <- vector(length = k) # Initialize omega_lower_inv
    for(j in 1:(k + 1)) # omega_lower_inv
    {
        if(alpha < 2)
        {
            if(beta0[j] != 0)
            {
                omega_lower_inv[j] <- alpha * (abs(beta0[j]) ** (alpha - 2)) * ((mu * sigma_lower[j]) ** (2 - alpha))
            }else
            {
                omega_lower_inv[j] <- 99999
            }
        }else if(alpha == 2)
        {
            omega_lower_inv[j] <- 1
        }
    }
    
    omega_upper_inv <- diag(omega_lower_inv) # Initialize omega_upper_inv
    
    temp <- matrix(nrow = iter,ncol = k + 1)# Temporary variables
    colnames(temp) <- colnames(x)
    
    ############################## Iterations
    for(step in 1:iter)
    {
        print(paste("Step: ",step,sep = ""))
        ##### Step 1
        # b_upper_inv
        e <- tryCatch(temp1 <- ginv(sigma_upper), error = function(e){
            sigma_upper[which(sigma_upper == Inf,arr.ind = TRUE)] = 99999
            sigma_upper[which(sigma_upper == -Inf,arr.ind = TRUE)] = -99999
            sigma_upper[which(is.na(sigma_upper),arr.ind = TRUE)] = 0.00001
            temp1 <<- ginv(sigma_upper)
            })
        b_upper_inv <- ((mu ** (-2)) * temp1 %*% omega_upper_inv) + (t(x) %*% lambda_upper_inv %*% x)
        # b_lower
        e <- tryCatch(temp2 <- ginv(b_upper_inv),error = function(e){
            b_upper_inv[which(b_upper_inv == Inf,arr.ind = TRUE)] = 99999
            b_upper_inv[which(b_upper_inv == -Inf,arr.ind = TRUE)] = -99999
            b_upper_inv[which(is.na(b_upper_inv),arr.ind = TRUE)] = 0.00001
            temp2 <<- ginv(b_upper_inv)
            })
        b_lower <- temp2 %*% t(x) %*% (matrix(rep(1,n),ncol = 1) + matrix(lambda_lower_inv,ncol = 1))
        
        set.seed(1) # beta0
        beta0 <- mvrnorm(n = 1, b_lower, temp2)
        
        ##### Step 2
        for(i in 1:n)
        {
            set.seed(1)
            dis_mu <- 1/abs(1 - y_num[i,,drop = FALSE] %*% x[i,,drop = FALSE] %*% beta0)
            distr <- udig(mu = dis_mu, lambda = 1)
            gen <- pinvd.new(distr)
            lambda_lower_inv[i] <- ur(gen,1)
        }
        lambda_upper_inv <- diag(lambda_lower_inv)
        
        ##### Step 3
        for(j in 1:(k + 1))
        {
            set.seed(1)
            dis_mu <- mu * sigma_lower[j] * (abs(beta0[j])**(-1))
            distr <- udig(mu = dis_mu,lambda = 1)
            gen <- pinvd.new(distr)
            omega_lower_inv[j] <- ur(gen,1)
        }
        omega_upper_inv <- diag(omega_lower_inv)
        
        ##### Record
        temp[step,] <- beta0
        
        if(alpha == 1)
        {
            next
        }
        
        ##### Step 4
        # mu
        set.seed(1)
        mu_inv_1 <- rgamma(1,shape = amu + k + 1,scale = bmu + sum(beta0))
        mu <- ginv(mu_inv_1) # 1/mu

        # alpha
        beta0 <<- beta0
        mu <<- mu
        sigma_lower <<- sigma_lower
        k <<- k 
        # M-H
        alpha <- M_H(1000)[1000]
        
    }
    
    ##### Finally
    beta0_final <- apply(temp,2,mean)
    res <- list(beta0_final,temp)
    return(res)
}

####################################################
MCSS_SVM <- function(x,y,mu = 0.1,alpha = 1,iter)
{
    ############################## Pre Dealing
    k <- ncol(x) # The number of the variables                               
    n <- nrow(x) # The number of the samples  
    if(class(x) != "matrix")
    {
        x <- as.matrix(x, ncol = k)
    }
    # Change the label into {-1,1};the second type is -1
    y_num <- matrix(rep(1, length(y)), ncol = 1)
    y_num[which(levels(y)[2] == y),] <- -1
    # Initialize
    sigma_lower <- c(1, apply(x, 2, sd)) # The first elements is interception
    names(sigma_lower)[1] <- "intercept"
    sigma_upper <- diag(sigma_lower^2)
    
    x <- cbind(rep(1,n),x) # Add the intercept
    tryCatch({colnames(x)[1] <- "intercept"},error = function(e){
        colnames(x) <<- c("intercept",paste("x",1:k,sep = ""))
    })
    
    set.seed(1) # Initialize beta0
    beta0 <- matrix(runif(k + 1,0,1),ncol = 1) # The first element is the intercept
    
    ############################## Iterations
    lambda_lower_inv <- vector(length = n)
    for(step in 1:iter)
    {
        non <<- which(beta0 != 0)
        ##### Step 1
        # lambda_upper_inv
        for(i in 1:n)
        {
            set.seed(1)
            dis_mu <- 1/abs(1 - y_num[i,,drop = FALSE] %*% x[i,non,drop = FALSE] %*% beta0[non])
            distr <- udig(mu = dis_mu, lambda = 1)
            gen <- pinvd.new(distr)
            lambda_lower_inv[i] <- ur(gen,1)
        }
        
        lambda_upper_inv <<- diag(lambda_lower_inv)
        
        ##### Step 2
        # omega
        omega_lower_inv <- vector(length = length(non)) # Initialize omega_lower_inv
        for(j in 1:length(non)) # omega_lower_inv
        {
            if(alpha < 2)
            {
                if(beta0[j] != 0)
                {
                    omega_lower_inv[j] <- alpha * (abs(beta0[non[j]]) ** (alpha - 2)) * ((mu * sigma_lower[non[j]]) ** (2 - alpha))
                }else
                {
                    omega_lower_inv[j] <- 99999
                }
            }else if(alpha == 2)
            {
                omega_lower_inv[j] <- 1
            }
        }
        
        omega_upper_inv <- diag(omega_lower_inv) # Initialize omega_upper_inv
        # B & b
        e <- tryCatch(temp1 <- ginv(sigma_upper),error = function(e){
            sigma_upper[which(sigma_upper == Inf,arr.ind = TRUE)] = 99999
            sigma_upper[which(sigma_upper == -Inf,arr.ind = TRUE)] = -99999
            sigma_upper[which(is.na(sigma_upper),arr.ind = TRUE)] = 0.00001
            temp1 <<- ginv(sigma_upper)
        })
        
        b_upper_inv <- ((mu ** (-2)) * temp1[,non] %*% omega_upper_inv) + (t(x[,non]) %*% lambda_upper_inv %*% x[,non])
        # b_lower
        e <- tryCatch(temp2 <- ginv(b_upper_inv),error = function(e){
            b_upper_inv[which(b_upper_inv == Inf,arr.ind = TRUE)] = 99999
            b_upper_inv[which(b_upper_inv == -Inf,arr.ind = TRUE)] = -99999
            b_upper_inv[which(is.na(b_upper_inv),arr.ind = TRUE)] = 0.00001
            temp2 <- ginv(b_upper_inv)
        })
        
        b_lower <- temp2 %*% t(x[,non]) %*% (matrix(rep(1,n),ncol = 1) + matrix(lambda_lower_inv,ncol = 1))
        
        k <<- k
        mu <<- mu 
        alpha <<- alpha 
        beta0 <<- beta0
        sigma_lower <<- sigma_lower
        lambda_lower_inv <<- lambda_lower_inv
        b_upper_inv <<- b_upper_inv
        b_lower <<- b_lower
        M_H(100)[100]
        
        ##### Step 3
        set.seed(1) # beta0
        beta0 <- mvrnorm(n = 1, b_lower[non], b_upper_inv[,non])
    }
    ##### Finally
    beta0_final <- apply(temp,2,mean)
    res <- list(beta0_final,temp)
    return(res)
}

f <- function(x0)
{
    pi_all <- rep(pi,k + 1)
    gamma_lower <- rep(0,k + 1)
    gamma_lower[non] <- 1
    p_gamma <- prod(pi_all ** x0) * prod((1 - pi_all) ** (1 - gamma_lower))
    
    # First part
    temp1 <- (det(ginv(sigma_upper[,non]) / mu ** 2)) ** 0.5 / (det(b_upper_inv[,non]) ** 0.5)
    # Second part
    temp2 <- sum((1  + lambda_lower_inv - array((x %*% beta0) * y_num)) ** 2 / lambda_lower_inv)
    # Third part
    temp3 <- 1 / (2 * mu ** 2) * t(b_lower[non,]) %*% ginv(sigma_upper[,non]) %*% b_lower[non,]

    res <- p_gamma * temp1 * exp(- 1/2 * temp2 - temp3)
    return(res)
}
####################################################
# M_H
M_H <- function(s)
{
    x <- vector(length = s)
    x[1] <- 0
    # uniform variable: u  
    set.seed(1)
    u <- runif(s)
    std = 1
    # sample the student distrubution with 5 freedom
    for(i in 2:s)
    {
        set.seed(i)
        y = rnorm(1,mean = x[i - 1],sd = std)
        p_accept = f(y) * pnorm(x[i - 1],y,std) / pnorm(x[i - 1],y,std) / (f(x[i - 1]))
        if(p_accept == Inf) {p_accept = 99999}
        if(p_accept == -Inf) {p_accept = -99999}
            
        if ((u[i] <= p_accept))  
        {  
            x[i] = y
        }  
        else{ 
            x[i] = x[i - 1] }  
    }
    return(x)
}

f <- function(x)
{
    return(0.5*x**2*exp(-x))
}


####################################################Simulation
res <- EM_SVM(x3,y3,alpha = 1.5,mu = 0.01,iter = 8000)
decision <- Predict(x3,res[[1]])
label <- sign(decision)
label[which(label == -1)] <- levels(y3)[2]
label[which(label == 1)] <- levels(y3)[1]
sum(label == y3)/nrow(x3)
Plot_beta0(res[[2]])

res <- ECME_SVM(x3,y3,alpha = 2,mu = 0.01,amu = 0.1,bmu = 0.1,iter = 1000)
decision <- Predict(x3,res[[1]])
label <- sign(decision)
label[which(label == -1)] <- levels(y3)[2]
label[which(label == 1)] <- levels(y3)[1]
sum(label == y3)/nrow(x3)
Plot_beta0(res[[2]])

res <- MCMC_SVM(x3,y3,alpha = 1,mu = 0.01,amu = 0.1,bmu = 0.1,iter = 500)
decision <- Predict(x3,res[[1]])
label <- sign(decision)
label[which(label == -1)] <- levels(y3)[2]
label[which(label == 1)] <- levels(y3)[1]
sum(label == y3)/nrow(x3)
Plot_beta0(res[[2]])

####################################################Spam
res <- EM_SVM(x2,y2,alpha = 2,mu = 0.01,iter = 300)
decision <- Predict(x2,res[[1]])
label <- sign(decision)
label[which(label == -1)] <- levels(y2)[2]
label[which(label == 1)] <- levels(y2)[1]
sum(label == y2)/nrow(x2)
dev.new()
lapply(1:ncol(res[[2]]),Plot_beta0_One,temp = res[[2]])
# Plot_beta0_One(1,res[[2]])

res2 <- ECME_SVM(x2,y2,alpha = 2,mu = 0.01,amu = 0.1,bmu = 0.1,iter = 300)
decision <- Predict(x2,res2[[1]])
label <- sign(decision)
label[which(label == -1)] <- levels(y2)[2]
label[which(label == 1)] <- levels(y2)[1]
sum(label == y2)/nrow(x2)
dev.new()
lapply(1:ncol(res[[2]]),Plot_beta0_One,temp = res[[2]])


res3 <- MCMC_SVM(x2,y2,mu = 1,iter = 300)
decision <- Predict(x2,res2[[1]])
dev.new()
lapply(1:ncol(res[[2]]),Plot_beta0_One,temp = res[[2]])
####################################################Iris
res <- EM_SVM(x1,y1,alpha = 2,mu = 0.01,iter = 1000)
decision <- Predict(x1,res[[1]])
label <- sign(decision)
label[which(label == -1)] <- levels(y1)[2]
label[which(label == 1)] <- levels(y1)[1]
sum(label == y1)/nrow(x1)
Plot_beta0(res[[2]])

res2 <- ECME_SVM(x1,y1,alpha = 2,mu = 0.01,amu = 0.1,bmu = 0.1,iter = 1000)
decision <- Predict(x1,res2[[1]])
label <- sign(decision)
label[which(label == -1)] <- levels(y1)[2]
label[which(label == 1)] <- levels(y1)[1]
sum(label == y1)/nrow(x1)
Plot_beta0(res2[[2]])

res3 <- MCMC_SVM(x1,y1,mu = 1,iter = 1000)
decision <- Predict(x1,res2[[1]])
Plot_beta0(res2[[2]])
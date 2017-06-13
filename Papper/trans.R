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
    #x <- cbind(rep(1,n),x)
    # colnames(x)[1] <- "intercept"
    pre <- x %*% beta0
    return(pre)
}

####################################################
# error-free Cholesky factorization
# same as chol(A) if A is positive definite, 
# otherwise, try adding to A the positive definite diagnoal matrix mI multiple times
# until Cholesky factorization is applicable
choll <- function(P)
{
    # print("Roll")
    if(length(P) == 0)
    {
        return(0)
    }
    A <- P
    if(length(P) == 1)
    {
        return(1/P)
    }
    m <- 1e-12
    cc <- 0
    while(TRUE)
    {
        e <- tryCatch({res <- chol(P)},error = function(e)return("Error"))
        if(length(e) == 1)
        {
            diag(P) <- diag(A) + m
            cc <- cc + 1
            m <- m * 2
        }else{
            break
        }
    }
    # print("Done Roll")
    return(res)
}

# sample from Inverse Gaussian distribution
# inputs:
# mu, lambda - parameters of the IG distribution
#   supported format:
#     1. two arrays of exactly the same size
#     2. mu is array & lambda is scalar (lambda is reused for each sample)
# n - number of samples per parameter setting (defaults to 1)
# 
# output: 
# R - returned samples ( size(R)==size(squeeze(zeros([size(mu),n]))) )
invnrnd <- function(mu, lambda, n = 1)
{
    if(length(lambda) != 1)
    {
        if(!all(dim(lambda) == dim(mu)))
        {
            print("Error")
            return
        }
        lambda <- array(rep(lambda,n),dim = c(dim(lambda),n))
    }
    
    if(n > 1)
    {
        mu <- array(rep(mu,n),dim = c(dim(mu),n))
    }
    
    dimtemp <- dim(mu)
    set.seed(11)
    nu <- matrix((rnorm(dimtemp[1] * dimtemp[2])),ncol = dimtemp[2])
    y = nu ^ 2
    muy = y * mu
    
    x = mu * (1 + (muy - sqrt(muy * (4 * lambda + muy))) / (2 * lambda))
    R = x
    set.seed(20)
    elsind <- ((matrix(runif(dimtemp[1] * dimtemp[2],0,1),ncol = dimtemp[2]) * (mu + x)) > mu)
    R[elsind,] <- (mu[elsind,] ^ 2) / x[elsind,]
    return(R)
}

# objective function in the binary linear SVM problem
# 
# inputs:
# X - data matrix (K*N, data stored column-wisely)
# y - label vector (N*1, '-1' for negative, '1' for positive)
# w - weight vector (K*1)
# lambda - regularization constant
# ell - margin parameter (usually would be 1)
# 
fobj <- function(x,y,w,lambda,ell)
{
    res <- 0.5 * lambda * sum(w ^ 2) + 2 * sum(max(0, ell - t(y) * t(w) %*% x))
    return(res)
}
####################################################
# inputs:
# X - data matrix (K*N, data stored column-wisely)
# y - label vector (N*1, '-1' for negative, '1' for positive)
# XX, yy - same for test or validation data
# lambda - regularization constant
# ell - margin parameter (usually would be 1)
# nepoch - number of epochs in Gibbs sampling
# burnin - discard the first 'burnin' samples
# emormc - use EM (0) or MCMC sampling (1)
# iw - initial value of w

# output: 
# w - the last sample in EM or the averaged sample in MCMC
# fvals - objective function values of single samples during the iteration
# accu - test or validation accuracy of single samples during the iteration
# mfval, macc - same for averaged samples (after burnin)
# iw - initial value of w
emsvm <- function(x, y, lambda, iw = NULL, ell = 1, nepoch = 200)
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
    
    if(is.null(iw)) # iw have no initial value
    {
        invsigma = diag(lambda * rep(1,k))
        mu = ginv(invsigma) %*% (t(x) %*% y_num)
        R = choll(invsigma)
        iw = mu + ginv(R) %*% matrix((rnorm(k)),ncol = 1)
    }
    w = iw
    mw = matrix(rep(0,k),ncol = 1)
    record <- matrix(nrow = nepoch,ncol = k ) # Temporary variables

    for(i in 1:nepoch)
    {
        sprintf('loop %dth', i)
        
        invgamma = 1 / t(abs(ell - t(y_num) * t(x %*% w)))
        indinf = which(invgamma == Inf,arr.ind = TRUE)
        if(length(indinf) != 0)
        {
            invgamma[indinf] = max(invgamma[-indinf]) ^ 2
        }
        
        tig = t(sqrt(invgamma))
        sX = t(x) * tig[rep(1,k),]
        invsigma = sX %*% t(sX) # to save memory usage (as compared with: X*diag(invlambda)*X')
        diag(invsigma) = diag(invsigma) + lambda # add lambda to diagonal entries
        R = choll(invsigma)
        mu = ginv(invsigma) %*% (t(x) %*% (y_num * (1 + ell * invgamma))) # to save computation time (as compared with: invsigma\(X*(y.*(1+ell*invgamma))))
        
        w = mu
        record[i,] = w
    }
    res <- list(w,record)
    names(res) <- c('w','record')
    return(res)
}

em = emsvm(x3, y3, lambda = 1, ell = 1, nepoch = 200)
pre = Predict(x3,em$w)
label <- sign(pre)
label[which(label == -1)] <- levels(y3)[2]
label[which(label == 1)] <- levels(y3)[1]
sum(label == y3)/nrow(x3)
Plot_beta0(em$record)

####################################################

# inputs:
# X - data matrix (K*N, data stored column-wisely)
# y - label vector (N*1, '-1' for negative, '1' for positive)
# XX, yy - same for test or validation data
# lambda - regularization constant
# ell - margin parameter (usually would be 1)
# nepoch - number of epochs in Gibbs sampling
# burnin - discard the first 'burnin' samples
# emormc - use EM (0) or MCMC sampling (1)
# iw - initial value of w

# output: 
# w - the last sample in EM or the averaged sample in MCMC
# fvals - objective function values of single samples during the iteration
# accu - test or validation accuracy of single samples during the iteration
# mfval, macc - same for averaged samples (after burnin)
# iw - initial value of w
mcmcsvm <- function(x, y, lambda, iw = NULL, ell = 1, nepoch = 200, burnin = 1)
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
    
    if(is.null(iw)) # iw have no initial value
    {
        invsigma = diag(lambda * rep(1,k))
        mu = ginv(invsigma) %*% (t(x) %*% y_num)
        R = choll(invsigma)
        iw = mu + ginv(R) %*% matrix((rnorm(k*1)),ncol = 1)
    }
    w = iw
    mw = matrix(rep(0,k*1),ncol = 1)
    record <- matrix(nrow = nepoch,ncol = k ) # Temporary variables
    
    for(i in 1:nepoch)
    {
        sprintf('loop %dth', i)
        invgamma = 1 / t(abs(ell - t(y_num) * t(x %*% w)))
        indinf = which(invgamma == Inf,arr.ind = TRUE)
        if(length(indinf) != 0)
        {
            invgamma[indinf] = max(invgamma(-indinf)) ^ 2
        }
        
        invgamma = invnrnd(invgamma, 1)
        
        tig = t(sqrt(invgamma))
        sX = t(x) * tig[rep(1,k),]
        invsigma = sX %*% t(sX) # to save memory usage (as compared with: X*diag(invlambda)*X')
        diag(invsigma) = diag(invsigma) + lambda # add lambda to diagonal entries
        R = choll(invsigma)
        mu = ginv(invsigma) %*% (t(x) %*% (y_num * (1 + ell * invgamma))) # to save computation time (as compared with: invsigma\(X*(y.*(1+ell*invgamma))))
        
        set.seed(i)
        temp <- matrix((rnorm(k*1)),ncol = 1)
        w = mu + ginv(R) %*% temp
        if(i > burnin)
        {
            mw = mw + (w - mw) / (i - burnin)
        }
        record[i,] = mw
    }
    res <- list(mw,record)
    names(res) <- c('w','record')
    return(res)
}

mcmc = mcmcsvm(x3, y3, lambda = 1, ell = 1, nepoch = 600, burnin = 1)
pre = Predict(x3,mcmc$w)
label <- sign(pre)
label[which(label == -1)] <- levels(y3)[2]
label[which(label == 1)] <- levels(y3)[1]
sum(label == y3)/nrow(x3)
Plot_beta0(mcmc$record)

####################################################


sssvm <- function(x, y, lambda, p, ell = 1, nepoch = 200)
{
    k <- ncol(x) # The number of the variables                               
    n <- nrow(x) # The number of the samples  
    if(class(x) != "matrix")
    {
        x <- as.matrix(x, ncol = k)
    }
    # Change the label into {-1,1};the second type is -1
    y_num <- matrix(rep(1, length(y)), ncol = 1)
    y_num[which(levels(y)[2] == y),] <- -1
    
    if(length(p) == 1)
    {
        p <- p * matrix(rep(1,k),ncol = 1)
    }
    invsigma = diag(lambda * rep(1,k))
    Xmym1pil = t(x) %*% y_num
    mu = ginv(invsigma) %*% Xmym1pil
    set.seed(17)
    gamma = (matrix((runif(k,0,1)),ncol = 1) < p)
    
    R = choll(invsigma[gamma,gamma])
    mu_ = ginv(invsigma[gamma,gamma]) %*% Xmym1pil[gamma]
    wopt = mu
    
    fopt = fobj(t(x), y_num, wopt, lambda, ell)
    w = matrix(rep(0,k),ncol = 1)
    
    record <- matrix(nrow = nepoch,ncol = k ) # Temporary variables
    for(i in 1:nepoch)
    {
        # sampling w
        w[!gamma] <- 0
        nnz <- length(which(gamma == 1))
        set.seed(i)
        w[gamma] = mu_ + ginv(R) %*% matrix(rnorm(nnz),ncol = 1)
        ftemp = fobj(t(x),y_num,w,lambda,ell)
        if(ftemp < fopt)
        {
            fopt <- ftemp
            wopt <- w
        }
        sprintf('loop %dth: fobj = %.4f, |gamma|=%d', i, ftemp, nnz)
        # sampling gamma
        for(j in 1:k)
        {
            gamma[j] <- TRUE
            invsigma_1 <-  invsigma[gamma,gamma]
            R1 <- choll(invsigma_1)
            tismm_1 <- Xmym1pil[gamma]
            mu_1 = ginv(invsigma_1) %*% tismm_1
            
            gamma[j] <- FALSE
            if(any(gamma == 1))
            {
                invsigma_0 <- invsigma[gamma,gamma]
                R0 <- choll(invsigma_0)
                tismm_0 <- Xmym1pil[gamma]
                mu_0 <-  ginv(invsigma_0) %*% tismm_0
            }else{
                invsigma_0 <- 0
                R0 <- 0
                tismm_0 <- 0
                mu_0 <- 0
            }
            
            set.seed(j)
            gamma[j] <- runif(1,0,1) * (1 + p[j] / (1 - p[j]) * prod(c(diag(R0),1) / diag(R1)) * exp(0.5 * (t(mu_1) %*% tismm_1 - t(mu_0) %*% tismm_0)))
            gamma[j] <- (gamma[j] > 1)
        }
        
        # sampling invlambda
        invlambda = invnrnd(1 / t(abs(ell - t(y_num) * t(x %*% w))), 1)
        Xmym1pil = t(x) %*% (y_num * (1 + ell * invlambda))
        
        til = t(sqrt(invlambda))
        sX = t(x) * til[rep(1,k),]
        invsigma = sX %*% t(sX) # to save memory usage (as compared with: X*diag(invlambda)*X')
        diag(invsigma) = diag(invsigma) + lambda # add lambda to diagonal entries
        invsigma_ = invsigma[gamma,gamma]
        R = choll(invsigma_)
        mu_ = ginv(invsigma_) %*% Xmym1pil[gamma]
        
        record[i,] <- wopt
    }
    res <- list(wopt,record)
    names(res) <- c('w','record')
    return(res)
}

ss = sssvm(x3, y3, lambda = 2, p = 0.7, ell = 1, nepoch = 300)
pre = Predict(x3,ss$w)
label <- sign(pre)
label[which(label == -1)] <- levels(y3)[2]
label[which(label == 1)] <- levels(y3)[1]
sum(label == y3)/nrow(x3)
Plot_beta0(ss$record)

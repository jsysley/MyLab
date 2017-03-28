dir_path <- "F:/git/trial/R/Pre/Face_Recognition"
if(!require(bmp))install.packages("bmp")
require(bmp)
read_bmp <- function(dir_path,i)
{
    file_name <- paste(dir_path,"/face",'/f',i,'.bmp',sep="")
    res <- read.bmp(file_name)
    return(res[,,1])
}
face_data <- t(sapply(1:11, read_bmp,dir_path=dir_path))
dim(face_data)#每列为一张

if(!require(pixmap))install.packages("pixmap")
require(pixmap)

#Plot the “average” face
m <- colMeans(face_data)
pr=pixmapRGB(matrix(m,nrow=243),ncol=320)
plot(pr)
#Center the data by columns
face_data_ss <- t(apply(face_data,1,function(x)return(x-m)))
#Calculate the correlation matrix
A <- face_data_ss%*%t(face_data_ss)
#Calculate the eigenvalues and eigenvextors
eigen_value <- eigen(A)

#Plot the pca components plot
par(mfrow=c(3,4))
xnew <- m
pr=pixmapRGB(matrix(xnew,nrow=243),ncol=320)
plot(pr)
for(i in 1:11)
{
    v <- t(face_data_ss)%*%eigen_value$vector[,i]/sqrt(abs(eigen_value$values[i]))
    b <- t(v)%*%face_data_ss[2,]
    xnew <- xnew+v%*%b
    pr <- pixmapRGB(matrix(xnew,nrow=243,ncol=320))
    plot(pr)
}
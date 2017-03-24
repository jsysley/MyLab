dir_path <- "/Users/jsysley/Desktop/多元统计/Pre/Face_Recognition"
if(!require(bmp))install.packages("bmp")
require(bmp)
read_bmp <- function(dir_path,i)
{
    file_name <- paste(dir_path,"/face",'/f',i,'.bmp',sep="")
    res <- read.bmp(file_name)
    return(res[,,1])
}
face_data <- sapply(1:11, read_bmp,dir_path=dir_path)
dim(face_data)#每列为一张
#每列减去均值
m <- colMeans(face_data)
if(!require(pixmapRGB))install.packages("pixmapRGB")
require(pixmapRGB)
pr=pixmapRGB(matrix(m,nrow=243),ncol=320)
plot(pr)
face_data_ss <- t(apply(face_data,1,function(x)return(x-m)))

#############################################################
#    image 3d  plot 
#                                   wangbinzjcc 2017-3-16
############################################################

###############
data.name <- "F:\\DataW\\PublBoard\\figure4\\data\\elev83.csv"
djt<-read.csv(data.name)
djt<-as.list(djt[,c(1,2,4)])
library(akima)
##
x.min<-min(djt$x)
x.max<-max(djt$x)
##
y.min<-min(djt$y)
y.max<-max(djt$y)
##
djt$z<-djt[[3]]
z.min<-round(min(djt$z))
z.max<-max(djt$z)
##
n_1= 10
xo=seq(x.min+0.2,x.max-0.2, length=(x.max-x.min) * n_1)
yo=seq(y.min+0.2,y.max-0.2, length=(y.max-y.min) * n_1)

djt.smooth <- with(djt, interp(x, y, z, xo=xo, yo=yo,linear=T))
##

#
z<-djt.smooth$z
dim(z)/n_1
x <- (1:nrow(z))/n_1
y <- (1:ncol(z))/n_1

#########
###########

# 
# require(datasets)
# require(grDevices)
# require(graphics)


 ## (3) Now something more complex
 z0 <- min(z) - 5
 z <- rbind(z0, cbind(z0, z, z0), z0)
 x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
 y <- c(min(y) - 1e-10, y, max(y) + 1e-10)
 fill <- matrix("green3", nrow = nrow(z)-1, ncol = ncol(z)-1)
 fill[ , i2 <- c(1,ncol(fill))] <- "gray"
 fill[i1 <- c(1,nrow(fill)) , ] <- "gray"
 ## `image like' colors on top :
 fcol <- fill

 zi <- djt.smooth$z[ -1,-1] + djt.smooth$z[ -1,-ncol(djt.smooth$z)] +
           djt.smooth$z[-nrow(djt.smooth$z),-1] + djt.smooth$z[-nrow(djt.smooth$z),-ncol(djt.smooth$z)]  ## / 4

 fcol[-i1,-i2] <-
     terrain.colors(20)[cut(zi,
                            stats::quantile(zi, seq(0,1, length.out = 21)),
                            include.lowest = TRUE)]

####


#############################################################

windowsFonts(Times = windowsFont("Times New Roman"))   
jpeg('F:\\DataW\\PublBoard\\figure4\\弄岗样地2017.jpg', family="Times",   
     pointsize=12,  width = 40, height = 30, units = "cm",  
     res=600) 


par(mex=0.5,mar=c(5.1, 6.3,1,1))
persp(x, y, z, theta =  21, phi = 71,
      expand = 1.65 , col = fcol, scale = FALSE,
      ltheta = -100 , shade = 0.5, border = NA,
      box = T,ticktype = "detailed", 
      xlab = "\n西-东 W-E (m)",
      ylab = "\n南-北 S-N (m)", 
      zlab ="\n海拔\n Elevation (m)",
  #     xlab = "\n\n西-东(m)", 
  #     ylab = "\n\n南-北(m)", 
  #     zlab ="\n\n海拔(m)",
      cex.axis=1.4, cex.lab=1.6)



dev.off()
#  
##############################################################




 
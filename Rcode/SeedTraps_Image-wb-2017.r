##########


dat.nam <- 'F:\\DataW\\PublBoard\\figure4\\data\\种子雨坐标2017.csv'
seed.dat <- read.csv(dat.nam)
# head(seed.dat)

###############
djt<-read.csv("elev83.csv")
head(djt)
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
djt.smooth <- with(djt, interp(x, y, z, xo=seq(x.min+0.2,x.max-0.2, length=(x.max-x.min)),
                               yo=seq(y.min+0.2,y.max-0.2, length=(y.max-y.min)),linear=T))
##
zz<-djt.smooth$z
xx <- (1:nrow(zz))
yy <- (1:ncol(zz))
############### 
jpeg("F:\\DataW\\PublBoard\\figure4\\种子雨布设图2017.jpg",
     width = 50, height = 35,
     units = "cm",
     pointsize = 10,
     res = 300)
par(mar=c(3.5, 3.0, 1.5, 1.5) )
image(xx, yy,zz, col = terrain.colors(100), axes = F,
      xlab="",ylab="",cex.lab=5)
contour(xx, yy,zz, levels = seq(z.min, z.max, by = 15),
        xlim = c(18, 482),ylim = c(10, 289),
        axes = F,xlab="",ylab="",labcex=2,lwd = 3,
        add = T, col = "peru")
axis(1, at = seq(0.5 , 500.5, by = 20), labels =seq(0, 500, by = 20), cex.axis=2,padj=1)
axis(2, at = seq(0.5 , 300.5, by = 20), labels =seq(0, 300, by = 20), cex.axis=2,padj=0 )
box()
abline(v=seq(20.5,480,len=24), h=seq(20.5,280,len=14),lty=3,cex=5,lwd=2 )
points(seed.dat$xx,seed.dat$yy, pch=22,bg=4, cex=4, col=4, lwd=3)
# mtext("种子雨收集器和幼苗框  Seed traps and seedling subplots",side=1,line= 15 ,cex=10 )
# mtext('西-东 W-E (m)', side = 1,cex=3, padj=3)
# mtext('南-北 S-N (m)', side = 2,cex=3, padj=-2.2)

dev.off()


################ 

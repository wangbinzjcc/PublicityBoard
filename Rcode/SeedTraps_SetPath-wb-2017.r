setwd("F:\\DataW\\PublBoard\\figure4")


jpeg("种子幼苗说明2017.jpg",width = 30, height = 30,
     units = "cm", pointsize = 8, res = 600 )
par(mar=c(5,5,0.5,0.5) )
plot(1:19, type="n", xlab='', ylab='', axes=F)
box(lwd=4)
# grid()
rect(10-0.5, 10-0.5, 10.81-0.35, 10.81-0.35, col="blue", border="black", lwd =3) 
rect(10-0.5, 10+2-0.25, 10+1-0.5, 10+2+1-0.25, col="red", border="black", lwd =3) 
rect(10+2-0.5, 10-1-0.5, 10+2+1-0.5, 10-1+1-0.5, col="red", border="black", lwd =3) 
rect(10-2-0.5, 10-1-0.5, 10-2+1-0.5, 10-1+1-0.5, col="red", border="black", lwd =3) 
rect(2.9-1, 17.1-1, 2.9+1, 17.1+1, col="red", border="black", lwd =3) 
rect(17.1-1, 2.9-1, 17.1+1, 2.9+1, col="red", border="black", lwd =3) 
arrows(10, 10, 2.9, 17.1, angle= 10, code=2, length = 0.5, lwd =3) 
text(6,13-0.2,"10 m", cex=3)
text(3,15.7-0.2,'幼苗框', cex=3)
text(3,14.9-0.2,'Seedling plot', cex=3)  
text(3,14-0.2,expression(paste("( ",4*~m^2," )")), cex=3)
text(13.9+1,9.5+0.1,'幼苗框', cex=3)  
text(14.4+1,8.78,'Seedling plot', cex=3)  
text(13.8+1,8,expression(paste("( ",1*~m^2," )")), cex=3)
text(10,8.8-0.4,'种子\n收集器', cex=3)  
text(10,7.7-0.7,'Seed trap', cex=3)  
text(10,7-0.8,expression(paste("( ",0.5*~m^2," )")), cex=3)
arrows(10, 10, 10, 12.25, angle= 12, code=2, length = 0.5,lwd=3) 
text(10.8,11,"2 m", cex=3)
 mtext('20 m', side = 1,cex=4, padj=1.5)
 mtext('20 m', side = 2,cex=4, padj=-1)

dev.off()


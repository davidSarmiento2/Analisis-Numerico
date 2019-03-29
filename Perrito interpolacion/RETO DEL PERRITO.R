rm(list=ls())

x=c(1,2,5,6,7.5,8.1,10,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29,30)     
y=c(3,3.7,3.9,4.5,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3)

length(x)
length(y)
#plot(x,y, pch=19, cex=1, col = "", asp=1,xlab="X", ylab="Y", main="PERRITO ") 


funcion<-splinefun(x,y,method="monoH.FC",ties=mean)
print(funcion(y))

curve(funcion(x),add = TRUE, col = "black", n = 1001)
par(new = TRUE)

a=c(3.6,7.7,9.2,11.6,9.7,14,15.5,18.7,20.2,25,26.7,28.9)
b=c(2.6,2.3,3,1.7,1.3,1.3,1.5,1.4,1.3,0.7,0.9,1.6)+0.3

funcion2<-splinefun(a,b,method="monoH.FC",ties=mean)
curve(funcion2(x),add = TRUE, col = "black", n = 1001)

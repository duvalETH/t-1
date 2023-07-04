n<-200
e=rnorm(200,0,5)
a<-11;b<-3
i<-1:n
start<-1
step<-0.1
x<-start+step*i
y<-a+b*x+e
plot(x,y)


b0<-(mean(x*y)-(mean(x)*mean(y)))/(mean(x*x)-mean(x)^2)
b0
a0<-mean(y)-b0*mean(x)
a0
y0<-a0+b0*x
S2<- (1/(n-2))*sum((y-y0)^2)
S2

S2b=S2/sum((x-mean(x))^2)
S2b

S2a<-mean(x^2)*S2b
S2a

R2<-1-(sum((y-y0)^2)/sum((y-mean(y))^2))
R2

F<-(R2/(1-R2))*(n-2)
Fa<-qf(0.95,1,n-2)
F>Fa

xy=data.frame(cbind(x,y))
reg=lm(y~x,data=xy)
summary(reg)

ta=a0/sqrt(S2a)
ta
t<-qt(0.975,n-2)
ta>t


int=predict(reg,interval='conf',level=0.95)
matlines(xy$x,cbind(int),lty=c(1,2,2),col=c(2,4,4))

intpred=predict(reg,interval='pred',level=0.95)
matlines(xy$x,cbind(intpred),lty=c(1,2,2),col=c(2,3,3))


x1=seq(min(xy[,"x"]),max(xy[,"x"]),length=n)
grid=data.frame(x1)


int=predict(reg,new=grid,interval='conf',level=0.95)
matlines(grid$x1,cbind(int),lty=c(1,2,2),col=c(2,4,4))


intpred=predict(reg,new=grid,interval='pred',level=0.95)
matlines(grid$x1,cbind(intpred),lty=c(1,2,2),col=c(2,3,3))
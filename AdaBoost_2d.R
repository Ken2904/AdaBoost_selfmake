#学習データを作る
n=2000
Y=function(x){(sin(10*x))}
#Y=function(x){(sin(20*x))}
x=runif(n,-1,1)
y=runif(n,-1,1)
label=sign(y-Y(x))
ix=which(label==1)
plot(x[ix],y[ix],col=2,ylim=c(-1,1),xlim=c(-1,1))
points(x[-ix],y[-ix],col=4)
points(seq(-1,1,0.01),Y(seq(-1,1,0.01)),type="l")
#### prepare data ##########
#f=function(para){
#  yy=para[1]*x+para[2]
#  return(sign(y-yy))
#}
f2=function(para){
  ix=which(sign(y-(para[1]*x+para[2]))!=label)
  return(sum(w[,t][ix])/sum(w[,t]))
}

i=200#学習回数
#データを保存する領域を確保
minf=numeric(i)
para=matrix(NA,i,2)
a=numeric(i)
w=matrix(NA,n,i)
w[,1]=rep(1/n,n)

#学習
for(t in 1:i){
  print(t)
  if(t==1) res1=optim(c(0,0),fn=f2,method="SANN")#min(f)のパラメータ推定
  else res1=optim(para[t-1,],fn=f2,method="SANN")
  para[t,]=res1$par
  minf[t]=f2(para[t,])
  a[t]=(0.5)*log((1-minf[t])/minf[t])#alphaを求める
  if(t==i) {break}
  w[,t+1]=w[,t]*exp(-a[t]*f(para[t,])*label)#wの更新
}
#sign(sum(f))を算出
f3=function(x,y,para){
  return(sign(y-para[1]*x+para[2]))
}
result=function(x,y,para){
    ff=function(j){(a[j]*sign(y-para[j,1]*x+para[j,2]))}
    return(sign(sum(sapply(1:i,ff))))
}
#テストデータを作る
n=2000
x=runif(n,-1,1)
y=runif(n,-1,1)

#Tの値毎に正解率を求める
TT=i
rate=numeric(TT)
for(i in 1:TT){
 fff=function(k){result(x[k],y[k],para)}
 lab=sapply(1:n,fff)
 anslab=sign(y-Y(x))
 rate[i]=sum(anslab==lab)
 print(i)
}
rate=rate/2000

#結果のplot
plot(1:200,rate,type="l",ylab="rate",xlab="t",main=" correct answer rate")

ix=which(lab==1)
plot(x[ix],y[ix],col=2,ylim=c(-1,1),xlim=c(-1,1),main="res")
points(x[-ix],y[-ix],col=4)
points(seq(-1,1,0.01),Y(seq(-1,1,0.01)),type="l")


plot(1:length(a),a,type="l",ylab="value of a",xlab="a",main="value of a")
plot(1:length(minf),minf,type="l",main="minimun value of f_t(x)",ylab="minimun value of f_t(x)",xlab="t")



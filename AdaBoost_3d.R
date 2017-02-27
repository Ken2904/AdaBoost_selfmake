#### prepare data ##########
n=2500
Z=function(x,y){(x*sin(10*x))+(y*cos(10*y)+(x*y))}
x1=seq(-1,1,0.008)
y1=seq(-1,1,0.008)
z1=outer(x1,y1,Z)
persp3d(x=seq(-1,1,0.008),y=seq(-1,1,0.008),z1,alpha=0.4,zlim=c(-1,1))
x2=runif(n,-1,1)
y2=runif(n,-1,1)
z2=runif(n,-1,1)
label=sign(z2-Z(x2,y2))
ix=which(label==1)
points3d(x2[ix],y2[ix],z2[ix],col=2)
points3d(x2[-ix],y2[-ix],z2[-ix],col=4)
#### prepare data ##########

f2=function(para){
  ix=which(sign(z2-((para[1]*x2)+(para[2]*y2)+(para[3])))!=label)
  return(sum(w[,t][ix])/sum(w[,t]))
}

i=1000

minf=numeric(i)
para=matrix(NA,i,3)
a=numeric(i)
w=matrix(NA,n,i)
w[,1]=rep(1/n,n)
for(t in 1:i){
  print(t)
  if(t==1){res1=optim(c(0,0,0),fn=f2,method="SANN")}
  else {res1=optim(para[t-1,],fn=f2,method="SANN")}
  para[t,]=res1$par
  minf[t]=f2(para[t,])
  a[t]=(0.5)*log((1-minf[t])/minf[t])
  if(t==i) {break}
  w[,t+1]=w[,t]*exp(-a[t]*(sign(z2-((para[t,1]*x2)+(para[t,2]*y2)+para[t,3])*label)))
  cat(a[t])
}

result=function(x,y,z,para){
  ff=function(j){(a[j]*sign(z-(para[j,1]*x+para[j,2]*y+para[j,3])))}
  return(sapply(1:i,ff))
}


n=2000
x2=runif(n,-1,1)
y2=runif(n,-1,1)
z2=runif(n,-1,1)
ft=result(x2,y2,z2,para)
labf=sign(rowSums(ft))
anslab=sign(z2-Z(x2,y2))

TT=i
rate=numeric(TT)
for(i in 1:TT){
  if(i == 1){lab=sign(ft[,1])}
  else{lab=sign(rowSums(ft[,1:i]))}
  rate[i]=sum(anslab==lab)
  print(i)
}
rate=rate/n

#結果のplot
plot(1:i,rate,type="l",ylab="rate",xlab="t",main=" correct answer rate",ylim=c(0,1))

ix=which(labf==1)
persp3d(x=seq(-1,1,0.008),y=seq(-1,1,0.008),z1,alpha=0.4,xlim=c(-1,1),ylim=c(-1,1),zlim=c(-1,1))
points3d(x2[-ix],y2[-ix],z2[-ix],col=4)
points3d(x2[ix],y2[ix],z2[ix],col=2)


plot(1:length(a),a,type="l",ylab="value of a",xlab="t",main="value of a",ylim=c(0,1))
plot(1:length(minf),minf,type="l",main="minimun value of f_t(x)",ylab="minimun value of f_t(x)",xlab="t",ylim=c(0,0.5))


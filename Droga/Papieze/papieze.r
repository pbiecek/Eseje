L2=scan("http://oce.catholic.com/index.php?title=List_of_Popes",what="character")

index=which(L2=="</td><td>Reigned")
X=L2[index+1]
Y=strsplit(X,split="-")

diffyears=function(x){
   s=NA
   if(sum(substr(x,1,1)=="c")>0){x[substr(x,1,1)=="c"]=substr(x[substr(x,1,1)=="c"],3,nchar(x[substr(x,1,1)=="c"]))}
   if(length(x)==1){s=1}
   if(length(x)==2){s=diff(as.numeric(x))}
   return(s)}
whichyearsbeg=function(x){
   s=NA
   if(sum(substr(x,1,1)=="c")>0){x[substr(x,1,1)=="c"]=substr(x[substr(x,1,1)=="c"],3,nchar(x[substr(x,1,1)=="c"]))}
   if(length(x)==1){s=as.numeric(x)}
   if(length(x)==2){s=as.numeric(x)[1]}
   return(s)}
whichyearsend=function(x){
   s=NA
   if(sum(substr(x,1,1)=="c")>0){x[substr(x,1,1)=="c"]=substr(x[substr(x,1,1)=="c"],3,nchar(x[substr(x,1,1)=="c"]))}
   if(length(x)==1){s=as.numeric(x)}
   if(length(x)==2){s=as.numeric(x)[2]}
   return(s)}

Years=unlist(lapply(Y,whichyearsbeg))
 YearsB=c(Years[1:91],752,Years[92:length(Years)])
 YearsB[187]=1276
 Years=unlist(lapply(Y,whichyearsend))
 YearsE=c(Years[1:91],752,Years[92:length(Years)])
 YearsE[187]=1276
 YearsE[266]=2013
 YearsE[122]=914 
 W=unlist(lapply(Y,diffyears))
 W=c(W[1:91],1,W[92:length(W)])
 W[W==-899]=1
 which(is.na(W))

 W[187]=1
 W[266]=2013-2005



plot(YearsB,W,type="h")
plot(YearsB,W,type="s",xlim=c(1800,2000))


plot(YearsB,cumsum(W) - YearsB,type="s")


 n=200
 YEARS = seq(0,2000,length=n)
 Z=rep(NA,n)
 for(i in 2:(n-1)){
   index=which((YearsB>YEARS[i]-50)&(YearsE<YEARS[i]+50))
   Z[i] = mean(W[index])}
 plot(YEARS,Z,type="l",ylim=c(0,30))
 n=50
 YEARS = seq(0,2000,length=n)
 Z=rep(NA,n)
 for(i in 2:(n-1)){
   index=which((YearsB>YEARS[i]-50)&(YearsE<YEARS[i]+50))
   Z[i] = mean(W[index])}
 lines(YEARS,Z,type="l",col="grey")



library(XML)
library(wordcloud)

zz = readHTMLTable("http://oce.catholic.com/index.php?title=List_of_Popes")

pops <- zz[[4]][,2]
pops <- as.character(pops)

wektor <- sapply(strsplit(pops, split=", "), '[', 2)
wektor <- as.character(factor(wektor))
wektor[is.na(wektor)] = ""
wektor[265] = "Blessed"

l.pp <- table(YearsB %/% 100)
l.y <- sapply(l.pp, function(x) 1:x)

Iwektor <- sapply(strsplit(pops, split=" [XVI]"), '[', 1)
Iwektor <- sapply(strsplit(Iwektor, split=","), '[', 1)
sort(summary(factor(Iwektor)))

Iwektor <- sapply(strsplit(pops, split=","), '[', 1)

pdf("lpapiezeK.pdf",16,5)
par(mar=c(3,5,1,1))
plot(YearsB %/% 100, YearsB %% 100, yaxt="n", xaxt="n", ylab="", xlab="", pch=19, bty="n", type="n")
text(YearsB %/% 100, YearsB %% 100, Iwektor, col=c("grey", "red3", "blue2", "orange")[factor(wektor)], cex=0.6, adj=c(0,0))
axis(2, seq(0,100,10), las=1)
axis(1, 0:20, c("I", "II", "III", "IV", "V", "VI","VII","VIII","IX","X",
"XI", "XII", "XIII", "XIV", "XV", "XVI","XVII","XVIII","XIX","XX","XXI"), las=1)
dev.off()



zaznacz <- !duplicated(YearsB %/% 100)

pdf("papiezeK.pdf",30,8)
par(mar=c(8,1,1,1))
(pp <- barplot(diff(c(YearsB,2013))+0.5, col="grey", border="grey"))
abline(v=pp[which(zaznacz)], lty=3)
text(pp[which(zaznacz)]-0.5, 30, YearsB[zaznacz], srt=90, cex=0.8,adj=c(0.5,-0.1))
axis(1,pp, Iwektor, las=3)
dev.off()





CairoSVG("lpapiezy.svg",8,6)
plot(unlist(l.y), YearsB %/% 100, yaxt="n", xlab="liczba papie¿y", ylab="wiek", 
     pch=15, cex=1.8, xaxt="n", bty="n", 
     col=c("grey","red3","blue2", "orange")[as.numeric(factor(wektor))], xlim=c(1,30))
axis(2, 0:20, c("XX",paste(1:20,"XX",sep="")),las=1)
axis(1,c(0,5,10,15,20))
legend("topright", c("Papie¿", "B³ogos³awiony", "Œwiêty", "Aposto³"), fill=c("grey","blue2", "orange","red3"), bty="n")
dev.off()



prop.table(summary(factor(wektor)))

wektor <- sapply(strsplit(pops, split=" [XVI]"), '[', 1)
wektor <- sapply(strsplit(wektor, split=","), '[', 1)
sort(summary(factor(wektor)))


CairoSVG("imionap.svg",10,6)
par(mar=c(5,5,2,8))
imiona = c("John", "Benedict", "Gregory", "Clement", "Innocent")
kolory = c("black", "red3", "green3", "blue3", "orange3")
plot(YearsB, cumsum(wektor == imiona[1]), type="s", lwd=3, las=1, xlab="rok wybrania", ylab="liczba papie¿y o danym imieniu")
for (i in 2:length(imiona))
  lines(YearsB, cumsum(wektor == imiona[i]), type="s", col=kolory[i], lwd=3)
axis(4,table(wektor)[imiona], paste(imiona, ", ", table(wektor)[imiona], sep=""),las=1)
dev.off()



library(Cairo)


CairoSVG("swiety.svg",8,3)
par(mar=c(1,10,1,1))
plot(c(1:25,1:25,1:25,1:25,1:25,1:25,1:25,1:18,1:5,1:25,1:25,1:17,1),
     rep(c(0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4),c(25,25,25,25,25,25,25,18,5,25,25,17,1)),
     , pch=15,yaxt="n", ylab="", xlab="", col=rep(c("black","orange","gold3","red3"),c(193,5,67,1)), bty="n",xaxt="n",cex=1.8)
axis(2,c(0.7,1.6,1.9,2.4),c("Pope (196)","Pope, Blessed (5)","Pope, Saint (67)","Pope, Apostole (1)"),las=1)
dev.off()

ts <- table(factor(wektor))

CairoSVG("chmura.svg",12,9)
wordcloud(paste(names(ts), " x", ts,sep=""), ts, min.freq=0, rot.per=0, random.order=FALSE)
dev.off()


library(survival)
pdf("czaszycia.pdf",8,5)
zmarl <- !((1:266) %in% c(266, 193))
par(mar=c(4,5,2,2))
plot(survfit(Surv(time=W, event=zmarl)~1), conf.int=0, las=1, ylim=c(0,1), xlab="lat zycia po wyborze na papieza", ylab="% papiezy",yaxt="n", bty="n",yaxs="i")
sapply(c(0.25,0.5,0.75), function(q) {
  lines(c(0,quantile(W,q)),1-c(q,q),lty=3)
  lines(c(quantile(W,q),quantile(W,q)),c(0,1-q),lty=3)
})
axis(1,quantile(W,c(0.25,0.5,0.75)))
axis(2,c(0.25,0.5,0.75,0,1),las=1)
dev.off()


CairoSVG("hazard.svg",9,7)
hh <- sapply(1:25, function(a) {
  c(prop.test(sum(W < a+0.5 & W > a-0.5), sum(W > a-0.5))$conf.int,
  prop.test(sum(W < a+0.5 & W > a-0.5), sum(W > a-0.5))$estimate)
})
plot(hh[3,], pch=19, ylim=c(0,0.5),las=1, xlab="rok urzêdowania", ylab="funkcja hazardu")
for(i in 1:ncol(hh)) lines(c(i,i),hh[1:2,i])
dev.off()



hazard.ratio.plot(rep(1,266), survival:::Surv(time=W, event=zmarl), e=20)


library(lawstat)
runs.test((W < 6) + 0)

W2 <- factor(W >= 6, labels=rev(c("6lat lub wiecej", "5 lat lub mniej")))
table(poprzednik = W2[-266], nastepca = W2[-1])

fisher.test(table(poprzednik = W2[-266], nastepca = W2[-1]))

library(randomizeBE)
runs.pvalue(W)


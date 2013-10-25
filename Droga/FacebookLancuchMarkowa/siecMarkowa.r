dane <- read.table("relationship_status_changes.csv", sep="\t",quote='~', h=T)

indPowt <- names(table(dane[,1]))[table(dane[,1]) > 1]

daneRet <- do.call(rbind, sapply(1:length(indPowt), function(i) {
  cbind(as.character(dane[dane[,1] == indPowt[i],3])[-sum(dane[,1] == indPowt[i])],
  as.character(dane[dane[,1] == indPowt[i],2])[-1])
}))

maxDate <- max(as.Date(dane[,4]))

indeksy <- unique(dane[,1])

pary <- do.call(rbind, lapply(1:length(indeksy), function(i) {
  tmp <- dane[dane[,1] == indeksy[i],]
  tmpd <- as.Date(tmp[,4])  
  do.call(rbind, lapply(1:nrow(tmp),function(j) {
    if (j == nrow(tmp)) {
      a3 <- as.character(tmp[j,3])
      delta <- maxDate - tmpd[j]
      data.frame(a=a3, b="", c=delta, stringsAsFactors=F)
    } else {
      a1 <- as.character(tmp[j,2])
      a2 <- as.character(tmp[j+1  ,2])
      a3 <- as.character(tmp[j,3])
      delta <- tmpd[j+1] - tmpd[j]
      if (a2==a3) {
        data.frame(a=a1, b=a3, c=delta, stringsAsFactors=F)
      } else {
        data.frame(a=c(a1,a3), b=c(a3,a2), c=delta*c(0.5,0.5), stringsAsFactors=F)
      }
    }
  }))
}))

t1 <- table(dane[,2], dane[,3])
t2 <- table(daneRet[,1], daneRet[,2])

poziomy <- unique(c(levels(dane[,2]), levels(dane[,3])))
mat <- matrix(0, length(poziomy), length(poziomy))
rownames(mat) <- poziomy
colnames(mat) <- poziomy

mat[rownames(t1), colnames(t1)] <-  mat[rownames(t1), colnames(t1)] + t1
mat[rownames(t2), colnames(t2)] <-  mat[rownames(t2), colnames(t2)] + t2

poziomy <- paste(poziomy, " (", round(tapply(pary[,3],pary[,1], median)[poziomy]), "d)", sep="")
rownames(mat) <- poziomy
colnames(mat) <- poziomy

mat2 <- prop.table(mat,1)
Edges <- data.frame(rep(poziomy, length(poziomy)),
  rep(poziomy, each=length(poziomy)),
  round(as.vector(mat2)*100,1))
Edges <- na.omit(Edges)
Edges <- Edges[Edges[,3]>10,]

library("qgraph")
library("Cairo")
CairoSVG("siec2.svg",13,13)
qgraph(Edges,esize=5,gray=TRUE,
       edge.labels=paste(Edges[,3],"%",sep=""), 
       asize=5)
dev.off()




read.csv("data.csv")
read.table("data.csv",sep="\t")
tabin <- read.table("data.csv",sep="\t")
head(tabin)
tabin <- read.table("data.csv",sep="\t",head=T)
head(tabin)
install.packages("Rtsne")
source("FastPCA.R")
dat
dat <- tabin[,8:ncol(tabin)]
head(dat)
library(Rtsne)
tsne <- Rtsne(FastPCA(dat,30),2,verbose=T,pca=F)
?Rtsne
dim(unique(dat))
dim(dat)
tsne <- Rtsne(FastPCA(unique(dat),30),2,pca=F)
tsne <- Rtsne(FastPCA(unique(dat),20),2,pca=F)
tsne <- Rtsne(FastPCA(unique(dat),100),2,pca=F,perplexity=25)
tsne <- Rtsne(FastPCA(unique(dat),50),2,pca=F,perplexity=17)
tsne <- Rtsne(FastPCA(unique(dat),50),2,pca=F,perplexity=16)
tsne <- Rtsne(FastPCA(unique(dat),50),2,pca=F,perplexity=15)
pca <- FastPCA(unique(dat),25)
pca <- FastPCA(unique(dat),50)
tsne <- Rtsne(pca$x,2,pca=F)
plot(tsne$Y,pch='.')
head(tabin)
?FastPCA
FastPCA
hist(tabin$hand)
hist(tabin$race)
hist(tabin$age)
hist(tabin$age[tabin$age<100])
hist(tabin$age[tabin$age<100],breaks=100)
hist(tabin$source)
hist(tabin$country)
hist(tabin$engnat)
hist(tabin$gender)
plot(tsne$Y,pch='.')
tsne2 <- Rtsne(dat,2,pca=F,perplexity=5)
tsne2 <- Rtsne(as.matrix(dat),2,pca=F,perplexity=5)
tsne2 <- Rtsne(as.matrix(dat),2,pca=F,perplexity=5,check_duplicates=F)
dev.new()
plot(tsne2$Y,pch='.')
tsne3 <- Rtsne(as.matrix(dat),2,pca=T,perplexity=50,check_duplicates=F)
dev.new()
plot(tsne3$Y,pch='.')
plot(tsne3$Y,pch='.',col=tabin$race)
plot(tsne3$Y,pch='.',col=tabin$age)
plot(tsne3$Y,pch='.',col=tabin$country)
plot(tsne3$Y,pch='.',col=tabin$engnat)
plot(tsne3$Y,pch='.',col=tabin$gender)
plot(tsne3$Y,pch='.',col=tabin$race)
plot(tsne3$Y,pch='.',col=tabin$hand)
plot(tsne3$Y,pch='.',col=tabin$source)
plot(tsne3$Y,pch='.',col=tabin$age)
library(MASS)
kd <- kde2d(tsne3$Y[,1],tsne3$Y[,2], n=50)
countour(kd)
contour(kd)
plot(tsne3$Y,pch='.',col=tabin$gender)
kd.m <- kde2d(tsne3$Y[tabin$gender==1,1],tsne3$Y[tabin$gender==1,2], n=50)
kd.f <- kde2d(tsne3$Y[tabin$gender==2,1],tsne3$Y[tabin$gender==2,2], n=50)
contour(kd.m,add=T,col='red')
contour(kd.f,add=T,col='black')
dev.new()
contour(kd.m,add=T,col='red')
contour(kd.m,add=F,col='red')
contour(kd.f,add=T,col='black')
kd.m <- kde2d(tsne3$Y[tabin$gender==1,1],tsne3$Y[tabin$gender==1,2], n=200)
kd.f <- kde2d(tsne3$Y[tabin$gender==2,1],tsne3$Y[tabin$gender==2,2], n=200)
contour(kd.m,add=F,col='red')
contour(kd.f,add=T,col='black')
plot(tsne3$Y[tabin$gender==1,],pch='.',col=tabin$gender)
plot(tsne3$Y[tabin$gender==1,],pch='.',col=tabin$gender[tabin$gender==1])
contour(kd.m,add=T,col='red')
contour(kd.m,add=T,col='black')
dev.new()
plot(tsne3$Y[tabin$gender==2,],pch='.',col=tabin$gender[tabin$gender==2])
contour(kd.f,add=T,col='black')
?contour
contour(kd.f,add=T,col='black',drawlevels=F)
plot(tsne3$Y[tabin$gender==2,],pch='.',col=tabin$gender[tabin$gender==2])
contour(kd.f,add=T,col='black',drawlevels=F)
?contour
contour(kd.f,add=T,col='black',drawlabels=F)
dev.new()
plot(tsne3$Y[tabin$gender==1,],pch='.',col=tabin$gender[tabin$gender==1])
contour(kd.m,add=T,col='black',drawlabels=F)
dev.new()
ls()
rowSums(dat[,seq(1,100,5)
seq(1,100,5)
ENACO
qs <- read.table("qs.txt",sep="\t",head=F)
qs <- read.table("qs.txt",sep="\t",head=F,quote="")
qs
qs <- read.table("qs.txt",sep="\t",head=F,quote="")
rowSums(dat[,seq(1,100,5)])
dat[,seq(1,100,5)]
seq(1,100,5)
dim(dat)
rowSums(dat[,seq(1,50,5)])
rowSums(dat[,seq(1,50,5)]*qs$V1)
scoreE <- rowSums(dat[,seq(1,50,5)]*qs$V1)
scoreE <- rowSums(dat[,seq(1,50,5)]*qs$V1[seq(1,50,5)])
scoreE
scoreN <- rowSums(dat[,seq(2,50,5)]*qs$V1[seq(2,50,5)])
scoreA <- rowSums(dat[,seq(3,50,5)]*qs$V1[seq(3,50,5)])
scoreC <- rowSums(dat[,seq(4,50,5)]*qs$V1[seq(4,50,5)])
scoreO <- rowSums(dat[,seq(5,50,5)]*qs$V1[seq(5,50,5)])
plot(tsne3$Y,pch='.',cex=scoreE)
plot(tsne3$Y,pch='.',cex=scoreE/10)
plot(tsne3$Y,pch='.',cex=(scoreE-min(scoreE))/10)
hist(scoreE-min(scoreE)))
hist(scoreE-min(scoreE))
hist(scoreE)
hist(scoreE,breaks=100)
hist(scoreE,breaks=50)
hist(scoreE,breaks=20)
hist(scoreE,breaks=25)
hist(scoreN,breaks=25)
hist(scoreA,breaks=25)
hist(scoreC,breaks=25)
hist(scoreO,breaks=25)
hist(scoreO,breaks=50)
hist(scoreE,breaks=25)
hist(scoreN,breaks=25)
hist(scoreA,breaks=25)
hist(scoreC,breaks=25)
hist(scoreO,breaks=50)
hist(dat)
hist(as.matrix(dat))
?kde2d
kd.f
library(KernSmooth)
??KernSmooth
?KernSmooth
library(ks)
install.packages("ks")
library(ks)
b1 <- binning(tsne3$Y,100,100,w=scoreE)
b1 <- binning(tsne3$Y,w=scoreE)
b1
dev.new()
contour(b1)
str(b1)
contour(b1$eval.points)
?binning
contour(cbind(b1$eval.points,b1$w))
str(b1)
contour(cbind(b1$eval.points,b1$counts))
b1 <- kcde(tsne3$Y,w=scoreE)
contour(b1)
str(b1)
contour(cbind(b1$eval.points,b1$estimate)))
contour(cbind(b1$eval.points,b1$estimate))
cbind(b1$eval.points, b1$estimate)
?contour
contour(x=1:151,y=1:151,z=b1$estimate)
b1 <- kde(tsne3$Y,w=scoreE)
str(b1)
image(b1$estimate)
binE <- kde(tsne3$Y,w=scoreE+min(scoreE))
binN <- kde(tsne3$Y,w=scoreN+min(scoreN))
binA <- kde(tsne3$Y,w=scoreA+min(scoreA))
binC <- kde(tsne3$Y,w=scoreC+min(scoreC))
binO <- kde(tsne3$Y,w=scoreO+min(scoreO))
par(mfrow=c(1,5))
image(binO)
image(binO$estimate)
image(binC$estimate)
image(binE$estimate)
image(binA$estimate)
image(binN$estimate)
binW <- kde(tsne3$Y,w=tabin$gender)
dev.new()
image(binW$estimate)
binW <- kde(tsne3$Y,w=tabin$gender-1)
image(binW$estimate)
binW <- kde(tsne3$Y,w=abs(tabin$gender-2))
image(binW$estimate)
binM <- binW
binW <- kde(tsne3$Y,w=tabin$gender-1)
dev.new()
image(binW$estimate)
binEi <- kde(tsne3$Y,w=abs(scoreE-max(scoreE)))
binNi <- kde(tsne3$Y,w=abs(scoreN-max(scoreN)))
binAi <- kde(tsne3$Y,w=abs(scoreA-max(scoreA)))
binCi <- kde(tsne3$Y,w=abs(scoreC-max(scoreC)))
binOi <- kde(tsne3$Y,w=abs(scoreO-max(scoreO)))
dev.new()
par(mfrow=c(2,5))
dev.new()
image(binO$estimate)
par(mfrow=c(2,5))
image(binO$estimate)
image(binOi$estimate)
image(binO$estimate)
par(mfrow=c(2,5))
image(binO$estimate)
image(binC$estimate)
image(binA$estimate)
image(binN$estimate)
image(binE$estimate)
image(binOi$estimate)
image(binCi$estimate)
image(binAi$estimate)
image(binNi$estimate)
image(binEi$estimate)
dev.new()
image(binM$estimate)
dat16 <- read.table("16PF_data.csv",head=T,)
dat16 <- read.table("16PF_data.csv",head=T,quote="")
dat16 <- read.table("16PF_data.csv",head=T,quote="\"")
dim(dat16)
dat16 <- read.table("16PF_data.csv",head=T,quote="\"",fill=T)
dat16[40278,]
dat16[40277,]
dat16[40279,]
image(binO)
image(binO$estimate)
image(binC$estimate)
image(binE$estimate)
image(binA$estimate)
image(binN$estimate)
binW <- kde(tsne3$Y,w=tabin$gender)
dev.new()
image(binW$estimate)
binW <- kde(tsne3$Y,w=tabin$gender-1)
image(binW$estimate)
binW <- kde(tsne3$Y,w=abs(tabin$gender-2))
image(binW$estimate)
binM <- binW
binW <- kde(tsne3$Y,w=tabin$gender-1)
dev.new()
image(binW$estimate)
binEi <- kde(tsne3$Y,w=abs(scoreE-max(scoreE)))
binNi <- kde(tsne3$Y,w=abs(scoreN-max(scoreN)))
binAi <- kde(tsne3$Y,w=abs(scoreA-max(scoreA)))
binCi <- kde(tsne3$Y,w=abs(scoreC-max(scoreC)))
binOi <- kde(tsne3$Y,w=abs(scoreO-max(scoreO)))
dev.new()
par(mfrow=c(2,5))
dev.new()
image(binO$estimate)
par(mfrow=c(2,5))
image(binO$estimate)
image(binOi$estimate)
image(binO$estimate)
par(mfrow=c(2,5))
image(binO$estimate)
image(binC$estimate)
image(binA$estimate)
image(binN$estimate)
image(binE$estimate)
image(binOi$estimate)
image(binCi$estimate)
image(binAi$estimate)
image(binNi$estimate)
image(binEi$estimate)
dev.new()
image(binM$estimate)
dat16 <- read.table("16PF_data.csv",head=T,)
dat16 <- read.table("16PF_data.csv",head=T,quote="")
dat16 <- read.table("16PF_data.csv",head=T,quote="\"")
dim(dat16)
dat16 <- read.table("16PF_data.csv",head=T,quote="\"",fill=T)
dat16[40278,]
dat16[40277,]
dat16[40279,]
tsne16_1 <- Rtsne(as.matrix(dat16),2,pca=T,perplexity=50,check_duplicates=F)
history()
history(1000000)


#wget https://openpsychometrics.org/_rawdata/IPIP-FFM-data-8Nov2018.zip
#unzip IPIP-FFM-data-8Nov2018.zip
#grep -v NULL IPIP-FFM-data-8Nov2018/data-final.csv > IPIP-FFM-data-8Nov2018/data-final-rec.csv
#wget http://openpsychometrics.org/_rawdata/BIG5.zip
#unzip BIG5.zip

#manually edit codebook to get qs.txt
library(data.table)
library("Rtsne")
source("FastPCA.R")
library(plyr)
library(MASS)
library(ks)

# IPIP-FFM 1M responses
ffm.in <- fread("IPIP-FFM-data-8Nov2018/data-final-rec.csv")
ffm.qs <- read.table("IPIP-FFM-data-8Nov2018/qs.txt",sep="\t",quote="",head=F)
colnames(ffm.qs) <- c("dir","Q","text")

ffm.dat <- as.data.frame(ffm.in)[,1:50]
ffm.time <- as.data.frame(ffm.in)[,51:100]
ffm.drop1 <- apply(ffm.time,1,function(x) any(x<=0)) #drop any that have <= 0 ms between answers = 144146
ffm.drop2 <- ffm.in$IPC>1 #drop any that came from IP address used multiple times

ffm.clean <- as.data.frame(ffm.in[!(ffm.drop1 | ffm.drop2),]) #left with 599291 responses
ffm.time.clean <- as.data.frame(ffm.time[!(ffm.drop1 | ffm.drop2),])
ffm.time.clean[ffm.time.clean < 0] <- 0
ffm.time.clean[ffm.time.clean > 6e4] <- 6e4

ffm.mat <- ffm.clean[,1:50]
ffm.meta <- ffm.clean[,108:110]

#BIG5 20k responses + metadata
big.in <- fread("BIG5/data.csv")
big.qs <- read.table("BIG5/qs.txt",sep="\t",head=F,quote="")
colnames(big.qs) <- c("dir","Q","text")

big.mat <- as.data.frame(big.in)[,8:57]
big.meta <- as.data.frame(big.in)[,1:7]
colnames(big.mat) <- colnames(ffm.mat)

all.mat <- rbind(ffm.mat,big.mat)
all.meta <- rbind.fill(ffm.meta,big.meta)

#move QC here to redo

nrow(unique(all.mat))/nrow(all.mat) #600k responses, 99.8% unique

all.pca <- FastPCA(all.mat,50)
all.tsne <- Rtsne(all.pca$x, 2, pca=F, perplexity=50, check_duplicates=F, verbose=T, theta=0.5)


#get rid of low-correlation responses (bots)

all.cor <- cor(all.mat)
neg.cor <- which(all.cor < -0.5,arr.ind=T)
pos.cor <- which(all.cor > 0.5,arr.ind=T)
pos.cor <- pos.cor[!(pos.cor[,"row"]==pos.cor[,"col"]),]

all.cor.pos <- apply(all.mat,1,function(x) cor( as.numeric(x[pos.cor[,1]]), as.numeric(x[pos.cor[,2]])))
all.cor.neg <- apply(all.mat,1,function(x) cor( as.numeric(x[neg.cor[,1]]), as.numeric(x[neg.cor[,2]])))

all.cor.both <- all.cor.pos - all.cor.neg + 1

rbPal <- colorRampPalette(c('red','blue'))
rbCol <- rbPal(10)[as.numeric(cut(all.cor.both,breaks = 10))]

pdf(file="cor.pdf",width=24,height=24)
plot(all.tsne$Y,pch=21,col=NULL,bg=alpha(rbCol,0.5), cex=all.cor.both/5)
incon <- all.cor.both < 1
plot(all.tsne$Y[!incon,],pch=21,col=NULL,bg=alpha(rbCol,0.5)[!incon], cex=all.cor.both[!incon]/5)
incon <- all.cor.both < 1.25
plot(all.tsne$Y[!incon,],pch=21,col=NULL,bg=alpha(rbCol,0.5)[!incon], cex=all.cor.both[!incon]/5)
incon <- all.cor.both < 1.5
plot(all.tsne$Y[!incon,],pch=21,col=NULL,bg=alpha(rbCol,0.5)[!incon], cex=all.cor.both[!incon]/5)
#go with this
incon <- all.cor.both < 1.75
plot(all.tsne$Y[!incon,],pch=21,col=NULL,bg=alpha(rbCol,0.5)[!incon], cex=all.cor.both[!incon]/5)
incon <- all.cor.both < 2
plot(all.tsne$Y[!incon,],pch=21,col=NULL,bg=alpha(rbCol,0.5)[!incon], cex=all.cor.both[!incon]/5)
dev.off()

image(all.cor)
plot(sort(all.cor.both),type="l")
 
incon <- all.cor.both < 1.75
incon[is.na(incon)] <- TRUE

#remove samples with sd==0
all.sd <- apply(all.mat,1,sd)

tokeep <- !incon & all.sd>0

all.good <- all.mat[tokeep,]
good.tsne <- as.data.frame(all.tsne$Y[tokeep,])
colnames(good.tsne) <- c("x","y")
good.meta <- all.meta[tokeep,]

ffm.time.good <- rowSums(ffm.time.clean)

ffm.keep <- tokeep[1:length(ffm.time.good)]
good.time <- ffm.time.good[ffm.keep]
good.time[is.nan(good.time)] <- 0

ffm.tsne <- as.data.frame(all.tsne$Y[1:length(ffm.time.good),])
ffm.tsne <- ffm.tsne[ffm.keep,]

pdf(file="plots.pdf",width=24,height=24)
plot(good.tsne,pch=21,col=NULL,bg=alpha("black",alpha=0.1),cex=0.5)
points(good.tsne,pch=21,col=NULL,bg=good.meta$gender+1,cex=0.5)
dev.off()

#center to neutral=0, disagree=-2, agree=2
all.center <- all.good - 3

#reorient negative questions to be positive
all.orient <- cbind(
sweep(all.center[,1:10],MARGIN=2,big.qs$dir[1:10],`*`),
sweep(all.center[,11:20],MARGIN=2,big.qs$dir[11:20],`*`),
sweep(all.center[,21:30],MARGIN=2,big.qs$dir[21:30],`*`),
sweep(all.center[,31:40],MARGIN=2,big.qs$dir[31:40],`*`),
sweep(all.center[,41:50],MARGIN=2,big.qs$dir[41:50],`*`)
)

#calculate overall scores
scoreE <- rowSums(all.orient[,1:10])
scoreN <- rowSums(all.orient[,11:20])
scoreA <- rowSums(all.orient[,21:30])
scoreC <- rowSums(all.orient[,31:40])
scoreO <- rowSums(all.orient[,41:50])

hist(scoreE,breaks=100)
hist(scoreN,breaks=100)
hist(scoreA,breaks=100)
hist(scoreC,breaks=100)
hist(scoreO,breaks=100)

gs <- 1000

# should resample on grid?
#kernel density estimates
binE <- kde(good.tsne,w=scoreE-min(scoreE), gridsize=gs)
binN <- kde(good.tsne,w=scoreN-min(scoreN), gridsize=gs)
binA <- kde(good.tsne,w=scoreA-min(scoreA), gridsize=gs)
binC <- kde(good.tsne,w=scoreC-min(scoreC), gridsize=gs)
binO <- kde(good.tsne,w=scoreO-min(scoreO), gridsize=gs)

#verrry slow
#intE <- interp(x=good.tsne$x,y=good.tsne$y,z=scoreE-min(scoreE), duplicate="median")


#genders etc.
male <- !is.na(good.meta$gender) & good.meta$gender==1
female <- !is.na(good.meta$gender) & good.meta$gender==2
binW <- kde(good.tsne[female,],w=good.meta$gender[female], gridsize=gs)
binM <- kde(good.tsne[male,],w=good.meta$gender[male], gridsize=gs)
#everyone
binU <- kde(good.tsne, gridsize=gs)
#timing
binT <- kde(ffm.tsne, w=good.time^3, gridsize=gs)
#consistency
good.cor.pos <- apply(as.matrix(all.good),1,function(x) cor( x[pos.cor[,1]], x[pos.cor[,2]]))
good.cor.neg <- apply(as.matrix(all.good),1,function(x) cor( x[neg.cor[,1]], x[neg.cor[,2]]))
good.cor.both <- good.cor.pos - good.cor.neg + 1
binD <- kde(good.tsne, w=good.cor.both^3, gridsize=gs)

# #kernel density estimates of inverses
binEi <- kde(good.tsne,w=abs(scoreE-max(scoreE)), gridsize=gs)
binNi <- kde(good.tsne,w=abs(scoreN-max(scoreN)), gridsize=gs)
binAi <- kde(good.tsne,w=abs(scoreA-max(scoreA)), gridsize=gs)
binCi <- kde(good.tsne,w=abs(scoreC-max(scoreC)), gridsize=gs)
binOi <- kde(good.tsne,w=abs(scoreO-max(scoreO)), gridsize=gs)
binDi <- kde(good.tsne,w=(1/good.cor.both)^3, gridsize=gs)

pdf(file="all.pdf",width=70,height=20)

#for(i in 1:20) {

par(mfrow=c(2,8))
xs <- seq(from=min(good.tsne$x),to=max(good.tsne$x),length.out=1000)
ys <- seq(from=min(good.tsne$y),to=max(good.tsne$y),length.out=1000)
pt <- sample(1:nrow(good.tsne),1)

#points(x=-0.8,y=-0.85) #center artifact; are these bots? check for concordance of answers, or randomness

hl <- 16
image(x=xs,y=ys,z=t(binU$estimate),col=terrain.colors(hl), useRaster=T);title("All",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binD$estimate),col=terrain.colors(hl), useRaster=T);title("Consistency",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binW$estimate),col=terrain.colors(hl), useRaster=T);title("Women",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binE$estimate),col=terrain.colors(hl), useRaster=T);title("Extraversion",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binN$estimate),col=terrain.colors(hl), useRaster=T);title("Neuroticism",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binA$estimate),col=terrain.colors(hl), useRaster=T);title("Agreeableness",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binC$estimate),col=terrain.colors(hl), useRaster=T);title("Conscientiousness",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binO$estimate),col=terrain.colors(hl), useRaster=T);title("Openness",cex.main=4)
points(good.tsne[pt,], cex=2)

image(x=xs,y=ys,z=t(binT$estimate),col=terrain.colors(hl), useRaster=T);title("Time",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binDi$estimate),col=terrain.colors(hl), useRaster=T);title("Inonsistency",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binM$estimate),col=terrain.colors(hl), useRaster=T);title("Men",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binEi$estimate),col=terrain.colors(hl), useRaster=T);title("Introversion",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binNi$estimate),col=terrain.colors(hl), useRaster=T);title("Emotional Stability",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binAi$estimate),col=terrain.colors(hl), useRaster=T);title("Disagreeableness",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binCi$estimate),col=terrain.colors(hl), useRaster=T);title("Unconscientiousness",cex.main=4)
points(good.tsne[pt,], cex=2)
image(x=xs,y=ys,z=t(binOi$estimate),col=terrain.colors(hl), useRaster=T);title("Closedness",cex.main=4)
points(good.tsne[pt,], cex=2)

# }

dev.off()

dev.off()

# heatmaps are a combination of number of people reporting and strength of component
# but should they just be one or the other?



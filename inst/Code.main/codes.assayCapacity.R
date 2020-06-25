#*****************************************************************************#
###############################################################################
# Main codes for generating Figures and Tables for assessing assay capacity   #
# Author: Xiaohua Douglas Zhang       06/2020                                 #
###############################################################################
#*****************************************************************************#

library(HTScapacity)

################################################################################
# Figure 1 for demonstrating assay capacity in general                         #
################################################################################
FoldHit.vec=c(-3, -1, -1/3, -2.5)
nCase = length(FoldHit.vec)
FoldHit2 = -1/1.6
mu1 = 0; sd1 = sd2 = sd3 = sdh = 0.5
var1 = sd1^2; var2 = sd2^2; var3 = sd3^2; varh=sdh^2
muh = mu1 + (-3)*sqrt(var1+varh)
mu2.vec = mu1+FoldHit.vec*abs(muh-mu1)
mu3 = mu1+FoldHit2*abs(muh-mu1)
normCut = 3
nPointSim = 600

MAR0 = c(5.1, 4.1, 4.1, 2.1)

##################################################################################
# display data for Figure 1a, c, e, g, corresponding to i=1, 2, 3, 4 respectively
##################################################################################
par(mar=c(1.1, 6.1, 1.1, 6.1))
#for( i in 1:nCase ) {
  i=1; # i=2; # i=3; # i=4 
  mu2 = mu2.vec[i]
  y1.vec = rnorm(nPointSim, mean=mu1, sd=sd1)
  y2.vec = rnorm(nPointSim, mean=mu2, sd=sd2)
  yh.vec = rnorm(nPointSim, mean=muh, sd=sdh )

  if(i==1) {
    xRange = c(-19, nPointSim+20)  
	yRange = range(c(y1.vec, yh.vec, y2.vec))
  }
  plot( xRange, yRange, type="n", xlab="", ylab="", axes=F)
  if( i == 2 ) {
    axis(2, at = c(mu1, mu2), 
       labels=c(expression(mu[1]), expression(mu[mH]==mu[2])), las=2 )  
	axis(4, at = c(mu1, mu2), labels=c(0, expression(gamma==-1)), las=2 )
    mtext("Measured response", side=2, line=4.5)
    mtext("Scale 1: Effect gauged by MDhit", side=4, line=4)
  } else if( i == 4 ) {
    axis(2, at = c(mu1, muh, mu2), 
       labels=c(expression(mu[1]), expression(mu[mH]), expression(mu[2])), las=2)
    axis(4, at = c(mu1, muh, mu2), labels=c(0, -1, expression(gamma[1])), las=2 )  
    mtext("Measured response", side=2, line=3.5)
    mtext("Scale 1: Effect gauged by MDhit", side=4, line=3)
  } else {
    axis(2, at = c(mu1, muh, mu2), 
       labels=c(expression(mu[1]), expression(mu[mH]), expression(mu[2])), las=2)
    axis(4, at = c(mu1, muh, mu2), labels=c(0, -1, expression(gamma)), las=2 ) 
    mtext("Measured response", side=2, line=3.5)
    mtext("Scale 1: Effect gauged by MDhit", side=4, line=3)	
  }
  box()
  points(1:nPointSim, y1.vec, col="green", pch=1)
  points(1:nPointSim, y2.vec, col="red", pch=2)
  points(1:nPointSim, yh.vec, col="black", pch=3)
  lines( c(-nPointSim*2, nPointSim*2), rep(mu1, 2), col="green")
  lines( c(-nPointSim*2, nPointSim*2), rep(mu2, 2), col="red")
  lines( c(-nPointSim*2, nPointSim*2), rep(muh, 2), col="black")
  arrows(xRange[1], muh, xRange[1], yRange[1]*1.03, lwd=3, col="blue")
  arrows(xRange[2], muh, xRange[2], yRange[1]*1.03, lwd=3, col="blue")

  if( i==4 ) {
    y3.vec = rnorm(nPointSim, mean=mu3, sd=sd3)	
    axis(2, at = mu3, labels=expression(mu[3]), las=2 )   	
    axis(4, at = mu3, labels=expression(gamma[2]), las=2 )   	
    points(1:nPointSim, y3.vec, col="purple", pch=4)	
    lines( c(-nPointSim*2, nPointSim*2), rep(mu3, 2), col="purple")	
  }
#}

##################################################################################
# Plot density for Figure 1b, d, f, h, corresponding to i=1, 2, 3, 4 respectively
##################################################################################

par(mar=c(8.1, 4.1, 1.1, 1.1))
#for( i in 1:nCase ) {
  i=1; # i=2; # i=3; # i=4 
  mu2 = mu2.vec[i]
  Y1.vec = seq( from=mu1-normCut*sd1, to=mu1+normCut*sd1, length.out=1000)
  d1.vec = dnorm(Y1.vec, mean=mu1, sd=sd1)
  Y2.vec = seq( from=mu2-normCut*sd2, to=mu2+normCut*sd2, length.out=1000)
  d2.vec = dnorm(Y2.vec, mean=mu2, sd=sd2)
  
  Yh1.vec = seq( from=muh-normCut*sdh, to=muh+normCut*sdh, length.out=1000)
  dh1.vec = dnorm(Yh1.vec, mean=muh, sd=sdh)
  if(i==1) { 
	xRange = range(c(Y1.vec, Yh1.vec, Y2.vec))
	y.vec = c(d1.vec, d2.vec, dh1.vec) 
	yRange =c( min(y.vec), max(y.vec)*1.5 )
  }
  plot( xRange, yRange, type="n", xlab="", ylab="", axes=F)
  mtext("Density        ", side=2, line=0)	
  axis(1, at = c(-1000, 1000), labels=rep("", 2) )   
  axis(1, at = c(-1000, 1000), labels=rep("", 2), line=2.8 )  
  mtext("Scale 1", at=par('usr')[1], side=1, line=1)
  mtext("Scale 2", at=par('usr')[1], side=1, line=3.7)
  
  if( i == 2 ) {
	axis(1, at = c(mu1, mu2), labels=c(0, expression(gamma==-1)) )
	axis(1, at = c(mu1, mu2), labels=c(0, expression(tau==-1)), line=2.8 ) 
  } else if( i == 4 ) {
    axis(1, at = c(mu1, muh, mu2, mu3), 
	     labels=c(0, -1, expression(gamma[1]), expression(gamma[2]) ) ) 
    axis(1, at = c(mu1, muh, mu2, mu3), 
	     labels=c(0, expression(tau[1]), -1, 
		          expression(gamma[2]/abs(gamma[1]))), line=2.8 ) 
    axis(1, at = c(-1000, 1000), labels=rep("", 2), line=5.5 )  
    axis(1, at = c(mu1, muh, mu2, mu3), 
	     labels=c(0, expression(tau[2]), 
		          expression(gamma[1]/abs(gamma[2])), -1), line=5.5 ) 
    mtext("Scale 3", at=par('usr')[1], side=1, line=6)
  } else {
    axis(1, at = c(mu1, muh, mu2), labels=c(0, -1, expression(gamma)) ) 
    axis(1, at = c(mu1, muh, mu2), labels=c(0, expression(tau), -1), line=2.8 ) 
  }

  lines(Yh1.vec, dh1.vec, col="black", lwd=3)
  lines(Y1.vec, d1.vec, col="green", lwd=3)
  lines(Y2.vec, d2.vec, col="red", lwd=3)
  lines(rep(mu1, 2), c(-0.2, max(d1.vec)/8) )
  lines(rep(mu2, 2), c(-0.2, max(d2.vec)/8) )  
  lines(rep(muh, 2), c(-0.2, yRange[2])*0.9 )
  arrows(muh, yRange[2]*0.8, xRange[1], yRange[2]*0.8, lwd=2, col="blue")
  text( muh*2.3, yRange[2]*0.9, "Range of Detectable Hits" )
  text( muh, yRange[2], "MDhit" )
  
  if( i==4 ) {
    Y3.vec = seq( from=mu3-normCut*sd3, to=mu3+normCut*sd3, length.out=1000)
    d3.vec = dnorm(Y3.vec, mean=mu3, sd=sd3)   
	lines(Y3.vec, d3.vec, col="purple", lwd=3)
	lines(rep(mu3, 2), c(-0.2, max(d3.vec)/8) ) 
  }
#}

par(mar=MAR0)

######################################################################
### Figure 2 and Table 2A for Mucin siRNA HTS
######################################################################

#dataMucin.df = read.csv("data.mucinHTS.csv")
#save(dataMucin.df, file="data.mucinHTS.RData")
data("data.mucinHTS", package="HTScapacity")
data.df = dataMucin.df
data.df[1:3, ]
#    plate         wellType  intensity
#1 plate01  No Cell Control -3.5306719
#2 plate01 Positive Control -1.3234041
#3 plate01            MISC1  0.2775412

plateName = "plate"; rowName = "row"; colName = "column"
wellName = "wellType"
unique(data.df[, wellName])
#[1] No Cell Control  Positive Control MISC1            Test siRNAs     
#[5] MISC2            MISC3            Negative Control MISC4           
#8 Levels: MISC1 MISC2 MISC3 MISC4 Negative Control ... Test siRNAs

posName="Positive Control"; negName="Negative Control"; sampleName = "Test siRNAs"
Intensity = "intensity" 
strongPos = "No Cell Control" 
weakPos = "Positive Control"

plate.vec = as.vector(data.df[, plateName])
plates = unique(plate.vec)
nPlate = length(plates) #[1] 79
nRow=16; nCol=24; nWell = nRow * nCol
uniqWells = unique(data.df[, wellName])
uniqWells

############### calculate QA metrics for each plate ################################
uniqWells
#[1] No Cell Control  Positive Control MISC1            Test siRNAs     
#[5] MISC2            MISC3            Negative Control MISC4           
#8 Levels: MISC1 MISC2 MISC3 MISC4 Negative Control ... Test siRNAs

compNames = c("strongPos.sample", "weakPos.sample")

zFactor.mat = matrix( NA, nrow=length(plates), ncol=length(compNames) )
dimnames(zFactor.mat) = list( plates, compNames )
FoldHitEst.mat = FoldHit0.mat = zFactor.mat

compNames2 = c("Sample", "strongPos", "weakPos")
mean.mat = matrix(NA, nrow=length(plates), ncol=length(compNames2))
dimnames(mean.mat) = list( plates, compNames2 )
sd.mat = mean.mat

for( i in 1:length(plates)) {
#  i=50
  dataIn.df = data.df[plate.vec==plates[i], c(rowName, colName, wellName, Intensity)]
  wellType <- dataIn.df[, wellName]
  inten.vec = dataIn.df[, Intensity]
  x.vec = inten.vec[wellType == sampleName]
  x.vec = x.vec[!is.na(x.vec)]
  cutoffs = quantile( x.vec, probs=c(0.025, 0.975) )
  condt = x.vec >= cutoffs[1] & x.vec <= cutoffs[2]
  x1.vec = x.vec[condt]
  y1.vec = inten.vec[wellType == strongPos]
  y2.vec = inten.vec[wellType == weakPos]
  mean.mat[i,] = c(mean(x1.vec, na.rm=TRUE), 
                   mean(y1.vec, na.rm=TRUE), mean(y2.vec, na.rm=TRUE) )
  sd.mat[i,] = c(sd(x1.vec, na.rm=TRUE),  
                   sd(y1.vec, na.rm=TRUE), sd(y2.vec, na.rm=TRUE) )  
  zFactor.mat[i,1] = zFactor.fn(x=x1.vec, y=y1.vec, k=3)  
  zFactor.mat[i,2] = zFactor.fn(x=x1.vec, y=y2.vec, k=3)  
  FoldHitEst.mat[i,1] = FoldHit.homoVAR.UMVUE.fn(Y1=x1.vec, Y2=y1.vec)  
  FoldHitEst.mat[i,2] = FoldHit.homoVAR.UMVUE.fn(Y1=x1.vec, Y2=y2.vec)  
  FoldHit0.mat[i,1] = FoldHit0.homoVAR.fn(Y1=x1.vec, Y2=y1.vec)  
  FoldHit0.mat[i,2] = FoldHit0.homoVAR.fn(Y1=x1.vec, Y2=y2.vec)  
}

##################### Figure 2 for Mucin siRNA HTS ###########################
# Figure 2a: Data in a mucin siRNA screen
par(mfrow=c(1,1))
par(mar=c(5.1, 4.4, 1.1, 1.1))
uniqWells
#[1] No Cell Control  Positive Control MISC1            Test siRNAs     
#[5] MISC2            MISC3            Negative Control MISC4           
#8 Levels: MISC1 MISC2 MISC3 MISC4 Negative Control ... Test siRNAs
col.vec=c("red", "purple", "pink", "green", "grey", "blue","yellow","brown")
pch.vec = rep(1, length(col.vec))

x.df=cbind(plateWelltoX.fn(data.df[,c(plateName, rowName, colName)]),
             "wellType"=data.df[, wellName])

y.vec = data.df[, Intensity]
yrange =  range(y.vec, na.rm=T) # yrange #[1] -4.860312  1.439067
plot( range(x.df$x), yrange, type="n", xlab="Plate Number (Plate-well series by row)",
      ylab="Intensity in log2 scale", axes=FALSE, main="")
axis(2, las=2); box()
axis(1, at=unique(x.df[,"plateOrder"])*384, label=unique(x.df[,"plateOrder"]) )
wellplotOrders1 = c(4, 7, 1, 2)
for( i in wellplotOrders1 ) {
  condt = x.df[,"wellType"] == uniqWells[i]
  points( x.df[condt,"x"], y.vec[condt], col=col.vec[i], cex=0.5)
}
	   
#Figure 2b: FoldHit overall in a screen (mucin)
MEDIAN = apply(cbind(FoldHitEst.mat[,1], FoldHit0.mat[,1], FoldHitEst.mat[,2], FoldHit0.mat[,2],
               1/FoldHitEst.mat[,1], 1/FoldHit0.mat[,1], 1/FoldHitEst.mat[,2], 1/FoldHit0.mat[,2]),
			  2, median, na.rm=TRUE )
names(MEDIAN) = c("FoldHitE.Strong", "FoldHit0.Strong", "FoldHitE.Weak", "FoldHit0.Weak",
                  "rFoldHitE.Strong", "rFoldHit0.Strong", "rFoldHitE.Weak", "rFoldHit0.Weak")
MEDIAN
# FoldHitE.Strong  FoldHit0.Strong    FoldHitE.Weak    FoldHit0.Weak 
#      -4.3080574       -3.8807839       -1.6744951       -1.5068780 
#rFoldHitE.Strong rFoldHit0.Strong   rFoldHitE.Weak   rFoldHit0.Weak 
#      -0.2321232       -0.2576799       -0.5971949       -0.6636237  
FoldHitEst.mat[1:3, ]
#        strongPos.sample weakPos.sample
#plate01        -3.643034      -1.777951
#plate02        -4.744731      -2.627302
#plate03        -4.690391      -1.924638
FoldHit.vec = c(0, -1, MEDIAN[c(1,3)]) 
par(mfrow=c(1,1))
capacityPlot.homoVAR.fn( FoldHit=FoldHit.vec, color=c("green","black","red", "purple"))

#Figure 2c: Data in one plate (mucin)
par(mar=c(5.1, 4.4, 1.1, 1.1))
par(mfrow=c(1,1))
y.vec = data.df[, Intensity]

x.df0=cbind(plateWelltoX.fn(data.df[,c(plateName, rowName, colName)]),
             "wellType"=data.df[, wellName])
iPlate=11
idx = x.df0[,"plateOrder"] == iPlate
x.df = x.df0[idx,]
y.vec = data.df[idx, Intensity]
yrange =  range(y.vec, na.rm=T) 
xrange = c( min(x.df$x), max(x.df$x)+190 ) 
plot( xrange, yrange, type="n", xlab="Well series by row in one plate",
      ylab="Intensity in log2 scale", axes=FALSE, 
	  main="")
axis(2, las=2); box()
axis(1, at=seq(24,384, length.out=16)+(iPlate-1)*384, label=1:16, las=2)
wellplotOrders1 = c(4,7,1,2)
for( i in wellplotOrders1 ) {
  condt = x.df[,"wellType"] == uniqWells[i]
  points( x.df[condt,"x"], y.vec[condt], col=col.vec[i], cex=1)
}
legend("bottomright", legend=c("test siRNAs", "negative control", "no-cell control", "positive control"), 
       col=col.vec[wellplotOrders1], pch=1, cex=1)

#Figure 2d: FoldHit for the 50th plate in a screen (mucin)
FoldHit.vec = c(0, -1, FoldHitEst.mat[iPlate, ]) 
par(mfrow=c(1,1))
capacityPlot.homoVAR.simple.fn( FoldHit=FoldHit.vec, color=c("green","black","red", "purple"))

# Figure 2e: Estimated FoldHit (mucin)
par(mar=c(5.1, 4.4, 1.1, 1.1))	
par(mfrow=c(1,1)) 
col.vec1 = c("red", "purple")
xat = c(1, 1:7*10, 79); yat = 0:(-8)
theFoldHit.mat = FoldHitEst.mat[, 1:2 ]
yRange = c(0, -8)  # yRange = range(theFoldHit.mat)
plot( c(0, length(plates)), yRange, type="n", axes=FALSE, 
      xlab="Plate Number", ylab="Estimated FoldHit value", 
	  main="")
axis(1, at=xat, labels=xat, las=2); axis(2, at=yat, labels=yat, las=2); box()
for(i in 1:ncol(theFoldHit.mat) ) {
  points( 1:length(plates), theFoldHit.mat[,i], col=col.vec1[i] )
  lines( c(1-10, length(plates)+10 ), rep(median(theFoldHit.mat[,i]),2), 
         col=col.vec1[i], lty=2)
  lines( 1:length(plates), theFoldHit.mat[,i], col=col.vec1[i] )  
}
lines( c(1-10, length(plates)+10 ), rep(-1, 2), col="grey" ) 

# Figure 2f: FoldHit with error control (mucin)
theFoldHit.mat = FoldHit0.mat[, 1:2 ]
#yRange = c(0, -8)  # yRange = range(theFoldHit.mat)
plot( c(0, length(plates)), yRange, type="n", axes=FALSE,
      xlab="Plate Number", ylab="FoldHit with error control", main="")
axis(1, at=xat, labels=xat, las=2); axis(2, at=yat, labels=yat, las=2); box()
for(i in 1:ncol(theFoldHit.mat) ) {
  points( 1:length(plates), theFoldHit.mat[,i], col=col.vec1[i] )
  lines( 1:length(plates), theFoldHit.mat[,i], col=col.vec1[i] )  
}
lines( c(1-10, length(plates)+10 ), rep(-1, 2), col="grey" ) 
lines( c(1-10, length(plates)+10 ), rep(-0.81, 2), col="lightblue" ) 

# Figure 2g: z-factor (mucin)
the.mat = zFactor.mat[, 1:2 ]
yRange = c(-1, 1) #yRange = range(the.mat)
plot( c(0, length(plates)), yRange, type="n", axes=FALSE, 
      xlab="Plate Number", ylab="z-factor", main="")
axis(1, at=xat, labels=xat, las=2); axis(2, las=2); box()
for(i in 1:ncol(the.mat) ) {
  points( 1:length(plates), the.mat[,i], col=col.vec1[i] )
  lines( 1:length(plates), the.mat[,i], col=col.vec1[i] )  
}
lines( c(1-10, length(plates)+10 ), rep(0.5, 2), col="grey" ) 
lines( c(1-10, length(plates)+10 ), rep(0, 2), col="grey" ) 

################################ for Table 2A #############################
### estimated FoldHit
FoldHitEst.mat[iPlate,]
#strongPos.sample   weakPos.sample 
#       -4.948223        -1.849400 
round(FoldHitEst.mat[iPlate,], 2)
#strongPos.sample   weakPos.sample 
#           -4.95            -1.85 
c( apply(FoldHitEst.mat, 2, median), apply(FoldHitEst.mat, 2, mad) )
#strongPos.sample   weakPos.sample strongPos.sample   weakPos.sample 
#      -4.3080574       -1.6744951        2.0024960        0.3574136 
apply(FoldHitEst.mat, 2, CIempirical.fn)
#      strongPos.sample weakPos.sample
#2.5%         -7.708645      -2.949595
#97.5%        -1.477453      -1.163107

### estimated rFoldHit
1/FoldHitEst.mat[iPlate,]
round(1/FoldHitEst.mat[iPlate,],2)
# strongPos.sample    weakPos.sample strongPos.negCTRL   weakPos.negCTRL    negCTRL.sample 
#            -0.20             -0.54             -0.27             -0.62              4.07 
 c( apply(1/FoldHitEst.mat, 2, median), apply(1/FoldHitEst.mat, 2, mad) )
#strongPos.sample   weakPos.sample strongPos.sample   weakPos.sample 
#      -0.2321232       -0.5971949        0.1006271        0.1415071 
 apply(1/FoldHitEst.mat, 2, CIempirical.fn)
#      strongPos.sample weakPos.sample
#2.5%        -0.6768484     -0.8609409
#97.5%       -0.1297248     -0.3390325

### FoldHit with error control
apply(FoldHit0.mat, 2, function(x) { sum(abs(x)>1) } ) 
#strongPos.sample   weakPos.sample 
#              79               77 

round(FoldHit0.mat[iPlate,],2)
#strongPos.sample   weakPos.sample 
#           -4.62            -1.67 

### z-factor for QC
idx = 1
theZ = zFactor.mat[, idx]
c( sum( theZ < 0), sum( theZ>0 & theZ<0.5), sum( theZ >= 0.5) )
#[1]  1 27 51
 
idx = 2
theZ = zFactor.mat[, idx]
c( sum( theZ < 0), sum( theZ>0 & theZ<0.5), sum( theZ >= 0.5) )
#[1] 23 56  0


######################################################################
### Figure 3a-e and Table 2B for CVB3 CRISPR/CAS9 primary screen 
######################################################################

#dataCVB3.df = read.csv("data.CVB3.csv")
#save(dataCVB3.df, file="data.CVB3CRISPR.RData")
data("data.CVB3CRISPR", package="HTScapacity")

data.df = dataCVB3.df
data.df[1:3, ]
#    plate wellType row column intensity
#1 plate01     MISC   1      1        NA
#2 plate01     MISC   1      2        NA
#3 plate01   Sample   1      3 -1.123124

plateName = "plate"; rowName = "row"; colName = "column"
wellName = "wellType"
unique(data.df[, wellName])
#[1] MISC             Sample           Negative Control Positive Control
#Levels: MISC Negative Control Positive Control Sample

negName="Negative Control"; sampleName = "Sample"

strongPos = "Positive Control"
Intensity = "intensity" 

plate.vec = as.vector(data.df[, plateName])
plates = unique(plate.vec)
nPlate = length(plates) #[1] 20
nRow=8; nCol=12; nWell = nRow * nCol
uniqWells = unique(data.df[, wellName])
uniqWells

############### calculate QA metrics for each plate ################################
uniqWells
#[1] MISC             Sample           Negative Control Positive Control
#Levels: MISC Negative Control Positive Control Sample

compNames = "strongPos.sample"
zFactor.mat = matrix( NA, nrow=length(plates), ncol=length(compNames) )
dimnames(zFactor.mat) = list( plates, compNames )
FoldHitEst.mat = FoldHit0.mat = zFactor.mat

compNames2 = c("Sample", "strongPos")
mean.mat = matrix(NA, nrow=length(plates), ncol=length(compNames2))
dimnames(mean.mat) = list( plates, compNames2 )
sd.mat = mean.mat

for( i in 1:length(plates)) {
#  i=4
  dataIn.df = data.df[plate.vec==plates[i], c(rowName, colName, wellName, Intensity)]
  wellType <- dataIn.df[, wellName]
  inten.vec = dataIn.df[, Intensity]
  x.vec = inten.vec[wellType == sampleName]
  x.vec = x.vec[!is.na(x.vec)]
  cutoffs = quantile( x.vec, probs=c(0.025, 0.975) )
  condt = x.vec >= cutoffs[1] & x.vec <= cutoffs[2]
  x1.vec = x.vec[condt]
  y1.vec = inten.vec[wellType == strongPos]
  mean.mat[i,] = c(mean(x1.vec, na.rm=TRUE), mean(y1.vec, na.rm=TRUE) ) 
  sd.mat[i,] = c(sd(x1.vec, na.rm=TRUE), sd(y1.vec, na.rm=TRUE) )  
  zFactor.mat[i,1] = zFactor.fn(x=x1.vec, y=y1.vec, k=3)  
  FoldHitEst.mat[i,1] = FoldHit.homoVAR.UMVUE.fn(Y1=x1.vec, Y2=y1.vec)  
  FoldHit0.mat[i,1] = FoldHit0.homoVAR.fn(Y1=x1.vec, Y2=y1.vec)  
}

#################### Figure 3a-e for CVB3 CRISPR/CAS9 primary screen #####################
par(mar=c(5.1, 4.4, 1.1, 1.1))
par(mfrow=c(1,1))
uniqWells
# [1] empty        Sample       negativeCTRL positiveCTRL
col.vec=c("black", "green", "yellow","red")
wellplotOrders = 1:4
pch.vec = rep(1, length(col.vec))

x.df=cbind(plateWelltoX.fn(data.df[,c(plateName, rowName, colName)], nRow, nCol, byRow=FALSE),
            "wellType"=data.df[, wellName])

Intensity = "intensity"  
y.vec = data.df[, Intensity]
yrange =  range(y.vec, na.rm=T) 
plot( range(x.df$x), yrange, type="n", xlab="Plate Number (Plate-well series by column)",
      ylab="Normalized Intensity", axes=FALSE, main="")
axis(2, las=2)
axis(1, at=unique(x.df[,"plateOrder"]-1)*nWell, label=unique(x.df[,"plateOrder"]), las=2 )
box()
for( i in wellplotOrders ) {
  condt = x.df[,"wellType"] == uniqWells[i]
  points( x.df[condt,"x"], y.vec[condt], col=col.vec[i], cex=0.5)
}
legend("bottomright", legend=c("Sample", "negative control", "positive control"), 
       col=col.vec[2:4], pch=1, cex=1)

#Figure 3b: FoldHit for median in a screen 
MEDIAN = apply(cbind(FoldHitEst.mat[,1], FoldHit0.mat[,1], 1/FoldHitEst.mat[,1], 1/FoldHit0.mat[,1]),
			   2, median, na.rm=TRUE )
names(MEDIAN) = c("FoldHitE.Strong", "FoldHit0.Strong", "rFoldHitE.Strong", "rFoldHit0.Strong")
MEDIAN
# FoldHitE.Strong  FoldHit0.Strong rFoldHitE.Strong rFoldHit0.Strong 
#      -2.1865426       -1.7555839       -0.4573565       -0.5699116 

FoldHit.vec = c(0, -1, MEDIAN[1]) 
par(mfrow=c(1,1))
capacityPlot.homoVAR.fn( FoldHit=FoldHit.vec, color=c("green","black","red"))

# Figure 3c: Estimated FoldHit (CVB3)
par(mar=c(5.1, 4.4, 1.1, 1.1))	

col.vec1 = c("red", "black")
xat = c(1:20); yat = 0:(-10)
theFoldHit.mat = matrix(FoldHitEst.mat[, 1 ], ncol=1)
yRange = c(0, -10)  # yRange = range(theFoldHit.mat)
plot( c(0, length(plates)), yRange, type="n", axes=FALSE, 
      xlab="Plate Number", ylab="Estimated FoldHit value", 
	  main="")
axis(1, at=xat, labels=xat, las=2); axis(2, at=yat, labels=yat, las=2); box()
for(i in 1:ncol(theFoldHit.mat) ) {
  points( 1:length(plates), theFoldHit.mat[,i], col=col.vec1[i] )
  lines( c(1-10, length(plates)+10 ), rep(median(theFoldHit.mat[,i]),2), 
         col=col.vec1[i], lty=2)
  lines( 1:length(plates), theFoldHit.mat[,i], col=col.vec1[i] )  
}
lines( c(1-10, length(plates)+10 ), rep(-1, 2), col="grey" ) 

# Figure 3d: FoldHit with error control (CVB3)
col.vec1 = c("red", "black")
xat = c(1:20); yat = 0:(-10)
theFoldHit.mat = matrix(FoldHit0.mat[, 1], ncol=1)
plot( c(0, length(plates)), yRange, type="n", axes=FALSE,
      xlab="Plate Number", ylab="FoldHit with error control", 
	  main="")
axis(1, at=xat, labels=xat, las=2); axis(2, at=yat, labels=yat, las=2); box()
for(i in 1:ncol(theFoldHit.mat) ) {
  points( 1:length(plates), theFoldHit.mat[,i], col=col.vec1[i] )
  lines( 1:length(plates), theFoldHit.mat[,i], col=col.vec1[i] )  
}
lines( c(1-10, length(plates)+10 ), rep(-1, 2), col="grey" ) 

# Figure 3e: z-factor (CVB3)
the.mat = matrix(zFactor.mat[, 1], ncol=1)
yRange = c(-1, 1)
plot( c(0, length(plates)), yRange, type="n", axes=FALSE, 
      xlab="Plate Number", ylab="z-factor", main="")
axis(1, at=xat, labels=xat, las=2); axis(2, las=2); box()
for(i in 1:ncol(the.mat) ) {
  points( 1:length(plates), the.mat[,i], col=col.vec1[i] )
  lines( 1:length(plates), the.mat[,i], col=col.vec1[i] )  
}
lines( c(1-10, length(plates)+10 ), rep(0.5, 2), col="grey" ) 
lines( c(1-10, length(plates)+10 ), rep(0, 2), col="grey" ) 

################################ for Table 2B #############################
### estimated FoldHit
c( apply(FoldHitEst.mat, 2, median), apply(FoldHitEst.mat, 2, mad) )
#strongPos.sample strongPos.sample 
#       -2.186543         1.208446 
apply(FoldHitEst.mat, 2, CIempirical.fn)
#      strongPos.sample
#2.5%         -7.988359
#97.5%        -1.016974

### estimated rFoldHit
 c( apply(1/FoldHitEst.mat, 2, median), apply(1/FoldHitEst.mat, 2, mad) )
#strongPos.sample strongPos.sample 
#      -0.4573565        0.2930957 

 apply(1/FoldHitEst.mat, 2, CIempirical.fn)
#      strongPos.sample
#2.5%        -0.9852867
#97.5%       -0.1328134

### FoldHit with error control
apply(FoldHit0.mat, 2, function(x) { sum(abs(x)>1) } ) 
#strongPos.sample 
#              17 

### z-factor for QC
idx = 1
theZ = zFactor.mat[, idx]
c( sum( theZ < 0), sum( theZ>0 & theZ<0.5), sum( theZ >= 0.5) )
#[1]  4 11  5

######################################################################
### Figure 3f-j and Table 2C for HCV siRNA primary screen 
######################################################################

#dataHCV.df = read.csv("data.HCV.csv")
#save(dataHCV.df, file="data.hcvHTS.RData")
data("data.hcvHTS", package="HTScapacity")
data.df = dataHCV.df

data.df[1:3, ]
#    plate                wellType row column intensity
#1 plate01   Weak Positive Control   1      1 1.0923555
#2 plate01 Strong Positive Control   1      2 0.5917118
#3 plate01             Test siRNAs   1      3 1.6400746

plateName = "plate"; rowName = "row"; colName = "column"; wellName = "wellType"
unique(data.df[, wellName])
#[1] Weak Positive Control   Strong Positive Control Test siRNAs            
#[4] MISC1                   MISC2                   Negative Control       
#6 Levels: MISC1 MISC2 Negative Control Strong Positive Control ... Weak Positive Control

negName="Negative Control"; sampleName = "Test siRNAs"
strongPos = "Strong Positive Control" 
weakPos = "Weak Positive Control"
Intensity = "intensity" 

plate.vec = as.vector(data.df[, plateName])
plates = unique(plate.vec)
nPlate = length(plates) #[1] 79
nRow=16; nCol=24; nWell = nRow * nCol
uniqWells = unique(data.df[, wellName])
uniqWells

############### calculate QA metrics for each plate ################################
uniqWells
#[1] Weak Positive Control   Strong Positive Control Test siRNAs            
#[4] MISC1                   MISC2                   Negative Control       
#6 Levels: MISC1 MISC2 Negative Control Strong Positive Control ... Weak Positive Control

compNames = c("strongPos.sample", "weakPos.sample")
zFactor.mat = matrix( NA, nrow=length(plates), ncol=length(compNames) )
dimnames(zFactor.mat) = list( plates, compNames )
FoldHitEst.mat = FoldHit0.mat = zFactor.mat

compNames2 = c("Sample", "strongPos", "weakPos")
mean.mat = matrix(NA, nrow=length(plates), ncol=length(compNames2))
dimnames(mean.mat) = list( plates, compNames2 )
sd.mat = mean.mat

for( i in 1:length(plates)) {
#  i=50
  dataIn.df = data.df[plate.vec==plates[i], c(rowName, colName, wellName, Intensity)]
  wellType <- dataIn.df[, wellName]
  inten.vec = dataIn.df[, Intensity]
  x.vec = inten.vec[wellType == sampleName]
  x.vec = x.vec[!is.na(x.vec)]
  cutoffs = quantile( x.vec, probs=c(0.025, 0.975) )
  condt = x.vec >= cutoffs[1] & x.vec <= cutoffs[2]
  x1.vec = x.vec[condt]
  y1.vec = inten.vec[wellType == strongPos]
  y2.vec = inten.vec[wellType == weakPos]
  mean.mat[i,] = c(mean(x1.vec, na.rm=TRUE), 
                   mean(y1.vec, na.rm=TRUE), mean(y2.vec, na.rm=TRUE) )
  sd.mat[i,] = c(sd(x1.vec, na.rm=TRUE), 
                 sd(y1.vec, na.rm=TRUE), sd(y2.vec, na.rm=TRUE) )  
  zFactor.mat[i,1] = zFactor.fn(x=x1.vec, y=y1.vec, k=3)  
  zFactor.mat[i,2] = zFactor.fn(x=x1.vec, y=y2.vec, k=3)  
  FoldHitEst.mat[i,1] = FoldHit.homoVAR.UMVUE.fn(Y1=x1.vec, Y2=y1.vec)  
  FoldHitEst.mat[i,2] = FoldHit.homoVAR.UMVUE.fn(Y1=x1.vec, Y2=y2.vec)  
  FoldHit0.mat[i,1] = FoldHit0.homoVAR.fn(Y1=x1.vec, Y2=y1.vec)  
  FoldHit0.mat[i,2] = FoldHit0.homoVAR.fn(Y1=x1.vec, Y2=y2.vec)  
}

##################### Figure 3 for HCV siRNA HTS ###########################
# Figure 3f: Data in a HCV siRNA screen
par(mfrow=c(1,1))
par(mar=c(5.1, 4.4, 1.1, 1.1))
uniqWells
#[1] Weak Positive Control   Strong Positive Control Test siRNAs            
#[4] MISC1                   MISC2                   Negative Control       
#6 Levels: MISC1 MISC2 Negative Control Strong Positive Control ... Weak Positive Control
col.vec= c( "purple", "red", "green", "black", "blue", "yellow")
pch.vec = rep(1, length(col.vec))

x.df=cbind(plateWelltoX.fn(data.df[,c(plateName, rowName, colName)]),
             "wellType"=data.df[, wellName])

y.vec = data.df[, Intensity]
yrange =  range(y.vec, na.rm=T) 
plot( range(x.df$x), yrange, type="n", xlab="Plate Number (Plate-well series by row)",
      ylab="Intensity in log2 scale", axes=FALSE, main="")
axis(2, las=2); box()
axis(1, at=unique(x.df[,"plateOrder"])*384, label=unique(x.df[,"plateOrder"]) )
wellplotOrders1 = c(3, 6, 2, 1)
for( i in wellplotOrders1 ) {
  condt = x.df[,"wellType"] == uniqWells[i]
  points( x.df[condt,"x"], y.vec[condt], col=col.vec[i], cex=0.5)
}
	   
#Figure 3g: FoldHit overall in a screen (HCV)
MEDIAN = apply(cbind(FoldHitEst.mat[,1], FoldHit0.mat[,1], FoldHitEst.mat[,2], FoldHit0.mat[,2],
               1/FoldHitEst.mat[,1], 1/FoldHit0.mat[,1], 1/FoldHitEst.mat[,2], 1/FoldHit0.mat[,2]),
			  2, median, na.rm=TRUE )
names(MEDIAN) = c("FoldHitE.Strong", "FoldHit0.Strong", "FoldHitE.Weak", "FoldHit0.Weak",
                  "rFoldHitE.Strong", "rFoldHit0.Strong", "rFoldHitE.Weak", "rFoldHit0.Weak")
MEDIAN
# FoldHitE.Strong  FoldHit0.Strong    FoldHitE.Weak    FoldHit0.Weak 
#      -4.3080574       -3.8807839       -1.6744951       -1.5068780 
#rFoldHitE.Strong rFoldHit0.Strong   rFoldHitE.Weak   rFoldHit0.Weak 
#      -0.2321232       -0.2576799       -0.5971949       -0.6636237  
FoldHitEst.mat[1:3, ]
#        strongPos.sample weakPos.sample
#plate01        -3.643034      -1.777951
#plate02        -4.744731      -2.627302
#plate03        -4.690391      -1.924638
FoldHit.vec = c(0, -1, MEDIAN[c(1,3)]) 
par(mfrow=c(1,1))
capacityPlot.homoVAR.fn( FoldHit=FoldHit.vec, color=c("green","black","red", "purple"))

# Figure  3h: Estimated FoldHit (HCV)
par(mar=c(5.1, 4.4, 1.1, 1.1))	
col.vec1 = c("red", "purple")
xat = c(1, 1:9*10, 97); yat = 0:(-8)
theFoldHit.mat = FoldHitEst.mat[, 1:2 ]
yRange = c(-0.1, -3)  # yRange = range(theFoldHit.mat)
plot( c(0, length(plates)), yRange, type="n", axes=FALSE, 
      xlab="Plate Number", ylab="Estimated FoldHit value", 
	  main="")
axis(1, at=xat, labels=xat, las=2); axis(2, at=yat, labels=yat, las=2); box()
for(i in 1:ncol(theFoldHit.mat) ) {
  points( 1:length(plates), theFoldHit.mat[,i], col=col.vec1[i] )
  lines( c(1-10, length(plates)+10 ), rep(median(theFoldHit.mat[,i]),2), 
         col=col.vec1[i], lty=2)
  lines( 1:length(plates), theFoldHit.mat[,i], col=col.vec1[i] )  
}
lines( c(1-10, length(plates)+10 ), rep(-1, 2), col="grey" ) 
 
# Figure 3i: FoldHit with error control
theFoldHit.mat = FoldHit0.mat[, 1:2 ]
#yRange = c(-0.1, -3.1)  
plot( c(0, length(plates)), yRange, type="n", axes=FALSE,
      xlab="Plate Number", ylab="FoldHit with error control", main="")
axis(1, at=xat, labels=xat, las=2); axis(2, at=yat, labels=yat, las=2); box()
for(i in 1:ncol(theFoldHit.mat) ) {
  points( 1:length(plates), theFoldHit.mat[,i], col=col.vec1[i] )
  lines( 1:length(plates), theFoldHit.mat[,i], col=col.vec1[i] )  
}
lines( c(1-10, length(plates)+10 ), rep(-1, 2), col="grey" ) 
lines( c(1-10, length(plates)+10 ), rep(-0.424, 2), col="lightblue" ) 

# Figure 3j: z-factor
the.mat = zFactor.mat[, 1:2 ]
yRange = c(-5, 1) 
plot( c(0, length(plates)), yRange, type="n", axes=FALSE, 
      xlab="Plate Number", ylab="z-factor", main="")
axis(1, at=xat, labels=xat, las=2); axis(2, las=2); box()
for(i in 1:ncol(the.mat) ) {
  points( 1:length(plates), the.mat[,i], col=col.vec1[i] )
  lines( 1:length(plates), the.mat[,i], col=col.vec1[i] )  
}
lines( c(1-10, length(plates)+10 ), rep(0.5, 2), col="grey" ) 
lines( c(1-10, length(plates)+10 ), rep(0, 2), col="grey" ) 

################################ for Table 2C #############################
c( apply(FoldHitEst.mat, 2, median), apply(FoldHitEst.mat, 2, mad) )
#strongPos.sample   weakPos.sample strongPos.sample   weakPos.sample 
#      -2.0467745       -1.0666304        0.3724334        0.2240756 
apply(FoldHitEst.mat, 2, CIempirical.fn)
#      strongPos.sample weakPos.sample
#2.5%         -2.765709     -1.4359347
#97.5%        -1.202286     -0.5299444

c( apply(1/FoldHitEst.mat, 2, median), apply(1/FoldHitEst.mat, 2, mad) )
#strongPos.sample   weakPos.sample strongPos.sample   weakPos.sample 
#     -0.48857362      -0.93753188       0.08354318       0.17887861 
 apply(1/FoldHitEst.mat, 2, CIempirical.fn)
#      strongPos.sample weakPos.sample
#2.5%        -0.8325736     -1.8903298
#97.5%       -0.3617572     -0.6964221


### FoldHit with error control
c( apply(FoldHit0.mat, 2, function(x) { sum(abs(x)>1) } ), 
   apply(FoldHitEst.mat, 2, function(x) { sum(abs(x)>1) } ) )
#strongPos.sample   weakPos.sample strongPos.sample   weakPos.sample 
#              96               33               97               62  

### z-factor for QC
idx = 1
theZ = zFactor.mat[, idx]
c( sum( theZ < 0), sum( theZ>0 & theZ<0.5), sum( theZ >= 0.5) )
#[1]  9 81  7
 
idx = 2
theZ = zFactor.mat[, idx]
c( sum( theZ < 0), sum( theZ>0 & theZ<0.5), sum( theZ >= 0.5) )
#[1] 85 12  0



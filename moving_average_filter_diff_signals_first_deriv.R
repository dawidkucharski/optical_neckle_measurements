{\rtf1\ansi\ansicpg1250\cocoartf2513
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset0 Monaco;\f1\fnil\fcharset0 Menlo-Regular;}
{\colortbl;\red255\green255\blue255;\red14\green14\blue14;\red244\green244\blue244;\red35\green38\blue42;
\red166\green65\blue5;\red68\green104\blue13;}
{\*\expandedcolortbl;;\cssrgb\c6667\c6667\c6667;\cssrgb\c96471\c96471\c96471;\cssrgb\c18431\c20000\c21569;
\cssrgb\c71765\c33333\c392;\cssrgb\c32941\c47451\c5098;}
\paperw11900\paperh16840\margl1440\margr1440\vieww27120\viewh13940\viewkind0
\deftab720
\pard\pardeftab720\partightenfactor0

\f0\fs24 \cf2 \cb3 \expnd0\expndtw0\kerning0
\
#----------------------------------------------------------raw_signal_detectiom-----------------------------------------\
library(pracma)\
\pard\pardeftab720\qj\parhyphenfactor20\partightenfactor0
\cf2 library("car")\
\
\
\pard\pardeftab720\partightenfactor0
\cf2 library(pracma)\
\pard\pardeftab720\qj\parhyphenfactor20\partightenfactor0
\cf2 library("car")\
library(tikzDevice)\
\pard\pardeftab720\partightenfactor0
\cf2 \
im_num = length(list.files("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Optical_neckle_measurements/new_sensors/#8/6mm/"))\
c_num = 2\
\
while (c_num <= im_num) \{ \
\
file = file.path("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Optical_neckle_measurements/new_sensors/#8/6mm/", \
                     paste("sample_6mm_1ch_", c_num, ".dat", sep=""))\
\
neckle_data = read.csv(file, skip=50, nrows = 3600,header = FALSE, sep = " ", stringsAsFactors = FALSE)\
x<-neckle_data[,1]\
y<-neckle_data[,2]\
\
x<-
\f1\fs26 \cf4 seq(from = \cf5 0\cf4 , to = \cf5 length(y)-1\cf4 , by = \cf5 1\cf4 )
\f0\fs24 \cf2 \
\
\
\
movingAverage <- function(x, n=1, centered=FALSE) \{\
    \
    if (centered) \{\
        before <- floor  ((n-1)/2)\
        after  <- ceiling((n-1)/2)\
    \} else \{\
        before <- n-1\
        after  <- 0\
    \}\
    \
    # Track the sum and count of number of non-NA items\
    s     <- rep(0, length(x))\
    count <- rep(0, length(x))\
    \
    # Add the centered data \
    new <- x\
    # Add to count list wherever there isn't a \
    count <- count + !is.na(new)\
    # Now replace NA_s with 0_s and add to total\
    new[is.na(new)] <- 0\
    s <- s + new\
    \
    # Add the data from before\
    i <- 1\
    while (i <= before) \{\
        # This is the vector with offset values to add\
        new   <- c(rep(NA, i), x[1:(length(x)-i)])\
        \
        count <- count + !is.na(new)\
        new[is.na(new)] <- 0\
        s <- s + new\
        \
        i <- i+1\
    \}\
    \
    # Add the data from after\
    i <- 1\
    while (i <= after) \{\
        # This is the vector with offset values to add\
        new   <- c(x[(i+1):length(x)], rep(NA, i))\
        \
        count <- count + !is.na(new)\
        new[is.na(new)] <- 0\
        s <- s + new\
        \
        i <- i+1\
    \}\
    \
    # return sum divided by count\
    s/count\
\}\
\
\
\
\
\
# Calculate symmetrical moving average  with new method and overplot with green\
y_sym_na.rm <- movingAverage(y,80, TRUE)\
\
\
\
\
\
fit1<-smooth.spline(y_sym_na.rm, w = NULL, df=27)\
\
pred1<-predict(fit1, deriv = 1)\
plot(pred1,col='black',lwd=2,lty=1, type="l", ylab= 
\f1\fs26 \cf5 expression\cf4 (d~y~\cf6 '/'\cf4 ~d~x)
\f0\fs24 \cf2 , xlab= ("Pixels"))\
grid(lty="dotted")\
\
peaks<-max(pred1$y)\
minima<-min(pred1$y)\
#peaks\
#minima\
\
max1<-x[which(pred1$y == max(pred1$y))]\
min1<-x[which(pred1$y == min(pred1$y))]\
\
\
#points(max1, max(pred1$y), pch=4, col="black")\
#points(min1, min(pred1$y), pch=4, col="black")\
\
abline(v=min1, lty=6, lwd=3)\
abline(v=max1, lty=6, lwd=3)\
\
#pred2<-predict(fit1, deriv = 2)\
#plot(pred2,col='blue',lwd=2,lty=1, type="l", ylab=expression(d~y^2~'/'~d~x^2), xlab= ("Pixels"))\
#grid()\
\
# Make same plots from before, with thicker lines\
plot(x, y, type="l", col=grey(.5), ylab=(expression(ADC[pixel]*" ["*a.u.*"]")), xlab= ("Pixels"))\
grid(lty="dotted")\
lines(x, y_sym_na.rm, col="black",lwd=2)\
lines(fit1,col='black',lwd=3,lty=3)\
\
max2<-y[which(fit1$x == max1)]\
min2<-y[which(fit1$x == min1)]\
\
#points(max1, max2, pch=20, col="green")\
#points(min1, min2, pch=20, col="green")\
\
abline(v=min1, lty=6, lwd=3)\
abline(v=max1, lty=6, lwd=3)\
\
radius=abs(max1-min1)\
print(radius)\
pred3<-predict(fit1)\
\
\
write.table(radius, file="/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Optical_neckle_measurements/new_sensors/#8/6mm/raw_signal_1ch.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)\
  c_num = c_num+1\
\}\
neckle_data = read.csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Optical_neckle_measurements/new_sensors/#8/6mm/raw_signal_1ch.csv", header = FALSE)\
avarage=mean(neckle_data$V1);\
dim(neckle_data)\
avarage\
std=2*sd(neckle_data$V1);\
std\
\
\
#Q1 <- quantile(neckle_data$V1, 0);\
#Q3 <- quantile(neckle_data$V1, .75);\
#no_outliers <- subset(neckle_data, neckle_data$V1> (Q1));\
no_outliers<-neckle_data$V1[!neckle_data$V1 %in% boxplot.stats(neckle_data$V1)$out]\
\
length(neckle_data$V1)-length(no_outliers)\
avarage=mean(no_outliers);\
avarage\
std=2*sd(no_outliers);\
std\
\pard\pardeftab720\qj\parhyphenfactor20\partightenfactor0
\cf2 \
\pard\pardeftab720\partightenfactor0
\cf2 #qqnorm(neckle_data$V1, pch = 1, frame = FALSE)\
#qqline(neckle_data$V1, col = "steelblue", lwd = 2)\
\
#qqPlot(no_outliers,distribution='norm',envelope=.96,id=FALSE, lwd=2, pch=1, cex=par("cex"),ylab=paste("Sample quantiles"),grid=FALSE)\
#grid(lty="dotted")\
#shapiro.test(no_outliers)\
\
\
#gg <- ggplot(data = neckle_data, mapping = aes(sample = V1)) +\
 #   stat_qq_band(conf = 0.99) +\
  #  stat_qq_line() +\
   # stat_qq_point() +\
   # labs(x = "Theoretical Quantiles", y = "Sample Quantiles")\
\
#gg\
\
\
\
\
\
#------------------------------------------------------------------------graph to latex----------------------------------------------------------\
\
\
tikz(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Publikacje w\uc0\u322 asne/Manuscripts/Optical_neckle_meas/figs/R_graphs/first_der6mm.tex", width = 4, height = 4)\
plot(pred1,col='black',lwd=2,lty=1, type="l", ylab= 
\f1\fs26 \cf5 expression\cf4 (d~y~\cf6 '/'\cf4 ~d~x)
\f0\fs24 \cf2 , xlab= ("Pixels"))\
grid(lty="dotted")\
abline(v=min1, lty=6, lwd=3)\
abline(v=max1, lty=6, lwd=3)\
dev.off()\
\
\
\
\
tikz(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Publikacje w\uc0\u322 asne/Manuscripts/Optical_neckle_meas/figs/R_graphs/signal6mm.tex", width = 4, height = 4)\
plot(x, y, type="l", col=grey(.5), ylab=(expression(ADC[pixel]*" ["*a.u.*"]")), xlab= ("Pixels"))\
grid(lty="dotted")\
lines(x, y_sym_na.rm, col="black",lwd=2)\
lines(fit1,col='black',lwd=3,lty=3)\
abline(v=min1, lty=6, lwd=3)\
abline(v=max1, lty=6, lwd=3)\
dev.off()\
\
\
\
\
\
\
\
#--------------------------Power stability-------------------------------------------------\
first_diode = read.csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Optical_neckle_measurements/diode_power_stab/#4/#1_1diode.csv", header = FALSE, sep = ",")\
tikz(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Publikacje w\uc0\u322 asne/Manuscripts/Optical_neckle_meas/figs/R_graphs/#1diode_power_large.tex", width = 4, height = 4)\
x<-seq(from = 0, to = (length(first_diode$V5)-1)/60, by = 1/60)\
y<-first_diode$V5/10^-6\
df <- data.frame(x, y)\
m <- nls(y ~ a*exp(-b*x)+c, data=df, start=list(a=max(y), b=1, c=10), trace=T)\
y_est<-predict(m,df$x)\
plot(x,y, type="l",lwd=1,ylab=(expression(P*" ["*mu*"W]")), xlab= (expression(t*" ["*h*"]")),col=grey(.5))\
grid(lty="dotted")\
lines(x,y_est,col="red",lty=1,lwd=2)\
#abline(h=mean(first_diode$V5)/10^-6, col="black")\
#abline(h=3*sd(first_diode$V5)/10^-6+mean(first_diode$V5)/10^-6, type="l",lty=2, col="black")\
#abline(h=mean(first_diode$V5)/10^-6-3*sd(first_diode$V5)/10^-6, type="l", lty=2, col="black")\
#mean(first_diode$V5)/10^-6\
#3*sd(first_diode$V5)/10^-6\
#3*sd(first_diode$V5)*100/mean(first_diode$V5)\
#summary(m)\
abs(max(y_sym_na.rm)-min(y_sym_na.rm))\
y_sym_na.rm <- movingAverage(y, 8, TRUE)\
lines(x, y_sym_na.rm, col="blue",lw=2)\
fit1<-smooth.spline(y_sym_na.rm, w = NULL, df=10)\
lines(x,fit1$y,col='red',lwd=2,lty=1)\
abs(max(fit1$y)-min(fit1$y))\
dev.off()\
\
plot(x,y,type="l",lwd=2,ylab=(expression(P*" ["*mu*"W]")), xlab= (expression(t*" ["*h*"]")))\
grid(lty="dotted")\
#from this graph set approximate starting values\
a_start<-261.35 #param a is the y value when x=0\
b_start<-500*log(5000)/a_start #b is the decay rate\
#model\
m<-nls(y~a*exp(-b*x),start=list(a=a_start,b=b_start))\
#get some estimation of goodness of fit\
cor(y,predict(m))\
#plot the fit\
lines(x,predict(m),col="red",lty=2,lwd=3)\
\
\
\
\
\
plot(x,y, type="l",lwd=2,ylim=c(mean(first_diode$V5)/10^-6-3.5*sd(first_diode$V5)/10^-6,3.5*sd(first_diode$V5)/10^-6+mean(first_diode$V5)/10^-6),ylab=(expression(P*" ["*mu*"W]")), xlab= (expression(t*" ["*h*"]")))\
grid(lty="dotted")\
abline(h=mean(first_diode$V5)/10^-6, col="black")\
abline(h=3*sd(first_diode$V5)/10^-6+mean(first_diode$V5)/10^-6, type="l",lty=2, col="black")\
abline(h=mean(first_diode$V5)/10^-6-3*sd(first_diode$V5)/10^-6, type="l", lty=2, col="black")\
mean(first_diode$V5)/10^-6\
3*sd(first_diode$V5)/10^-6\
3*sd(first_diode$V5)*100/mean(first_diode$V5)\
\
\
\
\
#--------------------------detrend+sinus fit-------------------------------------------------\
library(pracma)\
library(tikzDevice)\
first_diode = read.csv("/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Optical_neckle_measurements/diode_power_stab/#7/#1diode_linear.csv", header = FALSE, sep = ",")\
\
tikz(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Publikacje w\uc0\u322 asne/Manuscripts/Optical_neckle_meas/figs/R_graphs/#1diode_power.tex", width = 4, height = 4)\
x<-seq(from = 0, to = (length(first_diode$V4)-1), by = 1)\
y<-first_diode$V4/10^-6\
df <- data.frame(x, y)\
m <- nls(y ~ a*x+b, data=df, start=list(a=max(y), b=1), trace=T)\
y_est<-predict(m,df$x)\
plot(x,y, type="l",lwd=1,ylab=(expression(P*" ["*mu*"W]")), xlab= (expression(t*" ["*min*"]")),col=grey(.5))\
grid(lty="dotted")\
lines(x,y_est,col="red",lty=1,lwd=2)\
y_sym_na.rm <- movingAverage(y, 8, TRUE)\
lines(x, y_sym_na.rm, col="blue",lw=2)\
\
summary(m)\
abs(max(y)-min(y))\
dev.off()\
\
\
\
\
tikz(file = "/Users/DawidKucharski/Library/Mobile Documents/com~apple~CloudDocs/Dokumenty/Publikacje w\uc0\u322 asne/Manuscripts/Optical_neckle_meas/figs/R_graphs/#1diode_power_detrend.tex", width = 4, height = 4)\
\
detr<-detrend(y_sym_na.rm, tt = 'linear', bp = c())\
\
\
x<-seq(from = 0, to = (length(first_diode$V4)-1), by = 1)\
A<- ((max(detr)-min(detr))/2)\
C<-((max(detr)+min(detr))/2)\
raw.fft = fft(detr)\
truncated.fft = raw.fft[seq(1, length(detr)/2 - 1)]\
truncated.fft[1] = 0\
omega = which.max(abs(truncated.fft)) * 2 * pi / length(detr)\
res<- nls(detr ~ A * cos(omega * x + phi) + C, data = data.frame(x, detr), \
          start=list(A = A, omega = 0.12, phi=0.5, C=C), trace = TRUE)\
y_est<-predict(res,x)\
plot(x=x, y=detr, type="l",lwd=2, ylab=(expression(P*" ["*mu*"W]")), xlab= (expression(t*" ["*min*"]")),col="blue")\
grid(lty="dotted")\
lines(x,y_est,col="red",lty=1,lwd=2)\
summary(res)\
abs(max(detr)-min(detr))\
dev.off()\
\
plot(x=x, y=abs(detr-y_est), type="l",lwd=2, ylab=(expression(P*" ["*mu*"W]")), xlab= (expression(t*" ["*min*"]")),col="green")\
abs(max(abs(detr-y_est))-min(abs(detr-y_est)))}
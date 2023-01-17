# install libraries
library(Equitable)
library("Hmisc")
library("SDMTools")

#-------------------------------
# general notes
#output from the TransformE function  Td<-transformE(d, Ave=TRUE)
#summary(Td)         
# Td contains list of matrices relevant to the Transform process
#Transforms are :Equitable =Td$ET.x  least squared=Td$l.s.x Equitable Transform based only on average column= Td$Ave.ET.x
#(access via Td$l.s.x etc)

# smat -original data set

#l.s. prefix indicates least squares result
#s=slope, b=intercept sse =std error of slope bse=std error of intercept
#r2=coef. of determination N # of points in fit pslope= prob. for no correlation node== indication if fit is due to a node
#l.s.s     l.s.sse                                                    m list used for transform
# l.s.b      l.s.bse   l.s.r2    l.s.N      l.s.pslope l.s.node 

# l.s.x= Least squared transform with std. dev. errors =l.s.xsd at each point
#l.s.x      l.s.xsd    l.s.Es l.s.Eb   l.s.Ep    : masked matrices included slope=Es  interceprt=Eb prob= Ep
# Equitable slopes (E.s) and intercepts (E.b) with convergence information (E.rtestxm,E.rtestbm) and errors (E.rtestxsd,E.rtestbsd)
#E.numrun is # runs to get convergence
# along with first iteration slopes/intercepts (E.s1,E.b1) and iterations with std. dev. errors  (E.sd1, E.bsd1) 
# E.s        E.numrun    E.rtestxm   E.rtestxsd  E.s1       E.sd1  E.sN   E.snode 
# E.b        E.numrun    E.rtestbm   E.rtestbsd  E.b1       E.bsd1  E.bN  
# Equitable tranform (ET.x) with std. dev. errors at each point (ET.xsd) and #number of points averaged to get Transform value (ET.N)
# ET.x       ET.xsd    ET.N   ET.Es ET.Eb   ET.Ep             : masked matrices included slope=Es  interceprt=Eb prob= Ep
# Ave.ET.x   Ave.ET.xsd Ave.ET.N Ave.ET.Es Ave.ET.Eb   Ave.ET.Ep transform based on only average column: masked matrices included

#--------------------------------------------------
# examples
#d is two dimensional data set
#for example 
d<-eg5(10,5)   # 150 and 75
#transform data set and produce least squares  and equitable matrices with associated probabilites (details listed above)
Td_noise<-transformE(d, Ave=TRUE)
runstats(Td_noise)
#if d is the data set to be analyzed skip down to:  how to easily visualize the transforms

#other examples of 2d separable functions at different resolutions
#4C D E F
d<-eg0(7,7)         # 105 rows and 70 columns   # True case for "averaging" f(x)=1, u=0 ,g=long period wave
d<-eg1(6,15)          # 90 and 150      #: f=u, g linear functions of r and c  simplifies to f(x)*(g(t)+1)  zero intercept
d<-eg2(10,15)         # 150 and 150     #example 2  long wavelength wave in f only : g and u linear in r and c
d<-eg3(20,10)        # 300 and 100      #example 3    f varies as sqrt of c ,u sum of linear term +long wavelength wave : g sum of 2 short period waves (same phase
d<-eg4(1,1)         #15 10              #example 4  f sum of sqrt of c and very large scale wave:  u linear in c, g sum of two waves 
d<-eg5(6,6)         # 90  90            #example 5: f sqrt of c * short wavelength wave,  u sum of linear in c and short period wave: g sum of two waves
d<-eg6(10,15)         # 150 and 150     #example 6 f is tan of c, u linear in c;  g linear in r
d<-eg7(10,15)         # 150 and 150     #example 7: like 5 but u is 0 
d<-eg8(5,5)         # 150 and 150     #example 8: normally distributed random numbers for f g and u (Equitable system completely described by f,g,u)
# 2N+M points describe NxM system


#if d is a two dimensional separable signal and you want to add noise to it: 
d<-eg4(10,5)
#normal noise run
Td<-transformE(d, Ave=TRUE)   #it is useful to construct the signal trnasform to compare later with the noisy transform
runstats(Td)                  #stats should show the transform correspond to the original data if the system was separable


sdd<-sd(d,na.rm=TRUE)         #find the std dev of the overall signal and add normally distributed noise
# that has a std. dev that is some fraction (fac) of this signal std dev 
fac<-3/4
d_noise<-d+rnorm(prod(dim(d)),mean=0,sd=fac*sdd)    #normal distribution with std dev of fac*d's std dev
d_noise<-matrix(d_noise,nrow=nrow(d),ncol=ncol(d))
rownames(d_noise)<-rownames(d); colnames(d_noise)<-colnames(d)
Td_noise<-transformE(d_noise, Ave=TRUE)   #it is useful to construct the signal trnasform to compare later with the noisy transform
runstats(Td_noise)                  #stats should show the transform correspond to the original data if the system was separable
runstatsNS(Td,Td_noise) 

#to shorten the above, use:   example 4 resolution of 10xRows,5XCols normally distributed noise
Td<-Tnorm(rmult=10,cmult=5,FUN=eg4,fac=1.5,noise=FALSE)   #constructs 2d data signal data and transform in Td along with stats
#change fac to add more or less this proportion of the signal as normally distributed noise
Td_noise<-Tnorm(rmult=10,cmult=5,FUN=eg4,fac=3/4)       # noisy data and Transform in Td_noise along with stats
runstatsNS(Td,Td_noise)                                               # noisy Transform compared to signal  stats                                     
#the above code creates tranformations of both a signal data set and a "noisy data set"


#How to visualize these Transforms
#The following plot functions can be used : multiple plots sre created dependent on flags (note remove signal=Td$smat option if no signal to compare with is possible)
#the plots below can be run for any transformed data set
plotsome(Td_noise, signal=Td$smat,indiv=TRUE,of=TRUE,errb=TRUE)     #images and contours of Equitable transform


plotsome(Td_noise,signal=Td$smat,images=FALSE,versus=TRUE,of=TRUE,lf=TRUE)  #compares Original data, Least squares and Equitable Transforms to signal
plotsome(Td_noise,images=FALSE,versus=TRUE,of=TRUE,lf=TRUE)  #compares Original data, Least squares and Equitable Tranforms to original if no signal available
#setting of or lf to FALSE will eliminate plots of orignal and least squres data respectively
plotsome(Td_noise,signal=Td$smat,images=FALSE,columns=TRUE,of=TRUE,lf=TRUE) #plots all columns together for original, least squares,signal and equitable
plotsome(Td_noise,images=FALSE,columns=TRUE,of=TRUE,lf=TRUE) #if no signal is available
plotsome(Td_noise,transpose=TRUE,images=FALSE,columns=TRUE,of=TRUE,lf=TRUE) #with signal show all the rows plotted together 
plotsome(Td_noise,signal=Td$smat,transpose=TRUE,images=FALSE,columns=TRUE,of=TRUE,lf=TRUE) # show 
plotsome(Td_noise,images=FALSE, xvsref=ncol(Td_noise$smat) )  #plot all columns from Ave contructed transform versus the Average (reference column) 
plotsome(Td,images=FALSE, xvsref=ncol(Td$smat) ) #plot all Signal columns from Ave contructed transform versus the Average (reference column) 

plotsquares(Td_noise, signal=Td,columns=TRUE,of=TRUE) #all columns plotted together
plotsquares(Td_noise, signal=Td,transpose=TRUE,images=FALSE,columns=TRUE,of=TRUE) #all rows plotted together
plotsquares(Td_noise,images=FALSE,indiv=TRUE,of=FALSE,errb=TRUE,stderror=TRUE,slimits=c(0,3) ,blimits=c(-10,+10))#std error of equitable slopes and interceprs
plotsquares(Td_noise,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE,stderror=TRUE,slimits=c(0,3) ,blimits=c(-10,+10)) #with least sq, fits

plotsquares(Td_noise,signal=Td,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE,stderror=TRUE,lf=TRUE,slimits=c(0,3) ,blimits=c(-10,+10)) #with signal

plotsquares(Td_noise,signal=Td,images=FALSE,indiv=TRUE,of=TRUE,ef=FALSE,errb=TRUE,lf=TRUE,slimits=c(0,3) ,blimits=c(-10,+10)) #leaast squares with std error
#plotting the errors shows which columns are the least equitable

#default number of individual plots is num=3
#option to put in number desirted eg. num<-10  plots 10 columns equally spaced across data OR
#set num<-c(1,5,6,10)  #plots columns 1 5 6 and 10
plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,lf=TRUE ) #plots indiviual columns along with signal, least squares fit and original
plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE )#plots individual columns of Equitable Transform with std dev at each point(with original data and signal)
plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots individual columns of Equitable Transform with std error of mean at each point

plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE )#plots individual columns of Equitable Transform with std dev at each point(with original data and signal)
plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots individual columns of Equitable Transform with std error of mean at each point

#example using particular columns
num<-c(1,10,11,15)
plotsome(Td_noise,num,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,lf=TRUE )

#same as above but for individual rows rather than columns
plotsome(Td_noise,transpose=TRUE,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,lf=TRUE ) #plots indiviual columns along with signal, least squares fit and original
plotsome(Td_noise,transpose=TRUE,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE )#plots individual columns of Equitable Transform with std dev at each point(with original data and signal)
plotsome(Td_noise,transpose=TRUE,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)


#this shows how input data sets to previously constructed Equitable matrices determines outcome : if signal is input , output is closer to signal
#if signal is nearly exactly reproduced, then the matrices accurately represent the signal matrices
#when the original data set is used instead , it is contaminated with noise and some of this noise is reproduced
# through averaging or some other means if a signal at one location has more validity then it or multiple locations coule be used to find other locations
#the vectors x and t can be any set of coordinates within the data set and any data set can then be transformed via the matrices 
x<-1:ncol(Td_noise$smat)
t<-1:nrow(Td_noise$smat)
ETS<-transave1(Td$smat,Td_noise,x=x,equita=TRUE,diagonal=TRUE)    #use signal transave if Em available directly
ET<-transave1(Td_noise$smat,Td_noise,x=x,equita=TRUE,diagonal=TRUE)    #use signal transave if Em available directly
sdat<-Td$smat         #signal 
odat<-Td_noise$smat   #original 
dat<-ET$Ave.ET.x      #equitable transform based on single noisy column and derived equitable matrices
dats<-ETS$Ave.ET.x     #equitable transform based on single signal column and derived equitable matrices
cat("\n sd T-E based on noisy ref: sdcalc", sd(Td_noise$smat-sdat, na.rm = TRUE)*sqrt(1/nrow(dats)+1/ncol(dats)))
sd(dat-sdat, na.rm = TRUE)
cat("\n sd T-E based on Signal ref: sdcalc", sd(Td_noise$smat-sdat, na.rm = TRUE)*sqrt(1/nrow(dats)+1/ncol(dats)))
sd(dats-sdat, na.rm = TRUE)     # error when using original signal for  input  to transform

limits<-c(min(dat,na.rm=TRUE),max(dat,na.rm=TRUE))
plot(sdat,odat,main=paste("Noisy: Original","\n vs Signal"),ylim=limits,xlim= limits)
lines(seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ),seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ))
plot(sdat,dat,main=paste("Noisy: Equitable Transform REFERENCED (noisy)","\n vs Signal"),ylim=limits,xlim= limits)
lines(seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ),seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ))
plot(sdat,dats,main=paste("Noise matrix: Equitable Transform REFERENCED (Signal)\n","\n vs Signal"),ylim=limits,xlim= limits)
lines(seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ),seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ))



#below calculates transform using only column that has maximum data value then plots what is available from this
#the options for x include the last column (usually the average), the max column or a range of column
#t  or a set of rows can be similarily defined. the first input to transave1 is a data set of values at these points that will be used to 
#define all other points. Useful in predicting spatial distribution given time profile at one or more location
x<-ncol(Td_noise$smat)  #use average : if t not specified all rows are used
#x<-NULL    #default uses average as well
#x<-"max"   #use column that contains maximum in data set
#x<-5:20    #use a range in columns to determine the transform
ETave<-transave1(Td_noise$smat,Td_noise,x=x,equita=TRUE,diagonal=TRUE)    #use transave if Em available directly
ETave<-transave1(Td_noise$smat,Td_noise,x=x,equita=TRUE,diagonal=TRUE)    #use transave if Em available directly
ETaveS<-transave1(Td$smat,Td_noise,x=x,equita=TRUE,diagonal=TRUE)    #use signal transave if Em available directly

ref<-x[1]
sdat<-Td$smat
odat<-Td_noise$smat
dat<-ETave$Ave.ET.x
dats<-ETaveS$Ave.ET.x
cat("\n sd T-E based on ref: sdcalc (rows and cols)", sd(Td_noise$smat-sdat, na.rm = TRUE)*sqrt(1/nrow(dats)+1/ncol(dats)))
sd(dat-sdat, na.rm = TRUE)
cat("\n sd T-E based on Signal ref: sdcalc using rows only", sd(Td_noise$smat-sdat, na.rm = TRUE)*sqrt(1/nrow(dats)))
sd(dats-sdat, na.rm = TRUE)     # error when using original signal for  input  to transform
plot_vsref(dat,ref,
           main=paste("Noisy: Constructed from Max Ref\n compared to Ref column ",ref," (Black)"),
           lty="l")
plot_vsref(dats,ref,
           main=paste("Noisy Matrix: Data Constructed from Ref:Signal column\n compared to Ref column",ref," (Black)"),
           lty="l")
plot_vsref(sdat,ref,
           main=paste(" Signal compared to Ref\n compared to Ref column",ref," (Black)"),
           lty="l")

limits<-c(min(dat,na.rm=TRUE),max(dat,na.rm=TRUE))
imagenan(dat,main=paste0("T  constructed from REFERENCED column : ",ref), zlim=limits/1)
imagenan(sdat,main=paste0("SIGNAL  "), zlim=limits/1)
imagenan(dats,main=paste0("T  constructed from REFERENCED SIGNAL column : ",ref), zlim=limits/1)
imagenan(odat,main=paste0("Original "), zlim=limits*1)

plot(odat,dat,main=paste("Noisy: Original","\n vs Signal"),ylim=limits,xlim= limits)
lines(seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ),seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ))
plot(sdat,dat,main=paste("Noisy: Equitable Transform REFERENCED",ref,"\n vs Signal"),ylim=limits,xlim= limits)
lines(seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ),seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ))
plot(sdat,dats,main=paste("Noise matrix: Equitable Transform REFERENCED(Signal)\n","\n vs Signal"),ylim=limits,xlim= limits)
lines(seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ),seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ))

plot(odat,dats,main=paste("Noise matrix: Equitable Transform REFERENCED(Signal)\n","\n vs Original"),ylim=limits,xlim= limits)
lines(seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ),seq(limits[1],limits[2],by = (limits[2]-limits[1])/100 ))



#to run multiple noise levels or to see the effects of changing the resolution on the tranform use:
rsdr1_10c4eg4_fac0.5n<-runsdnormal(start=1,end=10,cmult=4,fac=0.5, actualFunction=eg4) 
# above runs example 4 data set with 100 "columns" and from 15 to 150 rows using a normal noise distribution with 1/2 the std dev of the whole data
csdr10c4eg4_fac0_1.3n<-changenoisenormal(rmult=10,cmult=4,start=0.,end=1.3,inc=0.2, actualFunction=eg4) 
# above runs example 4 data set with 100 "columns" and 150 rows using a normal noise distribution from 0 to 1.3 times the std dev of the whole data
#in increments of 0.1


#more examples of multiple runs below
rsdr1_10c10eg0_fac0.5n<-runsdnormal(start=1,end=10,cmult=10,fac=0.5, actualFunction=eg0)
csdr10c10eg0_fac0_2n<-changenoisenormal(rmult=10,cmult=10,start=0.,end=1.3,inc=0.1, actualFunction=eg0) 

rsdr1_10c10eg1_fac0.5n<-runsdnormal(start=1,end=10,cmult=10,fac=0.5, actualFunction=eg1)
csdr10c10eg1_fac0_1.3n<-changenoisenormal(rmult=10,cmult=10,start=0.,end=1.3,inc=0.1, actualFunction=eg1) 

rsdr1_10c10eg2_fac0.5n<-runsdnormal(start=1,end=10,cmult=10,fac=0.5, actualFunction=eg2)
csdr10c10eg2_fac0_1.3n<-changenoisenormal(rmult=10,cmult=10,start=0.,end=1.3,inc=0.1, actualFunction=eg2) 

rsdr1_10c10eg3_fac0.5n<-runsdnormal(start=1,end=10,cmult=10,fac=0.5, actualFunction=eg3)
csdr10c10eg3_fac0_1.3n<-changenoisenormal(rmult=10,cmult=10,start=0.,end=1.3,inc=0.1, actualFunction=eg3) 

rsdr1_10c10eg4_fac0.5n<-runsdnormal(start=1,end=10,cmult=10,fac=0.5, actualFunction=eg4)
csdr10c10eg4_fac0_1.3n<-changenoisenormal(rmult=10,cmult=10,start=0.,end=1.3,inc=0.1, actualFunction=eg4) 

rsdr1_10c10eg5_fac0.5n<-runsdnormal(start=1,end=10,cmult=10,fac=0.5, actualFunction=eg5)
csdr10c10eg5_fac0_1.3n<-changenoisenormal(rmult=10,cmult=10,start=0.,end=1.3,inc=0.1, actualFunction=eg5) 

rsdr1_10c10eg6_fac0.5n<-runsdnormal(start=1,end=10,cmult=10,fac=0.5, actualFunction=eg6)
csdr10c10eg6_fac0_1.3n<-changenoisenormal(rmult=10,cmult=10,start=0.,end=1.3,inc=0.1, actualFunction=eg6) 




#uses old read data to access phenology and temperatures etc.
signame<-"originalSimulation with Ax pos and neg sig"
signame<-"originalSimulation step function signal"
x<-read.table(file=paste0("C:/Alex climate data_organized/archived/", signame), sep=",")
head(x)
d<-data.matrix(x)
Td<-transformE(d,Ave=FALSE)   # transforms d and results put in Td : summarized below
runstats(Td)

Td<-transformE(d,Ave=FALSE)   # transforms d and results put in Td : summarized below
runstats(Td)

sname<-"trial_data"
sname<-"Longterm"
sname<-"originalSimulation with Ax pos and neg with error3"
#sname<-"originalSimulation step function signal with NA"
sname<-"originalSimulation step function signal"
#sname<-"originalSimulation step function NA values"
sname<-"originalSimulation step function signal NAs"

x<-read.table(file=paste0("C:/Alex climate data_organized/archived/", sname), sep=",")
head(x)
d_noise<-data.matrix(x)

Td_noise<-transformE(d_noise,Ave=FALSE)   # transforms d and results put in Td : summarized below
runstats(Td_noise)
runstatsNS(Td,Td_noise)   

plotsome(Td_noise,of=TRUE)     #images and contours of Equitable transform
Td<-Td_noise
plotsome(Td_noise,signal=Td$smat,of=TRUE) 

#plotsome(Td_noise,signal=Td$smat,images=FALSE,versus=TRUE,of=TRUE,lf=TRUE)  #compares Original data, Least squares and Equitable Transforms to signal
plotsome(Td_noise,images=FALSE,versus=TRUE,of=TRUE,lf=TRUE)  #compares Original data, Least squares and Equitable Tranforms to original if no signal available
#setting of or lf to FALSE will eliminate plots of orignal and least squres data respectively
plotsome(Td_noise,signal=Td$smat,images=FALSE,columns=TRUE,of=TRUE,lf=TRUE) #plots all columns together for original, least squares,signal and equitable
plotsome(Td_noise,images=FALSE,columns=TRUE,of=TRUE,lf=TRUE) #if no signal is available
plotsome(Td_noise,transpose=TRUE,images=FALSE,columns=TRUE,of=TRUE,lf=TRUE) # show all the rows plotted together 
plotsome(Td_noise,images=FALSE, xvsref=ncol(Td_noise$smat) )  #plot all columns from Ave contructed transform versus the Average (reference column) 
plotsome(Td,images=FALSE, xvsref=ncol(Td$smat) ) #plot all Signal columns from Ave contructed transform versus the Average (reference column) 

#default number of individual plots is num=3
#option to put in number desirted eg. num<-10  plots 10 columns equally spaced across data OR
#set num<-c(1,5,6,10)  #plots columns 1 5 6 and 10
plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,lf=TRUE ) #plots indiviual columns along with signal, least squares fit and original
plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE )#plots individual columns of Equitable Transform with std dev at each point(with original data and signal)
plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots individual columns of Equitable Transform with std error of mean at each point

plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE )#plots individual columns of Equitable Transform with std dev at each point(with original data and signal)
plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots individual columns of Equitable Transform with std error of mean at each point

#example using particular columns
num<-c(1,8,11,15)
plotsome(Td_noise,num,images=FALSE,indiv=TRUE,of=TRUE,lf=TRUE )

#same as above but for individual rows rather than columns
plotsome(Td_noise,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,lf=TRUE ) #plots indiviual columns along with signal, least squares fit and original
plotsome(Td_noise,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE )#plots individual columns of Equitable Transform with std dev at each point(with original data and signal)
plotsome(Td_noise,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)

#limits can also be put on inplots (see end of this script)
plotsome(Td_noise,num,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,lf=TRUE )

#same as above but for individual rows rather than columns
plotsome(Td_noise,transpose=TRUE,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,lf=TRUE ) #plots indiviual columns along with signal, least squares fit and original
plotsome(Td_noise,transpose=TRUE,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE )#plots individual columns of Equitable Transform with std dev at each point(with original data and signal)
plotsome(Td_noise,transpose=TRUE,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)
plotsquares(Td_noise,signal=Td,transpose=TRUE,images=FALSE,columns=TRUE,of=TRUE)#Transposed images and contours of slope/intercept matrices
plotsquares(Td_noise,transpose=TRUE,images=FALSE,columns=TRUE,of=TRUE) #Transposedimages and contours of slope/intercept matrices
plotsquares(Td_noise,signal=Td, columns=TRUE,of=TRUE) #images and contours of slope/intercept matrices
plotsquares(Td_noise, columns=TRUE,of=TRUE) #images and contours of slope/intercept matrices
plotsquares(Td_noise, signal=Td,columns=TRUE,of=TRUE,slimits=c(0,2),blimits=c(-2,2)) #limits on slopes s and interceptsb
plotsquares(Td_noise, signal=Td,columns=TRUE,of=TRUE,xlimits=c(35,80),slimits=c(0,2),blimits=c(-2,2)) #also limit on row range
plotsome(Td_noise, signal=Td$smat,columns=TRUE,of=TRUE,limits=c(0,10)) #images and contours of s
plotsome(Td_noise,signal=Td$smat,images=FALSE, indiv=TRUE,of=TRUE,limits=c(0,2)) #images and contours of s
plotsome(Td_noise,signal=Td$smat, columns=TRUE,of=TRUE,limits=c(0,2)) #images and contours of s
plotsome(Td_noise,signal=Td$smat,images=FALSE, columns=TRUE,indiv=TRUE,of=TRUE,limits=c(0,2),xlimits=c(70,120)) #images and contours of s
plotsome(Td_noise,signal=Td$smat,images=FALSE, indiv=TRUE,of=TRUE,limits=c(0,2),xlimits=c(70,120)) 
plotsome(Td_noise,signal=Td$smat,images=FALSE, indiv=TRUE,of=TRUE,limits=c(0,2),xlimits=c(1,120),errb=TRUE) #images and co
plotsome(Td_noise,signal=Td$smat, columns=TRUE,of=TRUE) #images and contours of s


sname<-"Longterm"
x<-read.table(file=paste0("C:/Alex climate data_organized/archived/", sname), sep=",")
head(x)
d_noise<-data.matrix(x)

Td_noise<-transformE(d_noise,Ave=FALSE)   # transforms d and results put in Td : summarized below
runstats(Td_noise)
plotsquares(Td_noise,images=FALSE,columns=TRUE,of=TRUE)#Tra
num=c(8,32)
plotsquares(Td_noise,num,images=FALSE,indiv=TRUE,lf=TRUE)#Tra
plotsquares(Td_noise,num,images=FALSE,indiv=TRUE,of=FALSE,errb=TRUE)#Tra
plotsquares(Td_noise,num,images=FALSE,indiv=TRUE,lf=TRUE,errb=TRUE)#Tra    summary(Td_noise)
plotsquares(Td_noise,num,images=FALSE,indiv=TRUE,lf=TRUE,ef=FALSE,errb=TRUE)#Tra

plotsquares(Td_noise,num,images=FALSE,indiv=TRUE,errb=TRUE)#Tra
plotsquares(Td_noise,num,images=FALSE,indiv=TRUE,of=FALSE,errb=TRUE,stderror=TRUE,lf=TRUE,slimits=c(0,1.2) ,blimits=c(-4,+4))#Tra

plotsquares(Td_noise,images=FALSE,indiv=TRUE,of=TRUE)#Tra


sname<-"originalSimulation step function signal"
x<-read.table(file=paste0("C:/Alex climate data_organized/archived/", sname), sep=",")
head(x)
d<-data.matrix(x)

Td<-transformE(d,Ave=FALSE)   # transforms d and results put in Td : summarized below
runstats(Td)
sname<-"originalSimulation step function signal NAs"

x<-read.table(file=paste0("C:/Alex climate data_organized/archived/", sname), sep=",")
head(x)
d_noise<-data.matrix(x)

Td_noise<-transformE(d_noise,Ave=FALSE)   # transforms d and results put in Td : summarized below
runstats(Td_noise)
runstatsNS(Td,Td_noise)   

plotsome(Td_noise,of=TRUE)     #images and contours of Equitable transform
Td<-Td_noise
plotsome(Td_noise,signal=Td$smat,of=TRUE) 


# #testing normcol normrow and norm
# 
# #normrow results should show up best for small # of space(columns)
# #to shorten the above, use:   example 4 resolution of 10xRows,5XCols normally distributed noise (with each column noise mean over time =0)
# Td<-Tnormrow(rmult=3,cmult=1,FUN=eg4,fac=1.5,noise=FALSE)   #constructs 2d data signal data and transform in Td along with stats
# #change fac to add more or less this proportion of the signal as normally distributed noise (with each row noise mean over space =0)
# Td_noise<-Tnormrow(rmult=3,cmult=1,FUN=eg4,fac=1.5)       # noisy data and Transform in Td_noise along with stats
# runstatsNS(Td,Td_noise)                                               # noisy Transform compared to signal  stats                                     
# #the above code creates tranformations of both a signal data set and a "noisy data set"
# plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots indiv
# plotsome(Td_noise,signal=Td$smat,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots indiv
# 
# #compare to normrow run
# #to shorten the above, use:   example 4 resolution of 10xRows,5XCols normally distributed noise
# Td<-Tnorm(rmult=3,cmult=1,FUN=eg4,fac=1.5,noise=FALSE)   #constructs 2d data signal data and transform in Td along with stats
# #change fac to add more or less this proportion of the signal as normally distributed noise
# Td_noise<-Tnorm(rmult=3,cmult=1,FUN=eg4,fac=1.5)       # noisy data and Transform in Td_noise along with stats
# runstatsNS(Td,Td_noise)                                               # noisy Transform compared to signal  stats                                     
# #the above code creates tranformations of both a signal data set and a "noisy data set"
# plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots indiv
# plotsome(Td_noise,signal=Td$smat,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots indiv
# 
# #compare to normrow run
# #to shorten the above, use:   example 4 resolution of 10xRows,5XCols normally distributed noise
# Td<-Tnormcol(rmult=3,cmult=1,FUN=eg4,fac=1.5,noise=FALSE)   #constructs 2d data signal data and transform in Td along with stats
# #change fac to add more or less this proportion of the signal as normally distributed noise
# Td_noise<-Tnormcol(rmult=3,cmult=1,FUN=eg4,fac=1.5)       # noisy data and Transform in Td_noise along with stats
# runstatsNS(Td,Td_noise)                                               # noisy Transform compared to signal  stats                                     
# #the above code creates tranformations of both a signal data set and a "noisy data set"
# plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots indiv
# plotsome(Td_noise,signal=Td$smat,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots indiv
# 
# avecol<-colMeans(Td_noise$ET.x-Td$smat, na.rm=TRUE)
# plot(avecol, ylim=c(-2,2) ); lines(rep(0,10))
# averow<-rowMeans(Td_noise$ET.x-Td$smat, na.rm=TRUE)
# plot(averow, ylim=c(-2,2) ); lines(rep(0,10))
# 
# 
# #normcol results should show up best for small # ot times(rows)
# #to shorten the above, use:   example 4 resolution of 10xRows,5XCols normally distributed noise (with each column noise mean over time =0)
# Td<-Tnormcol(rmult=1,cmult=3,FUN=eg4,fac=1.5,noise=FALSE)   #constructs 2d data signal data and transform in Td along with stats
# #change fac to add more or less this proportion of the signal as normally distributed noise
# Td_noise<-Tnormcol(rmult=1,cmult=3,FUN=eg4,fac=1.5)       # noisy data and Transform in Td_noise along with stats
# runstatsNS(Td,Td_noise)                                               # noisy Transform compared to signal  stats                                     
# #the above code creates tranformations of both a signal data set and a "noisy data set"
# plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots indiv
# plotsome(Td_noise,signal=Td$smat,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots indiv
# 
# #compare to normcol run
# #to shorten the above, use:   example 4 resolution of 10xRows,5XCols normally distributed noise
# Td<-Tnorm(rmult=1,cmult=3,FUN=eg4,fac=1.5,noise=FALSE)   #constructs 2d data signal data and transform in Td along with stats
# #change fac to add more or less this proportion of the signal as normally distributed noise
# Td_noise<-Tnorm(rmult=1,cmult=3,FUN=eg4,fac=1.5)       # noisy data and Transform in Td_noise along with stats
# runstatsNS(Td,Td_noise)                                               # noisy Transform compared to signal  stats                                     
# #the above code creates tranformations of both a signal data set and a "noisy data set"
# plotsome(Td_noise,signal=Td$smat,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots indiv
# plotsome(Td_noise,signal=Td$smat,transpose=TRUE,images=FALSE,indiv=TRUE,of=TRUE,errb=TRUE ,stderror=TRUE)#plots indiv




library(Equitable)
library("Hmisc")
library("SDMTools")

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
#l.s.x      l.s.xsd    l.s.Es l.s.Eb   l.s.Ep    : masked matrices included masked slope=Es  interceprt=Eb prob= Ep used in Transform
# Equitable slopes (E.s) and intercepts (E.b) with convergence information (E.rtestxm,E.rtestbm) and errors (E.rtestxsd,E.rtestbsd)
#E.numrun is # runs to get convergence
# along with first iteration slopes/intercepts (E.s1,E.b1) and iterations with std. dev. errors  (E.sd1, E.bsd1) 
# E.s        E.numrun    E.rtestxm   E.rtestxsd  E.s1       E.sd1  E.sN   E.snode 
# E.b        E.numrun    E.rtestbm   E.rtestbsd  E.b1       E.bsd1  E.bN  
# Equitable tranform (ET.x) with std. dev. errors at each point (ET.xsd) and #number of points averaged to get Transform value (ET.N)
# ET.x       ET.xsd    ET.N   ET.Es ET.Eb   ET.Ep             : masked matrices included maskedslope=Es  interceprt=Eb prob= Ep
# Ave.ET.x   Ave.ET.xsd Ave.ET.N Ave.ET.Es Ave.ET.Eb   Ave.ET.Ep transform based on only average column: masked matrices included


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
fac<-1/2
d_noise<-d+rnorm(prod(dim(d)),mean=0,sd=fac*sdd)    #normal distribution with std dev of fac*d's std dev
d_noise<-matrix(d_noise,nrow=nrow(d),ncol=ncol(d))
rownames(d_noise)<-rownames(d); colnames(d_noise)<-colnames(d)
Td_noise<-transformE(d_noise, Ave=TRUE)   #it is useful to construct the signal trnasform to compare later with the noisy transform
runstats(Td_noise)                  #stats should show the transform correspond to the original data if the system was separable
runstatsNS(Td,Td_noise) 




#to shorten the above, use:   example 4 resolution of 10xRows,5XCols normally distributed noise
Td<-Tnorm(rmult=10,cmult=5,FUN=eg4,fac=1/2,noise=FALSE)   #constructs 2d data signal data and transform in Td along with stats
#change fac to add more or less this proportion of the signal as normally distributed noise
Td_noise<-Tnorm(rmult=10,cmult=5,FUN=eg4,fac=1/2)       # noisy data and Transform in Td_noise along with stats
                                              # noisy Transform compared to signal  stats                                     
#the above code creates tranformations of both a signal data set and a "noisy data set"
plotsummary(Td_noise,Td)


d<-eg2(10,5)
#normal noise run
Td<-transformE(d, Ave=TRUE)   #it is useful to construct the signal trnasform to compare later with the noisy transform
runstats(Td)     
plotsummary(Td)

d<-eg3(10,5)
#normal noise run
Td<-transformE(d, Ave=TRUE)   #it is useful to construct the signal trnasform to compare later with the noisy transform
runstats(Td)     
plotsummary(Td)

d<-eg4(10,5)
#normal noise run
Td<-transformE(d, Ave=TRUE)   #it is useful to construct the signal trnasform to compare later with the noisy transform
runstats(Td)     
plotsummary(Td)


d<-eg5(10,5)
#normal noise run
Td<-transformE(d, Ave=TRUE)   #it is useful to construct the signal trnasform to compare later with the noisy transform
runstats(Td)     
plotsummary(Td)

d<-eg6(10,5)
#normal noise run
Td<-transformE(d, Ave=TRUE)   #it is useful to construct the signal trnasform to compare later with the noisy transform
runstats(Td)     
plotsummary(Td)

d<-eg8(10,5)
#normal noise run
Td<-transformE(d, Ave=TRUE)   #it is useful to construct the signal trnasform to compare later with the noisy transform
runstats(Td)     
plotsummary(Td)

d<-eg1(10,5)
#normal noise run
Td<-transformE(d, Ave=TRUE)   #it is useful to construct the signal trnasform to compare later with the noisy transform
runstats(Td)     
plotsummary(Td)

d<-eg0(10,5)
#normal noise run
Td<-transformE(d, Ave=TRUE)   #it is useful to construct the signal trnasform to compare later with the noisy transform
runstats(Td)     
plotsummary(Td)


#below shows functions used in plotsummary that could be individually used
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

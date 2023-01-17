# Equitable Package  related to the Equitable Transform.

Elphinstone, C. and Henry, G. (submitted to Ecosphere, 2019) show the multipurpose uses for the equitable transform. Functions useful 
for analysing two dimensional data sets are included in this package. A two dimensional data set (d) is transformed via Td<-transformE(d) 
and a variety of matrices associated with the equitable transform is placed in Td. An example signal data set can be made via d<-eg4(2,2) 
(or other "eg?" type functions: see manual) and noise can be added to make a noisy 2D data set (d_noise) in a variety of ways as seen in the 
R script Short Examples.R. If Td and Td_noise are the results from running transformE then plotsummary(Td_noise,Td), plotsummary(Td) and 
plotsummary(Td_noise) summarize and plot information about the two transformations.


To install the package "Equitable", first install the package devtools. Then in R use:
library(devtools)
install_github("celphin/Equitable")
library(Equitable)


Some suporting files are found in: celphin/Equitable-Project

celphin/Equitable-Project/Equitable_Manual.pdf is a pdf containing a manual of the basic functions used for constructing an equitable transform from a 2 dimensional data set. 
For each function, examples are given to illustrate the use of the function.

celphin/Equitable-Project/temperatures.txt 

contains the temperature records from Alexandra fiord used in the paper Elphinstone and Henry (Ecosphere, submitted 2019). 
After reading this file into R into a variable d and installing the Equitable transform package,  one can runTd<-transformE(d) and plotsummary(Td) 
to get information on these temperture data.

celphin/Equitable-Project/Dryas_Phenology_1998_Site_Willow.txt

contains Dryas phenology data from 1998 at Willow site at Alexandra Fiord. Used in the paper Elphinstone and Henry (Ecosphere, submitted 2019). 

The files Short Examples.R and Examples.R in Equitable-Project show other examples of how the tranfrom can be used on 2D data sets. You can use your own real data or make simulated data. Short Examples.R shows how you can make your own noisy NA filled data sets in order to experiment with the transform (using the function transformE in the package).

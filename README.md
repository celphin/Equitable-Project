# Equitable-Project
Package and files related to the Equitable Transform.

To install the package "Equitable" first install the package devtools. Then in R use:

library(devtools)

install_github("celphin/Equitable-Project/Equitable")

The files Short Examples.R  and Examples.R in Equitable-Project show how the tranfroma can be used on 2D data sets. You can use your own real data or make simulted data. Short Examples.R shows how you can make your own noisy NA filled data sets in order to experiment with the transform (using the function transformE in the package).

Elphinstone, C. and Henry, G. (submitted to Ecology, 2018) show the multipurpose uses for the equitable transform. Functions useful for analysing two dimensional data sets are included in this package. A two dimensional data set (d) is transformed via Td<-transformE(d) and a variety of matrices associated with the equitable transform is placed in Td. A signal data set can be made via d<-eg4(2,2) and noise can be added to make a noisy 2D data set (d_noise) in a variety of ways as seen in the R script Short Examples.R. If Td and Td_noise are the results from running transformE then plotsummary(Td_noise,Td), plotsummary(Td) and plotsummary(Td_noise) summarize and plot information about the two transformations. 

# Equitable-Project files related to package "celphin/Equitable"
celphin/Equitable-Project/Equitable_0.1.0 is a pdf containing a manual of the basic fucntions used for constructing an equitable transform from a 2 dimensional data set. For each function, examples are given to illustrate the use of the function.

celphin/Equitable-Project/temperatures contains the temperature records from Alexandra fiord used in the paper Elphinstone and Henry (Ecosphere, submitted 2019). After reading this file into R into a variable d and installing the Equitable transform package, one can runTd<-transformE(d) and plotsummary(Td) to get information on these temperture data.

celphin/Equitable-Project/phenology Some Dryas phenology data used in the paper Elphinstone and Henry (Ecosphere, submitted 2019).

The files Short Examples.R and Examples.R in Equitable-Project show other examples of how the tranfrom can be used on 2D data sets. You can use your own real data or make simulated data. Short Examples.R shows how you can make your own noisy NA filled data sets in order to experiment with the transform (using the function transformE in the package).

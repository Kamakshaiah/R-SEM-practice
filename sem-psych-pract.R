#First, create a sem like model with a factor model of x and ys with correlation Phi
fx <-matrix(c( .9,.8,.6,rep(0,4),.6,.8,-.7),ncol=2)  
fy <- matrix(c(.6,.5,.4),ncol=1)
rownames(fx) <- c("V","Q","A","nach","Anx")
rownames(fy)<- c("gpa","Pre","MA")
Phi <-matrix( c(1,0,.7,.0,1,.7,.7,.7,1),ncol=3)
#now create this structure
gre.gpa <- sim.structural(fx,Phi,fy)
print(gre.gpa,2)  
#correct for attenuation to see structure
#the raw correlations are below the diagonal, the adjusted above
round(correct.cor(gre.gpa$model,gre.gpa$reliability),2) 

#These are the population values,
# we can also create a correlation matrix sampled from this population
GRE.GPA  <- sim.structural(fx,Phi,fy,n=250,raw=FALSE)
lowerMat(GRE.GPA$r)

#or we can show data sampled from such a population
GRE.GPA  <- sim.structural(fx,Phi,fy,n=250,raw=TRUE)
lowerCor(GRE.GPA$observed)



congeneric <- sim.structure(f=c(.9,.8,.7,.6)) # a congeneric model 
congeneric 

#now take this correlation matrix as a population value and create samples from it
example.congeneric <- sim.correlation(congeneric$model,n=200) #create a sample matrix
lowerMat(example.congeneric ) #show the correlation matrix
#or create another sample and show the data
example.congeneric.data <- simCor(congeneric$model,n=200,data=TRUE) 
describe(example.congeneric.data)
lowerCor(example.congeneric.data )
example.skewed <- simCor(congeneric$model,n=200,vars=c(1,2),data=TRUE,skew="log") 
describe(example.skewed)

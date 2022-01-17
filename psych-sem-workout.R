# SIMULATIONS

# congeneric (common factor)
# creates population, observed and also respective correlation matrices
sc <- sim.congeneric(c(0.8, 0.9, 0.8, 0.9, 0.6), N=30, short = FALSE)
sc$observed # simulated data
sc$model # correlation matrix
sc$pattern # variables vs. theta & error variables (latent structure)
sc$latent # latent
sc$r #correlation matrix

structure.diagram(c("a", "b", "c", "d")) # a, b, c, d are estimates of the structural model; tau equivalent assumes a=b=c=d

# hierarchical (g-factor + subfactors)

#hierarchical simulations
jensen <-  sim.hierarchical(n=50)
jensen$observed # observations (variables)

gload = matrix(c(0.9, 0.8, 0.7), nrow = 3)
fload <- matrix(c(0.9, 0.8, 0.7, rep(0, 9), 0.7, 0.6, 0.5, rep(0,9), 0.6, 0.5, 0.4), ncol = 3)
bifact <- sim.hierarchical(gload = gload, fload = fload)
omega(bifact) # bifactor model
omega(bifact, sl=FALSE) # hierarchical model

#simulate a non-hierarchical structure five factor model
fload <- matrix(c(c(c(.9,.8,.7,.6),rep(0,20)),c(c(.9,.8,.7,.6),rep(0,20)), c(c(.9,.8,.7,.6),rep(0,20)),c(c(c(.9,.8,.7,.6),rep(0,20)),c(.9,.8,.7,.6))),ncol=5)
gload <- matrix(rep(0,5))

five.factor <- sim.hierarchical(gload,fload,50,TRUE) # data set for five factor model
gload <- matrix(rep(.7,5)  )
five.factor.g <- sim.hierarchical(gload,fload,50,TRUE) #create sample data set

#compare these two with omega
#not run
#om.5 <- omega(five.factor$observed,5)
#om.5g <- omega(five.factor.g$observed,5)



# a hierachical structure


# 
# vector implies a congeneric model (4 variabls with single factor)

fx <- c(0.9, 0.8, 0.7, 0.6)
cong1 <- sim.structure(fx)
cong1

# matrix implies an independent (uncorrelated) factors model (3 factor model with 9 variables)

fx <- matrix(c(0.9, 0.8, 0.7, rep(0, 9), 0.7, 0.6, 0.5, rep(0, 9), 0.6, 0.5, 0.4), ncol = 3)
three.fact <- sim.structure(fx)
three.fact
structure.diagram(fx)

Phi = matrix(c(1, 0.5, 0.3, 0.5, 1, 0.2, 0.3, 0.2, 1), ncol = 3)
cor.f3 <- sim.structure(fx, Phi)
cor.f3
structure.diagram(fx, Phi)

# EFA and structure diagram

f3.p <- Promax(fa(cor.f3$model, 3))
structure.diagram(f3.p, cut = 0.2)

# fx and fy are matrices, and Phi =I represents their correlations
fx <- matrix(c(0.9, 0.8, 0.7, rep(0, 9), 0.7, 0.6, 0.5, rep(0, 9), 0.6, 0.5, 0.4), ncol = 3)
fy <- c(0.6, 0.5, 0.4)
Phi <- matrix(c(1, 0.48, 0.32, 0.4, 0.48, 1, 0.32, 0.3, 0.32, 0.32, 1, 0.2, 0.4, 0.3, 0.2, 1), ncol = 4)
twelveV <- sim.structure(fx, Phi, fy)$model
colnames(twelveV) <- rownames(twelveV) <- c(paste("x", 1:9, sep = ""), paste("y", 1:3, sep = ""))
round(twelveV, 2)
structure.diagram(fx, Phi, fy)

# EFA


# CFA
## 2P 2H model without competition

rm(list = ls())

fecA <- vector()
susA <- vector()
betaA <- vector() #since all beta's are same for a given pathogen, I need to store only one value
beta.MatA <- list()

fecB <- vector()
susB <- vector()
betaB <- vector()
beta.MatB <- list()

for(i in 1:100){
  fA <- rlnorm(1, -3 , 2)
  sA <- rlnorm(1, -3, 2) 
  fecA[i] <- fA  # storing the values outside the loop
  susA[i] <- sA
  betaA[i] <- fecA[i]*susA[i] #creating betas
  temp.A <- matrix(c(betaA[i], betaA[i]/2, betaA[i]/2, betaA[i]), nrow = 2, ncol = 2, byrow = T) # creating beta matrix
  beta.MatA[[i]] <- temp.A
  
  fB <- rlnorm(1, -3 , 2)
  sB <- rlnorm(1, -3, 2)
  fecB[i] <- fB  
  susB[i] <- sB
  betaB[i] <- fecB[i]*susB[i] #creating beta
  temp.B <- matrix(c(betaB[i], betaB[i]/2, betaB[i]/2, betaB[i]), nrow = 2, ncol = 2, byrow = T) # creating beta matrix
  beta.MatB[[i]] <- temp.B
  #storing the beta matrix for all trials in a list form for pathogen B
}  
  
#The beta matrix for pathogen A and pathogen B has been created. Each has 1000 trials.
#Each trial would create a matrix of 2x2 for each pathogen. They're stores in the form of 
#a list. Each element of list beta.mat is a trial beta matrix for that species.

# I am holding growth, and death rate constant at the moment.
  r <- matrix(c(r1 = 0.09, r2 = 0.1), nrow = 2, ncol = 1, byrow = T)
  vA <- c(vA1 = 1/8, vA2 = 1/7) #death due to infection by pathogen A - virulence 
  vB <- c(vB1 = 1/9, vB2 = 1/9) #death due to infection by pathogen A
  d <- c(d1 = 0.001, d2 = 0.001) # natural death
  delta <- matrix(c(vA[1]+vB[1]+d[1], vA[2]+ vB[2]+ d[2]),  nrow = 2, ncol = 1, byrow = T ) #totald death in the population
  
# I am not going to run the ODE function because I don't need to for stability analysis, 
#and it is taking a very long time to compile.

## Getting the equilibrium densities for 2P2H no comp dyn
I1.eq <- vector()
I2.eq <- vector() #Storing the equilibrium values. Some of these values are below 1.
S1.eq <- vector() #This can be used as a threshold for extinction.
S2.eq <- vector()  

Eigen <- list() #saving the jacobian matrix at each step
E.values <- list()
Dom.ev <- vector()

for(i in 1:length(beta.MatA)){
  betaA <- beta.MatA[[i]] #these are temp variables that store the matrix value for that loop
  betaB <- beta.MatB[[i]]
  
  I1.eq <- (r[1]*(betaA[2,2]+betaB[2,2]) - r[2]*(betaA[1,2]+betaB[1,2])) / ((betaA[1,1]+betaB[1,1])*(betaA[2,2]+betaB[2,2]) - (betaA[1,2]+betaB[1,2])*(betaA[2,1]+betaB[2,1]))
  I2.eq <- (r[2]*(betaA[1,1]+betaB[1,1]) - r[1]*(betaA[2,1]+betaB[2,1])) / ((betaA[1,1]+betaB[1,1])*(betaA[2,2]+betaB[2,2]) - (betaA[1,2]+betaB[1,2])*(betaA[2,1]+betaB[2,1]))
  S1.eq <- (delta[1] * I1.eq)/r[1]
  S2.eq[i] <- (delta[2] * I2.eq)/r[2]

  #Since the expression doesn't take non numeric entries, store each into a separate variable
betaA11 <- betaA[1,1]
betaA12 <- betaA[1,2]
betaA21 <- betaA[2,1]
betaA22 <- betaA[2,2]
betaB11 <- betaB[1,1]
betaB12 <- betaB[1,2]
betaB21 <- betaB[2,1]
betaB22 <- betaB[2,2]
r1 <- r[1]
r2 <- r[2]
d1 <- d[1]
d2 <- d[2]
vA1 <- vA[1]
vA2 <- vB[2]
vB1 <- vA[1]
vB2 <- vB[2]

S1 <- S1.eq
S2 <- S2.eq
I1 <- I1.eq
I2 <- I2.eq


dS1.dt <- expression(r1*S1 - S1*(betaA11*I1 + betaA12*I2) - S1*(betaB11*I1 + betaB12*I2))
par.dS1.S1 <- D(dS1.dt, 'S1')
par.dS1.S2 <- D(dS1.dt, 'S2')
par.dS1.I1 <- D(dS1.dt, 'I1')
par.dS1.I2 <- D(dS1.dt, 'I2')

dS2.dt <- expression(r2*S2 - S2*(betaA22*I2 + betaA21*I1) - S2*(betaB22*I2 + betaB21*I1))
par.dS2.S1 <- D(dS2.dt, 'S1')
par.dS2.S2 <- D(dS2.dt, 'S2')
par.dS2.I1 <- D(dS2.dt, 'I1')
par.dS2.I2 <- D(dS2.dt, 'I2')

dI1.dt <- expression(S1*(betaA11*I1 + betaA12*I2) + S1*(betaB11*I1 + betaB12*I2) - (d1 + vA1 + vB1)*I1)
par.dI1.S1 <- D(dI1.dt, 'S1')
par.dI1.S2 <- D(dI1.dt, 'S2')
par.dI1.I1 <- D(dI1.dt, 'I1')
par.dI1.I2 <- D(dI1.dt, 'I2')

dI2.dt <- expression(S2*(betaA22*I2 + betaA21*I1) + S2*(betaB22*I2 + betaB21*I1) - (d2 + vA2 + vB2)*I2)
par.dI2.S1 <- D(dI2.dt, 'S1')
par.dI2.S2 <- D(dI2.dt, 'S2')
par.dI2.I1 <- D(dI2.dt, 'I1')
par.dI2.I2 <- D(dI2.dt, 'I2')

## Getting the Jacobian for the community for this trial of beta = fxs 
#for the community interaction

J <- expression(matrix(c(eval(par.dS1.S1), eval(par.dS1.S2), eval(par.dS1.I1), eval(par.dS1.I2),
                         eval(par.dS2.S1), eval(par.dS2.S2), eval(par.dS2.I1), eval(par.dS2.I2),
                         eval(par.dI1.S1), eval(par.dI1.S2), eval(par.dI1.I1), eval(par.dI1.I2),
                         eval(par.dI2.S1), eval(par.dI2.S2), eval(par.dI2.I1), eval(par.dI2.I2)),                                                                        
                       nrow = 2, ncol = 2))
# doesn't make sense to store the Jacobian outside the loop because it is just an expression

eigenStable <- eigen(eval(J))  
ev <- eigenStable[['values']]
Eigen[[i]] <- eigenStable
E.values[[i]] <- ev
Dom.ev[i] <- max(E.values[[i]])
}

fecundityA <- log(fecA)
susceptibilityA <- log(susA)
plot(susceptibilityA, Dom.ev)
fecundityB <- log(fecB)
susceptibilityB <- log(susB)

#Source the regression sampler
source("C. Bayesian sampler/Regression sampler MH.R")
#source the simulation study functions
source("E. Code for simulation (annotated)/MH Sampler/Simulation functions study MH.R")

#KORT

#Study 1: 1 linear predictor
load("E. Code for simulation (annotated)/datasets/dataF1a.Rdata")
Simulation_l(500,10,0.5,0.5,0,0,0,1,2250, 750,1, seed=101,"date","F1a new2",BURN=0, data=dataF1a)
load("E. Code for simulation (annotated)/datasets/dataF1b.Rdata")
Simulation_l(500,50,0.5,0.5,0,0,0,1,2250,750,1, seed=101,"date","F1b new2",BURN=0, data=dataF1b)
load("E. Code for simulation (annotated)/datasets/dataF1c.Rdata")

Simulation_l(500,100,0.5,0.5,0,0,0,1,2250,750,1, seed=101,"date","F1c new2",BURN=0, data=dataF1c)
load("E. Code for simulation (annotated)/datasets/dataF2b.Rdata")
Simulation_l(500,50,0.5,0.5,0,0,-4,1,2250,750,1, seed=101,"date","F2b new2",BURN=0, data=dataF2b)
load("E. Code for simulation (annotated)/datasets/dataF2c.Rdata")
Simulation_l(500,100,0.5,0.5,0,0,-4,1,2250,750,1, seed=101,"date","F2c new2",BURN=0, data=dataF2c)

load("E. Code for simulation (annotated)/datasets/dataG1a.Rdata")
Simulation_l(500,10,-0.2,-0.2,0,0,0,1,2250,750,1, seed=101,"date","G1a new2",BURN=0, data=dataG1a)
load("E. Code for simulation (annotated)/datasets/dataG1b.Rdata")
Simulation_l(500,50,-0.2,-0.2,0,0,0,1,2250,750,1, seed=101,"date","G1b new2",BURN=0, data=dataG1b)
load("E. Code for simulation (annotated)/datasets/dataG1c.Rdata")
Simulation_l(500,100,-0.2,-0.2,0,0,0,1,2250,750,1, seed=101,"date","G1c new2",BURN=0, data=dataG1c)

load("E. Code for simulation (annotated)/datasets/dataG2a.Rdata")
Simulation_l(500,10,-0.2,-0.2,0,0,-4,1,2250,750,1, seed=101,"date","G2a new2",BURN=0, data=dataG2a)
load("E. Code for simulation (annotated)/datasets/dataG2b.Rdata")
Simulation_l(500,50,-0.2,-0.2,0,0,-4,1,2250,750,1, seed=101,"date","G2b new2",BURN=0, data=dataG2b)
load("E. Code for simulation (annotated)/datasets/dataG2c.Rdata")
Simulation_l(500,100,-0.2,-0.2,0,0,-4,1,2250,750,1, seed=101,"date","G2c new2",BURN=0, data=dataG2c)



#Study 2: 1 circular predictor 

load("E. Code for simulation (annotated)/datasets/dataI1b.Rdata")
Simulation_1c(500,50,0.5,0.5,0.5,0.5,0,0,0,2,2250,750,1,seed=101,"date","I1b new2",BURN=0, data=dataI1b)
load("E. Code for simulation (annotated)/datasets/dataI1c.Rdata")
Simulation_1c(500,100,0.5,0.5,0.5,0.5,0,0,0,2,2250,750,1,seed=101,"date","I1c new2",BURN=0, data=dataI1c)

load("E. Code for simulation (annotated)/datasets/dataI2b.Rdata")
Simulation_1c(500,50,0.5,0.5,0.5,0.5,0,0,0,10,2250,750,1,seed=101,"date","I2b new2",BURN=0, data=dataI2b)
load("E. Code for simulation (annotated)/datasets/dataI2c.Rdata")
Simulation_1c(500,100,0.5,0.5,0.5,0.5,0,0,0,10,2250,750,1,seed=101,"date","I2c new2",BURN=0, data=dataI2c)

load("E. Code for simulation (annotated)/datasets/dataJ1b.Rdata")
Simulation_1c(500,50,-0.2,-0.2,-0.2,-0.2,0,0,0,2,2250,750,1,seed=101,"date","J1b new2",BURN=0, data=dataJ1b)
load("E. Code for simulation (annotated)/datasets/dataJ1c.Rdata")
Simulation_1c(500,100,-0.2,-0.2,-0.2,-0.2,0,0,0,2,2250,750,1,seed=101,"date","J1c new2",BURN=0, data=dataJ1c)
load("E. Code for simulation (annotated)/datasets/dataJ2b.Rdata")

Simulation_1c(500,50,-0.2,-0.2,-0.2,-0.2,0,0,0,10,2250,750,1,seed=101,"date","J2b new2",BURN=0, data=dataJ2b)
load("E. Code for simulation (annotated)/datasets/dataJ2c.Rdata")
Simulation_1c(500,100,-0.2,-0.2,-0.2,-0.2,0,0,0,10,2250,750,1,seed=101,"date","J2c new2",BURN=0, data=dataJ2c)
load("E. Code for simulation (annotated)/datasets/dataJ3a.Rdata")

#Study 3: 2 linear predictors
load("E. Code for simulation (annotated)/datasets/dataG5b.Rdata")
Simulation_2l(500,50,-0.2,-0.2,-0.2,-0.2,0,0,0,1,0,2250,750,1, seed=101,"date","G5b new2",BURN=0, data=dataG5b)
load("E. Code for simulation (annotated)/datasets/dataG5c.Rdata")
Simulation_2l(500,100,-0.2,-0.2,-0.2,-0.2,0,0,0,1,0,2250,750,1, seed=101,"date","G5c new2",BURN=0, data=dataG5c)

load("E. Code for simulation (annotated)/datasets/dataF4b.Rdata")
Simulation_2l(500,50,0.5,-0.2,0.5,-0.2,0,0,0,1,0,2250,750,1, seed=101,"date","F4b new2",BURN=0, data=dataF4b)
load("E. Code for simulation (annotated)/datasets/dataF4c.Rdata")
Simulation_2l(500,100,0.5,-0.2,0.5,-0.2,0,0,0,1,0,2250,750,1, seed=101,"date","F4c new2",BURN=0, data=dataF4c)

load("E. Code for simulation (annotated)/datasets/dataF5b.Rdata")
Simulation_2l(500,50,0.5,0.5,0.5,0.5,0,0,0,1,0,2250,750,1, seed=101,"date","F5b new2",BURN=0, data=dataF5b)
load("E. Code for simulation (annotated)/datasets/dataF5c.Rdata")
Simulation_2l(500,100,0.5,0.5,0.5,0.5,0,0,0,1,0,2250,750,1, seed=101,"date","F5c new2",BURN=0, data=dataF5c)



#Study 4: 1 circular and 1 linear predictor

load("E. Code for simulation (annotated)/datasets/dataL1b.Rdata")
Simulation_1c1l(500,50,-0.2, -0.2, 0.5, -0.2, -0.2, 0.5,0,0,0,2,0,1,2250,750,1, seed=101,"date","L1b new2",BURN=0, data=dataL1b)
load("E. Code for simulation (annotated)/datasets/dataM1b.Rdata")
Simulation_1c1l(500,50,-0.2, -0.2, -0.2, -0.2, -0.2, -0.2,0,0,0,2,0,1,2250,750,1, seed=101,"date","M1b new2",BURN=0, data=dataM1b)
load("E. Code for simulation (annotated)/datasets/dataN1b.Rdata")
Simulation_1c1l(500,50,0.5, 0.5, -0.2, 0.5, 0.5, -0.2,0,0,0,2,0,1,2250,750,1, seed=101,"date","N1b new2",BURN=0, data=dataN1b)
load("E. Code for simulation (annotated)/datasets/dataO1b.Rdata")
Simulation_1c1l(500,50,0.5, 0.5, 0.5, 0.5, 0.5, 0.5,0,0,0,2,0,1,2250,750,1, seed=101,"date","O1b new2",BURN=0, data=dataO1b)

load("E. Code for simulation (annotated)/datasets/dataL1c.Rdata")
Simulation_1c1l(500,100,-0.2, -0.2, 0.5, -0.2, -0.2, 0.5,0,0,0,2,0,1,2250,750,1, seed=101,"date","L1c new2",BURN=0, data=dataL1c)
load("E. Code for simulation (annotated)/datasets/dataM1c.Rdata")
Simulation_1c1l(500,100,-0.2, -0.2, -0.2, -0.2, -0.2, -0.2,0,0,0,2,0,1,2250,750,1, seed=101,"date","M1c new2",BURN=0, data=dataM1c)
load("E. Code for simulation (annotated)/datasets/dataN1c.Rdata")
Simulation_1c1l(500,100,0.5, 0.5, -0.2, 0.5, 0.5, -0.2,0,0,0,2,0,1,2250,750,1, seed=101,"date","N1c new2",BURN=0, data=dataN1c)
load("E. Code for simulation (annotated)/datasets/dataO1c.Rdata")
Simulation_1c1l(500,100,0.5, 0.5, 0.5, 0.5, 0.5, 0.5,0,0,0,2,0,1,2250,750,1, seed=101,"date","O1c new2",BURN=0, data=dataO1c)

load("E. Code for simulation (annotated)/datasets/dataL1d.Rdata")
Simulation_1c1l(500,200,-0.2, -0.2, 0.5, -0.2, -0.2, 0.5,0,0,0,2,0,1,2250,750,1, seed=101,"date","L1d new2",BURN=0, data=dataL1d)
load("E. Code for simulation (annotated)/datasets/dataL1e.Rdata")
Simulation_1c1l(500,400,-0.2, -0.2, 0.5, -0.2, -0.2, 0.5,0,0,0,2,0,1,2250,750,1, seed=101,"date","L1e new2",BURN=0, data=dataL1e)


#Study 5: different regression equations for sine and cosine component

load("E. Code for simulation (annotated)/datasets/dataEmpirical1.Rdata")
Simulation_diff(500,50,1.2,-0.002,0.5,-0.4,1.5,0.5,-0.9,0,0,0,2,0,1,0,1,2250,750,1, seed=101,"date","Empirical1 new2",BURN=0, data=dataEmpirical1)
load("E. Code for simulation (annotated)/datasets/dataEmpirical2.Rdata")
Simulation_diff(500,100,1.2,-0.002,0.5,-0.4,1.5,0.5,-0.9,0,0,0,2,0,1,0,1,2250,750,1, seed=101,"date","Empirical2 new2",BURN=0, data=dataEmpirical2)

load("E. Code for simulation (annotated)/datasets/dataEmpirical3.Rdata")
Simulation_diff(500,50,1.2,1.2,0.5,-0.4,-0.4,0.5,-0.9,0,0,0,2,0,1,0,1,2250,750,1, seed=101,"date","Empirical3 new2",BURN=0, data=dataEmpirical3)
load("E. Code for simulation (annotated)/datasets/dataEmpirical4.Rdata")
Simulation_diff(500,100,1.2,1.2,0.5,-0.4,-0.4,0.5,-0.9,0,0,0,2,0,1,0,1,2250,750,1, seed=101,"date","Empirical4 new2",BURN=0, data=dataEmpirical4)

load("E. Code for simulation (annotated)/datasets/dataEmpirical5.Rdata")
Simulation_diff(500,50,-0.1,1.5,0.5,-0.1,1.5,0.5,-0.9,0,0,0,2,0,1,0,1,2250,750,1, seed=101,"date","Empirical5 new2",BURN=0, data=dataEmpirical5)
load("E. Code for simulation (annotated)/datasets/dataEmpirical6.Rdata")
Simulation_diff(500,100,-0.1,1.5,0.5,-0.1,1.5,0.5,-0.9,0,0,0,2,0,1,0,1,2250,750,1, seed=101,"date","Empirical6 new2",BURN=0, data=dataEmpirical6)

#LANG

#Study 1: 1 linear predictor
load("E. Code for simulation (annotated)/datasets/dataF2a.Rdata")
Simulation_l(500,10,0.5,0.5,0,0,-4,1,20000,750,1, seed=101,"date","F2a new2",BURN=0, data=dataF2a)

load("E. Code for simulation (annotated)/datasets/dataG3a.Rdata")
Simulation_l(500,10,-0.2,-0.2,0,0,10,1,20000,750,1, seed=101,"date","G3a new2",BURN=0, data=dataG3a)
load("E. Code for simulation (annotated)/datasets/dataG3b.Rdata")
Simulation_l(500,50,-0.2,-0.2,0,0,10,1,20000,750,1, seed=101,"date","G3b new2",BURN=0, data=dataG3b)
load("E. Code for simulation (annotated)/datasets/dataG3c.Rdata")
Simulation_l(500,100,-0.2,-0.2,0,0,10,1,20000,750,1, seed=101,"date","G3c new2",BURN=0, data=dataG3c)


load("E. Code for simulation (annotated)/datasets/dataH1a.Rdata")
Simulation_l(500,10,2,2,0,0,0,1,20000,750,1, seed=101,"date","H1a new2",BURN=0, data=dataH1a)
load("E. Code for simulation (annotated)/datasets/dataH1b.Rdata")
Simulation_l(500,50,2,2,0,0,0,1,20000,750,1, seed=101,"date","H1b new2",BURN=0, data=dataH1b)
load("E. Code for simulation (annotated)/datasets/dataH1c.Rdata")
Simulation_l(500,100,2,2,0,0,0,1,20000,750,1, seed=101,"date","H1c new2",BURN=0, data=dataH1c)

load("E. Code for simulation (annotated)/datasets/dataH3a.Rdata")
Simulation_l(500,10,2,2,0,0,10,1,20000,750,1, seed=101,"date","H3a new2",BURN=0, data=dataH3a)
load("E. Code for simulation (annotated)/datasets/dataH3b.Rdata")
Simulation_l(500,50,2,2,0,0,10,1,20000,750,1, seed=101,"date","H3b new2",BURN=0, data=dataH3b)
load("E. Code for simulation (annotated)/datasets/dataH3c.Rdata")
Simulation_l(500,100,2,2,0,0,10,1,20000,750,1, seed=101,"date","H3c new2",BURN=0, data=dataH3c)

#Study 2: 1 circular predictor 

load("E. Code for simulation (annotated)/datasets/dataI2a.Rdata")
Simulation_1c(500,10,0.5,0.5,0.5,0.5,0,0,0,10,20000,750,1,seed=101,"date","I2a new2",BURN=0, data=dataI2a)

load("E. Code for simulation (annotated)/datasets/dataJ2a.Rdata")
Simulation_1c(500,10,-0.2,-0.2,-0.2,-0.2,0,0,0,10,20000,750,1,seed=101,"date","J2a new2",BURN=0, data=dataJ2a)


load("E. Code for simulation (annotated)/datasets/dataK1b.Rdata")
Simulation_1c(500,50,2,2,2,2,0,0,0,2,20000,750,1,seed=101,"date","K1b new2",BURN=0, data=dataK1b)
load("E. Code for simulation (annotated)/datasets/dataK1c.Rdata")
Simulation_1c(500,100,2,2,2,2,0,0,0,2,20000,750,1,seed=101,"date","K1c new2",BURN=0, data=dataK1c)

load("E. Code for simulation (annotated)/datasets/dataK2b.Rdata")
Simulation_1c(500,50,2,2,2,2,0,0,0,10,20000,750,1,seed=101,"date","K2b new2",BURN=0, data=dataK2b)
load("E. Code for simulation (annotated)/datasets/dataK2c.Rdata")
Simulation_1c(500,100,2,2,2,2,0,0,0,10,20000,750,1,seed=101,"date","K2c new2",BURN=0, data=dataK2c)

#Study 3: 2 linear predictors

load("E. Code for simulation (annotated)/datasets/dataF6b.Rdata")
Simulation_2l(500,50,0.5,2,0.5,2,0,0,0,1,0,20000,750,1, seed=101,"date","F6b new2",BURN=0, data=dataF6b)
load("E. Code for simulation (annotated)/datasets/dataF6c.Rdata")
Simulation_2l(500,100,0.5,2,0.5,2,0,0,0,1,0,20000,750,1, seed=101,"date","F6c new2",BURN=0, data=dataF6c)

load("E. Code for simulation (annotated)/datasets/dataG4b.Rdata")
Simulation_2l(500,50,-0.2,2,-0.2,2,0,0,0,1,0,20000,750,1, seed=101,"date","G4b new2",BURN=0, data=dataG4b)
load("E. Code for simulation (annotated)/datasets/dataG4c.Rdata")
Simulation_2l(500,100,-0.2,2,-0.2,2,0,0,0,1,0,20000,750,1, seed=101,"date","G4c new2",BURN=0, data=dataG4c)



#heeeeel lang

load("E. Code for simulation (annotated)/datasets/dataH2a.Rdata")
Simulation_l(500,10,2,2,0,0,-4,1,40000,750,1, seed=101,"date","H2a new2",BURN=0, data=dataH2a)
load("E. Code for simulation (annotated)/datasets/dataH2b.Rdata")
Simulation_l(500,50,2,2,0,0,-4,1,40000,750,1, seed=101,"date","H2b new2",BURN=0, data=dataH2b)
load("E. Code for simulation (annotated)/datasets/dataH2c.Rdata")
Simulation_l(500,100,2,2,0,0,-4,1,40000,750,1, seed=101,"date","H2c new2",BURN=0, data=dataH2c)


load("E. Code for simulation (annotated)/datasets/dataK1a.Rdata")
Simulation_1c(500,10,2,2,2,2,0,0,0,2,40000,750,1,seed=101,"date","K1a new2",BURN=0, data=dataK1a)
load("E. Code for simulation (annotated)/datasets/dataK2a.Rdata")
Simulation_1c(500,10,2,2,2,2,0,0,0,10,40000,750,1,seed=101,"date","K2a new2",BURN=0, data=dataK2a)


load("E. Code for simulation (annotated)/datasets/dataJ1a.Rdata")
Simulation_1c(500,10,-0.2,-0.2,-0.2,-0.2,0,0,0,2,40000,750,1,seed=101,"date","J1a new2",BURN=0, data=dataJ1a)

load("E. Code for simulation (annotated)/datasets/dataI1a.Rdata")
Simulation_1c(500,10,0.5,0.5,0.5,0.5,0,0,0,2,40000,750,1,seed=101,"date","I1a new2",BURN=0, data=dataI1a)


load("E. Code for simulation (annotated)/datasets/dataF3a.Rdata")
Simulation_l(500,10,0.5,0.5,0,0,10,1,40000,750,1, seed=101,"date","F3a new2",BURN=0, data=dataF3a)
load("E. Code for simulation (annotated)/datasets/dataF3b.Rdata")
Simulation_l(500,50,0.5,0.5,0,0,10,1,40000,750,1, seed=101,"date","F3b new2",BURN=0, data=dataF3b)
load("E. Code for simulation (annotated)/datasets/dataF3c.Rdata")
Simulation_l(500,100,0.5,0.5,0,0,10,1,40000,750,1, seed=101,"date","F3c new2",BURN=0, data=dataF3c)

#04-04-2017 extra circular pred kappa= 1


load("E. Code for simulation (annotated)/datasets/dataI4a.Rdata")
Simulation_1c(500,10,0.5,0.5,0.5,0.5,0,0,0,1,20000,750,1,seed=101,"date","I4a new2",BURN=0, data=dataI4a)
load("E. Code for simulation (annotated)/datasets/dataI4b.Rdata")
Simulation_1c(500,50,0.5,0.5,0.5,0.5,0,0,0,1,2250,750,1,seed=101,"date","I4b new2",BURN=0, data=dataI4b)
load("E. Code for simulation (annotated)/datasets/dataI4c.Rdata")
Simulation_1c(500,100,0.5,0.5,0.5,0.5,0,0,0,1,2250,750,1,seed=101,"date","I4c new2",BURN=0, data=dataI4c)

load("E. Code for simulation (annotated)/datasets/dataJ4a.Rdata")
Simulation_1c(500,10,-0.2,-0.2,-0.2,-0.2,0,0,0,1,20000,750,1,seed=101,"date","J4a new2",BURN=0, data=dataJ4a)
load("E. Code for simulation (annotated)/datasets/dataJ4b.Rdata")
Simulation_1c(500,50,-0.2,-0.2,-0.2,-0.2,0,0,0,1,2250,750,1,seed=101,"date","J4b new2",BURN=0, data=dataJ4b)
load("E. Code for simulation (annotated)/datasets/dataJ4c.Rdata")
Simulation_1c(500,100,-0.2,-0.2,-0.2,-0.2,0,0,0,1,2250,750,1,seed=101,"date","J4c new2",BURN=0, data=dataJ4c)

load("E. Code for simulation (annotated)/datasets/dataK4a.Rdata")
Simulation_1c(500,10,2,2,2,2,0,0,0,1,20000,750,1,seed=101,"date","K4a new2",BURN=0, data=dataK4a)
load("E. Code for simulation (annotated)/datasets/dataK4b.Rdata")
Simulation_1c(500,50,2,2,2,2,0,0,0,1,20000,750,1,seed=101,"date","K4b new2",BURN=0, data=dataK4b)
load("E. Code for simulation (annotated)/datasets/dataK4c.Rdata")
Simulation_1c(500,100,2,2,2,2,0,0,0,1,20000,750,1,seed=101,"date","K4c new2",BURN=0, data=dataK4c)

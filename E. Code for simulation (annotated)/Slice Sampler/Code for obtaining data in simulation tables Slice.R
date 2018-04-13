#Source the regression sampler
source('C. Bayesian sampler/Regression sampler Slice.R', echo=TRUE)
#source the simulation study functions
source('E. Code for simulation (annotated)/Slice Sampler/Simulation functions study Slice.R', echo=TRUE)

#KORT

#Study 1: 1 linear predictor
load("E. Code for simulation (annotated)/datasets/dataF1a.Rdata")
Simulation_l(500,10,0.5,0.5,0,0,0,1,2250,750,1, seed=101,"date","F1a",BURN=0, data=dataF1a)
load("E. Code for simulation (annotated)/datasets/dataF1b.Rdata")
Simulation_l(500,50,0.5,0.5,0,0,0,1,2250,750,1, seed=101,"date","F1b",BURN=0, data=dataF1b)
load("E. Code for simulation (annotated)/datasets/dataF1c.Rdata")
Simulation_l(500,100,0.5,0.5,0,0,0,1,2250,750,1, seed=101,"date","F1c",BURN=0, data=dataF1c)
load("E. Code for simulation (annotated)/datasets/dataF2b.Rdata")
Simulation_l(500,50,0.5,0.5,0,0,-4,1,2250,750,1, seed=101,"date","F2b",BURN=0, data=dataF2b)
load("E. Code for simulation (annotated)/datasets/dataF2c.Rdata")
Simulation_l(500,100,0.5,0.5,0,0,-4,1,2250,750,1, seed=101,"date","F2c",BURN=0, data=dataF2c)

load("E. Code for simulation (annotated)/datasets/dataG1a.Rdata")
Simulation_l(500,10,-0.2,-0.2,0,0,0,1,2250,750,1, seed=101,"date","G1a",BURN=0, data=dataG1a)
load("E. Code for simulation (annotated)/datasets/dataG1b.Rdata")
Simulation_l(500,50,-0.2,-0.2,0,0,0,1,2250,750,1, seed=101,"date","G1b",BURN=0, data=dataG1b)
load("E. Code for simulation (annotated)/datasets/dataG1c.Rdata")
Simulation_l(500,100,-0.2,-0.2,0,0,0,1,2250,750,1, seed=101,"date","G1c",BURN=0, data=dataG1c)
load("E. Code for simulation (annotated)/datasets/dataG2a.Rdata")
Simulation_l(500,10,-0.2,-0.2,0,0,-4,1,2250,750,1, seed=101,"date","G2a",BURN=0, data=dataG2a)
load("E. Code for simulation (annotated)/datasets/dataG2b.Rdata")
Simulation_l(500,50,-0.2,-0.2,0,0,-4,1,2250,750,1, seed=101,"date","G2b",BURN=0, data=dataG2b)
load("E. Code for simulation (annotated)/datasets/dataG2c.Rdata")
Simulation_l(500,100,-0.2,-0.2,0,0,-4,1,2250,750,1, seed=101,"date","G2c",BURN=0, data=dataG2c)



#Study 2: 1 circular predictor 

load("E. Code for simulation (annotated)/datasets/dataI1b.Rdata")
Simulation_1c(500,50,0.5,0.5,0.5,0.5,0,0,0,2,2250,750,1,seed=101,"date","I1b",BURN=0, data=dataI1b)
load("E. Code for simulation (annotated)/datasets/dataI1c.Rdata")
Simulation_1c(500,100,0.5,0.5,0.5,0.5,0,0,0,2,2250,750,1,seed=101,"date","I1c",BURN=0, data=dataI1c)
load("E. Code for simulation (annotated)/datasets/dataI2b.Rdata")
Simulation_1c(500,50,0.5,0.5,0.5,0.5,0,0,0,10,2250,750,1,seed=101,"date","I2b",BURN=0, data=dataI2b)
load("E. Code for simulation (annotated)/datasets/dataI2c.Rdata")
Simulation_1c(500,100,0.5,0.5,0.5,0.5,0,0,0,10,2250,750,1,seed=101,"date","I2c",BURN=0, data=dataI2c)


load("E. Code for simulation (annotated)/datasets/dataJ1b.Rdata")
Simulation_1c(500,50,-0.2,-0.2,-0.2,-0.2,0,0,0,2,2250,750,1,seed=101,"date","J1b",BURN=0, data=dataJ1b)
load("E. Code for simulation (annotated)/datasets/dataJ1c.Rdata")
Simulation_1c(500,100,-0.2,-0.2,-0.2,-0.2,0,0,0,2,2250,750,1,seed=101,"date","J1c",BURN=0, data=dataJ1c)
load("E. Code for simulation (annotated)/datasets/dataJ2b.Rdata")
Simulation_1c(500,50,-0.2,-0.2,-0.2,-0.2,0,0,0,10,2250,750,1,seed=101,"date","J2b",BURN=0, data=dataJ2b)
load("E. Code for simulation (annotated)/datasets/dataJ2c.Rdata")
Simulation_1c(500,100,-0.2,-0.2,-0.2,-0.2,0,0,0,10,2250,750,1,seed=101,"date","J2c",BURN=0, data=dataJ2c)


load("E. Code for simulation (annotated)/datasets/dataK2b.Rdata")
Simulation_1c(500,50,2,2,2,2,0,0,0,10,2250,750,1,seed=101,"date","K2b",BURN=0, data=dataK2b)
load("E. Code for simulation (annotated)/datasets/dataK2c.Rdata")
Simulation_1c(500,100,2,2,2,2,0,0,0,10,2250,750,1,seed=101,"date","K2c",BURN=0, data=dataK2c)
load("E. Code for simulation (annotated)/datasets/dataK3a.Rdata")
Simulation_1c(500,10,2,2,2,2,0,0,0,60,2250,750,1,seed=101,"date","K3a",BURN=0, data=dataK3a)
load("E. Code for simulation (annotated)/datasets/dataK3b.Rdata")
Simulation_1c(500,50,2,2,2,2,0,0,0,60,2250,750,1,seed=101,"date","K3b",BURN=0, data=dataK3b)
load("E. Code for simulation (annotated)/datasets/dataK3c.Rdata")
Simulation_1c(500,100,2,2,2,2,0,0,0,60,2250,750,1,seed=101,"date","K3c",BURN=0, data=dataK3c)

#Study 3: 2 linear predictors
load("E. Code for simulation (annotated)/datasets/dataG5b.Rdata")
Simulation_2l(500,50,-0.2,-0.2,-0.2,-0.2,0,0,0,1,0,2250,750,1, seed=101,"date","G5b",BURN=0, data=dataG5b)
load("E. Code for simulation (annotated)/datasets/dataG5c.Rdata")
Simulation_2l(500,100,-0.2,-0.2,-0.2,-0.2,0,0,0,1,0,2250,750,1, seed=101,"date","G5c",BURN=0, data=dataG5c)

load("E. Code for simulation (annotated)/datasets/dataF4b.Rdata")
Simulation_2l(500,50,0.5,-0.2,0.5,-0.2,0,0,0,1,0,2250,750,1, seed=101,"date","F4b",BURN=0, data=dataF4b)
load("E. Code for simulation (annotated)/datasets/dataF4c.Rdata")
Simulation_2l(500,100,0.5,-0.2,0.5,-0.2,0,0,0,1,0,2250,750,1, seed=101,"date","F4c",BURN=0, data=dataF4c)

load("E. Code for simulation (annotated)/datasets/dataF5b.Rdata")
Simulation_2l(500,50,0.5,0.5,0.5,0.5,0,0,0,1,0,2250,750,1, seed=101,"date","F5b",BURN=0, data=dataF5b)
load("E. Code for simulation (annotated)/datasets/dataF5c.Rdata")
Simulation_2l(500,100,0.5,0.5,0.5,0.5,0,0,0,1,0,2250,750,1, seed=101,"date","F5c",BURN=0, data=dataF5c)



#Study 4: 1 circular and 1 linear predictor

load("E. Code for simulation (annotated)/datasets/dataL1b.Rdata")
Simulation_1c1l(500,50,-0.2, -0.2, 0.5, -0.2, -0.2, 0.5,0,0,0,2,0,1,2250,750,1, seed=101,"date","L1b",BURN=0, data=dataL1b)
load("E. Code for simulation (annotated)/datasets/dataM1b.Rdata")
Simulation_1c1l(500,50,-0.2, -0.2, -0.2, -0.2, -0.2, -0.2,0,0,0,2,0,1,2250,750,1, seed=101,"date","M1b",BURN=0, data=dataM1b)
load("E. Code for simulation (annotated)/datasets/dataN1b.Rdata")
Simulation_1c1l(500,50,0.5, 0.5, -0.2, 0.5, 0.5, -0.2,0,0,0,2,0,1,2250,750,1, seed=101,"date","N1b",BURN=0, data=dataN1b)
load("E. Code for simulation (annotated)/datasets/dataO1b.Rdata")
Simulation_1c1l(500,50,0.5, 0.5, 0.5, 0.5, 0.5, 0.5,0,0,0,2,0,1,2250,750,1, seed=101,"date","O1b",BURN=0, data=dataO1b)

load("E. Code for simulation (annotated)/datasets/dataL1c.Rdata")
Simulation_1c1l(500,100,-0.2, -0.2, 0.5, -0.2, -0.2, 0.5,0,0,0,2,0,1,2250,750,1, seed=101,"date","L1c",BURN=0, data=dataL1c)
load("E. Code for simulation (annotated)/datasets/dataM1c.Rdata")
Simulation_1c1l(500,100,-0.2, -0.2, -0.2, -0.2, -0.2, -0.2,0,0,0,2,0,1,2250,750,1, seed=101,"date","M1c",BURN=0, data=dataM1c)
load("E. Code for simulation (annotated)/datasets/dataN1c.Rdata")
Simulation_1c1l(500,100,0.5, 0.5, -0.2, 0.5, 0.5, -0.2,0,0,0,2,0,1,2250,750,1, seed=101,"date","N1c",BURN=0, data=dataN1c)
load("E. Code for simulation (annotated)/datasets/dataO1c.Rdata")
Simulation_1c1l(500,100,0.5, 0.5, 0.5, 0.5, 0.5, 0.5,0,0,0,2,0,1,2250,750,1, seed=101,"date","O1c",BURN=0, data=dataO1c)

load("E. Code for simulation (annotated)/datasets/dataL1d.Rdata")
Simulation_1c1l(500,200,-0.2, -0.2, 0.5, -0.2, -0.2, 0.5,0,0,0,2,0,1,2250,750,1, seed=101,"date","L1d",BURN=0, data=dataL1d)
load("E. Code for simulation (annotated)/datasets/dataL1e.Rdata")
Simulation_1c1l(500,400,-0.2, -0.2, 0.5, -0.2, -0.2, 0.5,0,0,0,2,0,1,2250,750,1, seed=101,"date","L1e",BURN=0, data=dataL1e)



#LANG

#Study 1: 1 linear predictor
load("E. Code for simulation (annotated)/datasets/dataF2a.Rdata")
Simulation_l(500,10,0.5,0.5,0,0,-4,1,20000,750,1, seed=101,"date","F2a",BURN=0, data=dataF2a)

load("E. Code for simulation (annotated)/datasets/dataG3a.Rdata")
Simulation_l(500,10,-0.2,-0.2,0,0,10,1,20000,750,1, seed=101,"date","G3a",BURN=0, data=dataG3a)
load("E. Code for simulation (annotated)/datasets/dataG3b.Rdata")
Simulation_l(500,50,-0.2,-0.2,0,0,10,1,20000,750,1, seed=101,"date","G3b",BURN=0, data=dataG3b)
load("E. Code for simulation (annotated)/datasets/dataG3c.Rdata")
Simulation_l(500,100,-0.2,-0.2,0,0,10,1,20000,750,1, seed=101,"date","G3c",BURN=0, data=dataG3c)


load("E. Code for simulation (annotated)/datasets/dataH1a.Rdata")
Simulation_l(500,10,2,2,0,0,0,1,20000,750,1, seed=101,"date","H1a",BURN=0, data=dataH1a)
load("E. Code for simulation (annotated)/datasets/dataH1b.Rdata")
Simulation_l(500,50,2,2,0,0,0,1,20000,750,1, seed=101,"date","H1b",BURN=0, data=dataH1b)
load("E. Code for simulation (annotated)/datasets/dataH1c.Rdata")
Simulation_l(500,100,2,2,0,0,0,1,20000,750,1, seed=101,"date","H1c",BURN=0, data=dataH1c)


#Study 2: 1 circular predictor 
load("E. Code for simulation (annotated)/datasets/dataI2a.Rdata")
Simulation_1c(500,10,0.5,0.5,0.5,0.5,0,0,0,10,20000,750,1,seed=101,"date","I2a",BURN=0, data=dataI2a)
load("E. Code for simulation (annotated)/datasets/dataI3a.Rdata")
Simulation_1c(500,10,0.5,0.5,0.5,0.5,0,0,0,60,20000,750,1,seed=101,"date","I3a",BURN=0, data=dataI3a)
load("E. Code for simulation (annotated)/datasets/dataI3b.Rdata")
Simulation_1c(500,50,0.5,0.5,0.5,0.5,0,0,0,60,20000,750,1,seed=101,"date","I3b",BURN=0, data=dataI3b)
load("E. Code for simulation (annotated)/datasets/dataI3c.Rdata")
Simulation_1c(500,100,0.5,0.5,0.5,0.5,0,0,0,60,20000,750,1,seed=101,"date","I3c",BURN=0, data=dataI3c)

load("E. Code for simulation (annotated)/datasets/dataJ2a.Rdata")
Simulation_1c(500,10,-0.2,-0.2,-0.2,-0.2,0,0,0,10,20000,750,1,seed=101,"date","J2a",BURN=0, data=dataJ2a)
load("E. Code for simulation (annotated)/datasets/dataJ3a.Rdata")
Simulation_1c(500,10,-0.2,-0.2,-0.2,-0.2,0,0,0,60,20000,750,1,seed=101,"date","J3a",BURN=0, data=dataJ3a)
load("E. Code for simulation (annotated)/datasets/dataJ3b.Rdata")
Simulation_1c(500,50,-0.2,-0.2,-0.2,-0.2,0,0,0,60,20000,750,1,seed=101,"date","J3b",BURN=0, data=dataJ3b)
load("E. Code for simulation (annotated)/datasets/dataJ3c.Rdata")
Simulation_1c(500,100,-0.2,-0.2,-0.2,-0.2,0,0,0,60,20000,750,1,seed=101,"date","J3c",BURN=0, data=dataJ3c)

load("E. Code for simulation (annotated)/datasets/dataK1b.Rdata")
Simulation_1c(500,50,2,2,2,2,0,0,0,2,20000,750,1,seed=101,"date","K1b",BURN=0, data=dataK1b)
load("E. Code for simulation (annotated)/datasets/dataK1c.Rdata")
Simulation_1c(500,100,2,2,2,2,0,0,0,2,20000,750,1,seed=101,"date","K1c",BURN=0, data=dataK1c)


#Study 3: 2 linear predictors
load("E. Code for simulation (annotated)/datasets/dataF6b.Rdata")
Simulation_2l(500,50,0.5,2,0.5,2,0,0,0,1,0,20000,750,1, seed=101,"date","F6b",BURN=0, data=dataF6b)
load("E. Code for simulation (annotated)/datasets/dataF6c.Rdata")
Simulation_2l(500,100,0.5,2,0.5,2,0,0,0,1,0,20000,750,1, seed=101,"date","F6c",BURN=0, data=dataF6c)

load("E. Code for simulation (annotated)/datasets/dataG4b.Rdata")
Simulation_2l(500,50,-0.2,2,-0.2,2,0,0,0,1,0,20000,750,1, seed=101,"date","G4b",BURN=0, data=dataG4b)
load("E. Code for simulation (annotated)/datasets/dataG4c.Rdata")
Simulation_2l(500,100,-0.2,2,-0.2,2,0,0,0,1,0,20000,750,1, seed=101,"date","G4c",BURN=0, data=dataG4c)


#Study 5: different regression equations for sine and cosine component
load("E. Code for simulation (annotated)/datasets/dataEmpirical1.Rdata")
Simulation_diff(500,50,1.2,-0.002,0.5,-0.4,1.5,0.5,-0.9,0,0,0,2,0,1,0,1,20000,750,1, seed=101,"date","Empirical1",BURN=0, data=dataEmpirical1)
load("E. Code for simulation (annotated)/datasets/dataEmpirical2.Rdata")
Simulation_diff(500,100,1.2,-0.002,0.5,-0.4,1.5,0.5,-0.9,0,0,0,2,0,1,0,1,20000,750,1, seed=101,"date","Empirical2",BURN=0, data=dataEmpirical2)

load("E. Code for simulation (annotated)/datasets/dataEmpirical3.Rdata")
Simulation_diff(500,50,1.2,1.2,0.5,-0.4,-0.4,0.5,-0.9,0,0,0,2,0,1,0,1,20000,750,1, seed=101,"date","Empirical3",BURN=0, data=dataEmpirical3)
load("E. Code for simulation (annotated)/datasets/dataEmpirical4.Rdata")
Simulation_diff(500,100,1.2,1.2,0.5,-0.4,-0.4,0.5,-0.9,0,0,0,2,0,1,0,1,20000,750,1, seed=101,"date","Empirical4",BURN=0, data=dataEmpirical4)

load("E. Code for simulation (annotated)/datasets/dataEmpirical5.Rdata")
Simulation_diff(500,50,-0.1,1.5,0.5,-0.1,1.5,0.5,-0.9,0,0,0,2,0,1,0,1,20000,750,1, seed=101,"date","Empirical5",BURN=0, data=dataEmpirical5)
load("E. Code for simulation (annotated)/datasets/dataEmpirical6.Rdata")
Simulation_diff(500,100,-0.1,1.5,0.5,-0.1,1.5,0.5,-0.9,0,0,0,2,0,1,0,1,20000,750,1, seed=101,"date","Empirical6",BURN=0, data=dataEmpirical6)


#heeeeel lang

load("E. Code for simulation (annotated)/datasets/dataF3a.Rdata")
Simulation_l(500,10,0.5,0.5,0,0,10,1,80000,750,1, seed=101,"date","F3a",BURN=0, data=dataF3a)
load("E. Code for simulation (annotated)/datasets/dataF3b.Rdata")
Simulation_l(500,50,0.5,0.5,0,0,10,1,80000,750,1, seed=101,"date","F3b",BURN=0, data=dataF3b)
load("E. Code for simulation (annotated)/datasets/dataF3c.Rdata")
Simulation_l(500,100,0.5,0.5,0,0,10,1,80000,750,1, seed=101,"date","F3c",BURN=0, data=dataF3c)

load("E. Code for simulation (annotated)/datasets/dataH2a.Rdata")
Simulation_l(500,10,2,2,0,0,-4,1,80000,750,1, seed=101,"date","H2a",BURN=0, data=dataH2a)
load("E. Code for simulation (annotated)/datasets/dataH2b.Rdata")
Simulation_l(500,50,2,2,0,0,-4,1,80000,750,1, seed=101,"date","H2b",BURN=0, data=dataH2b)
load("E. Code for simulation (annotated)/datasets/dataH2c.Rdata")
Simulation_l(500,100,2,2,0,0,-4,1,80000,750,1, seed=101,"date","H2c",BURN=0, data=dataH2c)

load("E. Code for simulation (annotated)/datasets/dataH3a.Rdata")
Simulation_l(500,10,2,2,0,0,10,1,80000,750,1, seed=101,"date","H3a",BURN=0, data=dataH3a)
load("E. Code for simulation (annotated)/datasets/dataH3b.Rdata")
Simulation_l(500,50,2,2,0,0,10,1,80000,750,1, seed=101,"date","H3b",BURN=0, data=dataH3b)
load("E. Code for simulation (annotated)/datasets/dataH3c.Rdata")
Simulation_l(500,100,2,2,0,0,10,1,80000,750,1, seed=101,"date","H3c",BURN=0, data=dataH3c)

load("E. Code for simulation (annotated)/datasets/dataK1a.Rdata")
Simulation_1c(500,10,2,2,2,2,0,0,0,2,80000,750,1,seed=101,"date","K1a",BURN=0, data=dataK1a)
load("E. Code for simulation (annotated)/datasets/dataK2a.Rdata")
Simulation_1c(500,10,2,2,2,2,0,0,0,10,80000,750,1,seed=101,"date","K2a",BURN=0, data=dataK2a)
load("E. Code for simulation (annotated)/datasets/dataJ1a.Rdata")
Simulation_1c(500,10,-0.2,-0.2,-0.2,-0.2,0,0,0,2,80000,750,1,seed=101,"date","J1a",BURN=0, data=dataJ1a)
load("E. Code for simulation (annotated)/datasets/dataI1a.Rdata")
Simulation_1c(500,10,0.5,0.5,0.5,0.5,0,0,0,2,80000,750,1,seed=101,"date","I1a",BURN=0, data=dataI1a)



#26-05-2015 HEEEEEEEEEEEL LANG


load("E. Code for simulation (annotated)/datasets/dataH2b.Rdata")
Simulation_l(500,50,2,2,0,0,-4,1,200000,750,1, seed=101,"date","H2blongnew",BURN=0, data=dataH2b)
load("E. Code for simulation (annotated)/datasets/dataH2b.Rdata")
Simulation_l(500,50,2,2,0,0,-4,1,500000,750,1, seed=101,"date","H2blongernew",BURN=0, data=dataH2b)


#04-04-2017 extra circular pred kappa= 1

load("E. Code for simulation (annotated)/datasets/dataI4a.Rdata")
Simulation_1c(500,10,0.5,0.5,0.5,0.5,0,0,0,1,2250,750,1,seed=101,"date","I4a",BURN=0, data=dataI4a)
load("E. Code for simulation (annotated)/datasets/dataI4b.Rdata")
Simulation_1c(500,50,0.5,0.5,0.5,0.5,0,0,0,1,2250,750,1,seed=101,"date","I4b",BURN=0, data=dataI4b)
load("E. Code for simulation (annotated)/datasets/dataI4c.Rdata")
Simulation_1c(500,100,0.5,0.5,0.5,0.5,0,0,0,1,2250,750,1,seed=101,"date","I4c",BURN=0, data=dataI4c)

load("E. Code for simulation (annotated)/datasets/dataJ4a.Rdata")
Simulation_1c(500,10,-0.2,-0.2,-0.2,-0.2,0,0,0,1,2250,750,1,seed=101,"date","J4a",BURN=0, data=dataJ4a)
load("E. Code for simulation (annotated)/datasets/dataJ4b.Rdata")
Simulation_1c(500,50,-0.2,-0.2,-0.2,-0.2,0,0,0,1,2250,750,1,seed=101,"date","J4b",BURN=0, data=dataJ4b)
load("E. Code for simulation (annotated)/datasets/dataJ4c.Rdata")
Simulation_1c(500,100,-0.2,-0.2,-0.2,-0.2,0,0,0,1,2250,750,1,seed=101,"date","J4c",BURN=0, data=dataJ4c)

load("E. Code for simulation (annotated)/datasets/dataK4a.Rdata")
Simulation_1c(500,10,2,2,2,2,0,0,0,1,2250,750,1,seed=101,"date","K4a",BURN=0, data=dataK4a)
load("E. Code for simulation (annotated)/datasets/dataK4b.Rdata")
Simulation_1c(500,50,2,2,2,2,0,0,0,1,2250,750,1,seed=101,"date","K4b",BURN=0, data=dataK4b)
load("E. Code for simulation (annotated)/datasets/dataK4c.Rdata")
Simulation_1c(500,100,2,2,2,2,0,0,0,1,2250,750,1,seed=101,"date","K4c",BURN=0, data=dataK4c)


#05-04-2017 circular pred, not centered

load("E. Code for simulation (annotated)/datasets/dataI1a2.Rdata")
Simulation_1c(500,10,0.5,0.5,0.5,0.5,0,0,0,2,80000,750,1,seed=101,"dateS","I1a2",BURN=0, data=dataI1a2)
load("E. Code for simulation (annotated)/datasets/dataI1b2.Rdata")
Simulation_1c(500,50,0.5,0.5,0.5,0.5,0,0,0,2,2250,750,1,seed=101,"dateS","I1b2",BURN=0, data=dataI1b2)
load("E. Code for simulation (annotated)/datasets/dataI1c2.Rdata")
Simulation_1c(500,100,0.5,0.5,0.5,0.5,0,0,0,2,2250,750,1,seed=101,"dateS","I1c2",BURN=0, data=dataI1c2)

load("E. Code for simulation (annotated)/datasets/dataI2a2.Rdata")
Simulation_1c(500,10,0.5,0.5,0.5,0.5,0,0,0,10,20000,750,1,seed=101,"dateS","I2a2",BURN=0, data=dataI2a2)
load("E. Code for simulation (annotated)/datasets/dataI2b2.Rdata")
Simulation_1c(500,50,0.5,0.5,0.5,0.5,0,0,0,10,2250,750,1,seed=101,"dateS","I2b2",BURN=0, data=dataI2b2)
load("E. Code for simulation (annotated)/datasets/dataI2c2.Rdata")
Simulation_1c(500,100,0.5,0.5,0.5,0.5,0,0,0,10,2250,750,1,seed=101,"dateS","I2c2",BURN=0, data=dataI2c2)


load("E. Code for simulation (annotated)/datasets/dataJ1a2.Rdata")
Simulation_1c(500,10,-0.2,-0.2,-0.2,-0.2,0,0,0,2,80000,750,1,seed=101,"dateS","J1a2",BURN=0, data=dataJ1a2)
load("E. Code for simulation (annotated)/datasets/dataJ1b2.Rdata")
Simulation_1c(500,50,-0.2,-0.2,-0.2,-0.2,0,0,0,2,2250,750,1,seed=101,"dateS","J1b2",BURN=0, data=dataJ1b2)
load("E. Code for simulation (annotated)/datasets/dataJ1c2.Rdata")
Simulation_1c(500,100,-0.2,-0.2,-0.2,-0.2,0,0,0,2,2250,750,1,seed=101,"dateS","J1c2",BURN=0, data=dataJ1c2)
load("E. Code for simulation (annotated)/datasets/dataJ2b2.Rdata")

load("E. Code for simulation (annotated)/datasets/dataJ2a2.Rdata")
Simulation_1c(500,10,-0.2,-0.2,-0.2,-0.2,0,0,0,10,20000,750,1,seed=101,"dateS","J2a2",BURN=0, data=dataJ2a2)
load("E. Code for simulation (annotated)/datasets/dataJ2b2.Rdata")
Simulation_1c(500,50,-0.2,-0.2,-0.2,-0.2,0,0,0,10,2250,750,1,seed=101,"dateS","J2b2",BURN=0, data=dataJ2b2)
load("E. Code for simulation (annotated)/datasets/dataJ2c2.Rdata")
Simulation_1c(500,100,-0.2,-0.2,-0.2,-0.2,0,0,0,10,2250,750,1,seed=101,"dateS","J2c2",BURN=0, data=dataJ2c2)

load("E. Code for simulation (annotated)/datasets/dataJ4b2.Rdata")
Simulation_1c(500,50,-0.2,-0.2,-0.2,-0.2,0,0,0,1,2250,750,1,seed=101,"dateS","J4b2",BURN=0, data=dataJ4b2)
load("E. Code for simulation (annotated)/datasets/dataJ4c2.Rdata")
Simulation_1c(500,100,-0.2,-0.2,-0.2,-0.2,0,0,0,1,2250,750,1,seed=101,"dateS","J4c2",BURN=0, data=dataJ4c2)


load("E. Code for simulation (annotated)/datasets/dataK1a2.Rdata")
Simulation_1c(500,10,2,2,2,2,0,0,0,2,80000,750,1,seed=101,"dateS","K1a2",BURN=0, data=dataK1a2)
load("E. Code for simulation (annotated)/datasets/dataK1b2.Rdata")
Simulation_1c(500,50,2,2,2,2,0,0,0,2,20000,750,1,seed=101,"dateS","K1b2",BURN=0, data=dataK1b2)
load("E. Code for simulation (annotated)/datasets/dataK1c2.Rdata")
Simulation_1c(500,100,2,2,2,2,0,0,0,2,20000,750,1,seed=101,"dateS","K1c2",BURN=0, data=dataK1c2)

load("E. Code for simulation (annotated)/datasets/dataK2a2.Rdata")
Simulation_1c(500,10,2,2,2,2,0,0,0,10,80000,750,1,seed=101,"dateS","K2a2",BURN=0, data=dataK2a2)

load("E. Code for simulation (annotated)/datasets/dataK4c2.Rdata")
Simulation_1c(500,100,2,2,2,2,0,0,0,1,2250,750,1,seed=101,"dateS","K4c2",BURN=0, data=dataK4c2)

#10-04-2017 LONG

load("E. Code for simulation (annotated)/datasets/dataI4a2.Rdata")
Simulation_1c(500,10,0.5,0.5,0.5,0.5,0,0,0,1,20000,750,1,seed=101,"dateS","I4a2",BURN=0, data=dataI4a2)
load("E. Code for simulation (annotated)/datasets/dataI4b2.Rdata")
Simulation_1c(500,50,0.5,0.5,0.5,0.5,0,0,0,1,20000,750,1,seed=101,"dateS","I4b2",BURN=0, data=dataI4b2)
load("E. Code for simulation (annotated)/datasets/dataI4c2.Rdata")
Simulation_1c(500,100,0.5,0.5,0.5,0.5,0,0,0,1,20000,750,1,seed=101,"dateS","I4c2",BURN=0, data=dataI4c2)

load("E. Code for simulation (annotated)/datasets/dataJ4a2.Rdata")
Simulation_1c(500,10,-0.2,-0.2,-0.2,-0.2,0,0,0,1,20000,750,1,seed=101,"dateS","J4a2",BURN=0, data=dataJ4a2)

load("E. Code for simulation (annotated)/datasets/dataK2b2.Rdata")
Simulation_1c(500,50,2,2,2,2,0,0,0,10,20000,750,1,seed=101,"dateS","K2b2",BURN=0, data=dataK2b2)
load("E. Code for simulation (annotated)/datasets/dataK2c2.Rdata")
Simulation_1c(500,100,2,2,2,2,0,0,0,10,20000,750,1,seed=101,"dateS","K2c2",BURN=0, data=dataK2c2)

load("E. Code for simulation (annotated)/datasets/dataK4a2.Rdata")
Simulation_1c(500,10,2,2,2,2,0,0,0,1,20000,750,1,seed=101,"dateS","K4a2",BURN=0, data=dataK4a2)
load("E. Code for simulation (annotated)/datasets/dataK4b2.Rdata")
Simulation_1c(500,50,2,2,2,2,0,0,0,1,20000,750,1,seed=101,"dateS","K4b2",BURN=0, data=dataK4b2)
#Set your own working directory
#setwd("C:/Users/diego/Documents/R/Projects/GitHub_Projects/Optimization/Multiple_Item_Capacitated_Lot_Sizing_Problem")

# Import lpSolve package
library(lpSolve)

#Import required packages
library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)

# Set Amount of Items
J <- 3

#Set demands
d <-  matrix(c(30,0,0,
               0,0,0,
               80,30,40,
               0,0,0,
               40,70,60), nrow = 5, byrow = TRUE )

#Set fixed cost
K <- c(400,150,100)

#Set unit production cost
c <-  matrix(c(0,0,0,
               0,0,0,
               0,0,0,
               0,0,0,
               0,0,0), nrow = 5, byrow = TRUE )

#Set unit inventory cost
h <-  matrix(c(4,3,2,
               4,3,2,
               4,3,2,
               4,3,2,
               4,3,2), nrow = 5, byrow = TRUE )

#Set capacities
C <-  matrix(c(100,100,100,
               100,100,100,
               100,100,100,
               100,100,100,
               100,100,100), nrow = 5, byrow = TRUE )

#Set number of periods (time instant t=0 is included)
T <- nrow(d)+1

#Set initial inventory Level
Io <- 0

#Set final inventory Level
In <- 0

#Build Model
Model <- MIPModel() %>%
  add_variable(q[t,j], t = 1:T , j =1:J,  type = "continuous", lb=0) %>%       #define variables
  add_variable(I[t,j], t = 1:T , j =1:J,  type = "continuous", lb=0) %>% 
  add_variable(y[t,j], t = 1:T , j =1:J,  type = "binary") %>% 
  set_objective(expr = sum_expr(K[j]*y[t,j] + c[t-1]*q[t,j] + h[t-1]*I[t,j],
                                t = 2:T,j =1:J),
                sense = "min") %>% #define objective
  add_constraint(I[t,j] == Io, t=1 , j =1:J) %>%        #define constraints
  add_constraint(I[t,j] == In, t=T, j =1:J) %>%
  add_constraint(q[t,j] + I[t-1,j] == d[t-1,j] + I[t,j], t = 2:T, j =1:J) %>% 
  add_constraint(q[t,j] <= C[t-1]*y[t,j], t = 2:T, j =1:J) %>% 
  solve_model(with_ROI(solver = "symphony", verbosity = 1))


#Model summary
##Status
print(paste("Model status is:", Model$status))

##Objective Function
print(paste("Objective value:", objective_value(Model)))

## Variables
for(r in 2:T) {
  print(paste("Time istant t=", r - 1))
  
  for(i in 1:J){
    print(paste("Item j=", i))
  tmp_y <- get_solution(Model, y[t,j]) %>%
    filter(variable == "y", t == r, j == i ) %>%
    select(value)
  
  print(paste("--->y[", (r - 1), "][",i,']=',tmp_y))
  
  tmp_I <- get_solution(Model, I[t,j]) %>%
    filter(variable == "I", t == r, i ==j) %>%
    select(value)
  
  print(paste("--->I[", (r - 1), "][",i,']=',tmp_I))
  
  tmp_q <- get_solution(Model, q[t,j]) %>%
    filter(variable == "q", t == r, i==j) %>%
    select(value)
  
  print(paste("--->q[", (r - 1), "][",i,']=',tmp_q))
  
  }
  print(' ')
}
  



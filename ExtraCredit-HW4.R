library(GA)
library(ggplot2)

# Initialized the i-beam cross section area function
Rastrigin = function(x1, x2, x3, x4)
{
  2*x2*x4 + x3*(x1 - 2*x4)
}


# Applied Genetic Algorithm
GA1 = ga(type = "real-valued",
        fitness = function(x) Rastrigin(x[1], x[2], x[3], x[4]),
        lower = c(10, 10, 0.9, 0.9), upper = c(80, 50, 5, 5),
        popSize = 50, maxiter = 100, run = 200,
        pcrossover = 0.75, pmutation = 0.001, seed = 123)

summary(GA1)         

f1 = GA1@fitnessValue  #store fitness value
f1

# Plot the fitness value
plot(GA1, main = "Cross Section Area")


# Initialized the i-beam static deflection function
Rastrigin = function(x1, x2, x3, x4)
{
  60000 / (x3*(x1-2*x4)^3 + 2*x2*x4**(4*x4^2 + 3*x1*(x1 - 2*x4)))
}

# Applied Genetic Algorithm
GA2 = ga(type = "real-valued",
        fitness = function(x) Rastrigin(x[1], x[2], x[3], x[4]),
        lower = c(10, 10, 0.9, 0.9), upper = c(80, 50, 5, 5),
        popSize = 50, maxiter = 100, run = 200,
        pcrossover = 0.75, pmutation = 0.001, seed = 123)

summary(GA2)

f2 = GA2@fitnessValue #store finess value
f2

# Plot the fitness value
plot(GA2, main = "Static Deflection")


# Initialize function F = a*f1 + b*f2
# Constrain a+b=1 -> a=1-b, therefore F = (1-b)*f1 + b*f2
# Rewrite function F = (1-x)*f1 + x*f2, is the one dimension function
f = function(x) (1-x)*f1 + x*f2


# Applied GA to minimize F
GAF = ga(type = "real-valued",
         fitness = f,
         lower = 0, upper = 1,
         popSize = 50, maxiter = 100, run = 200,
         pcrossover = 0.75, pmutation = 0.001, seed = 123)

summary(GAF)

b = GAF@solution
b
a = 1-b
a

# Plot the fitness value
plot(GAF, main = "Minimization of F(f1, f2)")


library(GA)

# Initialized the function
Rastrigin = function(x1, x2)
{
  21.5 + x1*sin(4*pi*x1) + x2*sin(20*pi*x2)
}

# Set lower & upper bound
x1 = seq(-3, 12.1, by = 0.1)
x2 = seq(4.1, 5.8, by = 0.1)


# Plot the function
f = outer(x1, x2, Rastrigin)
persp3D(x1, x2, f, theta = -30, phi = 30, col.palette = bl2gr.colors)

filled.contour(x1, x2, f, color.palette = bl2gr.colors)

# Applied Genetic Algorithm
GA = ga(type = "real-valued",
        fitness = function(x) Rastrigin(x[1], x[2]),
        lower = c(-3, 4.1), upper = c(12.1, 5.8),
        popSize = 1000, maxiter = 200, run = 100)

summary(GA)         

plot(GA)

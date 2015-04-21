source('importance_sampling.R')
importance_sampling()
# Illustrating computing the integral of exp(cos^2(x)) using self normalized
# importance sampling over [-pi/2, pi/2]

# The integral is computed using E_g [phi(x) * w(x)] / E_g [w(x)] where
# phi is the indicator function over the inteval [-pi/2, pi/2]

phi = function(x) {
    # Indicator function over [-pi/2, pi/2]
    if (-pi/2 <= x & x <= pi/2) {
        return (1)
    }
    else {
        return (0)
    }
}

seq(-pi, pi, length=10)
phi(x_rang)

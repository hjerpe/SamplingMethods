# Illustration of pseudo random numbers generated using importance sampling

importance_sampling <- function(n_samples, phi, instrumental_sample_function,
                                importance_weight_function) {
    # Returns the expected value tau = E_f [phi(X)], the expected value of
    # phi(X) under the density function f. The transformed calculation is
    # E_g [phi(x) * w(x)], w - weight_function.
    
    # ARGS:
    # n_samples - Number of samples used in the estimate tau_n.
    # phi - Fucntion to calculate expected value of under f.
    # instrumental_sample_function - Used when transforming the expectation
    # calculation, the function need to return n samples by the call f(n).
    # importance_weight_function - weight function used in the expected value.

    draws <- instrumental_sample_function(n_samples)
    return (sum(phi(draws) * importance_weight_function(draws))/n_samples)
}


phi <- function(x) {

    indicator <- function(x) {
        # The indicator function over [-5, 5]
        if (-5 <= x & x <= 5) {
            return (1)
        }
        else {
            return (0)
        }
    }
    return (mapply(x, FUN=indicator))
}

target_density <- function(x) {
    # return (exp(sin(x)) / (pi*(1 + x^2)))
    return (1 / (pi*(1+x^2)))
}

instrumental_sample_function <- function(x) {
    return (rnorm(x, mean=0, sd=1))
}

instrumental_density_function <- function(x) {
    return dnorm(x, mean=0, sd=1)
}

importance_weight_function <- function(x) {
    return ( target_density(x) / instrumental_density_function(x))
}

N_SAMPLES <- 25000
importance_sampling(N_SAMPLES, phi, instrumental_sample_function,
                    importance_weight_function)

x_range = seq(from=-5, to=5, by=1)
importance_weight_function(x_range)
phi(x_range)

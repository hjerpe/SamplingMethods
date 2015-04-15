# Illustration of pseudo random numbers generated using importance sampling

importance_sampling <- function(n_samples, phi, instrumental_sample_function,
                                importance_weight_function) {
    # Returns the expected value tau = E_f [phi(X)], the expected value of
    # phi(X) under the density function f. The transformed calculation is
    # E_g [phi(x) * w(x)].
    # The density function g may only be zero where f = 0.

    # Algorithm: Estimate tau = E_f [phi(X)] = E_g [phi(X) * w(X)]
    # for i = [1:n]
    #   draw Xi ~ g
    # end
    # tau_n = sum (phi(Xi) * w(Xi))/n
    # return tau_n
    # ------------
    # Choosing g s.t. phi(X)*w(X) is close to constant on sup(g) minimizes
    # var(tau_n).
    
    # ARGS:
    # n_samples - Number of samples used in the estimate tau_n.
    # phi - Fucntion to calculate expected value of under f.
    # instrumental_sample_function - Function returning samples having
    # distribution g outlined in the Algorithm part.
    # importance_weight_function - weight function w outlined in the Algorithm p.
    draws <- instrumental_sample_function(n_samples)
    return (sum(phi(draws) * importance_weight_function(draws))/n_samples)
}


target_density <- function(x) {
    # The probability density used when taking expectation over phi(X),
    # E_f [phi(X)], used when computing the importance weights.
    return (1 / (pi*(1+x^2)))
}


instrumental_density_function <- function(x) {
    # Density function governing the expectation value E_g [phi(x) * w(x)],
    # used when computing the importance weights.
    return (dnorm(x, mean=0, sd=1))
}


instrumental_sample_function <- function(x) {
    # Random number generator function having density g. Used in the sampling
    # step in the importance sampling method.
    return (rnorm(x, mean=0, sd=1))
}


importance_weight_function <- function(x) {
    # Returns the importance weights w(x) used in the importance_sampling_method
    return (target_density(x) / instrumental_density_function(x))
}


phi <- function(x) {
    # Return a vector of indicator [-5, 5] function outputs on a vector of numbers.
    indicator <- function(x) {
        if (-5 <= x & x <= 5) {
            return (1)
        }
        else {
            return (0)
        }
    }
    return (mapply(x, FUN=indicator))
}

# Calculating the probability P(-5 < X < 5) when X ~ Cauchy(0, 1) using
# importance sampling.
N_SAMPLES <- 25000
importance_sampling(N_SAMPLES, phi, instrumental_sample_function,
                    importance_weight_function)

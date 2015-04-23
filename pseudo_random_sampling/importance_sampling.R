# Generate random numbers with importance sampling

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

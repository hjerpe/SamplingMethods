# Generate random numbers with importance sampling

importance_sampling <- function(n_samples, phi, importance_weight_function,
                                instrumental_sample_function) {
    # Returns an estimate of the expected value tau = E_g [w(x) * phi(X)],
    # that is, the expected value of (phi(X) * w(X)) under the density g.
    # The density function g may only be zero where f = 0.

    # Algorithm: Estimate tau = E_g [phi(X) * w(X)]
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
    # phi - Function used in E_g [phi(X) * w(X)].
    # importance_weight_function - Function w used in E_g [phi(X) * w(X)].
    # instrumental_sample_function - Function returning samples having
    # distribution g outlined in the Algorithm part.
    # RETURNS: An estimate of the expected value E_g [phi(X) * w(X)].

    # COMMENTS: Valid if g = 0 implies f = 0
    # If w is chosen as f/g (x) then E_f [phi(X)] = E_g[phi(X) * w(X)].
    # If w is chosen as (f*c)/g (x) where c is the normalizing constant for
    # the distribution f.
    # Then E_f [phi(X)] = E_g [phi(X) * w(X)] / E_g [w(X)], and
    # E_g [w(X)] is an estimator of c.
    draws <- instrumental_sample_function(n_samples)
    return (sum(phi(draws) * importance_weight_function(draws))/n_samples)
}

metropolis_samples <- function(n_samples, proposal_kernel, 
                                proposal_sample_function, target_density) {
    # Returns an estimate of the expected value tau = E_f [phi(X)],
    # that is, the expected value of phi(X) under the density f.
    
    # ARGS:
    # n_samples - Number of samples used in the estimate tau_n.
    # proposal_kernel - transition density r(z|x).
    # proposal_sample_function - Generate samples having the conditional
    # distribution r(z|x).
    # target_density - Needs only to be specified up to a normalizing constant
    # i.e. target_density(x) = c * f(x).
    # RETURNS: Correlated samples satisfying mean(phi(X)) estimates the
    # expected value E_f [phi(X)].

    # Requirements:
    # The stationary distribution of (X_k) must conicide with the desired
    # distribution f.
    # The chain (Xk) must converge to f irrespectively of the initial value X1.
    # It is sufficient for the target density to only be specified up to a
    # normalizing constant i.e. target_density(x) = c * f(x).
    alpha <- function(X, X_draw) {
        numerator <- target_density(X) * proposal_kernel(X_draw, X)
        min(1 , (target_density(X_draw) * proposal_kernel(X, X_draw))/ numerator)
    }
    
    draw_sample <- function(X) {
        X_draw <- proposal_sample_function(X)
        acceptance_bound <- alpha(X, X_draw)
        if (runif(n=1, min=0, max=1) <= acceptance_bound) X_draw else X
    }
    # Generate markov-chain and calculate the estimate of E_f [phi(X)]
    markov_chain <- replicate(expr=0, n=n_samples)
    markov_chain <- unlist(lapply(FUN=draw_sample, X=markov_chain))
}

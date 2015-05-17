metropolis_hastings <- function(n_samples, burn_in, phi, proposal_kernel, 
                                proposal_sample_function, target_density) {
    # Returns an estimate of the expected value tau = E_f [phi(X)],
    # that is, the expected value of phi(X) under the density f.
    # ALGORITHM: Estimate the expected value E_f [phi(X)]
    # draw X1 ~ Chi (Initial distribution)
    # for i 1:N-1:
    #   draw X_star ~ r(z|X_i)
    #   set alpa = min(1, f(X_star) * r(X_i| X_star) / (f(X_i) * r(X_star| X_i)))
    #   draw U ~ U(0, 1)
    #   if U <= alpha:
    #     X_i+1 = X_star
    #   else:
    #     X_i+1 = X+i
    #   end
    # end
    # return mean(phi(X))

    # ARGS:
    # n_samples - Number of samples used in the estimate tau_n.
    # burn_in - number of starting samples to throw away,
    # used for stabilizing the estimate.
    # phi - Function used in E_f [phi(X)].
    # proposal_kernel - transition density r(z|x).
    # proposal_sample_function - Generate samples having the conditional
    # distribution r(z|x).
    # target_density - Needs only to be specified up to a normalizing constant
    # i.e. target_density(x) = c * f(x).
    # RETURNS: An estimate of the expected value E_f [phi(X)].
    
    # COMMENTS:
    # It is sufficient for the target density to only be specified up to a
    # normalizing constant i.e. target_density(x) = c * f(x).
    alpha <- function(X, X_draw) {
        numerator <- target_density(X) * proposal_kernel(X_draw, X)
        min(1 , (target_density(X_draw) * proposal_kernel(X, X_draw)) / numerator)
    }
    
    draw_sample <- function(X) {
        X_draw <- proposal_sample_function(X)
        acceptance_bound <- alpha(X, X_draw)
        if (runif(n=1, min=0, max=1) <= acceptance_bound) X_draw else X
    }
    # Generate markov-chain and calculate the estimate of E_f [phi(X)]
    markov_chain <- replicate(expr=0, n=n_samples)
    markov_chain <- unlist(lapply(FUN=draw_sample, X=markov_chain))
    mean(phi(markov_chain[(burn_in+1):TOT_SAMPLES]))
}

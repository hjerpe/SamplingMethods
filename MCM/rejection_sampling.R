# Illustration of pseudo random generated number using the rejection sampling

rejection_sampling <- function(n_samples, sample_function, acceptance_bound) {
    # Returns n_samples from a random variable having the density function
    # f. Where the density function f have the bound f(x) <= K*g(x).
    
    # ARGS:
    # n_samples - Number of drawn samples.
    # sample_function - Function returning a sample with syntax 
    # sample_fucntion()
    # acceptance_bound - Higher bound for accepting uniform draws,
    # U <= f(x*) / (k*g(x*)) = acceptance_bound(x*).
    draw_sample <- function(sample_function, acceptance_bound) {
        # Helper function that draws uniform samples until the acceptance bound
        # criteria is met.
        while (TRUE) {
            # Draw samples
            X <- sample_function()
            U <- runif(n=1, min=0, max=1)
            if (U <= do.call(acceptance_bound, as.list(X))) {
                return (X)
            }
        }
    }
    # Return vector with n_samples number of draws
    return (replicate(n_samples, draw_sample(sample_function, acceptance_bound)))
}

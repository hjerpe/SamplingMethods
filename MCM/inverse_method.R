sample_using_inverse_method <- function(n_samples, F_inverse) {
    # Returns n_samples from a random variable having the distribution function
    # F.

    # ARGS:
    # n_samples - Number of drawn samples.
    # F_inverse - Inverse function of the distribution function governing the
    # drawn samples.
    uniform_samples <- runif(n=n_samples, min=0, max=1)
    return (F_inverse(uniform_samples))
}

# Illustration of pseudo random generated number using the inverse method

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


inverse_exponential_d <- function(x) -log(1 - x)

# Illustrate drawn samples having exp(1) distributions using the inverse
# method
N_SAMPLES = 10000
exponential_samples <- sample_using_inverse_method(N_SAMPLES,
                                                   inverse_exponential_d)
x_range <- seq(0, 20, length=300)

plot_title <- c('10000 uniformly drawn samples mapped by the',
                'inverse Exp(1) distribution function')
hist(exponential_samples, breaks=250, probability=TRUE, main=plot_title,
     xlab='samples')
lines(x_range, dexp(x_range, rate=1), col='blue', lwd=1.5)
legend('topright', c('pdf of exp(1)'), col='blue', lwd=1.5)
# dev.copy(png, 'pseudo_random_illustration.png')
# dev.off()

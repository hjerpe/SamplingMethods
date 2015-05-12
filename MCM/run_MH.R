source('metropolis_hastings.R')
# Example computing the variance of the r.v. X having the distribution
# f = exp(cost^2(x)) / c using the Metropolis Hasting algorithm.

# The variance is calculated by E_f [phi(X)], using phi(x) = x^2
# and using the proposal kernel r(z|x) = N(z;x, 1)

proposal_kernel <- function(z, x) {
    pnorm(q=z, mean=x, sd=1)
}


proposal_sample_function <- function(x) {
    x_draw <- rnorm(n=1, mean=x, sd=1)
}


target_density <- function(x) { exp(cos(x)**2) }


mh_estimator <- function(n_samples) {
    metropolis_hastings(
        n_samples,
        phi = function(x) { x**2 },
        proposal_kernel = proposal_kernel,
        proposal_sample_function = proposal_sample_function,
        target_density = target_density)
    }

N_SAMPLES_START <- 500
N_SAMPLES <- 1000
# True normalizing constant calculated by WolphramAlpha
NORMALIZING_CONSTANT <- 5.50843
 # True variance calculated by WolphramAlpha
VARIANCE <- 0.587201
vec_n_samples <- seq(from=N_SAMPLES_START, N_SAMPLES)

variance_estimates <- mapply(vec_n_samples, FUN=mh_estimator)


# Plot the estimates obtained using different number of samples.
x_label <- c('N summed samples')
plot(vec_n_samples, variance_estimates, type = 'l', xlab=x_label,
     ylab=c('Estimated variance'))
abline(a=VARIANCE, b=0, lty=2, lwd=2, col='red')
legend('topright', c('True variance'), col='red', lwd=2)
# dev.copy(jpeg, 'estimated_variance_self_normalized_IS.jpeg')
# dev.off()

source('metropolis_hastings.R')
# Example computing the variance of the r.v. X having the distribution
# f = exp(cost^2(x)) / c using the Metropolis Hasting algorithm.

# The variance is calculated by E_f [phi(X)].
# Where,
# phi(x) = x^2
# and using the proposal kernel r(z|x) = N(z;x, 1)

proposal_kernel <- function(z, x) <- {
    pnorm(q=z, mean=x, sd=1)
}


proposal_sample_function <- function(x) {
    x_draw <- rnorm(mean=x, sd=1)
}


target_density <- function(x) { exp(cos(x)**2) }


mh_estimator <- function(n) {
    metropolis_hastings(
    n,
    phi=function(x) { x**2 },
    proposal_kernel = proposal_kernel,
    proposal_sample_function = proposal_sample_function,
    target_density = target_density)
    }

N_SAMPLES <- 800
# True normalizing constant calculated by WolphramAlpha
NORMALIZING_CONSTANT <- 5.50843
 # True variance calculated by WolphramAlpha
VARIANCE <- 0.587201
vec_n_samples <- seq(from=1, N_SAMPLES)

expected_values_numerator <- mapply(vec_n_samples, FUN=mh_estimator)
vector_estimated_variance <- expected_values_numerator / expected_values_denominator

# Plot the estimates obtained using different number of samples.
x_label <- c('N summed samples')
plot(vec_n_samples, vector_estimated_variance, type = 'l',
     xlab=x_label,
     ylab=c('Estimated variance'))
abline(a=VARIANCE, b=0, lty=2, lwd=2, col='red')
legend('topright', c('True variance'), col='red', lwd=2)
dev.copy(jpeg, 'estimated_variance_self_normalized_IS.jpeg')
dev.off()
plot(vec_n_samples, expected_values_denominator, type = 'l',
     xlab=x_label,
     ylab=c('c'),
     main=c('Estimated normalizing constant c for exp(cos^2(x)) / c'))
abline(a=NORMALIZING_CONSTANT, b=0, lty=2, lwd=2, col='red')
legend('topright', c('True normalizing constant'), col='red', lwd=2)
dev.copy(jpeg, 'estimated_normalizing_const_self_normalized_IS.jpeg')
dev.off()

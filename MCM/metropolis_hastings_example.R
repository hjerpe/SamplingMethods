source('metropolis_samples.R')
source('metropolis_hastings.R')
# Example computing the variance of the r.v. X having the distribution
# f = exp(cost^2(x)) / c using the Metropolis Hasting algorithm.

# The variance is calculated by E_f [phi(X)], using phi(x) = x^2
# and using the proposal kernel r(z|x) = N(z;x, SD).
std_deviation <- 10
# The robustness of the  algorithm is much dependent on the standard deviation.

proposal_kernel <- function(z, x) {
    pnorm(q=z, mean=x, sd=std_deviation)
}


proposal_sample_function <- function(x) {
    x_draw <- rnorm(n=1, mean=x, sd=std_deviation)
}


indicator <- function (x) { (-pi/2 <= x && x <= pi/2) }


target_density <- function(x) { exp(cos(x)**2) * indicator(x) }


mh_sampler <- function(n_samples) {
    metropolis_samples(
        n_samples,
        burn_in = BURN_IN,
        proposal_kernel = proposal_kernel,
        proposal_sample_function = proposal_sample_function,
        target_density = target_density)
}


N_SAMPLES <- 15000
BURN_IN <- 2000
denominators <- seq(1, N_SAMPLES)
# True normalizing constant calculated by WolphramAlpha
NORMALIZING_CONSTANT <- 5.50843
 # True variance calculated by WolphramAlpha
VARIANCE <- 0.587201
# Generate samples and calculate estimates
samples <- mh_sampler(N_SAMPLES)
variance_estimates <- cumsum(samples ** 2) / denominators

# Plot the estimates obtained using different number of samples.
x_label <- c('N summed samples')
plot(denominators, variance_estimates, type = 'l', xlab=x_label,
     ylab=c('Estimated variance'))
abline(a=VARIANCE, b=0, lty=2, lwd=2, col='red')
legend('topright', c('True variance'), col='red', lwd=2)
# dev.copy(jpeg, 'estimated_variance_self_normalized_IS.jpeg')
# dev.off()

source('importance_sampling.R')
# Example computing the variance of the r.v. X having the distribution
# f = exp(cost^2(x)) / c using self-normalized importance sampling.

# The variance is calculated by E_f [phi(X)] = E_g [phi(X) * w(x)] / E_g [w(x)]
# for some instrumental distribution g s.t. g = 0 implies that f = 0.
# Where,
# phi(x) = x^2
# w(x) = z(x) / g(x) = cf(x) / g(x).
# Further the deniminator E_g [w(x)] is an estimator for the normalizing
# constant c.

instrumental_sample_function <- function(n_samples) {
    # Return n_samples samples from U(-p/2, pi/2)
    runif(n_samples, min=-pi/2, max=pi/2)
}

importance_weight_function <- function(x) { (pi) * exp(cos(x)**2) }

expected_value_numerator <- function(n_samples) {
    importance_sampling(
        n_samples,
        phi=function(x) { x**2 },
        instrumental_sample_function=instrumental_sample_function,
        importance_weight_function=importance_weight_function
        )
    }

sampling_denominator <- function(n_samples) {
    importance_sampling(
        n_samples,
        phi=function(x) { 1 },
        instrumental_sample_function=instrumental_sample_function,
        importance_weight_function=importance_weight_function
        )
    }

N_SAMPLES <- 800
# True normalizing constant calculated by WolphramAlpha
NORMALIZING_CONSTANT <- 5.50843
 # True variance calculated by WolphramAlpha
VARIANCE <- 0.587201
vec_n_samples <- seq(from=1, N_SAMPLES)

expected_values_numerator <- mapply(vec_n_samples, FUN=expected_value_numerator)
expected_values_denominator <- mapply(vec_n_samples, FUN=sampling_denominator)

vector_estimated_variance <- expected_values_numerator / expected_values_denominator

# Plot the estimates obtained using different number of samples.
x_label <- c('N summed samples')
plot(vec_n_samples, vector_estimated_variance, type = 'l',
     xlab=x_label,
     ylab=c('Estimated variance'))
abline(a=VARIANCE, b=0, lty=2, lwd=2, col='red')
plot(vec_n_samples, expected_values_denominator, type = 'l',
     xlab=x_label,
     ylab=c('c'),
     main=c('Estimated normalizing constant c for exp(cos^2(x)) / c'))
abline(a=NORMALIZING_CONSTANT, b=0, lty=2, lwd=2, col='red')

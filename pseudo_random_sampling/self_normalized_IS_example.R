source('importance_sampling.R')
# Example computing the variance of the r.v. X having the distribution
# f = exp(cost^2(x)) / c using self-normalized importance sampling.

# The variance calculated by E_f [phi(X)] = E_g [phi(X) * w(x)] / E_g [w(x)]
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

expected_value_numerator <- function(used_n_samples) {
    importance_sampling(
        used_n_samples,
        phi=function(x) { x**2 },
        instrumental_sample_function=instrumental_sample_function,
        importance_weight_function=importance_weight_function
        )
    }

sampling_denominator <- function(used_n_samples) {
    importance_sampling(
        used_n_samples,
        phi=function(x) { 1 },
        instrumental_sample_function=instrumental_sample_function,
        importance_weight_function=importance_weight_function
        )
    }

N_SAMPLES <- 800
NORMALIZING_CONSTANT <- 5.50843 # True normalizing constant given by WolphramAlpha
VARIANCE <- 0.587201 # True variance given by WolphramAlpha
x_range <- seq(from=1, N_SAMPLES)
samples_numerator <- mapply(x_range, FUN=expected_value_numerator)
samples_denominator <- mapply(x_range, FUN=sampling_denominator)

vector_variances <- samples_numerator / samples_denominator

plot(x_range, vector_variances, type = 'l')
abline(a=VARIANCE, b=0, lty=2, lwd=2, col='red')
plot(x_range, samples_denominator, type = 'l')
abline(a=NORMALIZING_CONSTANT, b=0, lty=2, lwd=2, col='red')

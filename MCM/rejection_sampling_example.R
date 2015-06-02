# Illustrate drawing random samples from a Beta(2, 5) distribution, that is
# samples having the distribution function (x*(1 - x)^4) / B(2, 5)
source("rejection_sampling.R")
sample_function <- function(x) return (runif(n=x, min=0, max=1))
acceptance_bound_beta <-function(x) {
    # Function defining a higher bound used when accept/reject uniform samples.
    # We accept the samples if U <= f(x*) / (k*g(x*)) = acceptance_bound_beta(x*),
    # where x* is drawn from the function g

    # We have used g = U(0, 1) and f = Beta(2,5)
    return ((x*(1-x)^4) * (5^5 / 4^4))
}

N_SAMPLES <- 25000
beta_draws <- rejection_sampling(N_SAMPLES, sample_function, acceptance_bound_beta)
beta_pdf_range <- seq(0, 1, length=300)

# Illustate Beta(2, 5) distributed samples using rejection sampling
plot_title <- c('25000 accepted uniformly drawn samples using',
                'rejection sampling')
hist(beta_draws, breaks=250, probability=TRUE, main=plot_title)
lines(beta_pdf_range, dbeta(beta_pdf_range, shape1=2, shape2=5), col='blue',
      lwd=1.5)
legend('topright', c('pdf of Beta(2, 5)'), col='blue', lwd=1.5)
# dev.copy(jpeg, 'illustration_rejection_sampling.jpeg')
# dev.off()

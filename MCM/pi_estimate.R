# Illustrate estimating pi using using rejection sampling with f(x) = 1_A(x)/c
# where A = {x,y| x^2 + y^2 <= 1} and 1 being the indicator function.
source("rejection_sampling.R")

# Want f(x,y) <= Kg(x,y). We let g(x,y) be uniform over a [-1,1]^2 square, then
# g(x,y) = 1/4, K = 4.
sample_function <- function(x) return (runif(n=2, min=-1, max=1))

acceptance_bound <-function(x, y) {
    # Function defining a higher bound used when accept/reject uniform samples.
    # We accept the samples if U <= f(x*) / (k*g(x*)) = acceptance_bound_beta(x*),
    # where x* is drawn from the function g

    # We have g = U(-1, 1)^2 , K = 4 giving k*g = 4 
    # and f = 1_{x^2+y^2 <= 1}(x,y).
    ((x**2 + y**2) <= 1) * 1
}

N_SAMPLES <- 10
samples <- replicate(N_SAMPLES, sample_function())
list_samples <- split(samples, rep(1:ncol(samples), each = nrow(samples)))
print(list_samples)
print(class(list_samples))
print(sapply(list_samples, function(x) do.call(acceptance_bound, as.list(x))))
# mean(unif_circle_draws)
# beta_pdf_range <- seq(0, 1, length=300)

# Illustate Beta(2, 5) distributed samples using rejection sampling
# plot_title <- c('25000 accepted uniformly drawn samples using',
#                 'rejection sampling')
# hist(beta_draws, breaks=250, probability=TRUE, main=plot_title)
# lines(beta_pdf_range, dbeta(beta_pdf_range, shape1=2, shape2=5), col='blue',
#       lwd=1.5)
# legend('topright', c('pdf of Beta(2, 5)'), col='blue', lwd=1.5)
# dev.copy(jpeg, 'illustration_rejection_sampling.jpeg')
# dev.off()

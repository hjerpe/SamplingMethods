# Illustration of pseudo random generated numbers having exp(1) distribution 
# the using inverse method.
source("inverse_method.R")

inverse_exponential_d <- function(x) -log(1 - x)

# Illustrate drawn samples having Exp(1) distributions 
N_SAMPLES = 10000
exponential_samples <- sample_using_inverse_method(N_SAMPLES,
                                                   inverse_exponential_d)
x_range <- seq(0, 20, length=300)

plot_title <- c('10000 uniformly drawn samples mapped by the',
                'inverse Exp(1) distribution function')
hist(exponential_samples, breaks=250, probability=TRUE, main=plot_title,
     xlab='samples')
lines(x_range, dexp(x_range, rate=1), col='blue', lwd=1.5)
legend('topright', c('pdf of Exp(1)'), col='blue', lwd=1.5)
# dev.copy(jpeg, 'illustration_inverse_method.jpeg')
# dev.off()

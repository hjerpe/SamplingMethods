# Illustrate estimating pi using using the ratio A_circle / A_square = pi/4 where 
# we estimate the areas by drawing points  uniformly inside a unit square 
# and estimating the A_circle by counting draws inside the circle.
sample_function <- function(x) return (runif(n=2, min=-1, max=1))

acceptance_bound <-function(x, y) {
    # Accepting all points inside a circle of unit radius.
    ((x**2 + y**2) < 1) * 1
}

N_SAMPLES <- 1e4 # Plotting takes some times when using > 1e5 nmbr of samples
samples <- replicate(N_SAMPLES, sample_function())

list_samples <- split(samples, rep(1:ncol(samples), each = nrow(samples)))

accepted_indices <- sapply(list_samples, 
                           function(x) acceptance_bound(x[1], x[2]))
accepted_samples <- samples[, accepted_indices==1]
rejected_samples <- samples[, accepted_indices==0]

# Illustrade the accepted and rejected samples
plot(accepted_samples[1,], accepted_samples[2,], xlim=c(-1,1), ylim=c(-1,1), 
     col='blue')
points(rejected_samples[1,], rejected_samples[2,], col='red')

# Estimate pi by the ratio of the number of accepted samples over the
# total number of samples
pi_estimate <- 4*dim(accepted_samples)[2] / N_SAMPLES
print(pi_estimate - pi)
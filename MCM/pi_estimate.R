library(ggplot2)
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
df_samples <- as.data.frame(do.call(rbind, list_samples))
colnames(df_samples) <- c("x", "y")
df_samples['label'] <- accepted_indices <- as.factor(sapply(
    list_samples, function(x) acceptance_bound(x[1], x[2])))
levels(df_samples$label) <- c("rejected", "accepted")

# Estimate pi by the ratio of the number of accepted samples over the
# total number of samples
pi_estimate <- 4*sum(df_samples$label=='accepted') / N_SAMPLES
print(pi_estimate - pi)

m <- ggplot(data = df_samples, aes(x=x, y=y, colour=label)) + 
    geom_point() + labs(title = paste(
        'Estimating pi by the ratio of areas, pi_estimate = ', 
        pi_estimate), xlab='x', ylab='y')
m
dev.copy(jpeg, 'fig_estimate_pi.jpeg')
dev.off()
# Example illustrating the CLT when picking uniformly distributed
# numbers from 1 to 6

mean_uniformly_rvs <- function(n_rvs) {
    # Returns the mean of the outcomes of a set of uniformly discrete
    # distributed R.V., i.e. S = 1/N sum X_i
    #
    # Args:
    # n_samples: Number of R.V. whose outcome to compute mean from
    # Returns:
    # The aritmethic mean of the outcomes of the R.V.

    random_numbers <- sample.int(6, size=n_rvs, replace=TRUE)
    return(mean(random_numbers))
}

z_samples <- function(n_rvs, n_samples) {
    # Returns n_samples samples of the R.V. Z = sqrt(n) * (Sn/n - mu), by CLT,
    # Z -> N(0, sigma2) in distribution as n -> \infty
    # 
    # Args:
    # n_rvs: Number of R.V. in the arithmetic mean Sn/n
    # n_samples: Number of samples to draw from Z
    # Returns:
    # Samples from Z
    
    mu <- 3.5
    mean_s_samples <- replicate(n_samples, mean_uniformly_rvs(n_rvs))
    Z <- sqrt(n_rvs) * (mean_s_samples - mu)
    return(Z)
}

my_plot <- function(data, x_lab) {
    # Function returns histogram with prob=TRUE, and # samples as xlabel
    # combined with a density plot of N(0, sigma2)
    #
    # ARGS:
    # data: z_samples for histogram, MEAN IS PRECOMPUTED
    # x_lab: number of variables used in the sum Sn 
    
    sigma2 <- 2.9
    breaks <- 400
    x_range = seq(from=-6, to=6, length.out=100)
    main<- paste('N samples:', toString(length(data)), 'N breaks:', breaks)
    
    hist(data, prob=TRUE, xlab=x_lab, main=main, breaks=breaks)
    lines(x_range, dnorm(x = x_range, mean=0, sd=sqrt(sigma2)),
      col='blue')
}

# Illustrate CLT sqrt(n) * (Sn/n - mean(X)) -> Z(0, sigma2) in distribution,
# where Sn is sum if n iid r.v.
n_rvs <- c(50, 250, 600, 1200, 1700, 5000) # Number of RV in arithmetic sum
n_samples <- rep.int(4000, times=length(n_rvs))
indices <- c(1:1:length(n_rvs)) # Lapply over multiple arguments

z_outcomes <- lapply(indices, function(x) z_samples(n_rvs[[x]], n_samples[[x]]))
x_labels <- paste('No terms in arithmetic mean Sn', lapply(n_rvs, toString))

par(mfrow=c(2,3))
invisible(lapply(indices, function(x) my_plot(z_outcomes[[x]], x_labels[[x]])))
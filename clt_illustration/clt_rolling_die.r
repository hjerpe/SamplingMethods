# Example illustrating the CLT theorem when drawing uniformly distributed
# numbers from 1 to 6

mean_uniformly_rvs <- function(n_rvs) {
    # Returns the outcome given by the  mean of the outcomes of a set of
    # uniformly discrete distributed R.V.,
    # i.e. the outcome of the r.v. S = 1/N sum X_i
    #
    # Args:
    # n_rvs: Number of R.V. used when computing the mean outcome of Sn
    # Returns:
    # The outcome of Sn

    random_numbers <- sample.int(6, size=n_rvs, replace=TRUE)
    return(mean(random_numbers))
}

z_samples <- function(n_rvs, n_samples) {
    # Returns n_samples samples of the R.V. Z = sqrt(n) * (Sn/n - mu),
    # by CLT, Z -> N(0, sigma2) in distribution as n -> \infty
    # 
    # Args:
    # n_rvs: Number of R.V. used when computing the mean outcome of Sn
    # n_samples: Number of samples to draw from Z
    # Returns:
    # Samples from Z
    
    mu <- 3.5
    mean_s_samples <- replicate(n_samples, mean_uniformly_rvs(n_rvs))
    Z <- sqrt(n_rvs) * (mean_s_samples - mu)
    return(Z)
}

my_plot <- function(z_samples, x_lab) {
    # Function plots the normalized histogram, and the number of  samples
    # used for Sn as xlabel combined with a density plot of N(0, sigma2)
    #
    # ARGS:
    # z_samples: List of draws from the r.v. Z (see z_samples)
    # x_lab: number of variables used in the sum Sn

    z_samples
    sigma2 <- 2.9
    breaks <- 400
    x_range = seq(from=-6, to=6, length.out=100)
    main<- paste('N samples:', toString(length(z_samples)), 'N breaks:', breaks)
    
    # Illustrate histogram and pdf function
    hist(z_samples, prob=TRUE, xlab=x_lab, main=main, breaks=breaks)
    l_w <- 2
    lines(x_range, dnorm(x = x_range, mean=0, sd=sqrt(sigma2)),
      col='blue', lwd=l_w)
    legend('topleft', c('pdf N(0, sigma2)'), col='blue', lwd=l_w, inset=c(-0.2,0))
}

# Illustrate CLT thm sqrt(n) * (Sn/n - mean(X)) -> Z(0, sigma2) in distribution,
# where Sn is sum if n iid r.v.

n_rvs <- c(50, 250, 600, 1200, 1700, 5000) # Number of r.v. used for Sn
n_samples <- rep.int(4000, times=length(n_rvs)) # Number of draws from the rv. Z
indices <- c(1:1:length(n_rvs)) # To lapply over multiple arguments

z_outcomes <- lapply(indices, function(x) z_samples(n_rvs[[x]], n_samples[[x]]))
x_labels <- paste('No terms in arithmetic mean Sn', lapply(n_rvs, toString))

par(mfrow=c(2,3), xpd=TRUE)
invisible(lapply(indices, function(x) my_plot(z_outcomes[[x]], x_labels[[x]])))

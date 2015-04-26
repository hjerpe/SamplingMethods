# Illustrating a Gaussian HMM model when estimating the filter means using
# sequential importance sampling.

# Y_n = X_n + S e_n
# X_n+1 = A*X_n + R*eps_n+1
# X_0 = R / (1 - A^2) * eps_0 where
# e_n and eps_n is Gaussian WN.
# We will estimate the filter means tau_n = E(X_n | Y_0:n = y_0:n)

# Instance model constants
A <- 1/5
R <- 1/50
S <- 1/100
n_samples <- 5000

length_sequence <- 60

sample_particles <- function(n_samples, particles, A, R) {
    A*particles + R*rnorm(n=n_samples)
}

weight_update_function <- function(observation, particle, S) {
    pnorm(q = observation, mean = particle, sd = S)
}

generate_observations <- function(states, S) {
    states + S*rnorm(n=length(states))
}

# Initiate states and observations
states <- replicate(expr=0, n=length_sequence)
states[1] <- R * (1 / (1 - A**2)) * rnorm(n=1)
for (i in 2:length_sequence) {
    states[i] <- sample_particles(1, states[i-1], A, R)
}
observations <- generate_observations(states, S)

# Initiate particles and estimates
taus <- replicate(expr=0, n=length_sequence)
particles <- R * (1 / (1 - A**2)) * rnorm(n=n_samples)
w <- weight_update_function(observations[1], particles, S)
taus[1] <- sum(w*particles) / sum(w)

for (k in 1:length_sequence-1) {
    particles <- sample_particles(n_samples, particles, A, R)
    w <- w*weight_update_function(
        observations[k+1], particles, S)
    taus[k+1] <- sum(particles*w) / sum(w)
}
taus
states

plot(1:length_sequence, states, type='l', lty=6, col='blue')
points(1:length_sequence, states , col='blue')
points(1:length_sequence, taus, type='l', lty=3, col='red')
points(1:length_sequence, taus , col='red')

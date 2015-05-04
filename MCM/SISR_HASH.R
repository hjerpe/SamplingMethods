library(hash)
# Computing the number of Self avoiding walks using sequential
# importance sampling with resampling. Where the instrumental distribution
# gn is uniformly over the free neighbors of X_0:k or if there are
# no neighbors then gn(x_k+1|x_1:k) = x_k with probability 1.

#### WE USE GLOBAL DATA THAT ARE ACCESSED BY VARIOUS FUCNTIONS FOR
#### DECREASING OVERHEAD AND NUMBER OF FUNCTION ARGUMENTS.
#### THE BAD STYLE MAY BE REMOVED BY DEFINING WRAPPER FUNCTIONS IN THE FORM
#### function(x, y) returns function(x).
# GLOABAL DATA:
# MAP_INDICES_TO_KEYS <<- matrix_point_to_string(length_sequence)
# LIST_TRAVELED_MAPS <<- replicate(n=n_particles,
#                                  expr=initial_hash_table(length_sequence))
# PARTICLES <<- replicate(n=n_particles, expr=c(0, 0), simplify=FALSE)
# LENGTH_SEQUENCE <<- length_sequence
# LIST_PARTICLE_NEIGHBORS <<- lapply(X=particle_indices,
#                                    generate_valid_neighbors)
# PARTICLES <<- lapply(X=particle_indices, FUN=mutate_particles)
# LIST_MOVEMENTS <<- list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))
################################################################################

LIST_MOVEMENTS <<- list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))
generate_valid_neighbors <- function(particle_index) {
    # Function genereates the free neighbors for the current position
    current_particle <- unlist(PARTICLES[particle_index])
    candidates <- lapply(X=LIST_MOVEMENTS,
                         FUN=function(x) { current_particle + x })
    # Filter out the valid candidates
    candidates[unlist(
        lapply(X=candidates,
               FUN=function(x) { valid_particle(x, particle_index) }))]
}

valid_particle <- function(particle, particle_index) {
    # Returns TRUE whether the particle position is unexplored else returns FALSE.
    row_index <- particle[1] + LENGTH_SEQUENCE + 1
    col_index <- particle[2] + LENGTH_SEQUENCE + 1
    hash_key <- MAP_INDICES_TO_KEYS[row_index, col_index]
    (has.key(key=hash_key, hash=LIST_TRAVELED_MAPS[[particle_index]]) == FALSE)
}

initiate_weights <- function(particle) {
    # Returns the function value z_0(x_0) / g_0(x_0)
    return (1)
}

matrix_point_to_string <- function(length_sequence) {
    # Matrix for maping a point (x,y) to string representation used when hashing
    # coordinates.
    integers <- -length_sequence:length_sequence
    outer(integers, integers, FUN=paste0)
}

initial_hash_table <- function(length_sequence) {
    hash_key <- MAP_INDICES_TO_KEYS[length_sequence+1, length_sequence+1]
    hash(hash_key, 1)
}

mutate_particles <- function(particle_index) {
    # Returns a new particle sampled under the distribution gn(x_n+1|x_0:n).
    # Where gn = uniformly over the free neighbors of X_0:k or if there are no
    # neighbors then gn(x_k+1|x_1:k) = x_k with probability 1.
    neighbors <- LIST_PARTICLE_NEIGHBORS[[particle_index]]
    n_neighbors <- length(neighbors)
    if (n_neighbors > 0) {
        random_index <- sample(1:n_neighbors, size=1, replace=T)
        neighbors[random_index]
    }
    else { PARTICLES[[particle_index]] }
}

weight_update_function <- function(particle_index) {
    # Returns the function value z_n+1(x_0:n+1) / (z_n(x_0:n) * g_n(x_n+1|x_0:n)).
    next_particle <- unlist(PARTICLES[particle_index])
    row_index <- next_particle[1] + LENGTH_SEQUENCE + 1
    col_index <- next_particle[2] + LENGTH_SEQUENCE + 1
    hash_key <- MAP_INDICES_TO_KEYS[row_index, col_index]
    particle_neighbors <- LIST_PARTICLE_NEIGHBORS[[particle_index]]
    
    if (has.key(key=hash_key, hash=LIST_TRAVELED_MAPS[[particle_index]]) == FALSE) {
        LIST_TRAVELED_MAPS[[particle_index]][hash_key] <<- 1
        n_neighbors <- length(particle_neighbors)
        proposal_update_function(n_neighbors)
    }
    else { 0 }
}

proposal_update_function <- function(n_neighbors) {
    if (n_neighbors == 0) 1 else n_neighbors
}

number_of_weights_n_random_walks <- function(n_particles, length_sequence) {
    #### GLOBAL DATA USED BY FUNCTIONS WHEN UPDATING PARTICLES AND WEIGHTS ####
    N_PARTICLES <<- n_particles
    MAP_INDICES_TO_KEYS <<- matrix_point_to_string(length_sequence)
    LIST_TRAVELED_MAPS <<- replicate(n=n_particles,
                                     expr=initial_hash_table(length_sequence))
    PARTICLES <<- replicate(n=n_particles, expr=c(0, 0), simplify=FALSE)
    LENGTH_SEQUENCE <<- length_sequence
    ###########################################################################
    particle_indices <- 1:N_PARTICLES
    w <- unlist(lapply(X=PARTICLES, FUN=initiate_weights))
    list_n_weights <- replicate(expr=0, n=LENGTH_SEQUENCE)
    list_n_weights[1] <- length(w)
    list_n_random_walks <- replicate(expr=0, n=LENGTH_SEQUENCE)
    list_n_random_walks[1] <- mean(w)
    # Update weights and samples for each sequence length
    for (k in 1:(length_sequence-1)) {
        # Count number of propagating particles
        living_indices <- w > 0
        list_n_weights[k+1] <- length(w[living_indices])
        cat('\nSequence length', k)
        cat('\nNumber of weights', list_n_weights[k+1])
        # Update weights and particles
        LIST_PARTICLE_NEIGHBORS <<- lapply(X=particle_indices,
                                           generate_valid_neighbors)
        PARTICLES <<- lapply(X=particle_indices, FUN=mutate_particles)
        w <- unlist(lapply(X=particle_indices, FUN=weight_update_function))
        # Resampling step
        resampling_indices <- sample.int(n=n_particles, size=n_particles,
                                         replace=TRUE, prob=w)
        PARTICLES <<- PARTICLES[resampling_indices]
        LIST_TRAVELED_MAPS <<- lapply(X=LIST_TRAVELED_MAPS[resampling_indices],
                                     FUN=copy)
        list_n_random_walks[k+1] <- list_n_random_walks[k] * mean(w)
    }
    list(n_weights=list_n_weights, n_random_walks=list_n_random_walks)
}
# Instance model constants
N_PARTICLES <- 500
LENGTH_SEQUENCE <- 3
N_REPETITIONS <- 3

# Replicate calculation N_REPETITIONS times
list_weights_and_walks <- replicate(
    n=N_REPETITIONS,
    expr=number_of_weights_n_random_walks(N_PARTICLES, LENGTH_SEQUENCE))
weight_matrix <- matrix(
    data=unlist(list_weights_and_walks[1,]),byrow = TRUE,
    nrow=N_REPETITIONS, ncol=LENGTH_SEQUENCE)
cn_matrix <- matrix(
    data=unlist(list_weights_and_walks[2,]),byrow = TRUE,
    nrow=N_REPETITIONS, ncol=LENGTH_SEQUENCE)

# Filter out desired metrics that the best, largest and smallest relative
# quotients c_est / c_n and the mean number of survived particles
mean_survived_weights <- apply(X=weight_matrix, MARGIN=2, FUN=mean)

index_hb <- min(72, LENGTH_SEQUENCE)
quotient_cn_matrix<- cn_matrix[,1:index_hb] / LIST_CN_GROUND_TRUTH[1:index_hb]
min_cn_quotients <- apply(X=quotient_cn_matrix, MARGIN=1, FUN=min)
max_cn_quotients <- apply(X=quotient_cn_matrix, MARGIN=1, FUN=max)

abs_difference_from_one <- apply(X=quotient_cn_matrix,
                                 MARGIN=c(1,2),
                                 FUN = function(x) { abs(x-1) })
best_indices <- apply(X=abs_difference_from_one, MARGIN=c(1), FUN=which.min)
best_cn_quotients <- quotient_cn_matrix[cbind(1:LENGTH_SEQUENCE,
                                              best_indices[1:LENGTH_SEQUENCE])]

LIST_CN_GROUND_TRUTH <- c(
    1, 4, 12, 36, 100, 284, 780, 2172, 5916, 16268, 44100, 
    120292, 324932, 881500, 2374444, 6416596, 17245332, 46466676, 124658732, 
    335116620, 897697164, 2408806028, 6444560484, 17266613812, 46146397316, 
    123481354908, 329712786220, 881317491628, 2351378582244, 6279396229332, 
    16741957935348, 44673816630956, 119034997913020, 317406598267076, 845279074648708, 
    2252534077759844, 5995740499124412, 15968852281708724, 42486750758210044, 
    113101676587853932, 300798249248474268, 800381032599158340,
    2127870238872271828, 5659667057165209612, 15041631638016155884,
    39992704986620915140, 106255762193816523332, 282417882500511560972,
    750139547395987948108, 1993185460468062845836, 5292794668724837206644,
    14059415980606050644844, 37325046962536847970116, 99121668912462180162908,
    263090298246050489804708, 698501700277581954674604, 1853589151789474253830500,
    4920146075313000860596140, 13053884641516572778155044, 34642792634590824499672196,
    91895836025056214634047716, 243828023293849420839513468, 
    646684752476890688940276172, 1715538780705298093042635884,
    4549252727304405545665901684, 12066271136346725726547810652,
    31992427160420423715150496804, 84841788997462209800131419244,
    224916973773967421352838735684, 596373847126147985434982575724,
    1580784678250571882017480243636, 4190893020903935054619120005916)

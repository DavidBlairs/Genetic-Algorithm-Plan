library(dplyr)

# Some global parameters for the problem
seed <- 1; 
A_y  <- 10;
B_x  <- 20;
R    <- 2;
pop_s <- 100; 
g_max <- 100;

# fitness function parameters
w <- 2;
a <- 1;

# this is the function for orientating our initial population
heuristic_function <- function(x){(((-1) * A_y * x) / B_x) + A_y}

# generate a population from the uniform distribution
initial_population_random <- function(A_y, genotype_length, number_of_genotypes){
  # return the matrix. 
  # Change 'runif' if a different distribution is wanted
  return(
    matrix(
      runif(
        genotype_length * number_of_genotypes, 
        min = 0,
        max = A_y
      ),
      nrow = genotype_length, 
      ncol = number_of_genotypes
    )
  )
}

# add in heuristically generated genotypes
initial_population_heuristic <- function(population, probability, seed, FUN_H = heuristic_function){
  return(
    apply(
      population, 2, FUN = function(x){
        return(
        ifelse(sample(
          # decide whether to apply the heuristic method for the phenotype
          c(0, 1), size = 1, replace = FALSE, prob = c(1 - probability, probability)
        ) == 1, {
          # if yes, apply the method
          index   <- 1:length(x); 
          lapply(index, FUN = FUN_H)
          # if not, return the original genotype
        }, as.vector(x))
        )
      }
    )
  )
}

# apply the cauchy mutation operator
mutation_cauchy <- function(population, location, spread){
  change <- matrix(
    rcauchy(nrow(population) * ncol(population), location = location, scale = spread),
    ncol = ncol(population),
    nrow = nrow(population)
  );
  
  return(
    # add on a random number to each element from a cauchy distribution
    matrix(as.numeric(population) + as.numeric(change), ncol = ncol(population), nrow = nrow(population))
  )
}

# bread two genotypes
crossover_bread <- function(gen_1, gen_2){
  pivot_point <- sample(2:(length(gen_1) - 1), size = 1);
  return(
    c(gen_1[1:pivot_point], gen_2[(pivot_point + 1):length(gen_2)])
  )
}

# apply the crossover algorithm
crossover_basic <- function(population, number_of_offspring){
  # Get the people chosen for crossover
  chosen_people <- population[,sample(1:ncol(population), size = number_of_offspring * 2, replace = F)];

  offspring <- matrix(ncol = 0, nrow = nrow(population));
  # for each child needed: 
  for (genotype in 1:(ncol(chosen_people) / 2)){
    # figure out who their parents will be (already randomly sorted)
    gen1 <- chosen_people[, genotype];
    gen2 <- chosen_people[, ncol(chosen_people) + 1 - genotype];
    
    # create the child
    current_offspring <- crossover_bread(gen1, gen2) %>% matrix(ncol = 1, nrow = nrow(population));
    offspring <- cbind(offspring, current_offspring);
  }
  # return the complete list of children created
  return(cbind(population, offspring))
}

# calculate fitness array of a population
fitness_function <- function(population){
  return(apply(population, MARGIN = 2, FUN = function(x){
  x1 <- as.numeric(x[1]);
  x2 <- as.numeric(x[2]);
  
  z <- (sin(2*x1)**2)*(sin(2*x2)**2)*(exp(1)**((-((x1 + x2)/1.5)))); 
  return(as.numeric(z))
  }))
}

# perform selection
selection <- function(population, number_to_remove){
  return(population[, order(fitness_function(population))][, number_to_remove:(ncol(population))])
}


original_population <- initial_population_random(A_y = A_y, genotype_length = R, number_of_genotypes = pop_s) #%>% 
                       #initial_population_heurstic(seed = 5, probability = 0.2, FUN_H = heuristic_function);

print(original_population)
for (i in 1:g_max){
  original_population <- mutation_cauchy(population = original_population, location = 0, spread = 1) %>%
                         crossover_basic(number_of_offspring = 7) %>%
                         selection(number_to_remove = 7);
  if (i == 1){
    print(mean(original_population[, 1]))
    before <- original_population;
  } else if (i == 99){
    print(mean(original_population[, 1]))
    after <- original_population;
  }
}

x <- c(as.vector(before[1, ]), as.vector(after[1, ]));
y <- c(as.vector(before[2, ]), as.vector(after[2, ]));

group <- c(rep("Before", length(before)), rep("After", length(after))); 
plot(x, y,
     pch = 19,
     col = factor(group))

legend("topleft",
       legend = levels(factor(group)),
       pch = 19,
       col = factor(levels(factor(group))))

full_data <- data.frame(list(x = -5:5, y = -5:5))

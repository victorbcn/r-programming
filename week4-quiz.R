week4.question1<-function() {
    set.seed(1)
    rpois(5, 2)
}

week4.question2 <- function() {
    rnorm(100, 0, 1)
}

week4.question3 <- function() {
    # When simulating data, why is using the set.seed() function
    # important? Select all that apply.
    
    # Only: It ensures that the sequence of random numbers starts
    #       in a specific place and is therefore reproducible.
    # OR: It can be used to specify which random number generating 
    #     algorithm R should use, ensuring consistency and reproducibility.
}


week4.question4 <- function() {
    # Which function can be used to evaluate the 
    # inverse cumulative distribution function for the Poisson distribution?
    
    # qpois
}

week4.question5 <- function() {
    
    set.seed(10)
    x <- rep(0:1, each = 5)
    e <- rnorm(10, 0, 20)
    y <- 0.5 + 2 * x + e
    
    # Generate data from a Normal linear model
}

week4.question6 <- function() {
    # What R function can be used to generate Binomial random variables?
    rbinom(100, 10, 0.9)
}

week4.question7 <- function() {
    # What aspect of the R runtime does the profiler keep track of when an R expression is evaluated?
    
    # the function call stack
}

week4.question8 <- function() {
    
    # 100%
}

week4.question9 <- function() {
    # It is the time spent by the CPU evaluating an expression
}

week4.question10 <- function() {
    print("elapsed time may be smaller than user time")
}
## Week 3 Quiz

week3.question1<-function() {
    
    library(datasets)
    data(iris)
    print(summary(iris))
    # mean(iris$Sepal.Length)
    tapply(iris$Sepal.Length, iris$Species, mean )
    
    # 6 incorrect!
    
}

week3.question2<-function() {
    library(datasets)
    data(iris)
    print(summary(iris))
    x<-apply(iris[,1:4], 2, mean)
    x
}

week3.question3<-function() {
    library(datasets)
    data(mtcars)
    
    tapply(mtcars$mpg, mtcars$cyl, mean)
    sapply(split(mtcars$mpg, mtcars$cyl), mean)
    with(mtcars, tapply(mpg, cyl, mean))
    
}

week3.question4<-function() {
    hp<-tapply(mtcars$hp, mtcars$cyl, mean)
    hp[[3]]-hp[[1]]
    # 127
}

week3.question5<-function() {
    
    ## Execution of 'ls' will suspend at the beginning of the function and you will be in the browser.
}


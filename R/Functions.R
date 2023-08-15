arg1 <- 3

square <- function(arg1) {
	arg1*arg1
}

square(arg1)


arg.raise <- 5
power <- 2
raise <- function(arg.raise,power){
	arg.raise^power
}


raise(5,2)
raise(6,7)
raise(4,1.2)


raise <- function(arg.raise,power=2){
	arg.raise^power
}


raise(5)

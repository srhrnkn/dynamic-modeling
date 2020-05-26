#functions
a <- -5
b <- 10
c <- .3
d <- 4

x <- seq(-20,50,by = 1)

f_line <- function(x,b,c){b*x + c}

f_parabola <- function(x,a,b,c){a*(x^2)+(b*x)+c}


f_rational <- function(x,a,b,c,d){(a*x+c)/(b*x+d)}


f_bell <- function(x,max_val,duration,time_to_start){
  max_val*exp(-(x-time_to_start-10)^2/duration)}

#s shaped
f_s_shape <- function(x,a,c,shift_right,stretch,scale){
  x <- x-shift_right
  x <- x/stretch
  (c*exp(a*(x))/(c*exp(a*(x))+(1-c)))*scale}


holds_placed_function_old <- function(i,top){round(top*exp(((1-i)+1)/5),0)}
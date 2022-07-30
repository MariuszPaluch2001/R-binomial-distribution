bion <- function(k,n,p){
	return (choose(n,k) * (p ^ k) * ((1-p) ^ (n-k)))
}
bion_sum <- function(k_begin,k_end,n,p){
	return (sum(sapply(k_begin:k_end, bion, n, p)))
}
n <- 100
p <- 0.00720
xs <- seq(1,20)
ys <- sapply(xs, bion, n, p) 
print(xs)
print(ys)
sprintf("Sum k from 0 to 100: %.4f", bion_sum(1,100,n,p))
data <- data.frame(
  name= xs,
  value=ys
)

barplot(height=data$value, names=data$name)

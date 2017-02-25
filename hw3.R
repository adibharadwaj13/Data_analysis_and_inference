data<-read.table("hcmv.txt",header=T)
site<-data$location
total=229354
n.region=60

#Function for the generation of a kai square table
chisqtable <- function(n.region, site, N){
  n <- length(site)
  lambda.est <- n/n.region
  count.int <- table(cut(site, breaks = seq(1, total, length.out=n.region+1), include.lowest=TRUE))
  count.vector <- as.vector(count.int)
  count.range <- max(count.vector) - min(count.vector) + 1
  
  table <- matrix(rep(NA, count.range*3), count.range, 3)
  for (i in 1:count.range){
    offset <- min(count.vector) - 1
    table[i, 1] <- i + offset
    table[i, 2] <- sum(count.vector == i + offset)
    if ((i + offset == min(count.vector)) && (min(count.vector) != 0))
      table[i, 3] <- ppois(i+offset, lambda.est)*n.region
    else if (i + offset == max(count.vector))
      table[i, 3] <- 1 - ppois(i + offset - 1, lambda.est)
    else
      table[i, 3] <- (ppois(i+offset, lambda.est) - ppois(i + offset - 1, lambda.est))*n.region
  }
  
  return (table)
}

site.random.tabtemp <- chisqtable(n.region, site, N)
# FIRST PLOT

rm(list=ls())
library(dplyr)
library(ggplot2)

dataset <- as_tibble(
    x <- read.csv(
        file = "http://michal.ramsza.org/lectures/2_r_programming/data/data_2.csv",
        encoding = 'ASCII'))

d <- data.frame(dataset, NumOfOffers=1)
aggregated = aggregate(NumOfOffers ~ Brand+Engine_capacity, data = d, FUN=sum)

plot1 <- (ggplot(aggregated, aes(x = Engine_capacity, y = Brand, size = NumOfOffers)) + 
            geom_point(col=rgb(0,0,1,0.2))
)

x11();
print(plot1)

# SECOND PLOT

x <- reshape(aggregated, dir="wide", idvar=c("Brand"), timevar="Engine_capacity")
name <- x[,1]
x <- as.matrix(x[,-1])
x[is.na(x)] <- 0 
N <- length(name)
M <- matrix( rep(0,N*N), ncol = N)

for (i in 1:N) {
    for (j in 1:N) {
        M[i,j] <- sqrt(sum( (x[i,] - x[j,])^2 ))
    }
}

rownames(M) <- name
colnames(M) <- name
loc <- as.data.frame(cmdscale(M))
colnames(loc) <- c("X", "Y")
loc$label <- rownames(loc)

plot2 <- (ggplot(loc, aes(x=X, y=Y, label=label)) + 
           scale_y_reverse() +
           geom_point(col=rgb(0,0,1,1), size=1) + 
           geom_text(col=rgb(0,0,1,0.3), cex=2, vjust=-1,)
)

x11();
print(plot2)
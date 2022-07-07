rm(list=ls())

radarChart <- function(data=d1,
                       polygon_col=rgb(0, 0, 1, .2),
                       polygon_border_col="blue",
                       polygon_border_lty=2,
                       polygon_border_lwd=2,
                       polygon_density=20, 
                       polygon_angle=45,
                       radar_col="grey",
                       radar_lty=3, 
                       radar_lwd=2){
    
    N <- length(data)
    R <- max(data)
    
    x_coordinates <- c()
    y_coordinates <- c()
    
    if(N>=3){
        x11()
        plot(x=0, y=0, type="n", xlim=c(-R,R), ylim=c(-R,R), xlab="",
             ylab="", asp=1)
        
        for(i in 1:N){
            a = (2*i*pi)/N
            x <- data[i]*cos(a)
            y <- data[i]*sin(a)
            
            x_coordinates <- append(x_coordinates, x)
            y_coordinates <- append(y_coordinates, y)
            
            x1 <- R*cos(a)
            y1 <- R*sin(a)
            
            label <- names(data[i])
            
            segments(x=0, y=0, x1=x1, y1=y1, col=radar_col, lty=radar_lty, 
                     lwd=radar_lwd)
            text(x=x1,y=y1,labels=label)
        }
        
        polygon(x_coordinates, y_coordinates, col=polygon_col,
                density=polygon_density, angle=polygon_angle, 
                border=polygon_border_col, lty=polygon_border_lty)
        
        for(i in 1:N){
            alpha <- c(2*pi*(1:360)/360)
            x <- data[i]*cos(alpha)
            y <- data[i]*sin(alpha)
            lines(x, y, col=radar_col, lty=radar_lty, lwd=radar_lwd)
        }    
        
    } else {print("In your dataset should be more than 2 values.")}
    
}

# FIRST EXAMPLE

d1 <- 1:10
names(d1) <- LETTERS[1:10]

radarChart(
    data = d1,
    polygon_col="violet",
    polygon_border_lty=1,
    polygon_border_col="darkblue"
)

# SECOND EXAMPLE

d2 <- abs(rnorm(20, mean = 1, sd = .3))
names(d2) <- LETTERS[1:20]

radarChart(
  data=d2,
  polygon_col=rgb(0, 0, 1, .2),
  polygon_border_lty="solid",
  polygon_border_col="blue",
  polygon_density=NA
)

# THIRD EXAMPLE

d3 <- 1:5
names(d3) <- LETTERS[1:5]

radarChart(data=d3,
  polygon_col="orange",
  polygon_border_col="darkred",
  polygon_border_lty=2,
  polygon_border_lwd=2,
  polygon_density=40, 
  polygon_angle=110,
  radar_col="grey",
  radar_lty=3, 
  radar_lwd=2
)

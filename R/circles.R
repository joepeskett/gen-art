## Circles and Trigonometry
library(tidyverse)
library(ggforce)
library(ggplot2)
library(tibble)


#' @title partial_circles
#' @author Joe 
#' @export
#' How should we parameterise the offsets in this case?

partial_circles <- function(radius) {
  return(function(angles, jitter){
    num_points = length(angles)
    if (jitter == TRUE){
      offset = c(rep(0.8, num_points/20), 
                 rep(1.1, num_points/20),
                 rep(0.8, num_points/20),
                 rep(1.1, num_points/20))
    }else{
      offset =1
    }
    
    x = radius * sin(angles) * offset
    y = radius * cos(angles) * offset
    return(tibble(x = x + (rnorm(1) * rbernoulli(1, 1/radius)),
                  y = y + (rnorm(1) * rbernoulli(1, 0.5)),
                  radius = radius,
                  point_num = 1:num_points))
  })
}

#' @title partial_spirals
#' @author Joe
#' @export
partial_spirals <- function(radius) {
  return(function(angles, jitter = FALSE){
    num_points = length(angles)
    if(jitter == TRUE){
      spiral_max = abs(rnorm(1))
    }else{
      spiral_max = 1
    }
    # Coords
    x = radius * sin(angles) * seq(0, spiral_max, length.out = num_points)
    y = radius * cos(angles) * seq(0, spiral_max, length.out = num_points)
    #Rand shift
    rand_shift <- rnorm(1) * rbernoulli(1, 0.01)
    return(tibble(x = x + rand_shift,
                  y = y + rand_shift,
                  radius = radius,
                  point_num = 1:num_points))
  })
}


#' @title create_circles
#' @author Joe
#' @export
create_circles <- function(num_circles = 10, mean_rad = 15, partial_fun = partial_spirals){
  radii <- rnorm(num_circles, mean_rad, 2)
  return(lapply(radii, partial_fun))
}

#' @title null_fun
#' @description Place holder when no transformation is applied coordinates
#' @author Joe
#' @export
null_fun <- function(x){
  return(x)
}

#' @title build_circle_frame
#' @description Function for building the `circle_frame`. Firstly we create a list of
#' circle building functions. Next we build a wrapper function for generating vectors
#' of angles and applying a circle_builder to that vector of angles. This wrapper function
#' is then applied to all of the circle_builder functions. Outputs for all circles are stacked
#' into circle frame, where there is the option to move the centre of the spiral/circle using
#' x_fun or y_fun.
#' @author Joe
#' @export
build_circle_frame <- function(num_circles = 10, 
                               x_fun = 0, y_fun = 0, 
                               seq_min = 0, seq_max = 2*pi, seq_length = 1000, 
                               mean_rad = 15,
                               partial_fun = partial_spirals){
  
  circle_builders <- create_circles(num_circles = num_circles, 
                                    mean_rad = mean_rad, 
                                    partial_fun)
  wrapping <- function(circle_builder){
    random_length <- rpois(1, 10)
    random_max <- rnorm(1, 2, 1)
    sequence <- seq(seq_min, seq_max + random_max, length.out = seq_length)
    output <- circle_builder(sequence, jitter = FALSE)
    return(output)
  }
  circle_tibbles <- do.call(rbind,
                            lapply(circle_builders, 
                                   wrapping
                                   )
                            )
  circle_frame <- circle_tibbles %>% 
    mutate(x = x + x_fun,
           y = y + y_fun)
  return(circle_frame)
}


#====================== Plotting Tests =============


output1 <- build_circle_frame(num_circles = 15, mean_rad = 50,
                              seq_min = 0, seq_max = 3*pi, seq_length = 5000, partial_fun = partial_circles)

output2 <- build_circle_frame(num_circles = 35, mean_rad = 15, 
                              seq_min = pi*0.8, seq_max = 4.2*pi, seq_length = 5000,
                              x_fun = tail(output1$x,1), y_fun = tail(output1$y,1), partial_fun = partial_circles)

output3 <- build_circle_frame(num_circles = 20, mean_rad = 40, 
                              seq_min = 0, seq_max = pi*1, seq_length = 5000,
                              x_fun = tail(output2$x, 1), y_fun = tail(output2$y, 1), partial_fun = partial_circles)
output4 <- build_circle_frame(num_circles = 20, mean_rad = 30,
                              seq_min = pi*2, seq_max = pi*7, seq_length = 5000,
                              x_fun = tail(output3$x, 1), y_fun = tail(output3$y, 1), partial_fun = partial_circles)

output5 <- build_circle_frame(num_circles = 35, mean_rad = 18,
                              seq_min = pi*2, seq_max = pi*5, seq_length = 5000,
                              x_fun = tail(output4$x, 1), y_fun = tail(output4$y, 1), partial_fun = partial_circles)
output6 <- build_circle_frame(num_circles = 15, mean_rad = 25,
                              seq_min = pi*2, seq_max = pi*6, seq_length = 5000,
                              x_fun = tail(output5$x, 1), y_fun = tail(output5$y, 1), partial_fun = partial_circles)


ggplot()+
  geom_path(data = output1, mapping = aes(x=x, 
                                          y=y,
                                          group = radius,
                                          colour = radius),
            alpha = 0.3, size = 0.95) +
  geom_path(data = output2, mapping = aes(x=x, 
                                           y=y,
                                           group = radius,
                                           colour = radius),
             alpha = 0.5, size = 0.3) +
  geom_path(data = output3, mapping = aes(x=x, 
                                          y=y,
                                          group = radius,
                                          colour = radius),
            alpha = 0.4, size = 0.7) +
  geom_path(data = output4, mapping = aes(x=x, 
                                          y=y,
                                          group = radius,
                                          colour = radius),
            alpha = 0.5, size = 0.8) +
  geom_path(data = output5, mapping = aes(x=x, 
                                          y=y,
                                          group = radius,
                                          colour = radius),
            alpha = 0.3, size = 0.6) +
  geom_path(data = output6, mapping = aes(x=x, 
                                          y=y,
                                          group = radius,
                                          colour = radius),
            alpha = 0.3, size = 0.5) +
  
  coord_fixed() +
  #xlim(-30, 30)+
  #ylim(-30, 30)+
  scale_color_gradient2(low = 'red',mid = 'green', high = 'red')+
  theme_void() + 
  theme(plot.background = element_rect(fill = 'black'), legend.position = 0)

ggplot(data = test_output, mapping = aes(x = x, y = y, group = radius, colour = point_num*5))+
  geom_path(alpha = 0.3, size = 0.25) + 
  coord_fixed() +
  #theme_void() + 
  theme(plot.background = element_rect(fill = 'black'), legend.position = 0)

circle <- tibble(angles = seq(0, 1*pi, length.out = 1000),
                 offset1 = rep(c(rep(1, 150), rep(2, 50)), 5),
                 offset2 = sin(seq(0,rad(720*3), length.out = 1000)),
                 x1 = (4 + offset2*1.1) * sin(angles),
                 y1 = (4 + offset2) * cos(angles),
                 x2 = 2 * offset1 * sin(angles),
                 y2 = 2 * offset1* cos(angles),
                 x3 = 5 + (3 * sin(angles)),
                 y3 = 3 * cos(angles))
ggplot(data = circle)+
  geom_path(aes(x = x1, y = y1, alpha = 0.2, colour = 'white')) +
  geom_path(aes(x = x2, y = y2, alpha = 0.2, colour = 'white')) +
  geom_path(aes(x = x3, y = y3, alpha = 0.6, colour = 'white')) +  
  coord_fixed() +
  theme_void() + 
  theme(plot.background = element_rect(fill = 'black'))


#======== Plot spirals around a circle ====================

# First we create a set of points around a circle

point_sequence <- seq(0, pi*2, length.out = 25)
radius = 75
point_tibble <- tibble(angle = point_sequence,
                       x = radius * sin(angle),
                       y = radius * cos(angle))
ggplot(data = point_tibble,mapping = aes(x = x, y = y)) +
  geom_point() + coord_fixed()

# Plot a basic spiral 

output1 <- build_circle_frame(num_circles = 15, mean_rad = 15,
                              seq_min = 0, seq_max = 3*pi, seq_length = 1000)

ggplot(data = output1, mapping = aes(x = x, y = y)) +
  geom_point() + coord_fixed()

# Build a function for apply 

shift_a_spiral <- function(x_alt, y_alt){
  output <- output1 %>% mutate(x = x + 0.1*y + x_alt,
                            y = y + 0.1*x+ y_alt)
  return(output)
}

spiral_tibble <- point_tibble %>% 
  rowwise()%>%
  mutate(spirals = list(shift_a_spiral(x, y)))

spiral_tibble$spirals

geom_spiral <- function(spiral_table){
  return(geom_path(data = spiral_table, mapping = aes(x=x, y=y, 
                                                      group = radius, 
                                                      colour = radius), 
                   alpha = 0.3, size = 0.7))
}
ggplot()+
  geom_spiral(spiral_tibble$spirals[[1]])+
  geom_spiral(spiral_tibble$spirals[[3]])+
  geom_spiral(spiral_tibble$spirals[[5]])+
  geom_spiral(spiral_tibble$spirals[[7]])+
  geom_spiral(spiral_tibble$spirals[[9]])+
  geom_spiral(spiral_tibble$spirals[[11]])+
  geom_spiral(spiral_tibble$spirals[[13]])+
  geom_spiral(spiral_tibble$spirals[[15]])+
  geom_spiral(spiral_tibble$spirals[[17]])+
  geom_spiral(spiral_tibble$spirals[[19]])+
  geom_spiral(spiral_tibble$spirals[[21]])+
  geom_spiral(spiral_tibble$spirals[[23]])+
  coord_fixed() +
  theme_void()


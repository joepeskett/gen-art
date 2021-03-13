## Circles and Trigonometry
library(tidyverse)
library(ggforce)
library(bezier)
library(circular)
library(ggplot2)


#' @title partial_circles
#' @author Joe 
#' @export
#' How should we parameterise the offsets in this case?

partial_circles <- function(radius) {
  return(function(angles){
    num_points = length(angles)
    offset = c(rep(0.8, num_points/20), 
               rep(1.1, num_points/20),
               rep(0.8, num_points/20),
               rep(1.1, num_points/20))
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
  return(function(angles){
    num_points = length(angles)
    offset = c(rep(0.8, num_points/20), 
               rep(1.1, num_points/20),
               rep(0.8, num_points/20),
               rep(1.1, num_points/20))
    x = radius * sin(angles) * seq(0, 1, length.out = num_points)
    y = radius * cos(angles) * seq(0, 1, length.out = num_points)
    return(tibble(x = x + (rnorm(1) * rbernoulli(1, 1/radius)),
                  y = y + (rnorm(1) * rbernoulli(1, 0.5)),
                  radius = radius,
                  point_num = 1:num_points))
  })
}


#' @title create_circles
#' @author Joe
#' @export
create_circles <- function(num_circles = 10, partial_fun){
  radii <- rnorm(num_circles, 15, 5)
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
build_circle_frame <- function(num_circles = 10, x_fun = null_fun, y_fun = null_fun, 
                               seq_min = 0, seq_max = 2*pi, seq_length = 1000, 
                               partial_fun = partial_spirals){
  
  circle_builders <- create_circles(num_circles = num_circles)
  #By default, all our circles have length 1000 points
  #Can change this in the future. 
  circle_tibbles <- do.call(rbind,
                            lapply(circle_builders, 
                                   function(x) x(seq(seq_min, seq_max, length.out = seq_length))
                                   )
                            )
  circle_frame <- circle_tibbles %>% 
    mutate(x = x_fun(x),
           y = y_fun(y))
  return(circle_frame)
}

output1 <- build_circle_frame(num_circles = 50, seq_min = 0, seq_max = pi, seq_length = 500)
output2 <- build_circle_frame(num_circles = 50, seq_min = pi, seq_max = 3*pi, seq_length = 1000)
output3 <- build_circle_frame(num_circles = 50, seq_min = pi*2, seq_max = pi*1, seq_length = 500)
output4 <- build_circle_frame(num_circles = 50, seq_min = pi*7, seq_max = pi*6, seq_length = 500)

ggplot()+
  geom_path(data = output1, mapping = aes(x=x, 
                                          y=y,
                                          group = radius,
                                          colour = point_num*3), alpha = 0.25) +
  
  geom_path(data = output2, mapping = aes(x=x, 
                                          y=y,
                                          group = radius,
                                          colour = point_num*3), alpha = 0.2)+
  geom_path(data = output3, mapping = aes(x=x, 
                                          y=y,
                                          group = radius,
                                          colour = point_num*3), alpha = 0.25)+
  geom_path(data = output4, mapping = aes(x=x, 
                                          y=y,
                                          group = radius,
                                          colour = point_num*3), alpha = 0.1)+
  
  coord_fixed() +
  xlim(-30, 30)+
  ylim(-30, 30)+
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

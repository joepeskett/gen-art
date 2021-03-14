# Attractors



library(tidyverse)

random_attractor <- function(num_points = 10000, x_init = 0, y_init= 0, v = 7){
  a = rnorm(1,1.5,1)
  b = rnorm(1,a,1)
  c = rnorm(1,b,a^2)
  d = rnorm(1,c,b^2)
  u_x <- c(x_init,x_init)
  u_y <- c(y_init,y_init)
  print(a)
  print(b)
  print(c)
  print(d)
  for (i in 1:num_points){
    e <- sin(i*v)
    u_x[[i+1]] <- e*(sin(a*u_y[[i]]) + c*cos(a*u_x[[i]]))
    u_y[[i+1]] <- e*(sin(b*u_x[[i]]) + d*cos(b*u_y[[i]]))
  }
  tibble(x = u_x,
         y = u_y)
}




ggplot(data = random_attractor(500000)) +
  geom_point(aes(x = x, y = y, colour = x*y), alpha = 0.05, size = 0.15) + 
  theme_void() +
  coord_fixed() +
  scale_color_gradient2(low = 'red',mid = 'orange', high = 'red')+
  theme(plot.background = element_rect(fill = 'black'), legend.position = 0)
#

library(tidyverse)
fixed_attractor <- function(num_points = 10000, 
                            x_init = 0, y_init= 0, v = 7,
                            a,b,c,d){
  u_x <- c(x_init,x_init)
  u_y <- c(y_init,y_init)
  print(a)
  print(b)
  print(c)
  print(d)
  for (i in 1:num_points){
    e <- sin(i*v)
    u_x[[i+1]] <- e*(sin(a*u_y[[i]]) + c*cos(a*u_x[[i]])) * cos(u_y[[i]])
    u_y[[i+1]] <- e*(sin(b*u_x[[i]]) + d*cos(b*u_y[[i]])) * sin(u_x[[i]])
  }
  tibble(x = u_x,
         y = u_y)
}
fixed_data <-  fixed_attractor(1000000, v = 8,
                             a = 0.9369414, b = 2.725296, c= 3.588914,d=1.928188)
ggplot(data = fixed_data) +
  geom_point(aes(x = x, y = y), alpha = 0.2, size = 0.3) + 
  theme_void() +
  coord_fixed() +
  scale_color_gradient2()+
  theme(plot.background = element_rect(fill = '#ffff7f'), legend.position = 0)
  
#=====Hopalong Attractor
  
x <- c(0,0)
y <- c(0,0)

a <- 1.13
b <- 3.315
c <- 4.63

num_points = 1e5

for (i in 2:num_points){
  x[[i]] <- y[[i-1]] - 2 - sqrt(abs(b - x[[i-1]]-1-c)) * sign(x[[i-1]]) 
  y[[i]] <- a - x[[i-1]] -1
}

plotting <- tibble(x = x,
                   y =y)
ggplot(data = plotting, aes(x = x, y=y))+
  geom_point(alpha = 0.05, size = 0.1) + 
  theme_void() + 
  coord_fixed()


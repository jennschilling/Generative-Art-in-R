# From https://fronkonstin.com/2019/03/27/drrrawing-with-purrr/

library(tidyverse)
 
# Create a pentagon and plot it
# accumulate is used to apply the cosine/sine function sequentially
pentagon <- tibble(
  x = accumulate(1:4, ~.x+cos(.y*2*pi/5), .init = 0),
  y    = accumulate(1:4, ~.x+sin(.y*2*pi/5), .init = 0),
  xend = accumulate(2:5, ~.x+cos(.y*2*pi/5), .init = cos(2*pi/5)),
  yend = accumulate(2:5, ~.x+sin(.y*2*pi/5), .init = sin(2*pi/5))
  )

# Plot the pentagon
ggplot(pentagon)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()


# A function to generate a regular polygon given a number of edges
polygon <- function(n) {
  tibble(
    x    = accumulate(1:(n-1), ~.x+cos(.y*2*pi/n), .init = 0),
    y    = accumulate(1:(n-1), ~.x+sin(.y*2*pi/n), .init = 0),
    xend = accumulate(2:n,     ~.x+cos(.y*2*pi/n), .init = cos(2*pi/n)),
    yend = accumulate(2:n,     ~.x+sin(.y*2*pi/n), .init = sin(2*pi/n)))
}

# Examples of plotting ploygons using the function
ggplot(polygon(6))+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

ggplot(polygon(7))+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

ggplot(polygon(8))+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

ggplot(polygon(9))+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

# Add segments to the polgyon
polygon(5) -> df1

df1 %>% mutate(angle = atan2(yend-y, xend-x)+pi/2,
               x = 0.5*x+0.5*xend,
               y = 0.5*y+0.5*yend,
               xend = x+0.2*cos(angle),
               yend = y+0.2*sin(angle)) %>% 
  select(x, y, xend, yend) -> df2

df1 %>% bind_rows(df2) -> df

ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

# Now create an inner polygon that joins the segments
polygon(5) -> df1

df1 %>% mutate(angle = atan2(yend-y, xend-x)+pi/2,
               x = 0.5*x+0.5*xend,
               y = 0.5*y+0.5*yend,
               xend = x+0.2*cos(angle),
               yend = y+0.2*sin(angle)) %>% 
  select(x, y, xend, yend) -> df2

df2 %>% mutate(
  x=xend,
  y=yend,
  xend=lead(x, default=first(x)),
  yend=lead(y, default=first(y))) %>% 
  select(x, y, xend, yend) -> df3

df1 %>% bind_rows(df2) %>% bind_rows(df3) -> df

ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

# A function to generate the midpoint lines
mid_points <- function(d) {
  d %>% mutate(
    angle=atan2(yend-y, xend-x) + pi/2,
    x=0.5*x+0.5*xend,
    y=0.5*y+0.5*yend,
    xend=x+0.2*cos(angle),
    yend=y+0.2*sin(angle)) %>% 
    select(x, y, xend, yend)
}

# A function to create an inner polygon that connects the midpoint lines
con_points <- function(d) {
  d %>% mutate(
    x=xend,
    y=yend,
    xend=lead(x, default=first(x)),
    yend=lead(y, default=first(y))) %>% 
    select(x, y, xend, yend)
}

# Use the functions to create repeated connected pentagons
polygon(5) -> df1
df2 <- mid_points(df1)
df3 <- con_points(df2)
df4 <- mid_points(df3)
df5 <- con_points(df4)
df1 %>% 
  bind_rows(df2) %>% 
  bind_rows(df3) %>% 
  bind_rows(df4) %>% 
  bind_rows(df5) -> df
ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

# Do the same thing as above but use accumulate to do it automatically
edges <- 5
niter <- 4
polygon(edges) -> df1
accumulate(.f = function(old, y) {
  if (y%%2!=0) mid_points(old) else con_points(old)
},
1:niter,
.init=df1) %>% 
  bind_rows() -> df
ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

# Update the mid_points function so that the line segments don't have to start from the middle
mid_points <- function(d, p) {
  d %>% mutate(
    angle=atan2(yend-y, xend-x) + pi/2,
    x=p*x+(1-p)*xend,
    y=p*y+(1-p)*yend,
    xend=x+0.2*cos(angle),
    yend=y+0.2*sin(angle)) %>% 
    select(x, y, xend, yend)
}
edges <- 7
niter <- 6
polygon(edges) -> df1
accumulate(.f = function(old, y) {
  if (y%%2==0) mid_points(old, 0.3) else con_points(old)
},
1:niter,
.init=df1) %>% 
  bind_rows() -> df
ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

# Update the mid_points function again so that the edges and line segments don't have to be perpendicular
mid_points <- function(d, p, a) {
  d %>% mutate(
    angle=atan2(yend-y, xend-x) + a,
    x=p*x+(1-p)*xend,
    y=p*y+(1-p)*yend,
    xend=x+0.2*cos(angle),
    yend=y+0.2*sin(angle)) %>% 
    select(x, y, xend, yend)
}
edges <- 7
niter <- 18
polygon(edges) -> df1
accumulate(.f = function(old, y) {
  if (y%%2!=0) mid_points(old, 0.3, pi/5) else con_points(old)
},
1:niter,
.init=df1) %>% 
  bind_rows() -> df
ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

# Change the longitude of the line segments by updating mid_points again
mid_points <- function(d, p, a, i, FUN = function(x) x) {
  d %>% mutate(
    angle=atan2(yend-y, xend-x) + a,
    radius=FUN(i),
    x=p*x+(1-p)*xend,
    y=p*y+(1-p)*yend,
    xend=x+radius*cos(angle),
    yend=y+radius*sin(angle)) %>% 
    select(x, y, xend, yend)
}

edges <- 7
niter <- 18
polygon(edges) -> df1
accumulate(.f = function(old, y) {
  if (y%%2!=0) mid_points(old, 0.3, pi/5, y) else con_points(old)
},
1:niter,
.init=df1) %>% 
  bind_rows() -> df
ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

# Play with number of iterations
edges <- 7
niter <- 250
step <- 2
polygon(edges) -> df1
accumulate(.f = function(old, y) {
  if (y%%step!=0) mid_points(old, 0.3, pi/5, y) else con_points(old)
},
1:niter,
.init=df1) %>% 
  bind_rows() -> df
ggplot(df)+
  geom_curve(aes(x=x, y=y, xend=xend, yend=yend),
             curvature = 0,
             color="black",
             alpha=0.1)+
  coord_equal()+
  theme(legend.position  = "none",
        panel.background = element_rect(fill="white"),
        plot.background  = element_rect(fill="white"),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())

# Final code
# This function creates the segments of the original polygon
polygon <- function(n) {
  tibble(
    x    = accumulate(1:(n-1), ~.x+cos(.y*2*pi/n), .init = 0),
    y    = accumulate(1:(n-1), ~.x+sin(.y*2*pi/n), .init = 0),
    xend = accumulate(2:n,     ~.x+cos(.y*2*pi/n), .init = cos(2*pi/n)),
    yend = accumulate(2:n,     ~.x+sin(.y*2*pi/n), .init = sin(2*pi/n)))
}

# This function creates segments from some mid-point of the edges
mid_points <- function(d, p, a, i, FUN = ratio_f) {
  d %>% mutate(
    angle=atan2(yend-y, xend-x) + a,
    radius=FUN(i),
    x=p*x+(1-p)*xend,
    y=p*y+(1-p)*yend,
    xend=x+radius*cos(angle),
    yend=y+radius*sin(angle)) %>% 
    select(x, y, xend, yend)
}

# This function connect the ending points of mid-segments
con_points <- function(d) {
  d %>% mutate(
    x=xend,
    y=yend,
    xend=lead(x, default=first(x)),
    yend=lead(y, default=first(y))) %>% 
    select(x, y, xend, yend)
}

edges <- 15   # Number of edges of the original polygon
niter <- 250 # Number of iterations
pond <- 0.67  # Weight to calculate the point on the middle of each edge
step  <- 9  # No of times to draw mid-segments before connect ending points
alph  <- 0.15 # transparency of curves in geom_curve
angle <- 1.27 # angle of mid-segment with the edge
curv <- 1.0   # Curvature of curves
line_color <- "black" # Color of curves in geom_curve
back_color <- "white" # Background of the ggplot
ratio_f <- function(x) {sin(x)} # To calculate the longitude of mid-segments

# Generation on the fly of the dataset
accumulate(.f = function(old, y) {
  if (y%%step!=0) mid_points(old, pond, angle, y) else con_points(old)
}, 1:niter,
.init=polygon(edges)) %>% bind_rows() -> df

# Plot
ggplot(df)+
  geom_curve(aes(x=x, y=y, xend=xend, yend=yend),
             curvature = curv,
             color=line_color,
             alpha=alph)+
  coord_equal()+
  theme(legend.position  = "none",
        panel.background = element_rect(fill=back_color),
        plot.background  = element_rect(fill=back_color),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())


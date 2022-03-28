source('patterns.r')
library(tidyverse)
library(gganimate)
library(magick)

size <- 50
mat <- matrix(
  data = rep(F, size^2),
  nrow = size, 
  ncol = size
) 
pattern <- patterns$glider_gun(size)
mat[pattern] <- TRUE #Set these to TRUE

as_tibble(mat) %>% mutate(row = row_number()) %>% 
  pivot_longer(1:(last_col() - 1), names_to = 'col') %>% 
  mutate(
    col = as.integer(str_extract(col, '\\d+'))
  ) %>% 
  ggplot(aes(col, row, fill = value)) + 
  geom_tile(col = 'lightgrey') + 
  scale_fill_manual(values = c('white', 'black')) + 
  coord_equal() + 
  guides(fill = 'none') + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_reverse()

cells <- expand_grid(
  row = 1:size,
  col = 1:size
)

niter <- 200
grids <- list()
grids[[1]] <- mat

for(i in 2:niter){
  walk2(cells$row, cells$col, function(rw, cl){
    upper <- if(rw != 1) {
      if(cl == 1){
        grids[[i-1]][(rw-1), cl:(cl+1)]
      } else if(cl == size){
        grids[[i-1]][(rw-1), (cl-1):cl]
      } else {
        grids[[i-1]][(rw-1), (cl-1):(cl+1)] 
      }
    } else {FALSE}
    lower <- if(rw != size) {
      if(cl == 1){
        grids[[i-1]][(rw+1), cl:(cl+1)]
      } else if(cl == size){
        grids[[i-1]][(rw+1), (cl-1):cl]
      } else {
        grids[[i-1]][(rw+1), (cl-1):(cl+1)] 
      }
    } else {FALSE}
    left <- if(cl == 1){
      FALSE
    } else {grids[[i-1]][rw, cl - 1]}
    right <- if(cl == size){
      FALSE
    } else {grids[[i-1]][rw, cl + 1]}
    
    surrounds <- sum(upper, lower, left, right, na.rm = T)
    
    if(mat[rw, cl] == FALSE){
      if(surrounds == 3){
        mat[rw, cl] <<- TRUE
      } else {
        mat[rw, cl] <<- FALSE
      }
    } else {
      if(surrounds < 2){
        mat[rw, cl] <<- FALSE
      } else if(surrounds %in% c(2,3)){
        mat[rw, cl] <<- TRUE
      } else {
        mat[rw, cl] <<- FALSE
      }
    }
    
  })
  
  grids[[i]] <- mat
}

data <- imap(grids, function(gd, i){
  as_tibble(grids[[i]]) %>% mutate(row = row_number()) %>% 
    pivot_longer(1:(last_col() - 1), names_to = 'col') %>% 
    mutate(
      col = as.integer(str_extract(col, '\\d+')),
      step = i
    ) 
}) %>% bind_rows()

p <- ggplot(data = data, aes(col, row, fill = value)) + 
  geom_tile(col = 'lightgrey') + 
  scale_fill_manual(values = c('white', 'black')) + 
  coord_equal() + 
  guides(fill = 'none') + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_reverse() + 
  transition_manual(step)


animate(p, nframes=niter, renderer=magick_renderer(), fps = 10)



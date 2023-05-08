pacman::p_load(rethinking)
library(pacman)
install.packages("rstan")
library("BiocManager")

install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
p_load(coda,mvtnorm,devtools,loo,dagitty,shape, rstan, rstanarm, cmdstanr, brms)


# we recommend running this is a fresh R session or restarting your current session

library(tidyverse)
d <-
  tibble(p1 = 0,
         p2 = rep(1:0, times = c(1, 3)),
         p3 = rep(1:0, times = c(2, 2)),
         p4 = rep(1:0, times = c(3, 1)),
         p5 = 1)

head(d)

### plot ###

d %>% 
  gather() %>% 
  mutate(x = rep(1:4, times = 5),
         possibility = rep(1:5, each = 4)) %>% 
  
  ggplot(aes(x = x, y = possibility, 
             fill = value %>% as.character())) +
  geom_point(shape = 21, size = 5) +
  scale_fill_manual(values = c("white", "navy")) +
  scale_x_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = c(.75, 4.25),
                  ylim = c(.75, 5.25)) +
  theme(legend.position = "none")

tibble(draw    = 1:3,
       marbles = 4) %>% 
  mutate(possibilities = marbles ^ draw) %>% 
  knitr::kable()

(
  d <-
    tibble(position = c((1:4^1) / 4^0, 
                        (1:4^2) / 4^1, 
                        (1:4^3) / 4^2),
           draw     = rep(1:3, times = c(4^1, 4^2, 4^3)),
           fill     = rep(c("b", "w"), times = c(1, 3)) %>% 
             rep(., times = c(4^0 + 4^1 + 4^2)))
)

d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_point(aes(fill = fill),
             shape = 21, size = 3) +
  scale_y_continuous(breaks = 1:3) +
  scale_fill_manual(values  = c("navy", "white")) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())

d <-
  tibble(position = c((1:4^1) / 4^0, 
                      (1:4^2) / 4^1, 
                      (1:4^3) / 4^2),
         draw     = rep(1:3, times = c(4^1, 4^2, 4^3)))

(
  d <-
    d %>% 
    bind_rows(
      d, d
    ) %>% 
    # here are the fill colors
    mutate(fill = c(rep(c("w", "b"), times = c(1, 3)) %>% rep(., times = c(4^0 + 4^1 + 4^2)),
                    rep(c("w", "b"), each  = 2)       %>% rep(., times = c(4^0 + 4^1 + 4^2)),
                    rep(c("w", "b"), times = c(3, 1)) %>% rep(., times = c(4^0 + 4^1 + 4^2)))) %>% 
    # now we need to shift the positions over in accordance with draw, like before
    mutate(denominator = ifelse(draw == 1, .5,
                                ifelse(draw == 2, .5 / 4,
                                       .5 / 4^2))) %>% 
    mutate(position = position - denominator) %>% 
    # here we'll add an index for which pie wedge we're working with
    mutate(pie_index = rep(letters[1:3], each = n()/3)) %>% 
    # to get the position axis correct for pie_index == "b" or "c", we'll need to offset
    mutate(position = ifelse(pie_index == "a", position,
                             ifelse(pie_index == "b", position + 4,
                                    position + 4 * 2)))
)

move_over <- function(position, index) {
  ifelse(index == "a", position,
         ifelse(index == "b", position + 4,
                position + 4 * 2)
  )
}

(
  lines_1 <-
    tibble(x    = rep((1:4), each = 4) %>% rep(., times = 3),
           xend = ((1:4^2) / 4)        %>% rep(., times = 3),
           y    = 1,
           yend = 2) %>% 
    mutate(x    = x - .5,
           xend = xend - .5 / 4^1) %>% 
    # here we'll add an index for which pie wedge we're working with
    mutate(pie_index = rep(letters[1:3], each = n()/3)) %>% 
    # to get the position axis correct for `pie_index == "b"` or `"c"`, we'll need to offset
    mutate(x    = move_over(position = x,    index = pie_index),
           xend = move_over(position = xend, index = pie_index))
)

(
  lines_2 <-
    tibble(x    = rep(((1:4^2) / 4), each = 4)  %>% rep(., times = 3),
           xend = (1:4^3 / 4^2)                 %>% rep(., times = 3),
           y    = 2,
           yend = 3) %>% 
    mutate(x    = x - .5 / 4^1,
           xend = xend - .5 / 4^2) %>% 
    # here we'll add an index for which pie wedge we're working with
    mutate(pie_index = rep(letters[1:3], each = n()/3)) %>% 
    # to get the position axis correct for `pie_index == "b"` or `"c"`, we'll need to offset
    mutate(x    = move_over(position = x,    index = pie_index),
           xend = move_over(position = xend, index = pie_index))
)

d <- 
  d %>% 
  mutate(remain = c(# `pie_index == "a"`
    rep(0:1, times = c(1, 3)),
    rep(0,   times = 4),
    rep(1:0, times = c(1, 3)) %>% 
      rep(., times = 3),
    rep(0,   times = 4 * 4),
    rep(c(0, 1, 0), times = c(1, 3, 4 * 3)) %>% 
      rep(., times = 3),
    # `pie_index == "b"`
    rep(0:1, each = 2),
    rep(0,   times = 4 * 2),
    rep(1:0, each = 2) %>% 
      rep(., times = 2),
    rep(0,   times = 4 * 4 * 2),
    rep(c(0, 1, 0, 1, 0), times = c(2, 2, 2, 2, 8)) %>% 
      rep(., times = 2),
    # `pie_index == "c"`
    rep(0:1, times = c(3, 1)),
    rep(0,   times = 4 * 3),
    rep(1:0, times = c(3, 1)), 
    rep(0,   times = 4 * 4 * 3),
    rep(0:1, times = c(3, 1)) %>% 
      rep(., times = 3),
    rep(0,   times = 4)
  )
  )

lines_1 <-
  lines_1 %>% 
  mutate(remain = c(rep(0,   times = 4),
                    rep(1:0, times = c(1, 3)) %>% 
                      rep(., times = 3),
                    rep(0,   times = 4 * 2),
                    rep(1:0, each  = 2) %>% 
                      rep(., times = 2),
                    rep(0,   times = 4 * 3),
                    rep(1:0, times = c(3, 1))
  )
  )

lines_2 <-
  lines_2 %>% 
  mutate(remain = c(rep(0,   times = 4 * 4),
                    rep(c(0, 1, 0), times = c(1, 3, 4 * 3)) %>% 
                      rep(., times = 3),
                    rep(0,   times = 4 * 8),
                    rep(c(0, 1, 0, 1, 0), times = c(2, 2, 2, 2, 8)) %>% 
                      rep(., times = 2),
                    rep(0,   times = 4 * 4 * 3),
                    rep(0:1, times = c(3, 1)) %>% 
                      rep(., times = 3),
                    rep(0,   times = 4)
  )
  )

lines_1 <-
  lines_1 %>% 
  mutate(remain = c(rep(0,   times = 4),
                    rep(1:0, times = c(1, 3)) %>% 
                      rep(., times = 3),
                    rep(0,   times = 4 * 2),
                    rep(1:0, each  = 2) %>% 
                      rep(., times = 2),
                    rep(0,   times = 4 * 3),
                    rep(1:0, times = c(3, 1))
  )
  )

lines_2 <-
  lines_2 %>% 
  mutate(remain = c(rep(0,   times = 4 * 4),
                    rep(c(0, 1, 0), times = c(1, 3, 4 * 3)) %>% 
                      rep(., times = 3),
                    rep(0,   times = 4 * 8),
                    rep(c(0, 1, 0, 1, 0), times = c(2, 2, 2, 2, 8)) %>% 
                      rep(., times = 2),
                    rep(0,   times = 4 * 4 * 3),
                    rep(0:1, times = c(3, 1)) %>% 
                      rep(., times = 3),
                    rep(0,   times = 4)
  )
  )


####

(d <- tibble(toss = c("w", "l", "w", "w", "w", "l", "w", "l", "w")))

(
  d <-
    d %>% 
    mutate(n_trials  = 1:9,
           n_success = cumsum(toss == "w"))
)

sequence_length <- 50

d2 <- d %>% 
  expand_grid(p_water = seq(from = 0, to = 1, length.out = sequence_length)) |> 
  group_by(p_water) 
  # you can learn more about lagging here: https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/lag or here: https://dplyr.tidyverse.org/reference/lead-lag.html
  d3 <- d2 |> 
    mutate(lagged_n_trials  = lag(n_trials,  k = 1L),
         lagged_n_success = lag(n_success, k = 1L)) 
  ungroup()  
  
  mutate(prior      = ifelse(n_trials == 1, .5,
                             dbinom(x    = lagged_n_success, 
                                    size = lagged_n_trials, 
                                    prob = p_water)),
         likelihood = dbinom(x    = n_success, 
                             size = n_trials, 
                             prob = p_water),
         strip      = str_c("n = ", n_trials)) 
  # the next three lines allow us to normalize the prior and the likelihood, 
  # putting them both in a probability metric 
  group_by(n_trials) %>% 
  mutate(prior      = prior      / sum(prior),
         likelihood = likelihood / sum(likelihood)) %>%   
  
  # plot!
  ggplot(aes(x = p_water)) +
  geom_line(aes(y = prior), linetype = 2) +
  geom_line(aes(y = likelihood)) +
  scale_x_continuous("proportion water", breaks = c(0, .5, 1)) +
  scale_y_continuous("plausibility", breaks = NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~strip, scales = "free_y")
  
  
  tibble(a = 0,
         b = c(1, 1.5, 2, 3, 9)) %>% 
    mutate(prob = 1 / (b - a))


  tibble(a = 0,
         b = c(1, 1.5, 2, 3, 9)) %>% 
    expand_grid(parameter_space = seq(from = 0, to = 9, length.out = 500)) %>% 
    mutate(prob = dunif(parameter_space, a, b),
           b = str_c("b = ", b)) %>% 
    
    ggplot(aes(x = parameter_space, ymin = 0, ymax = prob)) +
    geom_ribbon() +
    scale_x_continuous(breaks = c(0, 1:3, 9)) +
    scale_y_continuous(breaks = c(0, 1/9, 1/3, 1/2, 2/3, 1),
                       labels = c("0", "1/9", "1/3", "1/2", "2/3", "1")) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()) +
    facet_wrap(~b, ncol = 5)

  tibble(parameter_space = seq(from = 0, to = 1, length.out = 50)) %>% 
    # note our use of the `dbeta()` function
    mutate(prob = dbeta(parameter_space, 1, 1)) %>% 
    
    ggplot(aes(x = parameter_space, ymin = 0, ymax = prob)) +
    geom_ribbon() +
    ylim(0, 2) +
    theme(panel.grid = element_blank())  

  sequence_length <- 1e3
  
  d <-
    tibble(probability = seq(from = 0, to = 1, length.out = sequence_length)) %>% 
    expand_grid(row = c("flat", "stepped", "Laplace")) %>% 
    arrange(row, probability) %>% 
    mutate(prior = ifelse(row == "flat", 1,
                          ifelse(row == "stepped", rep(0:1, each = sequence_length / 2),
                                 exp(-abs(probability - 0.5) / 0.25) / (2 * 0.25))),
           likelihood = dbinom(x = 6, size = 9, prob = probability)) %>% 
    group_by(row) %>% 
    mutate(posterior = prior * likelihood / sum(prior * likelihood)) %>% 
    gather(key, value, -probability, -row) %>% 
    ungroup() %>% 
    mutate(key = factor(key, levels = c("prior", "likelihood", "posterior")),
           row = factor(row, levels = c("flat", "stepped", "Laplace")))   

  p1 <-
    d %>%
    filter(row == "flat") %>% 
    ggplot(aes(x = probability, y = value)) +
    geom_line() +
    scale_x_continuous(NULL, breaks = NULL) +
    scale_y_continuous(NULL, breaks = NULL) +
    theme(panel.grid = element_blank()) +
    facet_wrap(~key, scales = "free_y")
  
  p2 <-
    d %>%
    filter(row == "stepped") %>% 
    ggplot(aes(x = probability, y = value)) +
    geom_line() +
    scale_x_continuous(NULL, breaks = NULL) +
    scale_y_continuous(NULL, breaks = NULL) +
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank()) +
    facet_wrap(~key, scales = "free_y")
  
  p3 <-
    d %>%
    filter(row == "Laplace") %>% 
    ggplot(aes(x = probability, y = value)) +
    geom_line() +
    scale_x_continuous(NULL, breaks = c(0, .5, 1)) +
    scale_y_continuous(NULL, breaks = NULL) +
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank()) +
    facet_wrap(~key, scales = "free_y")
  
  p_load(patchwork)
  
  p1 / p2 / p3  

  (
    d <-
      tibble(p_grid = seq(from = 0, to = 1, length.out = 20),      # define grid
             prior  = 1) %>%                                       # define prior
      mutate(likelihood = dbinom(6, size = 9, prob = p_grid)) %>%  # compute likelihood at each value in grid
      mutate(unstd_posterior = likelihood * prior) %>%             # compute product of likelihood and prior
      mutate(posterior = unstd_posterior / sum(unstd_posterior))   # standardize the posterior, so it sums to 1
  )  

  p1 <-
    d %>% 
    ggplot(aes(x = p_grid, y = posterior)) +
    geom_point() +
    geom_line() +
    labs(subtitle = "20 points",
         x = "probability of water",
         y = "posterior probability") +
    theme(panel.grid = element_blank())  

  p2 <-
    tibble(p_grid = seq(from = 0, to = 1, length.out = 5),
           prior  = 1) %>%
    mutate(likelihood = dbinom(6, size = 9, prob = p_grid)) %>%
    mutate(unstd_posterior = likelihood * prior) %>%
    mutate(posterior = unstd_posterior / sum(unstd_posterior)) %>% 
    
    ggplot(aes(x = p_grid, y = posterior)) +
    geom_point() +
    geom_line() +
    labs(subtitle = "5 points",
         x = "probability of water",
         y = "posterior probability") +
    theme(panel.grid = element_blank())  

  p1 + p2 + plot_annotation(title = "More grid points make for smoother approximations")  
  

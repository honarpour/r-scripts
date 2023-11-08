library(tidyverse)

x <- seq(-4, 4, length = 100)

data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()


# plotting the normal distribution with dnorm
x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()
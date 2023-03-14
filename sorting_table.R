library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# order the states by population size
murders %>% arrange(population) %>% head()

# order the states by murder rate - the default is ascending order
murders %>% arrange(rate) %>% head()

# order the states by murder rate in descending order
murders %>% arrange(desc(rate)) %>% head()

# order the states by region and then by murder rate within region
murders %>% arrange(region, rate) %>% head()

# return the top 10 states by murder rate
murders %>% top_n(10, rate)

# return the top 10 states ranked by murder rate, sorted by murder rate
murders %>% arrange(desc(rate)) %>% top_n(10)
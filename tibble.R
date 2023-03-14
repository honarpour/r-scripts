# view the dataset
murders %>% group_by(region)

# see the class
murders %>% group_by(region) %>% class()

# compare the print output of a regular data frame to a tibble
gapminder
as_tibble(gapminder)

# compare subsetting a regular data frame and a tibble
class(murders[,1])
class(as_tibble(murders)[,1])

# access a column vector not as a tibble using $
class(as_tibble(murders)$state)

# compare what happens when accessing a column that doesn't exist in a regular data frame to in a tibble
murders$State
as_tibble(murders)$State

# create a tibble
tibble(id = c(1, 2, 3), func = c(mean, median, sd))
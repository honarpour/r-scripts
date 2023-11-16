# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# include the lm inside a summarize and it will work
dat %>%  
  group_by(HR) %>%
  summarize(slope = lm(R ~ BB)$coef[2])

# tidy function from broom returns estimates in and information in a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)

# add confidence intervals
tidy(fit, conf.int = TRUE)

# combine with group_by and summarize to get the table we want
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE))

# it's a data frame so we can filter and select the rows and columns we want
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

# visualize the table with ggplot
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()


library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)   # report 3 significant digits

stars %>%
  summarise(mean = mean(magnitude), sd = sd(magnitude))

stars %>%
  ggplot(aes(magnitude)) +
  geom_density()

stars %>%
  ggplot(aes(temp)) +
  geom_dotplot()

stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_point()

stars %>%
  ggplot(aes(temp, magnitude, label = star)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse() +
  geom_text_repel()

stars %>%
  ggplot(aes(temp, magnitude, label = type, color = type)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse() +
  geom_text_repel()
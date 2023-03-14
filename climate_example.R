library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  min()

temp_carbon %>%
  filter(!is.na(carbon_emissions) & year %in% c(1751, 2014)) %>%
  select(year, carbon_emissions) %>%
  ggplot(aes(year, carbon_emissions, label = carbon_emissions)) +
  geom_point() +
  geom_text_repel()

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  min()

temp_carbon %>%
  filter(!is.na(temp_anomaly) & year %in% c(1880, 2018)) %>%
  select(year, temp_anomaly) %>%
  ggplot(aes(year, temp_anomaly, label = temp_anomaly)) +
  geom_point() +
  geom_text_repel()

temp_carbon %>%
  filter(!is.na(temp_anomaly) & year >= 1880 & year <= 2018) %>%
  ggplot(aes(year, temp_anomaly, label = temp_anomaly)) +
  geom_line() +
  geom_line(aes(year, ocean_anomaly), col = "blue") +
  geom_line(aes(year, land_anomaly), col = "brown") +
  geom_hline(aes(yintercept = 0), col = "green") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  ylab("Temperature anomaly (degrees C)") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "green")

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas ~ ., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

temp_carbon %>%
  filter(!is.na(carbon_emissions) & year >= 1880 & year <= 2018) %>%
  ggplot(aes(year, carbon_emissions, label = carbon_emissions)) +
  geom_line() +
  geom_vline(xintercept = 1850)

historic_co2 %>%
  filter(!is.na(co2) & year >= -3000 & year <= 2018) %>%
  ggplot(aes(year, co2, label = co2, color = source)) +
  geom_line()

library(tidyverse)
devtools::install_github("ianhandel/epidemr")
library(epidemr)

head(mpg)

epi_binom(mpg, model, var_positive = "a4")

epi_binom(mpg, model, var_positive = "a4", conf_level = 0.99)

?binom::binom.confint

epi_binom(mpg, model, var_positive = "a4", methods = c("wilson", "exact"))


# a better example

#  some test data
dat <- matrix(runif(1000, 0, 100), ncol = 10) %>%
  as_tibble() %>%
  set_names(paste0("test_", LETTERS[1:10])) %>%
  mutate(division = sample(c("york", "kent", "peebles"), 100, replace = TRUE)) %>%
  select(division, everything())

dat

#  some cutoffs
cutoffs <- tibble(test = paste0("test_", LETTERS[1:10]),
                  cutoff = runif(10, 40, 60))

cutoffs


# estimate a  methods for each test and diviison combo
result <- dat %>%
  gather("elisa_test", "OD", test_A:test_J) %>%
  group_by(division) %>%
  inner_join(cutoffs, by = c(elisa_test = "test")) %>%
  mutate(dichot_result = OD >= cutoff) %>%
  group_by(division, elisa_test) %>%
  nest() %>%
  mutate(result = map(data, ~epi_binom(.x, dichot_result, conf_level = 0.99, method = "exact"))) %>%
  unnest(result) %>%
  rename(proportion = mean)

result

# plot estimates from the 10 tests, sort tests by mean prevelence
ggplot(result, aes(forcats::fct_reorder(elisa_test, proportion, mean),
                   proportion,
                   ymin = lower_0.99,
                   ymax = upper_0.99,
                   colour = division)) +
  geom_pointrange(position = position_dodge(w = 0.6), size = 0.2) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Prevalence estimates from 10 tests",
       x = "Test",
       y = "Prevelence",
       colour = "Division")




library(bayestestR)
library(ggplot2)
library(here)
library(scales)
library(tidyverse)

sar_and_salary <- read_csv(here("..", "model", "data", "mean_sar_and_salary.csv")) |>
  mutate(log_annual_salary = log(annual_salary),
         predicted_log_annual_salary = 13.5 + 0.734 * SAR,
         predicted_annual_salary = exp(predicted_log_annual_salary),
         diff = log_annual_salary - predicted_log_annual_salary,
         diff_pct = exp(diff) - 1) |>
  arrange(diff)

betas <- read_csv(here("..", "model", "data", "beta_estimates.csv")) |>
  pivot_wider(names_from = "beta", values_from = "estimate") |>
  rename(intercept = `0`, slope = `1`)

sars <- read_csv(here("..", "model", "data", "sar_estimates.csv")) |>
  left_join(sar_and_salary |>
              select(name_player, SAR_mean = SAR, log_annual_salary),
            by = join_by(name_player))

sars |>
  left_join(betas, by = join_by(chain, draw)) |>
  mutate(predicted_log_annual_salary = intercept + slope * SAR,
         diff = log_annual_salary - predicted_log_annual_salary,
         diff_pct = exp(diff) - 1) |>
  group_by(name_player) |>
  summarise(SAR = median(SAR),
            diff_pct_median = median(diff_pct),
            diff_pct_hdi_low = hdi(diff_pct, 0.94)$CI_low,
            diff_pct_hdi_high = hdi(diff_pct, 0.94)$CI_high) |>
  arrange(diff_pct_median) |>
  ggplot(aes(y = reorder(name_player, diff_pct_median),
             x = diff_pct_median,
             xmin = diff_pct_hdi_low,
             xmax = diff_pct_hdi_high,
             color = SAR)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_colour_gradient2(low = "red", mid = "black", high = "green") +
  labs(x = "Real salary difference compared to estimated salary", y = "Player name") +
  theme_minimal()
library(pacman)

p_load(
  "tidyverse",
  "drhutools"
)

# SWIID summary csv (rda fails to load with the current R-devel build)
df_swiid <- read_csv(
  "N:/Data/macro/02_socioeconomic/economy/inequality/swiid9_92/swiid9_92_summary.csv",
  show_col_types = FALSE
)

df_fourCountries <- df_swiid |>
  filter(country %in% c("United States", "Sweden", "India", "Russia")) |>
  mutate(
    country = fct_relevel(country, "United States", "Sweden", "India", "Russia"),
    ci_lower = gini_disp - 1.96 * gini_disp_se,
    ci_upper = gini_disp + 1.96 * gini_disp_se
  )

theme_set(
  theme_minimal(base_size = 18)
)

theme_update(
  plot.title = element_text(size = 18),
  axis.title = element_text(size = 22),
  axis.text = element_text(size = 18)
)

plot_giniFourCountries <- ggplot(df_fourCountries, aes(x = year, y = gini_disp, color = country, fill = country)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  scale_color_gb(palette = "digitMixed") +
  scale_fill_gb(palette = "digitMixed") +
  labs(
    x = NULL,
    y = "Gini Index (Disposable Income)",
    color = NULL,
    fill = NULL
  )

ggsave(
  "S:/figure/inequality_gini_fourCountries.png",
  plot_giniFourCountries,
  width = 8, height = 5, dpi = 300
)

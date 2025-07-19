#| include: false
#| label: setup

library(pacman)

p_load(
  "icons",
  "knitr",
  "drhutools",
  "tidyverse",
  "cranlogs",
  "purrr"
)

# Functions preload
set.seed(313)

theme_set(
  theme_minimal(base_size = 18)
)

theme_update(
  plot.title = element_text(size = 18),
  axis.title = element_text(size = 30),
  axis.text = element_text(size = 20),
  strip.text = element_text(size = 20)
)




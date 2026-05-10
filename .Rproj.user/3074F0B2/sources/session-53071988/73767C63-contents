library(tidyverse)
library(janitor)
library(sf)
library(geodata)
library(ggplot2)

pres <- read_csv("pres.csv", show_col_types = FALSE) %>%
  clean_names()

province_winners <- pres %>%
  pivot_longer(
    cols = starts_with("pres_"),
    names_to = "candidate",
    values_to = "votes"
  ) %>%
  mutate(
    candidate = str_remove(candidate, "pres_\\d+_"),
    candidate = str_replace_all(candidate, "_", " "),
    candidate = str_to_title(candidate),
  
    province_clean = str_to_upper(province),
    province_clean = str_squish(province_clean)
  ) %>%
  group_by(region, province, province_clean, candidate) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  slice_max(votes, by = province_clean, n = 1, with_ties = FALSE)

ph_map <- gadm(
  country = "PHL",
  level = 1,
  path = tempdir()
) %>%
  st_as_sf() %>%
  mutate(
    province_clean = str_to_upper(NAME_1),
    province_clean = str_squish(province_clean)
  )

province_map <- ph_map %>%
  left_join(province_winners, by = "province_clean")

province_map %>%
  st_drop_geometry() %>%
  count(candidate)

province_map %>%
  st_drop_geometry() %>%
  filter(is.na(candidate)) %>%
  select(NAME_1, province_clean)

pres %>%
  distinct(region, province) %>%
  arrange(province) %>%
  View()

    )
  )

pres %>%
  filter(province == "LANAO DEL SUR") %>%
  pivot_longer(
    starts_with("pres_"),
    names_to = "candidate",
    values_to = "votes"
  ) %>%
  mutate(
    candidate = str_remove(candidate, "pres_\\d+_"),
    candidate = str_replace_all(candidate, "_", " "),
    candidate = str_to_title(candidate)
  ) %>%
  group_by(candidate) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(votes))

province_map <- province_map %>%
  mutate(
    candidate = case_when(
      NAME_1 == "NATIONAL CAPITAL REGION" & is.na(candidate) ~ "Marcos",
      NAME_1 == "COMPOSTELA" & is.na(candidate) ~ "Marcos",
      NAME_1 == "COTABATO" & is.na(candidate) ~ "Marcos",
      TRUE ~ candidate
    )
  )

anti_join(
  province_winners,
  ph_map,
  by = "province_clean"
) %>%
  select(province, province_clean) %>%
  distinct()

candidate_colors <- c(
  "Abella" = "yellow",
  "De Guzman" = "green",
  "Domagoso" = "darkblue",
  "Gonzales" = "orange",
  "Lacson" = "gray40",
  "Mangondato" = "purple",
  "Marcos" = "red",
  "Montemayor" = "brown",
  "Pacquiao" = "blue",
  "Robredo" = "pink"
)

province_map <- ph_map %>%
  left_join(province_winners, by = "province_clean")

ggplot(province_map) +
  geom_sf(aes(fill = candidate), color = "white", size = 0.05) +
  scale_fill_manual(
    values = candidate_colors,
    na.translate = FALSE
  ) +
  labs(
    title = "2022 Philippine Presidential Winners by Province",
    fill = "Winner"
  ) +
  theme_minimal()

combined_report <- province_winners %>%
  group_by(region, candidate) %>%
  summarise(
    provinces_won = n(),
    total_votes = sum(votes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(region, desc(provinces_won), desc(total_votes))

combined_report

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(rstatix)
library(tidyr)
library(gt)

# List all csv files

files <- list.files(
  path = "Raw Data",
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Read one csv and standardize key columns

read_one <- function(f) {
  df <- read_csv(f, show_col_types = FALSE)
  if ("Detail" %in% names(df)) df$Detail <- as.character(df$Detail)
  
  term_label <- basename(dirname(f))
  Subject_std <- str_match(basename(f), "^[^-]+-[^-]+-([A-Z]{3,5})\\.csv$")[,2]
  Year <- as.integer(str_sub(term_label, 1, 4))
  Session <- str_sub(term_label, 5, 5)
  
  dist_cols <- grep("^<|^\\d+-\\d+", names(df), value = TRUE)
  bin_sum <- if (length(dist_cols) > 0) rowSums(df[, dist_cols, drop = FALSE], na.rm = TRUE) else rep(NA_real_, nrow(df))
  
  # Check weight
  weight_raw <- rep(NA_real_, nrow(df))
  if ("Reported" %in% names(df)) {
    weight_raw <- suppressWarnings(as.numeric(df$Reported))
  } else if ("Enrolled" %in% names(df)) {
    weight_raw <- suppressWarnings(as.numeric(df$Enrolled))
  }
  weight <- ifelse(is.na(weight_raw), bin_sum, weight_raw)
  
  df %>%
    transmute(
      term_label,
      Year,
      Session,
      Subject_std,
      Course = as.character(Course),
      Section = as.character(Section),
      Avg = suppressWarnings(as.numeric(Avg)),
      weight_raw,
      weight,
      source_file = basename(f)
    )
}

df_all <- bind_rows(lapply(files, read_one))

# Clean up data and calculate weighted average

# Remove 100 level courses

df_all_2xx <- df_all %>% filter(!str_starts(Course, "1"))

df_course_term <- df_all_2xx %>%
  mutate(is_overall = (Section == "OVERALL")) %>%
  group_by(Year, Session, Subject_std, Course) %>%
  summarise(
    total_reported = if_else(any(is_overall), max(weight[is_overall], na.rm = TRUE), sum(weight, na.rm = TRUE)),
    weighted_avg   = if_else(any(is_overall), max(Avg[is_overall], na.rm = TRUE),
                             if_else(total_reported > 0, sum(Avg * weight, na.rm = TRUE) / total_reported, NA_real_)),
    method = if_else(any(is_overall), "overall", "weighted_from_sections"),
    .groups = "drop"
  ) %>%
  mutate(
    level = case_when(
      str_starts(Course, "2") ~ "200-level",
      str_starts(Course, "3") ~ "300-level",
      str_starts(Course, "4") ~ "400-level",
      TRUE ~ NA_character_
    ),
    period = case_when(
      Year <= 2019 ~ "Pre",
      Year >= 2022 ~ "Post",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(level), !is.na(period)) %>%
  arrange(Year, Session, Subject_std, Course)

# Helper

summarise_term <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(
      denom = sum(total_reported),
      numer = sum(weighted_avg * total_reported),
      avg_grade = numer / denom,
      n_course = n(),
      .groups = "drop"
    ) %>%
    select(-numer, -denom) %>%
    mutate(term_label = paste0(Year, Session))
}

df_term_subject_overall_avg <- summarise_term(df_course_term, Year, Session, Subject_std) %>%
  mutate(period = if_else(Year <= 2019, "Pre", "Post"))

df_term_subject_level_avg <- summarise_term(df_course_term, Year, Session, Subject_std, level) %>%
  mutate(period = if_else(Year <= 2019, "Pre", "Post"))

term_levels <- df_term_subject_overall_avg %>%
  distinct(Year, Session) %>%
  arrange(Year, Session) %>%
  mutate(term_label = paste0(Year, Session)) %>%
  pull(term_label)

set_term_factor <- function(df) {
  df %>% mutate(term_label = factor(term_label, levels = term_levels))
}

df_term_subject_overall_avg <- set_term_factor(df_term_subject_overall_avg)
df_term_subject_level_avg   <- set_term_factor(df_term_subject_level_avg)

# Mann-Whitney U-test

# All courses

wilcox_all <- wilcox.test(weighted_avg ~ period, data = df_course_term, exact = FALSE)

df_desc_all <- df_course_term %>%
  group_by(period) %>%
  summarise(
    median_grade = median(weighted_avg, na.rm = TRUE),
    mean_grade   = mean(weighted_avg, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

# Level

wilcox_by_level <- df_course_term %>%
  group_by(level) %>%
  wilcox_test(weighted_avg ~ period) %>%
  add_significance()

df_desc_by_level <- df_course_term %>%
  group_by(level, period) %>%
  summarise(
    median_grade = median(weighted_avg, na.rm = TRUE),
    mean_grade   = mean(weighted_avg, na.rm = TRUE),
    n_obs        = n(),
    .groups = "drop"
  ) %>%
  arrange(level, period)

# Subjects

min_n_each_period <- 30

# Check sample size

subjects_ok <- df_course_term %>%
  count(Subject_std, period) %>%
  pivot_wider(names_from = period, values_from = n, values_fill = 0) %>%
  filter(Pre >= min_n_each_period, Post >= min_n_each_period) %>%
  pull(Subject_std)

wilcox_by_subject <- df_course_term %>%
  filter(Subject_std %in% subjects_ok) %>%
  group_by(Subject_std) %>%
  wilcox_test(weighted_avg ~ period) %>%
  add_significance() %>%
  arrange(p)

# Subjects + Level

# Check sample size

cells_ok <- df_course_term %>%
  count(Subject_std, level, period) %>%
  pivot_wider(names_from = period, values_from = n, values_fill = 0) %>%
  filter(Pre >= min_n_each_period, Post >= min_n_each_period) %>%
  select(Subject_std, level)

wilcox_by_subject_level <- df_course_term %>%
  semi_join(cells_ok, by = c("Subject_std", "level")) %>%
  group_by(Subject_std, level) %>%
  wilcox_test(weighted_avg ~ period) %>%
  add_significance() %>%
  arrange(p)

# Plots

theme_si <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Line chart

# All courses

df_term_all_avg <- summarise_term(df_course_term, Year, Session) %>%
  mutate(period = if_else(Year <= 2019, "Pre", "Post")) %>%
  set_term_factor()

ggplot(df_term_all_avg, aes(term_label, avg_grade, group = period)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  coord_cartesian(ylim = c(70, 85)) +
  labs(title = NULL, x = "Academic Term", y = "Average Grade (%)") +
  theme_si

# Level

df_term_level_avg <- summarise_term(df_course_term, Year, Session, level) %>%
  mutate(period = if_else(Year <= 2019, "Pre", "Post")) %>%
  set_term_factor()

ggplot(df_term_level_avg, aes(term_label, avg_grade, color = level, group = interaction(level, period))) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  coord_cartesian(ylim = c(70, 85)) +
  labs(title = NULL, x = "Academic Term", y = "Average Grade (%)", color = "Course level") +
  theme_si

# Selected subjects

# subjects_keep <- c("MATH", "CHEM")
# df_plot_overall <- df_term_subject_overall_avg %>% filter(Subject_std %in% subjects_keep)
# df_density_plot <- df_course_term %>% filter(Subject_std %in% subjects_keep)

# ggplot(df_plot_overall, aes(term_label, avg_grade, color = Subject_std, group = interaction(Subject_std, period))) +
#   geom_line(size = 1.2) +
#   geom_point(size = 2) +
#   coord_cartesian(ylim = c(50, 95)) +
#   labs(title = NULL, x = "Academic Term", y = "Average Grade (%)") +
#   theme_si

# Selected subject + level

# subject_focus <- "CHEM"
# ggplot(filter(df_term_subject_level_avg, Subject_std == subject_focus),
#        aes(term_label, avg_grade, color = level, group = interaction(level, period))) +
#   geom_line(size = 1.1) +
#   geom_point(size = 2) +
#   coord_cartesian(ylim = c(50, 95)) +
#   labs(title = NULL, x = "Academic Term", y = "Average Grade (%)") +
#   theme_si

# Distribution

# All courses

ggplot(df_course_term, aes(weighted_avg, fill = period)) +
  geom_density(alpha = 0.4) +
  labs(title = NULL, x = "Course-level weighted average (%)", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Level

ggplot(df_course_term, aes(weighted_avg, fill = period)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ level, ncol = 1) +
  labs(title = NULL, x = "Course-level weighted average (%)", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Selected subjects

# ggplot(df_density_plot, aes(weighted_avg, fill = period)) +
#   geom_density(alpha = 0.4) +
#   facet_wrap(~ Subject_std) +
#   labs(title = NULL, x = "Course-level weighted average (%)", y = "Density") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Boxplot

# Selected subjects

# ggplot(df_density_plot, aes(period, weighted_avg)) +
#   geom_boxplot() +
#   facet_wrap(~ Subject_std) +
#   labs(title = NULL, x = "Period", y = "Course-level weighted average (%)") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
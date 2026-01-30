library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(rstatix)

files <- list.files(
  path = "Raw Data",
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
length(files)
head(files, 5)

# Read the file

read_one <- function(f) {
  
  df <- read_csv(f, show_col_types = FALSE)
  
  subject <- str_extract(f, "CHEM|MATH")
  period  <- str_extract(f, "Pre-COVID|Post-COVID|2017-2019|2022-2024")
  
  df %>%
    mutate(
      Section = as.character(Section),
      Course  = as.character(Course),
      subject = subject,
      period  = period,
      source_file = basename(f)
    )
}

df_all <- bind_rows(lapply(files, read_one))

# Remove 100-level course

df_all_2xx <- df_all %>%
  filter(!str_starts(Course, "1"))

# Count professors

df_all_2xx %>%
  group_by(subject) %>%
  summarise(n_professor = n_distinct(Professor))

# Sum reported

dist_cols <- grep("^<|^\\d+-\\d+", names(df_all_2xx), value = TRUE)

df_all_2xx <- df_all_2xx %>%
  mutate(
    Reported = if_else(
      is.na(Reported),
      rowSums(across(all_of(dist_cols)), na.rm = TRUE),
      Reported
    )
  )

# Calculate weighted avg

df_tmp <- df_all_2xx %>%
  mutate(
    Reported = as.numeric(Reported),
    Avg      = as.numeric(Avg),
    Section  = as.character(Section),
    is_overall = (Section == "OVERALL")
  )

courses_with_overall <- df_tmp %>%
  group_by(Year, Session, Subject, Course) %>%
  summarise(has_overall = any(is_overall), .groups = "drop")

overall_rows <- df_tmp %>%
  filter(is_overall) %>%
  group_by(Year, Session, Subject, Course) %>%
  summarise(
    total_reported = max(Reported, na.rm = TRUE),
    weighted_avg   = max(Avg, na.rm = TRUE),
    method = "overall",
    .groups = "drop"
  )

weighted_rows <- df_tmp %>%
  group_by(Year, Session, Subject, Course) %>%
  filter(!any(is_overall)) %>%
  summarise(
    total_reported = sum(Reported, na.rm = TRUE),
    weighted_avg = if_else(
      total_reported > 0,
      sum(Avg * Reported, na.rm = TRUE) / total_reported,
      NA_real_
    ),
    method = "weighted_from_sections",
    .groups = "drop"
  )

df_all_2xx_weightedAvg <- bind_rows(overall_rows, weighted_rows) %>%
  arrange(Year, Session, Subject, Course)

# 
df_course_term <- df_all_2xx_weightedAvg %>%
  mutate(
    Course = as.character(Course),
    level = case_when(
      str_starts(Course, "2") ~ "200-level",
      str_starts(Course, "3") ~ "300-level",
      str_starts(Course, "4") ~ "400-level",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(level))

# Weighted avg by course/term/level

df_term_subject_level_avg <- df_course_term %>%
  group_by(Year, Session, Subject, level) %>%
  summarise(
    denom = sum(total_reported),
    numer = sum(weighted_avg * total_reported),
    avg_grade = numer / denom,
    n_course = n(),
    .groups = "drop"
  ) %>%
  select(-numer, -denom)

df_term_subject_overall_avg <- df_course_term %>%
  group_by(Year, Session, Subject) %>%
  summarise(
    denom = sum(total_reported),
    numer = sum(weighted_avg * total_reported),
    avg_grade = numer / denom,
    n_course = n(),
    .groups = "drop"
  ) %>%
  select(-numer, -denom)

# Line chart

df_term_subject_overall_avg <- df_term_subject_overall_avg %>%
  mutate(
    term_label = paste0(Year, Session)
  )

df_term_subject_level_avg <- df_term_subject_level_avg %>%
  mutate(
    term_label = paste0(Year, Session)
  )

term_levels <- df_term_subject_overall_avg %>%
  distinct(Year, Session) %>%
  arrange(Year, Session) %>%
  mutate(term_label = paste0(Year, Session)) %>%
  pull(term_label)

df_term_subject_overall_avg$term_label <- factor(
  df_term_subject_overall_avg$term_label,
  levels = term_levels
)

df_term_subject_level_avg$term_label <- factor(
  df_term_subject_level_avg$term_label,
  levels = term_levels
)

df_term_subject_overall_avg <- df_term_subject_overall_avg %>%
  mutate(period = if_else(Year <= 2019, "Pre", "Post"))

df_term_subject_level_avg <- df_term_subject_level_avg %>%
  mutate(period = if_else(Year <= 2019, "Pre", "Post"))

ggplot(df_term_subject_overall_avg,
       aes(term_label, avg_grade,
           color = Subject,
           group = interaction(Subject, period))) +
  
  geom_line(size=1.2) +
  geom_point(size=2) +
  coord_cartesian(ylim=c(50,95)) +
  labs(
    title = "CHEM vs MATH Average Grades by Term",
    x = "Academic Term",
    y = "Average Grade (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    axis.text.x = element_text(angle=45, hjust=1)
  )

ggplot(
  filter(df_term_subject_level_avg, Subject=="CHEM"),
  aes(term_label, avg_grade,
      color=level,
      group=interaction(level, period))
) +
  geom_line(size=1.1) +
  geom_point(size=2) +
  coord_cartesian(ylim=c(50,95)) +
  labs(
    title = "CHEM Grades by Course Level",
    x = "Academic Term",
    y = "Average Grade (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    axis.text.x = element_text(angle=45, hjust=1)
  )

ggplot(
  filter(df_term_subject_level_avg, Subject=="MATH"),
  aes(term_label, avg_grade,
      color=level,
      group=interaction(level, period))
) +
  geom_line(size=1.1) +
  geom_point(size=2) +
  coord_cartesian(ylim=c(50,95)) +
  labs(
    title = "MATH Grades by Course Level",
    x = "Academic Term",
    y = "Average Grade (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    axis.text.x = element_text(angle=45, hjust=1)
  )

# U test

df_course_term <- df_course_term %>%
  mutate(period = if_else(Year <= 2019, "Pre", "Post"))

df_course_term %>%
  filter(Subject=="MATH") %>%
  count(period)

df_course_term %>%
  filter(Subject=="CHEM") %>%
  count(period)

df_course_term %>%
  filter(Subject=="MATH") %>%
  wilcox_test(weighted_avg ~ period) %>%
  add_significance()

df_course_term %>%
  filter(Subject=="CHEM") %>%
  wilcox_test(weighted_avg ~ period) %>%
  add_significance()

ggplot(filter(df_course_term, Subject=="MATH"),
       aes(period, weighted_avg)) +
  geom_boxplot() +
  labs(title="MATH Pre vs Post (Course-level averages)",
       x="Period", y="Average Grade")

ggplot(filter(df_course_term, Subject=="CHEM"),
       aes(period, weighted_avg)) +
  geom_boxplot() +
  labs(title="CHEM Pre vs Post (Course-level averages)",
       x="Period", y="Average Grade")

ggplot(df_course_term,
       aes(weighted_avg, fill=period)) +
  geom_density(alpha=0.4) +
  facet_wrap(~Subject)
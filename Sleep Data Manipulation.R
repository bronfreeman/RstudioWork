sleep_data <- read.csv("C:/Users/Bronwyn/Documents/RstudioWork/sleep.csv")
head(sleep_data)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

calculate_mode <- function(x) {
  tbl <- table(x)
  mode_val <- as.numeric(names(tbl[tbl == max(tbl)]))
  return(mode_val)
}

occupation_stats <- sleep_data %>%
  group_by(Occupation) %>%
  summarize(
    mean_quality_of_sleep = mean(Quality.of.Sleep, na.rm = TRUE),
    median_quality_of_sleep = median(Quality.of.Sleep, na.rm = TRUE),
    mode_quality_of_sleep = calculate_mode(Quality.of.Sleep)
  ) %>%
  # Correct for duplicate occupations
  filter(!duplicated(Occupation))

occupation_stats <- occupation_stats %>%
  mutate(Occupation = case_when(
    Occupation %in% c("Nurse", "Nurse ") ~ "Nurse",
    Occupation %in% c("Scientist", "Scientist ") ~ "Scientist",
    TRUE ~ Occupation
  ))

print(occupation_stats)

ggplot(occupation_stats, aes(x = Occupation, y = mean_quality_of_sleep)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Mean Quality of Sleep by Occupation",
       x = "Occupation",
       y = "Mean Quality of Sleep") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

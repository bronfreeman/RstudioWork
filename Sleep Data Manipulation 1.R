
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

if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
library(stringr)


sleep_disorder_percentage <- sleep_data %>%
  group_by(Occupation, Sleep.Disorder) %>%
  summarise(count = n()) %>%
  group_by(Occupation) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()


sleep_disorder_percentage$Occupation <- str_replace_all(sleep_disorder_percentage$Occupation, "Sales Representative", "Sales Rep")


ggplot(sleep_disorder_percentage, aes(x = Occupation, y = percentage, fill = Sleep.Disorder)) +
  geom_bar(stat = "identity", width = 0.8) + 
  labs(title = "Percentage of Sleep Disorders by Occupation",
       x = "Occupation",
       y = "Percentage",
       fill = "Sleep Disorder") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

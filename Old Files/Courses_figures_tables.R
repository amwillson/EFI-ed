## Figures related to available courses for teaching sub-topics of ecological forecasting

## Authors: AM Willson & H Gallo

rm(list = ls())

library(tidyverse)
library(forcats)
library(gt)
library(readxl)

load('Data/cleaned_data.RData')

# Table of overall frequency of courses
summ = data %>%
  group_by(Sub.topic) %>%
  count() %>%
  arrange(desc(n))

colnames(summ) = c('Course Topic', 'n')

summ = as.data.frame(summ)

summ %>%
  gt() %>%
  fmt_markdown(columns = c('Course Topic', 'n')) %>%
  tab_header(
    title = md('Total Number of Courses in Each Topic')) %>%
  gtsave(filename = 'Plots/total_by_subtopic.png')

# Barplot of overall frequency of courses
data %>%
  ggplot() +
  geom_bar(stat = 'count', aes(x = fct_rev(fct_infreq(Sub.topic)))) +
  coord_flip() +
  ylab('Number of Courses') + xlab('') +
  theme_linedraw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

ggsave(last_plot(), filename = 'Plots/total_by_subtopic_bar.png')

# Table of average number of courses at each institution in each sub-topic
mean = data %>%
  select(College, Sub.topic) %>%
  group_by(College, Sub.topic) %>%
  count() %>%
  complete(Sub.topic, nesting(College), fill = list(n = 0)) %>%
  distinct() %>%
  group_by(Sub.topic) %>%
  summarize(mean(n))

colnames(mean) = c('Course Topic', 'Mean')

mean = mean %>%
  arrange(desc(Mean))

mean = as.data.frame(mean)

mean %>%
  gt() %>%
  fmt_markdown(columns = c('Course Topic', 'Mean')) %>%
  tab_header(
    title = md('Average Number of Courses in Each Topic')) %>%
  fmt_number(columns = 'Mean', decimals = 2) %>%
  gtsave(filename = 'Plots/mean_by_subtopic.png')

# Barplot of average number of courses at each institution in each sub-topic
mean %>%
  ggplot() +
  geom_bar(stat = 'identity', aes(x = reorder(`Course Topic`, Mean), y = Mean)) +
  coord_flip() +
  theme_linedraw() +
  ylab('Average Number of Courses on Each Topic across Universities') + xlab('') +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

ggsave(last_plot(), filename = 'Plots/mean_by_subtopic_bar.png')

# Table of number of courses by sub-topic and insitution type
topic_inst_count = data %>%
  select(Type, Sub.topic) %>%
  group_by(Type, Sub.topic) %>%
  count() %>%
  group_by(Type) %>%
  mutate(Type = factor(Type, levels = c('4-Year Public', '4-Year Private',
                                        'Community College', '2-Year Technical',
                                        'For Profit'))) %>%
  arrange(desc(n), .by_group = T)

topic_inst_count = as.data.frame(topic_inst_count)

colnames(topic_inst_count) = c('Institution Type', 'Course Topic', 'n')

topic_inst_count %>%
  gt() %>%
  fmt_markdown(columns = c('Institution Type', 'Course Topic', 'n')) %>%
  tab_header(
    title = md('Total Number of Courses in Each Topic by Institution Type')) %>%
  gtsave(filename = 'Plots/total_by_subtopic_type.png')

# Barplot of total number of courses by sub-topic and institution type
topic_inst_count %>%
  ggplot() +
  geom_bar(stat = 'identity', aes(x = reorder(`Course Topic`, n, sum), y = n, fill = `Institution Type`)) +
  coord_flip() +
  ylab('Number of Courses') + xlab('') +
  theme_linedraw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

ggsave(last_plot(), filename = 'Plots/total_by_subtopic_type_bar.png', height = 7.33, width = 10, units = 'in')

# Table of number of courses by sub-topic and highest degree offered
topic_degree_count = data %>%
  select(Highest.degree.offered, Sub.topic) %>%
  group_by(Highest.degree.offered, Sub.topic) %>%
  count() %>%
  group_by(Highest.degree.offered) %>%
  mutate(Highest.degree.offered = factor(Highest.degree.offered, levels = c('AA/AS', 'BA/BS',
                                        'MA/MS', 'PhD',
                                        'Other Terminal Degree'))) %>%
  arrange(desc(n), .by_group = T)

topic_degree_count = as.data.frame(topic_degree_count)

colnames(topic_degree_count) = c('Highest Degree Offered', 'Course Topic', 'n')

topic_degree_count %>%
  gt() %>%
  fmt_markdown(columns = c('Highest Degree Offered', 'Course Topic', 'n')) %>%
  tab_header(
    title = md('Total Number of Courses in Each Topic by Highest Degree Offered')) %>%
  gtsave(filename = 'Plots/total_by_subtopic_degree.png')

# Barplot of total number of courses by sub-topic and institution type
topic_degree_count %>%
  ggplot() +
  geom_bar(stat = 'identity', aes(x = reorder(`Course Topic`, n, sum), y = n, fill = `Highest Degree Offered`)) +
  coord_flip() +
  ylab('Number of Courses') + xlab('') +
  theme_linedraw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

ggsave(last_plot(), filename = 'Plots/total_by_subtopic_type_bar.png', height = 7.33, width = 10, units = 'in')

# Table of number of courses by sub-topic and Carnegie Classification
topic_carnegie_count = data %>%
  select(Carnegie.classification, Sub.topic) %>%
  group_by(Carnegie.classification, Sub.topic) %>%
  count() %>%
  group_by(Carnegie.classification) %>%
  mutate(Carnegie.classification = factor(Carnegie.classification, levels = c("Associate's Colleges: High Career & Technical-High Nontraditional",
                                                                              "Associate's Colleges: Mixed Transfer/Career & Technical-High Nontraditional",
                                                                              "Associate's Colleges: Mixed Transfer/Career & Technical-High Traditional",
                                                                              "Associate's Colleges: High Career & Technical-High Traditional",
                                                                              "Associate's Colleges: High Transfer-Mixed Traditional/Nontraditional",
                                                                              "Associate's Colleges: High Transfer-High Traditional",
                                                                              "Associate's Colleges: Mixed Transfer/Career & Technical-Mixed Traditional/Nontraditional",
                                                                              "Associate's Colleges: High Transfer-High Nontraditional",
                                                                              "Associate's - Public Urban-serving Single Campus",
                                                                              "Baccalaureate/Associate's Colleges: Associate's Dominant",
                                                                              "Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's",
                                                                              "Baccalaureate Colleges: Diverse Fields",
                                                                              "Baccalaureate Colleges: Arts & Sciences Focus",
                                                                              "M3", "M1",
                                                                              "R2", "R1",
                                                                              "D/PU", "Tribal Colleges"))) %>%
  arrange(desc(n), .by_group = T)

topic_carnegie_count = as.data.frame(topic_carnegie_count)

colnames(topic_carnegie_count) = c('Carnegie Classification', 'Course Topic', 'n')

topic_carnegie_count %>%
  gt() %>%
  fmt_markdown(columns = c('Carnegie Classification', 'Course Topic', 'n')) %>%
  tab_header(
    title = md('Total Number of Courses in Each Topic by Carnegie Classification')) %>%
  gtsave(filename = 'Plots/total_by_subtopic_carnegie.png')

# Barplot of total number of courses by sub-topic and institution type
topic_carnegie_count %>%
  ggplot() +
  geom_bar(stat = 'identity', aes(x = reorder(`Course Topic`, n, sum), y = n, fill = `Carnegie Classification`)) +
  coord_flip() +
  ylab('Number of Courses') + xlab('') +
  theme_linedraw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

ggsave(last_plot(), filename = 'Plots/total_by_subtopic_carnegie_bar.png', height = 7.33, width = 15, units = "in")

# Table of number of courses by sub-topic and Carnegie Classification
topic_carnegie_2_count = data %>%
  select(Carnegie.classification.2, Sub.topic) %>%
  group_by(Carnegie.classification.2, Sub.topic) %>%
  count() %>%
  group_by(Carnegie.classification.2) %>%
  mutate(Carnegie.classification.2 = factor(Carnegie.classification.2, levels = c("A", "A/B", "B",
                                                                                  "M3", "M1",
                                                                                  "R2", "R1",
                                                                                  "D/PU", "TC"))) %>%
  arrange(desc(n), .by_group = T)

topic_carnegie_2_count = as.data.frame(topic_carnegie_2_count)

colnames(topic_carnegie_2_count) = c('Carnegie Classification', 'Course Topic', 'n')

topic_carnegie_2_count %>%
  gt() %>%
  fmt_markdown(columns = c('Carnegie Classification', 'Course Topic', 'n')) %>%
  tab_header(
    title = md('Total Number of Courses in Each Topic by Carnegie Classification')) %>%
  gtsave(filename = 'Plots/total_by_subtopic_carnegie_2.png')

# Barplot of total number of courses by sub-topic and institution type
topic_carnegie_2_count %>%
  ggplot() +
  geom_bar(stat = 'identity', aes(x = reorder(`Course Topic`, n, sum), y = n, fill = `Carnegie Classification`)) +
  coord_flip() +
  ylab('Number of Courses') + xlab('') +
  theme_linedraw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

ggsave(last_plot(), filename = 'Plots/total_by_subtopic_carnegie_2_bar.png', height = 7.33, width = 10, units = 'in')
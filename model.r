
#install.packages(c("naniar", "tidyverse", "ComplexHeatmap"))
#install.packages(c("lme4", "lmerTest", "sjPlot"))
library(tidyverse)
library(dplyr)
library(ComplexHeatmap)
library(grid)
library(readr)
library(lme4)
library(lmerTest) # 提供 p 值與檢定
library(sjPlot)   # 視覺化與模型摘要表
df <- read_csv("/Users/hanklin/Documents/R/dsp2025/true_starting.csv")
colnames(df)
###most_talk_freq
df <- df %>%
  mutate(
    talk_min = pmin(talk_to_mother, talk_to_father, na.rm = TRUE)
  )
###sumhappy
df <- df %>%
  mutate(
    happy = life_quality+life_happiness
  )
colnames(df)
####二值化函數<= v為 1，否則為 0
binarize_var <- function(data, var_name, threshold, new_var_name) {
  data <- data %>%
    mutate(
      !!sym(new_var_name) := ifelse(.data[[var_name]] <= threshold, 1, 0)
    )
  return(data)
}

####幾乎、從未傾訴
df_new <- df %>%
  binarize_var("talk_min", 3, "talk_min_bin") %>%  #有傾訴為1 沒有為0
  binarize_var("internet_time", 2, "parent_manage_bin") #不管為1 有管為0


###model
# packages
library(lme4)
library(lmerTest) # p-values
library(sjPlot)   # for plots
#解讀1是男2是女 #有傾訴為1 沒有為0 #不管為1 有管為0
# 基本隨機截距模型####
m1 <- lmer(happy ~ factor(survey_year) + internet_use_combined + parent_manage_bin + talk_min_bin +
             gender + (1|respondent_id), data = df_new, REML = FALSE)
summary(m1)

# 加入交互項（檢驗年份是否改變上網影響）
m2 <- lmer(happy ~ factor(survey_year) * internet_use_combined + parent_manage_bin +talk_min_bin +
             gender + (1|respondent_id), data = df_new, REML = FALSE)
anova(m1, m2) # 比較模型
summary(m2)




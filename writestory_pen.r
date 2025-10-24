####
#install.packages(c("naniar", "tidyverse", "ComplexHeatmap"))
library(tidyverse)
library(dplyr)
library(ComplexHeatmap)
library(grid)
library(readr)

starting <- read_csv("/Users/hanklin/Documents/R/dsp2025/starting_data.csv")

colnames(starting)
####
cols_to_remove <- c("中低收入戶生活補助", "重大傷病、傷病醫療補助")
starting <- starting[, !(names(starting) %in% cols_to_remove)]

colnames(starting)



colnames(starting) <- c(
  "respondent_id", "survey_year", "survey_month", "gender",
  "birth_year_tw", "birth_month", "residence", "housing_type",
  "internet_use_highschool", "internet_use_university", "school_type",
  "education_rank_2018_2020", "course_difficulty", "school_rules_schedule",
  "sleepover_friends", "curfew_time", "dress_appearance", "daily_routine",
  "internet_time", "relationship_status", "money_usage", "job_choice",
  "father_health", "talk_to_father", "mother_health", "talk_to_mother",
  "life_happiness", "life_quality",
  "internet_addiction_1", "internet_addiction_2", "internet_addiction_3",
  "internet_addiction_4", "internet_addiction_5",
  "higher_edu_rank_2022", "university_rank_2022"
)

colnames(starting)
# 對 final 每個欄位列出所有可能值
all_unique_values <- lapply(starting, unique)

# 查看結果
all_unique_values

#varies_tune
#$survey_year 109 =>2020
starting$survey_year[starting$survey_year == 109] <- 2020
#in this section variables unique all need to be things that continue to 1
# 1 4 3 2 is fine
# 1 3 6 2 7 4need to turn 6 7 to NA
# #these are colnames which need this process[
# "survey_month",
#  "housing_type",
# "internet_use_highschool", "internet_use_university",
# "education_rank_2018_2020", "course_difficulty", "school_rules_schedule",
# "sleepover_friends", "curfew_time", "dress_appearance", "daily_routine",
# "internet_time", "relationship_status", "money_usage", "job_choice",
# "father_health", "talk_to_father", "mother_health", "talk_to_mother",
# "internet_addiction_1", "internet_addiction_2", "internet_addiction_3",
# "internet_addiction_4", "internet_addiction_5",
# "higher_edu_rank_2022", "university_rank_2022"
#  ]
# 指定需要處理的欄位
cols_to_check <- c(
  "survey_month",
  "housing_type",
  "internet_use_highschool",
  "education_rank_2018_2020", "course_difficulty", "school_rules_schedule",
  "sleepover_friends", "curfew_time", "dress_appearance", "daily_routine",
  "internet_time", "relationship_status", "money_usage", "job_choice",
  "father_health", "talk_to_father", "mother_health", "talk_to_mother",
  "internet_addiction_1", "internet_addiction_2", "internet_addiction_3",
  "internet_addiction_4", "internet_addiction_5",
  "higher_edu_rank_2022", "university_rank_2022"
)
make_continuous_from_one <- function(x) {
  # 轉成數字，不可轉的變 NA
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  
  # 只保留大於 0 的數字
  x_num[x_num <= 0 | is.na(x_num)] <- NA
  
  # 找出排序後唯一值
  x_unique <- sort(unique(na.omit(x_num)))
  
  # 從 1 開始的連續序列
  if(length(x_unique) == 0 || x_unique[1] != 1) return(rep(NA, length(x)))
  
  # 找連續數列的最大值
  max_valid <- 1
  for(v in x_unique[-1]) {
    if(v == max_valid + 1) {
      max_valid <- v
    } else {
      break
    }
  }
  
  # 只保留 1:max_valid，其餘 NA
  x_num[!x_num %in% 1:max_valid] <- NA
  return(x_num)
}


# 對指定欄位應用
set_starting <- starting  # 複製原資料
for(col in cols_to_check) {
  if(col %in% colnames(set_starting)) {
    set_starting[[col]] <- make_continuous_from_one(set_starting[[col]])
  }
}
#檢查
all_unique_values_set <- lapply(set_starting, unique)
all_unique_values_set
#需要指定變數$talk_to_father,$talk_to_mother,
#$internet_addiction_1,$internet_addiction_3,$internet_addiction_4
#改觀察[6]掉為na
# 需要處理的欄位
cols_replace6 <- c(
  "talk_to_father",
  "talk_to_mother",
  "internet_addiction_1",
  "internet_addiction_3",
  "internet_addiction_4"
)

# 以 set_starting 為例（如果你還沒生成就用 starting）
set_starting <- set_starting %>%
  dplyr::mutate(across(
    all_of(cols_replace6),
    ~ ifelse(as.numeric(as.character(.)) == 6, NA, .)
  ))
#針對set_starting $internet_use_university 僅保留1~10內的正整數
# 僅保留 1~10 的正整數，其餘設為 NA
set_starting$internet_use_university <- ifelse(
  as.numeric(as.character(set_starting$internet_use_university)) %in% 1:10,
  as.numeric(as.character(set_starting$internet_use_university)),
  NA
)

#check
all_unique_values_set <- lapply(set_starting, unique)
all_unique_values_set
######遺失值矩陣

plot_missing_heatmap <- function(df,
                                 cluster_rows = TRUE,
                                 cluster_columns = TRUE,
                                 show_row_names = FALSE,
                                 show_column_names = TRUE,
                                 show_legend = FALSE,
                                 row_dend_width = unit(0, "cm"),
                                 column_dend_height = unit(3, "cm"),
                                 missing_color = "red",
                                 not_missing_color = "gray") {
  # 將缺失值轉成二元矩陣 (1 = NA, 0 = 非NA)
  missing_mat <- as.matrix(is.na(df) * 1)
  
  # 設定行列名稱
  rownames(missing_mat) <- paste0("Row_", seq_len(nrow(missing_mat)))
  colnames(missing_mat) <- colnames(df)
  
  # 繪製熱圖
  Heatmap(
    missing_mat,
    name = "Missing",
    col = c("0" = not_missing_color, "1" = missing_color),
    cluster_rows = cluster_rows,
    cluster_columns = cluster_columns,
    show_row_names = show_row_names,
    show_column_names = show_column_names,
    show_heatmap_legend = show_legend,
    row_dend_width = row_dend_width,
    column_dend_height = column_dend_height
  )
}
plot_missing_heatmap(set_starting)
colnames(set_starting)
#寫一個函數檢查指定一個變數組
#e.g.("higher_edu_rank_2022" ,"education_rank_2018_2020","university_rank_2022")
#e,g,("internet_use_highschool" , "internet_use_university" )
#有無互斥（不會同時出現有值）如果成立變合併為一指定變數名
combine_if_mutually_exclusive <- function(data, vars, new_name) {
  # 檢查變數是否存在
  if(!all(vars %in% names(data))) {
    stop("部分變數名稱不存在於資料中。")
  }
  
  # 取出子資料
  sub <- data[vars]
  
  # 檢查每一列是否有超過 1 個非 NA
  conflict_rows <- apply(sub, 1, function(x) sum(!is.na(x)) > 1)
  
  if(any(conflict_rows, na.rm = TRUE)) {
    message("⚠️ 發現以下列在指定變數中同時有多個值，未合併：")
    print(which(conflict_rows))
    return(data)  # 不修改資料
  }
  
  # 若完全互斥，則合併
  data[[new_name]] <- apply(sub, 1, function(x) {
    vals <- na.omit(x)
    if(length(vals) == 0) return(NA)
    as.numeric(as.character(vals[1]))
  })
  
  # 刪除原變數（可選）
   data <- data[ , !(names(data) %in% vars)]
  
  message(paste0("✅ 已合併變數：", paste(vars, collapse = ", "),
                 " → 新變數 ", new_name))
  return(data)
}
# 範例1：檢查學歷排名類變數
set_starting_conb1 <- combine_if_mutually_exclusive(
  data = set_starting,
  vars = c("higher_edu_rank_2022", "education_rank_2018_2020", "university_rank_2022"),
  new_name = "edu_rank_combined"
)

# 範例2：檢查網路使用變數
true_starting <- combine_if_mutually_exclusive(
  data = set_starting_conb1,
  vars = c("internet_use_highschool", "internet_use_university"),
  new_name = "internet_use_combined"
)
#建立比較少的上網時數序組
library(dplyr)

true_starting <- true_starting %>%
  rename(internet_use_combined_ten = internet_use_combined) %>%  # 先改名保留原始欄位
  mutate(
    internet_use_combined = cut(
      internet_use_combined_ten,
      breaks = c(0, 2, 4, 6, 8, 10),
      labels = 1:5,
      right = TRUE
    )
  )

#check
write_csv(true_starting, "/Users/hanklin/Documents/R/dsp2025/true_starting.csv")
plot_missing_heatmap(true_starting)
colnames(true_starting)
# 計算每個欄位的缺失值數量
missing_counts <- sapply(true_starting, function(x) sum(is.na(x)))

# 顯示結果
missing_counts
#新的獨立步驟使用Ｒ代碼 回答空格,可用變數為_
#"survey_year" "life_happiness"＋ "life_quality" ＝生活快樂度 （滿分會變14 但幫我轉化成滿分10 分）,"internet_use_combined">5 => >3hr
#只需要數值結果不用幫我填空格
# 2018 年的青少年，上網只是放學後的一段放鬆。
# 那時的問卷顯示，每日上網大於三小時的樣本比例為＿％，「生活幸福度」平均為 __ 分。
# 但到了 2020 年，一切都變了。
# 學校被封、課程搬進螢幕、朋友的笑聲被麥克風吃掉——
# 上網大於三小時的樣本比例為＿％，而「生活幸福度」掉到了 __。
library(dplyr)


true_starting$life_score = (true_starting$life_happiness + true_starting$life_quality) * 10 / 14
# Step 2: 篩出 2018 和 2020 年
df_filtered <- true_starting %>%
  filter(survey_year %in% c(2018, 2020,2022))

# Step 3: 計算上網大於3小時比例與生活快樂度平均
summary_result <- df_filtered %>%
  group_by(survey_year) %>%
  summarise(
    n_total = n(),
    n_over3 = sum(internet_use_combined > 4.5, na.rm = TRUE),
    perc_over3 = round(n_over3 / n_total * 100, 1),
    mean_life_score = mean(life_score, na.rm = TRUE)
  )

summary_result
####遺失值情況
missing_by_year <-  true_starting%>%
  group_by(survey_year) %>%
  summarise(across(everything(), ~sum(is.na(.)), .names = "na_{col}"))
####樣本數
# 方法2：使用 summarise()
true_starting %>%
  group_by(survey_year) %>%
  summarise(sample_size = n())

####非遺失值樣本數

non_missing_by_year <- true_starting %>%
  group_by(survey_year) %>%
  summarise(across(everything(), ~sum(!is.na(.)), .names = "nonNA_{col}"))
#check
all_unique_values_true <- lapply(true_starting, unique)
all_unique_values_true
#######true_starting資料集畫直方圖life_score沒遺失值internet_use_combined 有可能有
library(dplyr)
library(ggplot2)

# 1️⃣ 二值化 life_score
plot_data <- true_starting %>%
  filter(!is.na(internet_use_combined)) %>%
  mutate(high_life = ifelse(life_score > 8, 1, 0))

# 2️⃣ 計算每個 internet_use_combined 的比例與樣本數
summary_data <- plot_data %>%
  group_by(internet_use_combined) %>%
  summarise(
    n = n(),
    prop_high = mean(high_life)
  )

# 3️⃣ 畫比例圖，顏色依樣本數
ggplot(summary_data, aes(x = factor(internet_use_combined), y = prop_high, fill = n)) +
  geom_col() +
  geom_text(aes(label = round(prop_high, 2)), vjust = -0.5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +  # 漸層色
  labs(
    x = "Net_time",
    y = "High_happy",
    fill = "count"
  ) +
  ylim(0, 1) +
  theme_minimal()



##########每年分開
library(dplyr)
library(ggplot2)



# 2️⃣ 計算每個 internet_use_combined + survey_year 的比例與樣本數
summary_data <- plot_data %>%
  group_by(survey_year, internet_use_combined) %>%
  summarise(
    n = n(),
    prop_high = mean(high_life)
  )

# 3️⃣ 畫分面圖，每個 survey_year 一張
ggplot(summary_data, aes(x = factor(internet_use_combined), y = prop_high, fill = n)) +
  geom_col() +
  geom_text(aes(label = round(prop_high, 2)), vjust = -0.5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(
    x = "Net_time",
    y = "High_happy",
    fill = "count"
  ) +
  ylim(0, 1) +
  facet_wrap(~survey_year) +
  theme_minimal()

# 3️⃣ 依 survey_year 生成三張圖並存檔
years <- unique(summary_data$survey_year)
for (yr in years) {
  tmp <- summary_data %>% filter(survey_year == yr)
  
  p <- ggplot(tmp, aes(x = factor(internet_use_combined), y = prop_high, fill = n)) +
    geom_col() +
    geom_text(aes(label = round(prop_high, 2)), vjust = -0.5) +
    scale_fill_gradient(low = "lightblue", high = "steelblue") +
    labs(
      x = "Net_time",
      y = "High_happy",
      fill = "count",
      title = paste("Survey Year:", yr)
    ) +
    ylim(0, 1) +
    theme_minimal()
  
  print(p)  # 這裡要加 print 才會顯示
}

#######擴展上面函數比較傾訴指數(max(true_starting$"talk_to_mother",true_starting$"talk_to_father" )有可能有遺失值)大於k 與不大於k 不同年下的網路使用分佈,高生活的比例直方圖

library(dplyr)
library(ggplot2)

# ⚙️ 設定門檻值 k
k <- 4   # 你可以改成 4 或其他數值

# 1️⃣ 建立傾訴指數與生活滿意度二值化
plot_data <- true_starting %>%
  filter(!is.na(internet_use_combined)) %>%  # 排除沒有上網資訊的樣本
  mutate(
    # 傾訴指數：母親或父親中最大值（有遺失值時自動忽略）
    talk_index = pmax(talk_to_mother, talk_to_father, na.rm = TRUE),
    # 分成兩組：傾訴指數是否大於 k
    talk_group = ifelse(talk_index > k, paste0("> ", k), paste0("≤ ", k)),
    # 高生活滿意度二值化
    high_life = ifelse(life_score > 8, 1, 0)
  )

# 2️⃣ 計算各年、各上網頻率、各傾訴組別的高生活比例
summary_data <- plot_data %>%
  group_by(survey_year, internet_use_combined, talk_group) %>%
  summarise(
    n = n(),
    prop_high = mean(high_life, na.rm = TRUE),
    .groups = "drop"
  )

# 2️⃣ 計算每年各組樣本比例
prop_data <- plot_data %>%
  group_by(survey_year, talk_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(survey_year) %>%
  mutate(
    total = sum(n),
    prop = n / total
  )
# 3️⃣ 顯示結果表
prop_data
# 4️⃣ 依年份個別存圖
years <- unique(summary_data$survey_year)
for (yr in years) {
  tmp <- summary_data %>% filter(survey_year == yr)
  
  p <- ggplot(tmp, aes(
    x = factor(internet_use_combined),
    y = prop_high,
    fill = n
  )) +
    geom_col(position = "dodge") +
    geom_text(aes(label = round(prop_high, 2)), vjust = -0.5, position = position_dodge(0.9)) +
    scale_fill_gradient(low = "lightblue", high = "steelblue") +
    labs(
      x = "Internet Use (Net_time)",
     y = "High Life Satisfaction (Proportion)",
      fill = "Sample Count",
      title = paste("Survey Year:", yr, "- Talk Index Threshold =", k)
    ) +
    ylim(0, 1) +
    facet_wrap(~talk_group) +  # 各傾訴組別各一張
    theme_minimal()
  
  print(p)
}

######
#擴展上面函數比較家長管教true_starting$"internet_time"有可能有遺失值)大於k 與不大於k 不同年下的網路使用分佈,高生活的比例直方圖
######

library(dplyr)
library(ggplot2)

# ⚙️ 設定上網時間門檻 k
k <- 2   # 你可以改成其他數值，例如 5

# 1️⃣ 建立分組與生活滿意二值化
plot_data <- true_starting %>%
  filter(!is.na(internet_use_combined)) %>%  # 排除沒有上網頻率的樣本
  mutate(
    # 以上網時間分組（處理可能的 NA）
    internet_group = case_when(
      is.na(internet_time) ~ "Missing",
      internet_time > k ~ paste0("> ", k),
      TRUE ~ paste0("≤ ", k)
    ),
    # 高生活滿意度二值化
    high_life = ifelse(life_score > 8, 1, 0)
  )

# 2️⃣ 計算各年、各上網頻率、各上網時間組別的高生活比例與樣本數
summary_data <- plot_data %>%
  group_by(survey_year, internet_use_combined, internet_group) %>%
  summarise(
    n = n(),
    prop_high = mean(high_life, na.rm = TRUE),
    .groups = "drop"
  )
# 2️⃣ 計算每年各組樣本比例
prop_data <- plot_data %>%
  group_by(survey_year, internet_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(survey_year) %>%
  mutate(
    total = sum(n),
    prop = n / total
  )

# 3️⃣ 顯示結果表
prop_data
# 4️⃣ 依年份輸出各自圖表
years <- unique(summary_data$survey_year)
for (yr in years) {
  tmp <- summary_data %>% filter(survey_year == yr)
  
  p <- ggplot(tmp, aes(
    x = factor(internet_use_combined),
    y = prop_high,
    fill = n
  )) +
    geom_col(position = "dodge") +
    geom_text(aes(label = round(prop_high, 2)),
              vjust = -0.5,
              position = position_dodge(0.9),
              size = 3) +
    scale_fill_gradient(low = "lightblue", high = "steelblue") +
    labs(
      x = "Internet Use (Net_time)",
      y = "High Life Satisfaction (Proportion)",
      fill = "Sample Count",
      title = paste("Survey Year:", yr, "- Internet Time control Threshold =", k)
    ) +
    ylim(0, 1) +
    facet_wrap(~internet_group) +  # 各上網時間組別各一張
    theme_minimal()
  
  print(p)
}

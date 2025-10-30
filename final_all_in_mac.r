#DSP2025#R4.5.1
#packages####
# 套件列表
packages <- c("naniar", "tidyverse", "lme4", "lmerTest", "sjPlot","mice", "missForest","broom",
              "psych", "ggplot2", 
              #mac 丟掉"devtools", 
              "VIM", "dplyr", "grid", "readr","ggalluvial","haven")

# 安裝未安裝的套件
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}



# 讀入
library(haven)
# 載入套件
library(ggplot2)
library(ggeffects)
library(mice)
library(missForest)ㄌㄌ
library(VIM)
#mac 丟掉
#library(devtools)
library(ComplexHeatmap)
library(tidyverse)
library(dplyr)
library(grid)
library(readr)
library(lme4)
library(lmerTest)
library(sjPlot)
library(psych)
library(ggalluvial)
library(broom)

# 如果 ComplexHeatmap 未安裝，使用 github 安裝
#if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
#  devtools::install_github("jokergoo/ComplexHeatmap")
#  library(ComplexHeatmap)
#}


#資料取得與合併####
# 設定資料夾位置需確認有C2022 C2020 C2018 合併編碼表的csv檔案####
setwd("/Users/hanklin/Documents/R/DSP2025")

# 確認目前工作目錄
getwd()
list.files()

# 匯入CSV#
C2022 <- read.csv("C2022.csv", header = TRUE, stringsAsFactors = FALSE)
C2020 <- read.csv("C2020.csv", header = TRUE, stringsAsFactors = FALSE)
C2018 <- read_sav("C2018.sav")
CODE <- read.csv("合併編碼表.csv", header = TRUE, stringsAsFactors = FALSE)

###問卷題目整理#

library(dplyr)

CODE <- CODE %>%
  mutate(across(starts_with("v20"), ~na_if(.x, "")))
CODE <- CODE %>%
  mutate(across(starts_with("v20"), ~ trimws(.x)))   # 去掉前後空白

####
extract_year <- function(df, CODE, year){
  # 找出該年的變數欄位名稱
  vars <- CODE %>% pull(!!sym(paste0("v", year)))
  vars <- vars[!is.na(vars)]   # 去掉 NA
  
  # 篩選資料
  out <- df[, vars, drop = FALSE]
  
  # rename 成 concept 名稱（只有有對應的才改名）
  rename_map <- CODE %>%
    filter(!is.na(!!sym(paste0("v", year)))) %>%
    select(concept, !!sym(paste0("v", year)))
  
  names(out) <- rename_map$concept
  return(out)
}
c2018_sel <- extract_year(C2018, CODE, 2018)
c2020_sel <- extract_year(C2020, CODE, 2020)
c2022_sel <- extract_year(C2022, CODE, 2022)
#資料合併
library(dplyr)

# 假設你已經有
# c2018_sel, c2020_sel, c2022_sel
# 並且這三份資料的欄位名稱都已經是 concept 名稱
# 找出所有年份的 concept（欄位名稱）
all_concepts <- unique(c(
  names(c2018_sel),
  names(c2020_sel),
  names(c2022_sel)
))

# 把三份資料的欄位統一成 all_concepts
c2018_sel <- c2018_sel %>% select(any_of(all_concepts))
c2020_sel <- c2020_sel %>% select(any_of(all_concepts))
c2022_sel <- c2022_sel %>% select(any_of(all_concepts))

# 合併
starting_data <- bind_rows(c2018_sel, c2020_sel, c2022_sel)
#清理與整理####
#all_unique_values_set <- lapply(starting_data, unique)
#all_unique_values_set
####初步清理完全用不到的<差補需要考慮所以先留著>
#cols_to_remove <- c( "residence" , "house_type"   ,"school_type","edu_2018","high_difficulty" 
#                     , "high_rule","control_sleepover", "control_curfew", "control_appearance" ,"control_routine",
#                     "control_relationship", "control_money","control_job","health_dad","health_mom",
#                     "poor_allowance","health_allowance","high_edu_rank","univ_edu_rank")
#starting_data <- starting_data[, !(names(starting_data) %in% cols_to_remove)]
####初步整理
#受訪西元年統一
starting_data$surv_y[starting_data$surv_y == 109] <- 2020
####遺失值編碼確定轉NA
#能統一處理的欄位
cols_to_check <- c(
  #useless
  "house_type","edu_2018","high_difficulty","high_rule","control_sleepover", "control_curfew", "control_appearance" ,"control_routine",
  "control_relationship", "control_money","control_job","health_dad","health_mom", "poor_allowance","health_allowance",
  "high_edu_rank","univ_edu_rank",
  #有用
  "surv_m", 
  "high_int_time",
  "control_internet",
  "talk_dad",
  "talk_mom",
  "intmind_1","intmind_2","intmind_3","intmind_4","intmind_5"
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
starting_data <-starting_data  # 複製原資料
for(col in cols_to_check) {
  if(col %in% colnames(starting_data)) {
    starting_data[[col]] <- make_continuous_from_one(starting_data[[col]])
  }
}
#需要指定變數改觀察[6]掉為na
# 需要處理的欄位
cols_replace6 <- c(
  "talk_dad",
  "talk_mom",
  "intmind_1","intmind_2","intmind_3","intmind_4","intmind_5"
)

#執行
starting_data <- starting_data %>%
  dplyr::mutate(across(
    all_of(cols_replace6),
    ~ ifelse(as.numeric(as.character(.)) == 6, NA, .)
  ))
#針對univ_int_time 僅保留1~10內的正整數
# 僅保留 1~10 的正整數，其餘設為 NA
starting_data$univ_int_time <- ifelse(
  as.numeric(as.character(starting_data$univ_int_time)) %in% 1:10,
  as.numeric(as.character(starting_data$univ_int_time)),
  NA
)
#檢查
#all_unique_values_set <- lapply(starting_data, unique)
#all_unique_values_set

#缺失值檢視#### 
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
plot_missing_heatmap(starting_data)
#遺失值隨機性確認####
check_mar_chisq <- function(data) {
  output <- data.frame(Variable = character(),
                       p_value = character(),
                       Judgment = character(),
                       stringsAsFactors = FALSE)
  
  # 篩出有遺失值的欄位
  vars_with_na <- names(data)[colSums(is.na(data)) > 0]
  
  for (v in vars_with_na) {
    miss_flag <- as.factor(ifelse(is.na(data[[v]]), 1, 0))  # 遺失標記
    
    min_p <- 1
    
    for (p in setdiff(names(data), v)) {
      pred <- as.factor(data[[p]])  # 全部當類別變數
      
      # 建立列聯表
      tbl <- table(miss_flag, pred)
      
      # 若格子太小，使用 Fisher，否則用卡方
      p_val <- tryCatch({
        if (any(tbl < 5)) {
          fisher.test(tbl)$p.value
        } else {
          suppressWarnings(chisq.test(tbl)$p.value)
        }
      }, error = function(e) NA)
      
      if (!is.na(p_val)) min_p <- min(min_p, p_val)
    }
    
    p_display <- ifelse(is.na(min_p), NA,
                        ifelse(min_p < 0.0001, "<0.0001", sprintf("%.4f", min_p)))
    
    judgment <- ifelse(is.na(min_p), NA,
                       ifelse(min_p < 0.05,
                              "Missingness related to observed vars (MAR)",
                              "No significant relation (likely MCAR)"))
    
    output <- rbind(output,
                    data.frame(Variable = v,
                               p_value = p_display,
                               Judgment = judgment,
                               stringsAsFactors = FALSE))
  }
  
  return(output)
}

# 使用方法
mar_result_chisq <- check_mar_chisq(starting_data)
print(mar_result_chisq)



#信度檢驗####
#結論Cronbach’s alpha 皆為0.81 可合併

list_starting_data_surv_y <- split(starting_data, starting_data$surv_y)

#確認上網行為特徵是否反應同一潛在構面
cols <- c("intmind_1","intmind_2","intmind_3","intmind_4","intmind_5")
# 合併 2020 與2022
test_intmind <- rbind(list_starting_data_surv_y[[2]][, cols], list_starting_data_surv_y[[3]][, cols])

# 確保都是數值
test_intmind <- data.frame(lapply(test_intmind, function(x) as.numeric(as.character(x))))
psych::alpha(test_intmind, check.keys = TRUE)
#確認生活過得好不好與快不快樂是否反應同一潛在構面
cols <- c("score_happy", "score_good")
test_happiness <-starting_data[,cols]
# 確保都是數值
test_happiness <- data.frame(lapply(test_happiness, function(x) as.numeric(as.character(x))))
psych::alpha(test_happiness, check.keys = TRUE)

#建立幸福感指標與整理敘述統計用因子及二值化####
#針對starting_data指標與因子整理
#建立幸福感指標-score_happy+score_good為score_happiness
#年齡計算
#上網行為特徵分數intmind_1~5平均為intmind_mean，注意如果個別受訪題目有遺失要根據答題計算平均值
#傾訴指標-talk_dad或talk_mom母頻率最高(數值最小)為talk_parent
#確認後合併有值的情況互斥的univ_int_time與high_int_time為int_time


library(dplyr)

df <- starting_data %>%
  # 幸福感指標
  mutate(score_happiness = score_happy + score_good) %>%
  
  # 上網行為特徵平均分
  rowwise() %>%
  mutate(
    intmind_mean = {
      vals <- c_across(intmind_1:intmind_5)
      if(all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE)
    }
  ) %>%
  ungroup() %>%
  
  # 傾訴指標取父母中母頻率最高 (數值最小)
  mutate(talk_parent = pmin(talk_dad, talk_mom, na.rm = TRUE)) %>%
  
  # 合併互斥上網時間
  mutate(int_time = coalesce(univ_int_time, high_int_time))
#年齡計算

# 先把民國年轉西元年
df <- df %>%
  mutate(birth_twy = birth_twy + 1911)

# 計算平均訪問月份（排除缺失）
mean_surv_m <- df %>%
  filter(!is.na(surv_m)) %>%
  summarise(mean_month = mean(surv_m)) %>%
  pull(mean_month)

df<- df %>%
  mutate(
    age = surv_y - birth_twy,  # 基本年齡
    # 處理生日是否已過
    age = case_when(
      !is.na(surv_m) & birth_m > surv_m ~ age - 1,
      is.na(surv_m) & birth_m >= mean_surv_m ~ age - 1,
      TRUE ~ age
    )
  )
#all_unique_values_set <- lapply(df, unique)
#all_unique_values_set

#敘述統計因子二值化
#"talk_parent"是否較常傾訴、"int_time"是否上網多於兩小時 、"control_internet"是否受到上網管制
#

binarize_numeric <- function(df, vars, threshold, direction = c("greater", "less_equal")) {
  direction <- match.arg(direction)
  
  for (var in vars) {
    if (!var %in% names(df)) {
      warning(paste("Variable", var, "not found in dataframe. Skipping."))
      next
    }
    
    new_var <- paste0(var, "_bin")
    
    if (direction == "greater") {
      df[[new_var]] <- ifelse(df[[var]] > threshold, 1, 0)
    } else if (direction == "less_equal") {
      df[[new_var]] <- ifelse(df[[var]] <= threshold, 1, 0)
    }
  }
  
  return(df)
}
df <- binarize_numeric(df, vars =c("talk_parent"), threshold = 3.5, direction = "less_equal")
df <- binarize_numeric(df, vars =c("int_time"), threshold = 5.5, direction = "greater")
df <- binarize_numeric(df, vars =c("control_internet"), threshold = 2.5, direction = "greater")
df_list <- split(df, df$surv_y)
#summary(df_list[[1]])
#summary(df_list[[2]])
#summary(df_list[[3]])
#plot_missing_heatmap(df_list[[3]])
#敘述統計與群體比較####
#各年份二值化因子比例折線圖
plot_multiple_yearly_mean <- function(df,
                                      vars_info = tibble::tibble(
                                        var = c("talk_parent_bin", "control_internet_bin", "int_time_bin"),
                                        label = c("較常傾訴比例", "上網受管制比例", "上網超過4小時比例"),
                                        reverse = c(FALSE, FALSE, FALSE)
                                      )) {
  # 檢查變數是否存在
  missing_vars <- setdiff(vars_info$var, names(df))
  if (length(missing_vars) > 0) {
    stop("資料框中缺少下列變數: ", paste(missing_vars, collapse = ", "))
  }
  
  # 1) 先按年度一次計算所有指定變數的平均值（即比例）
  df_summary <- df %>%
    group_by(surv_y) %>%
    summarise(
      across(all_of(vars_info$var),
             ~ mean(.x, na.rm = TRUE),
             .names = "{.col}")
    ) %>%
    ungroup()
  
  # 2) 轉長表
  df_long <- df_summary %>%
    pivot_longer(
      cols = all_of(vars_info$var),
      names_to = "var",
      values_to = "mean_value"
    )
  
  # 3) 加上 label 與 reverse 資訊（以 vars_info 為準）
  df_long <- df_long %>%
    left_join(vars_info, by = c("var" = "var")) %>%
    mutate(
      mean_value = ifelse(is.na(reverse), mean_value, ifelse(reverse, 1 - mean_value, mean_value)),
      variable = label
    )
  
  # 4) 繪圖（同張圖顯示三條線）
  p <- ggplot(df_long, aes(x = surv_y, y = mean_value, color = variable, group = variable)) +
    geom_line(size = 1.1) +
    geom_point(size = 3) +
    geom_text(
      aes(label = scales::percent(mean_value, accuracy = 0.1)),
      vjust = -0.8, size = 5,
      color = "black",
      show.legend = FALSE
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(max(min(df_long$mean_value) - 0.05, 0),
                                                                                 min(max(df_long$mean_value) + 0.05, 1))) +
    labs(
      title = "訪問結果重要因子比例變化",
      x = "調查年份",
      y = "比例",
      color = ""
    ) +
    theme_minimal(base_size = 20) +
    theme(legend.position = "top")
  
  return(p)
}
plot_multiple_yearly_mean(df)
#幸福度各因子單獨分組下各年份的差異與變化
library(dplyr)
library(ggplot2)
library(tidyr)
colnames(df)
vars <- c("talk_parent_bin", "int_time_bin", "control_internet_bin")

# 計算各變數的平均幸福感
df_mean_vars <- bind_rows(
  lapply(vars, function(v) {
    df %>%
      filter(!is.na(.data[[v]])) %>%  # 只排除該變數的 NA
      group_by(surv_y, group = .data[[v]]) %>%
      summarise(mean_score_happiness = mean(score_happiness, na.rm = TRUE), .groups = "drop") %>%
      mutate(variable = v)
  })
) %>%
  mutate(var_group = paste0(variable, "_", group))

# 計算該年度總均值
df_mean_total <- df %>%
  group_by(surv_y) %>%
  summarise(mean_score_happiness = mean(score_happiness, na.rm = TRUE)) %>%
  mutate(var_group = "total")

# 合併
df_plot <- bind_rows(df_mean_vars, df_mean_total)

# 顏色設定
color_map <- c(
  "control_internet_bin_0" = "#FF9999", "control_internet_bin_1" = "#CC0000",
  "talk_parent_bin_0" = "#99CCFF", "talk_parent_bin_1" = "#0033CC",
  "int_time_bin_0" = "#99FF99", "int_time_bin_1" = "#009900",
  "total" = "black"
)

label_map <- c(
  "control_internet_bin_0" = "較常傾訴組",
  "control_internet_bin_1" = "無/偶爾傾訴",
  "talk_parent_bin_0" = "不管上網",
  "talk_parent_bin_1" = "會管上網",
  "int_time_bin_0" = "上網少於4小時",
  "int_time_bin_1" = "上網多於4小時",
  "total" = "全體平均"
)

# 繪圖
ggplot(df_plot, aes(x = surv_y, y = mean_score_happiness, group = var_group, color = var_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(      color = "black",aes(label = round(mean_score_happiness, 2)), vjust = -0.5, size = 5) +
  scale_color_manual(values = color_map, labels = label_map, name = "分組") +
  labs(
    title = "不同二元變數下分組的平均幸福感變化",
    x = "調查年份",
    y = "平均幸福感"
  ) +
  theme_minimal(base_size = 14) +
  ylim(min(df_plot$mean_score_happiness)*0.99, max(df_plot$mean_score_happiness)*1.01)  # 調大Y軸空間


#受訪者流向圖####
df_wide_correct <- df %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = surv_y,
    values_from = present,
    values_fill = 0
  ) %>%
  group_by(id) %>%           # 聚合同一個 ID
  summarise(across(everything(), max))  # 同一年若有多筆回答，取最大值 1
years <- c("2018","2020","2022")

# 建立 df_wide_correct (已經有了)
df_long <- df_wide_correct %>%
  mutate(across(all_of(years), as.factor)) %>%
  unite("combo", all_of(years), sep="-", remove=FALSE)

ggplot(df_long,
       aes(axis1 = `2018`, axis2 = `2020`, axis3 = `2022`,
           y = 1, fill = combo)) +
  scale_x_discrete(limits = years, expand = c(.1, .05)) +
  geom_alluvium(width = 1/12) +
  geom_stratum(width = 1/12, fill = "White", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  labs(title = "受訪者流向", y = "人數")
#不差補的混和效應模型####
#不好的model
#m1 <- lmer(score_happiness ~ factor(surv_y) + int_time_bin + control_internet_bin * talk_parent_bin +
#             age+sex+(1 |id), data = df, REML = FALSE)
#summary(m1)
#m2 <- lmer(score_happiness ~ factor(surv_y) + int_time_bin + control_internet_bin * talk_parent_bin +intmind_mean*age+sex
#           +(1 |id), data = df, REML = FALSE)
#summary(m2)
#好的model
df <- df %>%
  mutate(
    intmind_mean_effect = ifelse(surv_y == 2018, 0, intmind_mean),
    intmind_available = ifelse(surv_y == 2018, 0, 1)
  )

m3 <- lmer(score_happiness ~ factor(surv_y) + int_time_bin +
             control_internet_bin * talk_parent_bin +
             sex + age +
             intmind_available:intmind_mean_effect +  # 只在有資料年份生效
             (1 | id),
           data = df, REML = FALSE)
summary(m3)
#解釋圖
gg <- ggpredict(m3, terms = c("control_internet_bin", "talk_parent_bin"))
plot(gg) + labs(title="上網管制 × 傾訴對幸福感的交互作用", y="預測幸福感")
gg2 <- ggpredict(m3, terms = c("intmind_mean_effect"))
plot(gg2) + labs(title="心智面向與幸福感關係（限有測量年度）", y="預測幸福感")

#多重差補與混合效應模型####
# ---- 1. 指定需差補的欄位 ----
vars_to_impute <- c("int_time", "control_internet")

# 取出並確保為數值型態
df_impute_subset <- data.frame(lapply(df[, vars_to_impute], as.numeric))

# ---- 2. 多重差補法 (MICE) ----
set.seed(123)
Imputations_mice <- mice(df_impute_subset, m = 30, method = "pmm", seed = 123)

# ---- 3. 隨機森林差補法 (missForest) ----
set.seed(123)
Imputations_rf <- missForest(df_impute_subset, verbose = TRUE)

# ---- 4. 定義二值化函數 ----
binarize_numeric <- function(df, vars, threshold, direction = "greater") {
  for (v in vars) {
    new_var <- paste0(v, "_bin")
    if (direction == "greater") {
      df[[new_var]] <- as.numeric(df[[v]] > threshold)
    } else if (direction == "less_equal") {
      df[[new_var]] <- as.numeric(df[[v]] <= threshold)
    }
  }
  return(df)
}

# ---- 5. 建立 talk_parent 與二值化邏輯 ----
make_talk_parent <- function(df) {
  df <- df %>%
    mutate(
      talk_parent = ifelse(
        !is.na(talk_dad) | !is.na(talk_mom),
        pmin(talk_dad, talk_mom, na.rm = TRUE),
        NA_real_
      )
    ) %>%
    binarize_numeric(vars = c("talk_parent"), threshold = 3.5, direction = "less_equal")
  return(df)
}

# ---- 6. 組合差補結果 ----
# (A) MICE
imp_list_mice <- complete(Imputations_mice, action = "all")
imp_full_list_mice <- lapply(imp_list_mice, function(imputed_subset) {
  df_copy <- df
  df_copy[, vars_to_impute] <- imputed_subset[, vars_to_impute]
  df_copy <- df_copy %>%
    make_talk_parent() %>%
    binarize_numeric(vars = c("int_time"), threshold = 5.5, direction = "greater") %>%
    binarize_numeric(vars = c("control_internet"), threshold = 2.5, direction = "greater")
  return(df_copy)
})

# (B) missForest
df_imputed_rf <- df
df_imputed_rf[, vars_to_impute] <- Imputations_rf$ximp[, vars_to_impute]
df_imputed_rf <- df_imputed_rf %>%
  make_talk_parent() %>%
  binarize_numeric(vars = c("int_time"), threshold = 5.5, direction = "greater") %>%
  binarize_numeric(vars = c("control_internet"), threshold = 2.5, direction = "greater")

# ---- 7. 建立混合效應模型 ----
formula <- score_happiness ~ factor(surv_y) + int_time_bin +
  control_internet_bin * talk_parent_bin +
  sex + age +
  intmind_available:intmind_mean_effect +
  (1 | id)

# MICE: 多重差補模型
fits_mice <- lapply(imp_full_list_mice, function(dat) {
  dat$surv_y <- as.factor(dat$surv_y)
  lmer(formula, data = dat, REML = FALSE)
})

# missForest: 單一模型
df_imputed_rf$surv_y <- as.factor(df_imputed_rf$surv_y)
fit_rf <- lmer(formula, data = df_imputed_rf, REML = FALSE)

# ---- 8. Rubin’s Rules 合併結果 ----
m <- length(fits_mice)
betas <- lapply(fits_mice, function(f) as.numeric(fixef(f)))
names_param <- names(fixef(fits_mice[[1]]))
vcovs <- lapply(fits_mice, function(f) as.matrix(vcov(f))[names_param, names_param])

betas_mat <- do.call(rbind, betas)
beta_bar <- colMeans(betas_mat)
W_mat <- Reduce("+", vcovs) / m
centered <- sweep(betas_mat, 2, beta_bar, FUN = "-")
B_mat <- t(centered) %*% centered / (m - 1)
T_mat <- W_mat + (1 + 1/m) * B_mat

pooled_se <- sqrt(diag(T_mat))
t_stat <- beta_bar / pooled_se
lower95 <- beta_bar - qnorm(0.975) * pooled_se
upper95 <- beta_bar + qnorm(0.975) * pooled_se

pooled_table <- data.frame(
  term = names_param,
  estimate = beta_bar,
  se = pooled_se,
  z = t_stat,
  ci_lower = lower95,
  ci_upper = upper95
)

# ---- 9. 結果輸出 ----
cat("\n===== 多重差補 (MICE) + Rubin 合併結果 =====\n")
print(pooled_table, digits = 3)

cat("\n===== 隨機森林差補 (missForest) 模型結果 =====\n")
summary(fit_rf)
#解釋圖
gg <- ggpredict(fit_rf, terms = c("control_internet_bin", "talk_parent_bin"))
plot(gg) + labs(title="上網管制 × 傾訴對幸福感的交互作用", y="預測幸福感")
gg2 <- ggpredict(fit_rf, terms = c("intmind_mean_effect"))
plot(gg2) + labs(title="心智面向與幸福感關係（限有測量年度）", y="預測幸福感")


#結果確認####
all_unique_values_set <- lapply(df_imputed_rf, unique)
all_unique_values_set
summary(m3)
summary(fit_rf)

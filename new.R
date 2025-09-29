
# 設定資料夾位置
setwd("E:/學/DSP2025/data")

# 確認目前工作目錄
getwd()
final <- read.csv("final.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(final )
library(dplyr)

# 1. 計算總訪問年份數
total_years <- final %>% 
  distinct(訪問年) %>% 
  nrow()

# 2. 找出完全追蹤的受訪者編號
complete_ids <- final %>%
  group_by(受訪者編號) %>%
  summarise(years = n_distinct(訪問年), .groups = "drop") %>%
  filter(years == total_years) %>%
  pull(受訪者編號)

# 3. 完全追蹤組
final_complete <- final %>%
  filter(受訪者編號 %in% complete_ids)

# 4. 對照組（差集）
final_incomplete <- final %>%
  filter(!(受訪者編號 %in% complete_ids))
###############
###############
###############
subset_final <- subset(
  final,
  (出生民國年 == 91 & 出生月 >= 9) |
    (出生民國年 == 92 & 出生月 <= 8)
)
#########################
########堆疊長方#########
#########################
plot_with_counts <- function(x, group = NULL, col = NULL, main = NULL, legend_title = NULL) {
  if (is.null(group)) {
    # 單變數情況
    counts <- table(x)
    bp <- barplot(counts, col = col, main = main)
    text(x = bp, y = counts/2, labels = counts, col = "black")
  } else {
    # 分層堆疊情況
    counts <- table(x, group)
    if (is.null(col)) col <- rainbow(nrow(counts))
    bp <- barplot(counts, col = col, main = main, beside = FALSE)
    
    # 在每個 segment 中加數字
    for (i in 1:nrow(counts)) {
      y_pos <- counts[i, ] / 2
      for (j in 1:ncol(counts)) {
        if (counts[i, j] > 0) {
          # 計算累積高度，用於堆疊
          y_cum <- sum(counts[1:i, j]) - counts[i, j]/2
          text(x = bp[j], y = y_cum, labels = counts[i, j], col = "black")
        }
      }
    }
    
    # 加圖例
    legend("topright", legend = rownames(counts), fill = col, title = legend_title)
  }
}

#########
plot_with_counts(final_complete$年齡,group = final_complete$訪問年)
##########
##########
###########################
# 缺失值矩陣檢視 function #
###########################

plot_missing_matrix <- function(df) {
  # 確保需要的套件已安裝
  if (!requireNamespace("naniar", quietly = TRUE)) {
    install.packages("naniar")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  
  library(naniar)
  library(ggplot2)
  
  # 繪製缺失值矩陣
  gg_miss_var(df) +
    labs(title = "Missing Values by Variable",
         x = "變數",
         y = "缺失數量")
  
  # 顯示 missingness pattern
  print("Missing data pattern (前 10 行):")
  print(miss_var_span(df, var = NULL, span_every = 10))
  
  # 可視化矩陣 (橫軸: 觀測值, 縱軸: 變數)
  vis_miss(df) + 
    labs(title = "Missingness Matrix")
}
###################
######## 使用方法 #
###################
 plot_missing_matrix(final)  # 假設你的資料框名稱叫 final

 ##############################
 # 查看指定變數的可能值與分布 #
 ##############################
# 查看所有變數的可能值與分布
check_all_values <- function(df) {
  results <- list()
  
  for (v in names(df)) {
    freq <- table(df[[v]], useNA = "ifany")
    
    results[[v]] <- list(
      frequency = freq
    )
    
    cat("\n====================\n")
    cat("變數:", v, "\n")
    cat("\n分布:\n")
    print(freq)
  }
  
  return(results)
}

# 使用方法：
check_all_values(final)

 


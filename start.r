# 設定資料夾位置
setwd("E:/學/DSP2025/data")

# 確認目前工作目錄
getwd()

############## 匯入CSV###########
C2022 <- read.csv("C2022.csv", header = TRUE, stringsAsFactors = FALSE)
C2020 <- read.csv("C2020.csv", header = TRUE, stringsAsFactors = FALSE)
C2018 <- read.csv("C2018.csv", header = TRUE, stringsAsFactors = FALSE)
CODE <- read.csv("合併編碼表.csv", header = TRUE, stringsAsFactors = FALSE)

#####################
###問卷題目整理######
#####################
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
  out <- df %>% select(all_of(vars))
  
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
#####################
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
write.csv(starting_data, "starting_data.csv", row.names = FALSE, fileEncoding = "UTF-8")

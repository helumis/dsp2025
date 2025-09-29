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
final <- bind_rows(c2018_sel, c2020_sel, c2022_sel)



colnames(final)


final <- final %>%
  mutate(
    # 學校種類：直接保留（假設已經對齊成 concept = 學校種類）
    學校種類 = coalesce(學校種類),
    
    # 教育排名：整合三種版本
    教育排名 = coalesce(教育排名_2018_2020,
                    高等教育排名_2022,
                    大學教育排名_2022)
  ) %>%
  # 只保留整理後的變數
  select(-教育排名_2018_2020, -高等教育排名_2022, -大學教育排名_2022)
# 如果訪問年小於 1911，就視為民國年，轉換成西元
final$訪問年 <- ifelse(final$訪問年 < 1911, 
                    final$訪問年 + 1911, 
                    final$訪問年)

# 假設 final 是你的資料框
final$出生西元年 <- final$`出生民國年` + 1911

# 初步年齡
final$年齡 <- final$`訪問年` - final$出生西元年

# 判斷是否已過生日，若訪問月 < 出生月，則年齡 - 1
final$年齡 <- ifelse(final$`訪問月` < final$`出生月`, 
                   final$年齡 - 1, 
                   final$年齡)


write.csv(final, "final.csv", row.names = FALSE, fileEncoding = "UTF-8")

colnames(final)
#########################



final <- read.csv("final.csv", header = TRUE, stringsAsFactors = FALSE)
# 確認交叉樣本數
table(final$年齡, final$訪問年)

########################################
########2018-2022上網時數變化###########
########################################

##視覺化##
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# 合併資料、只保留有效答案 0.1~10.1
df_combined <- final %>%
  mutate(
    上網_高中_num = suppressWarnings(as.numeric(上網_高中)),
    上網_大學_num = suppressWarnings(as.numeric(上網_大學))
  ) %>%
  pivot_longer(
    cols = c(上網_高中_num, 上網_大學_num),
    names_to = "學階",
    values_to = "ans_num"
  ) %>%
  filter(!is.na(ans_num), ans_num >= 0.1, ans_num <= 10.1) %>%
  mutate(
    學階 = recode(學階, 上網_高中_num = "高中", 上網_大學_num = "大學"),
    opt = sprintf("%02d", pmax(1, pmin(10, round(ans_num)))),
    opt = factor(opt, levels = sprintf("%02d",1:10), 
                 labels = c(
                   "(01) 不上網","(02) 不超過 1 小時","(03) 1–2 小時（不含）",
                   "(04) 2–3 小時（不含）","(05) 3–4 小時（不含）","(06) 4–5 小時（不含）",
                   "(07) 5–6 小時（不含）","(08) 6–7 小時（不含）","(09) 7–8 小時（不含）",
                   "(10) 8 小時以上"), ordered = TRUE)
  ) %>%
  count(訪問年, 學階, opt, name = "n") %>%
  group_by(訪問年, 學階) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  complete(訪問年, 學階, opt, fill = list(n = 0, prop = 0))
df_combined <- df_combined %>%
  mutate(學階 = factor(學階, levels = c("高中","大學" )))
# 繪圖
ggplot(df_combined, aes(x = factor(訪問年), y = prop, fill = opt)) +
  geom_col(position = "fill") +
  facet_wrap(~學階) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "訪問年",
    y = "比例",
    fill = "上網時數（學期中每日）",
    title = "高中 vs 大學每日上網時數分佈（按訪問年）",
    subtitle = "每年分佈（100% 堆疊），僅含有效答案"
  ) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

#############################################
##########樣本數、無效樣本(暫時跳過)#########
#############################################
#############################################
#############使用電腦時數變化顯著性的檢定####
#############################################
library(dplyr)

# 高中上網資料集
df_hs <- final %>%
  filter(!is.na(上網_高中)) %>%        # 移除缺失
  mutate(上網_高中_num = suppressWarnings(as.numeric(上網_高中))) %>%
  filter(上網_高中_num >= 0.9, 上網_高中_num <= 10.1)

# 大學上網資料集
df_uni <- final %>%
  filter(!is.na(上網_大學)) %>%        # 移除缺失
  mutate(上網_大學_num = suppressWarnings(as.numeric(上網_大學))) %>%
  filter(上網_大學_num >= 0.9, 上網_大學_num <= 10.1)

####
# 高中：新增指示變數 (1 = 使用時數 > 4.5, 0 = 其餘)
df_hs <- df_hs %>%
  mutate(hs_highuse = ifelse(上網_高中_num > 4.5, 1, 0))

# 大學：新增指示變數 (1 = 使用時數 > 4.5, 0 = 其餘)
df_uni <- df_uni %>%
  mutate(uni_highuse = ifelse(上網_大學_num > 4.5, 1, 0))
library(DescTools)

# 高中
tab_hs <- table(df_hs$訪問年, df_hs$hs_highuse)
CochranArmitageTest(tab_hs)

# 大學
tab_uni <- table(df_uni$訪問年, df_uni$uni_highuse)
CochranArmitageTest(tab_uni)

#####################
#####大學男女########
#####################
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# 假設 df_uni 有變數：訪問年、上網_高中、上網_大學、性別
df_combined <- df_uni %>%
  mutate(
    上網_高中_num = suppressWarnings(as.numeric(上網_高中)),
    上網_大學_num = suppressWarnings(as.numeric(上網_大學))
  ) %>%
  pivot_longer(
    cols = c(上網_高中_num, 上網_大學_num),
    names_to = "學階",
    values_to = "ans_num"
  ) %>%
  filter(!is.na(ans_num), ans_num >= 0.1, ans_num <= 10.1) %>%
  mutate(
    opt = sprintf("%02d", pmax(1, pmin(10, round(ans_num)))),
    opt = factor(opt, levels = sprintf("%02d",1:10), 
                 labels = c(
                   "(01) 不上網","(02) 不超過 1 小時","(03) 1–2 小時（不含）",
                   "(04) 2–3 小時（不含）","(05) 3–4 小時（不含）","(06) 4–5 小時（不含）",
                   "(07) 5–6 小時（不含）","(08) 6–7 小時（不含）","(09) 7–8 小時（不含）",
                   "(10) 8 小時以上"), ordered = TRUE),
    性別 = recode(factor(性別), `1` = "男", `2` = "女")
  ) %>%
  count(訪問年, 性別, opt, name = "n") %>%
  group_by(訪問年, 性別) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  complete(訪問年, 性別, opt, fill = list(n = 0, prop = 0))

# 性別順序
df_combined <- df_combined %>%
  mutate(性別 = factor(性別, levels = c("男", "女")))

# 繪圖
ggplot(df_combined, aes(x = factor(訪問年), y = prop, fill = opt)) +
  geom_col(position = "fill") +
  facet_wrap(~性別) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "訪問年",
    y = "比例",
    fill = "上網時數（學期中每日）",
    title = "大學組每日上網時數分佈（按訪問年）",
    subtitle = "每年分佈（100% 堆疊），僅含有效答案"
  ) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

##
##
###############
x1 <- as.numeric(df_uni$上網_大學[df_uni$訪問年 == 2018])
x2 <- as.numeric(df_uni$上網_大學[df_uni$訪問年 == 2020])
x3 <- as.numeric(df_uni$上網_大學[df_uni$訪問年 == 2022])
jt_test(list(x1,x2,x3), alternative="increasing")
x1 <- as.numeric(df_hs$上網_高中[df_hs$訪問年 == 2018])
x2 <- as.numeric(df_hs$上網_高中[df_hs$訪問年 == 2020])
x3 <- as.numeric(df_hs$上網_高中[df_hs$訪問年 == 2022])
jt_test(list(x1,x2,x3), alternative="increasing")
##
##
#############################################
##########2020vs2022網癮症狀比較#############
#############################################
plot_multi_likert <- function(data, varnames, var_labels = NULL, year_col = "訪問年",
                              valid_vals = 1:5, score_labels = NULL,
                              title = "Likert 題目分布", subtitle = NULL,
                              horizontal = TRUE) {
  
  # 自訂題目標籤
  if(is.null(var_labels)) var_labels <- varnames
  if(length(var_labels) != length(varnames)) stop("var_labels 長度須等於 varnames")
  
  # 自訂分數標籤
  if(is.null(score_labels)) score_labels <- as.character(valid_vals)
  
  dist_list <- lapply(seq_along(varnames), function(i){
    var <- varnames[i]
    label <- var_labels[i]
    df_clean <- data %>% filter(.data[[var]] %in% valid_vals)
    df_clean %>%
      group_by(.data[[year_col]], 分數 = .data[[var]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(.data[[year_col]]) %>%
      mutate(pct = n / sum(n) * 100,
             題目 = label) %>%
      ungroup()
  })
  
  dist_df <- bind_rows(dist_list) %>%
    mutate(
      分數 = factor(分數, levels = valid_vals, labels = score_labels, ordered = TRUE),
      題目 = factor(題目, levels = var_labels)
    )
  
  p <- ggplot(dist_df, aes(x = factor(.data[[year_col]]), y = pct, fill = 分數)) +
    geom_col(position = "fill") +
    geom_text(
      aes(label = paste0(round(pct, 1), "%")),
      position = position_fill(vjust = 0.5),
      size = 3,
      color = "red"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = "訪問年",
      y = "比例",
      fill = "狀況",
      title = title,
      subtitle = subtitle
    ) +
    guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  
  if(horizontal){
    p <- p + facet_wrap(~題目, nrow = 1)
  } else {
    p <- p + facet_wrap(~題目, ncol = 1)
  }
  
  return(p)
}

likert_vars <- paste0("網癮_", 1:5)
likert_labels <- c("習慣減少睡眠時
間，以便能有更多
時間上網
","只要有一段時間沒
有上網，就會覺得
自己好像錯過什麼","因為上網而減少和
家人朋友的面對面
互動
","上網的時間越來越
長","沒有網路，生活就
毫無樂趣可言
")

plot_multi_likert(
  df_sub,
  varnames = likert_vars,
  var_labels = likert_labels,
  year_col = "訪問年",
  valid_vals = 1:5,
  score_labels = c("完全不符合","不符合","普通","符合","完全符合"),
  title = "請問以下這些描述，符不符合您目前「工作或課業以
外」的上網狀況？",
  subtitle = "2020 vs 2022",
  horizontal = TRUE
)
###檢定##
library(dplyr)
library(MASS)  # polr 用於有序羅吉斯迴歸

# 1️⃣ 篩選只留 2020 與 2022 的資料
df_sub <- final %>%
  filter(訪問年 %in% c(2020, 2022))

# 2️⃣ 定義有效 Likert 值
valid_vals <- 1:5

# 3️⃣ 建立分析函數：Wilcoxon + Ordinal Logistic
analyze_question <- function(varname){
  # 篩選有效值
  df_clean <- df_sub %>% filter(.data[[varname]] %in% valid_vals)
  
  # Wilcoxon-Mann-Whitney test（單尾假設 2020 < 2022）
  wilcox_res <- wilcox.test(
    formula = as.formula(paste(varname, "~ 訪問年")),
    data = df_clean,
    alternative = "less"
  )
  
  # Ordinal Logistic Regression
  df_clean[[varname]] <- factor(df_clean[[varname]], ordered=TRUE)
  df_clean$訪問年 <- factor(df_clean$訪問年)
  olr_model <- polr(as.formula(paste(varname, "~ 訪問年")), data=df_clean, Hess=TRUE)
  olr_summary <- summary(olr_model)
  
  # 輸出
  list(
    wilcox_p = wilcox_res$p.value,
    olr_coeff = coef(olr_summary)["訪問年2022","Value"],
    olr_p = pnorm(abs(coef(olr_summary)["訪問年2022","t value"]), lower.tail = FALSE) * 2
  )
}

# 4️⃣ 針對網癮_1 ~ 網癮_5 做分析
results <- lapply(paste0("網癮_", 1:5), analyze_question)
names(results) <- paste0("網癮_", 1:5)
results

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
################
####清理變數####
################
df <- C2022 
#  "",#
drop_vars <- c(
#  "a03",#生肖
)

# 假設你的資料框是 df
df_clean <- df[, !(names(df) %in% drop_vars)]
#################################
##############持續追蹤###########
#################################
# 假設三年的資料分別叫 df2018, df2020, df2022
id2018 <- C2018$x01
id2020 <- C2020$x01
id2022 <- C2022$x01

# 找出三年都有的樣本
common_all <- Reduce(intersect, list(id2018, id2020, id2022))

# 找出任兩年都有的樣本 (例如 2018 與 2020)
common_18_20 <- intersect(id2018, id2020)

# 如果要檢查每個樣本出現在哪些年份，可以做一個資料表
all_ids <- unique(c(id2018, id2020, id2022))
library(dplyr)

tracking_table <- data.frame(
  x01 = all_ids,
  in2018 = all_ids %in% id2018,
  in2020 = all_ids %in% id2020,
  in2022 = all_ids %in% id2022
)



length(intersect(id2018, id2020))      # 18、20 都有
length(intersect(id2020, id2022))      # 20、22 都有
length(intersect(id2018, id2022))      # 18、22 都有
length(Reduce(intersect, list(id2018, id2020, id2022)))  # 三年都有

#####################
###年齡觀察:不均勻###
#####################
barplot(table(final$a02z01), main="a02z01 分布", col="lightgreen")
obs <- table(final$a02z01)
obs
k <- length(obs)
n <- sum(obs)
exp <- rep(n/k, k)
exp
chisq.test(x = obs, p = rep(1/k, k))




#####################
### 定義函數#########
#####################
#####################################
##################堆疊長方###########
#####################################
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

#####
plot_with_counts(final$學校種類,group = final$訪問年)
plot_with_counts(df_hs$訪問年)
plot_with_counts(df_uni$訪問年)
plot_with_counts(final$教育排名)
####jt_test 
jt_test <- function(groups, alternative = c("increasing","decreasing")){
  alternative <- match.arg(alternative)
  
  # 總統計量 J
  J <- 0
  n_total <- 0
  
  # 計算每一組之間的比較
  for(i in 1:(length(groups)-1)){
    for(j in (i+1):length(groups)){
      gi <- groups[[i]]
      gj <- groups[[j]]
      for(a in gi){
        for(b in gj){
          if(a < b) J <- J + 1
          if(a == b) J <- J + 0.5
        }
      }
      n_total <- n_total + length(gi)*length(gj)
    }
  }
  
  # 計算期望和標準差
  ni <- sapply(groups, length)
  mu_J <- 0.5 * sum(outer(ni, ni, "*")[lower.tri(outer(ni, ni, "*"))])
  sigma_J <- sqrt(sum(outer(ni, ni, "*")[lower.tri(outer(ni, ni, "*"))])/12)
  
  # Z 值
  Z <- (J - mu_J)/sigma_J
  if(alternative == "decreasing") Z <- -Z
  
  # p 值
  pval <- 2*pnorm(-abs(Z))
  if(alternative == "increasing") pval <- 1 - pnorm(Z)
  if(alternative == "decreasing") pval <- pnorm(Z)
  
  return(list(J = J, Z = Z, p.value = pval))
}

library(tidyverse)
library(ggplot2)
library(viridis)
library(patchwork)  # 用于组合图形


data_multi <- data.frame()
for (t in 1:10) {
  o <- paste0("prediction/JOB6_pred_", t, ".RData")
  load(o)
  
  temp_data <- data.frame(design_type = rep(c("MmOofA", "mMOofA", "UOofA", "UPOofA_AD"), each = 690),
                          rep_t = rep(t, 690*4),
                          error = c(abs(pred_Mm$true_resp-pred_Mm$pred_resp), abs(pred_mMD$true_resp-pred_mMD$pred_resp),
                                    abs(pred_dd$true_resp-pred_dd$pred_resp), abs(pred_ad$true_resp-pred_ad$pred_resp)))
  data_multi  <- rbind(data_multi, temp_data)
}


## 查看数据结构
glimpse(data_multi)


# ============================================================================
# 策略1：聚合ECDF（所有重复合并）
# ============================================================================

p1_aggregated <- ggplot(data_multi, aes(x = error, color = design_type, linetype = design_type)) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("#FF9900", "#339900", "#3333FF", "#FF6666")) +
  scale_linetype_manual(values = c("dashed", "dotdash", "dotted", "solid")) +
  #scale_color_viridis_d(begin = 0.1, end = 0.8) +
  labs(
#    title = "Aggregated ECDF (All 10 Replicates)",
    x = "Absolute Prediction Error",
    y = "Cumulative Probability F(x) = P(Error ≤ x)"
  #  color = "Design Type"
  ) +
#  theme_minimal(base_size = 12) +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.2),
    legend.background = element_rect(fill = "white", color = "gray80"),
  ) +
  # 添加参考线帮助解读
  geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed", alpha = 0.3) 
#  geom_vline(xintercept = seq(1, 15, by = 5), linetype = "dashed", alpha = 0.3)

print(p1_aggregated)



# ============================================================================
# 策略2：分次ECDF 
# ============================================================================

p_simple <- data_multi %>%
  ggplot(aes(x = error, color = design_type, linetype = design_type)) +
  stat_ecdf(geom = "step", size = 0.8, alpha = 0.8) +
  facet_wrap(~ rep_t, ncol = 5, labeller = labeller(rep = ~ paste("Rep", .))) +
  scale_color_manual(values = c("#FF9900", "#339900", "#3333FF", "#FF6666")) +
  scale_linetype_manual(values = c("dashed", "dotdash", "dotted", "solid")) +
  labs(
    x = "Absolute Prediction Error",
    y = "Cumulative Probability",
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90", color = NA)
  )

print(p_simple)


# ============================================================================
# 策略3：分位数分布分析
# ============================================================================

# 3.1 计算每次重复的关键分位数
error_quantiles <- data_multi %>%
  group_by(design_type, rep_t) %>%
  summarise(
    q10 = quantile(error, 0.10),
    q25 = quantile(error, 0.25),
    q50 = quantile(error, 0.50),  # 中位数
    q75 = quantile(error, 0.75),
    q90 = quantile(error, 0.90),
    mean = mean(error),
    .groups = "drop"
  )

# 3.2 绘制分位数分布箱线图
p3_quantiles <- error_quantiles %>%
  pivot_longer(cols = c(q10, q25, q50, q75, q90, mean),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, 
                         levels = c("q10", "q25", "q50", "q75", "q90", "mean"),
                         labels = c("10%", "25%", "50%", "75%", "90%", "Mean"))) %>%
  ggplot(aes(x = metric, y = value, color = design_type)) +
  geom_boxplot(position = position_dodge(0.9), alpha = 0.7) +
  #scale_fill_viridis_d(begin = 0.1, end = 0.8) +
  scale_color_manual(values = c("#FF9900", "#339900", "#3333FF", "#FF6666")) +
  labs(
#    title = "Distribution of Error Quantiles Across 10 Replicates",
    x = "Quantile / Statistic",
    y = "Absolute Prediction Error",
    #color = "Design Type"
  ) +
#  theme_minimal(base_size = 12) +
  theme_bw() +
  theme(
    legend.position = c(0.1, 0.8),
    legend.background = element_rect(fill = "white", color = "gray80")
   # axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

print(p3_quantiles)

# 3.3 重点关注中位数和90%分位数
p3_focus <- error_quantiles %>%
  select(design_type, rep, q50, q90) %>%
  pivot_longer(cols = c(q50, q90), names_to = "quantile", values_to = "error") %>%
  mutate(quantile = ifelse(quantile == "q50", "Median", "90th Percentile")) %>%
  ggplot(aes(x = design_type, y = error, fill = design_type)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1) +
  facet_wrap(~ quantile, scales = "free_y") +
  scale_fill_viridis_d(begin = 0.1, end = 0.8) +
  labs(
    title = "Key Quantiles: Median vs 90th Percentile",
    x = "Design Type",
    y = "Prediction Error"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "gray90"),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

print(p3_focus)




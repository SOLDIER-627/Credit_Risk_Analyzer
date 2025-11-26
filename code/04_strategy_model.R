# 04_strategy_model.R
# 基于违约概率制定信贷策略，并拟合利率-流失率关系

# 加载配置
source("code/00_config.R")


# 函数：加载数据和模型
load_data_and_model <- function() {
  # 加载附件1数据和训练好的模型
  # 返回：数据和模型
  
  cat("正在加载数据和模型...\n")
  
  # 加载附件1数据
  data_file <- paste0("data/processed/", "processed_company_data_with_credit.csv")
  if (!file.exists(data_file)) {
    stop("预处理数据文件不存在: ", data_file, "\n请先运行01_data_preprocessing.R")
  }
  company_data <- read.csv(data_file, fileEncoding = "GBK")
  
  # 加载模型
  model_file <- paste0("results/prediction_model/", "logistic_regression_model.rds")
  if (!file.exists(model_file)) {
    stop("模型文件不存在: ", model_file, "\n请先运行03_prediction_model.R")
  }
  logistic_model <- readRDS(model_file)
  
  cat("数据加载完成\n")
  cat("- 企业数量:", nrow(company_data), "\n")
  
  return(list(data = company_data, model = logistic_model))
}

# 函数：正确读取附件3数据
read_churn_data <- function() {
  # 正确读取附件3的利率-流失率数据
  # 返回：整理后的数据
  
  cat("正在读取附件3数据...\n")
  
  churn_file <- paste0("data/raw/", "附件3：银行贷款年利率与客户流失率关系的统计数据.xlsx")
  if (!file.exists(churn_file)) {
    stop("附件3文件不存在: ", churn_file)
  }
  
  # 读取原始数据，跳过第一行（标题行）
  raw_data <- readxl::read_excel(churn_file, sheet = "Sheet1", skip = 1)
  
  # 检查数据结构并重命名列
  if (ncol(raw_data) >= 4) {
    colnames(raw_data) <- c("利率", "流失率_A", "流失率_B", "流失率_C")
  } else {
    stop("附件3数据列数不正确，期望4列，实际", ncol(raw_data), "列")
  }
  
  # 移除可能的空行
  raw_data <- raw_data[complete.cases(raw_data$利率), ]
  
  # 确保利率列是数值型
  raw_data$利率 <- as.numeric(raw_data$利率)
  
  cat("附件3数据读取成功:\n")
  cat("- 数据点数:", nrow(raw_data), "\n")
  cat("- 利率范围:", min(raw_data$利率), "-", max(raw_data$利率), "\n")
  cat("- 列名:", paste(colnames(raw_data), collapse = ", "), "\n")
  
  return(raw_data)
}

# 函数：拟合利率-流失率关系
fit_churn_rate_models <- function(results_dir) {
  # 拟合附件3中的利率-流失率关系
  # 返回：三个信誉等级的流失率函数
  
  cat("\n=== 拟合利率-流失率关系 ===\n")
  
  # 读取附件3数据
  churn_data <- read_churn_data()
  
  # 检查数据质量
  if (any(is.na(churn_data$利率))) {
    stop("利率数据中存在缺失值")
  }
  
  # 使用多项式回归拟合每个信誉等级的流失率函数
  models <- list()
  
  # 信誉评级A
  fit_A <- lm(流失率_A ~ poly(利率, 3), data = churn_data)
  models$A <- fit_A
  
  # 信誉评级B  
  fit_B <- lm(流失率_B ~ poly(利率, 3), data = churn_data)
  models$B <- fit_B
  
  # 信誉评级C
  fit_C <- lm(流失率_C ~ poly(利率, 3), data = churn_data)
  models$C <- fit_C
  
  # 计算拟合优度
  r_squared <- c(
    A = summary(fit_A)$r.squared,
    B = summary(fit_B)$r.squared,
    C = summary(fit_C)$r.squared
  )
  
  cat("拟合优度 (R²):\n")
  cat("- 信誉评级A:", round(r_squared["A"], 4), "\n")
  cat("- 信誉评级B:", round(r_squared["B"], 4), "\n")
  cat("- 信誉评级C:", round(r_squared["C"], 4), "\n")
  
  # 绘制拟合曲线
  plot_churn_rate_fits(churn_data, models, results_dir)
  
  return(models)
}

# 函数：绘制利率-流失率拟合曲线
plot_churn_rate_fits <- function(churn_data, models, results_dir) {
  # 绘制利率-流失率拟合曲线
  
  # 生成预测用的利率序列
  rate_seq <- seq(0.04, 0.15, length.out = 100)
  
  # 预测各信誉等级的流失率
  pred_A <- predict(models$A, newdata = data.frame(利率 = rate_seq))
  pred_B <- predict(models$B, newdata = data.frame(利率 = rate_seq))
  pred_C <- predict(models$C, newdata = data.frame(利率 = rate_seq))
  
  # 创建绘图数据
  plot_data <- data.frame(
    利率 = rep(rate_seq, 3),
    流失率 = c(pred_A, pred_B, pred_C),
    信誉评级 = rep(c("A", "B", "C"), each = length(rate_seq))
  )
  
  # 原始数据点
  original_data <- data.frame(
    利率 = rep(churn_data$利率, 3),
    流失率 = c(churn_data$流失率_A, churn_data$流失率_B, churn_data$流失率_C),
    信誉评级 = rep(c("A", "B", "C"), each = nrow(churn_data))
  )
  
  # 绘制图形
  p <- ggplot() +
    geom_line(data = plot_data, aes(x = 利率, y = 流失率, color = 信誉评级), 
              size = 1.2) +
    geom_point(data = original_data, aes(x = 利率, y = 流失率, color = 信誉评级),
               size = 2, alpha = 0.7) +
    scale_color_manual(values = c("A" = COLOR_PALETTE[1], 
                                  "B" = COLOR_PALETTE[2], 
                                  "C" = COLOR_PALETTE[4])) +
    labs(title = "银行贷款年利率与客户流失率关系",
         subtitle = "点：原始数据，线：多项式拟合曲线",
         x = "贷款年利率",
         y = "客户流失率",
         color = "信誉评级") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  ggsave(paste0(results_dir, "churn_rate_fitting.png"), 
         p, width = 10, height = 6, dpi = 300)
  cat("利率-流失率拟合图已保存\n")
}

# 函数：预测违约概率
predict_default_probabilities <- function(data, model, results_dir) {
  # 使用模型预测所有企业的违约概率
  # 返回：包含违约概率的数据
  
  cat("\n=== 预测违约概率 ===\n")
  
  # 获取模型使用的变量（排除截距项）
  model_vars <- names(coef(model))[-1]
  
  # 检查数据中是否包含所有需要的变量
  missing_vars <- setdiff(model_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop("数据中缺少模型变量: ", paste(missing_vars, collapse = ", "))
  }
  
  # 预测违约概率
  default_probs <- predict(model, newdata = data, type = "response")
  
  # 添加到数据中
  data$违约概率 <- default_probs
  
  cat("违约概率预测完成\n")
  cat("- 平均违约概率:", round(mean(default_probs), 4), "\n")
  cat("- 违约概率范围:", round(min(default_probs), 4), "-", round(max(default_probs), 4), "\n")
  
  # 详细分析违约概率分布
  cat("\n=== 违约概率详细分析 ===\n")
  cat("违约概率分布:\n")
  prob_summary <- summary(default_probs)
  print(prob_summary)
  
  # 检查极端值
  extreme_probs <- data[default_probs > 0.9 | default_probs < 0.1, 
                        c("企业代号", "信誉评级", "是否违约", "违约概率")]
  
  cat("\n极端违约概率企业 (概率 > 0.9 或 < 0.1):\n")
  if (nrow(extreme_probs) > 0) {
    print(extreme_probs[order(extreme_probs$违约概率, decreasing = TRUE), ])
  } else {
    cat("无极端值\n")
  }
  
  # 保存所有企业的违约概率到CSV文件
  prob_output <- data[, c("企业代号", "企业名称", "信誉评级", "是否违约", "违约概率")]
  
  # 添加模型使用的特征变量
  for (var in model_vars) {
    if (var %in% colnames(data)) {
      prob_output[[var]] <- data[[var]]
    }
  }
  
  # 按违约概率排序
  prob_output <- prob_output[order(prob_output$违约概率, decreasing = TRUE), ]
  
  # 保存到文件
  prob_file <- paste0(results_dir, "all_companies_default_probabilities.csv")
  write.csv(prob_output, prob_file, row.names = FALSE, fileEncoding = "GBK")
  cat("\n所有企业违约概率已保存至:", prob_file, "\n")
  
  # 保存极端概率企业的详细信息
  if (nrow(extreme_probs) > 0) {
    extreme_file <- paste0(results_dir, "extreme_probability_companies.csv")
    write.csv(extreme_probs, extreme_file, row.names = FALSE, fileEncoding = "GBK")
    cat("极端概率企业详情已保存至:", extreme_file, "\n")
  }
  
  return(data)
}

# 函数：计算期望收益
calculate_expected_return <- function(data, churn_models, 
                                      loan_amount_range = c(10, 100),
                                      interest_rate_range = c(0.04, 0.15),
                                      total_budget = 10000) {
  # 计算每个企业的期望收益
  # 参数：贷款参数设置
  
  cat("\n=== 计算期望收益 ===\n")
  cat("贷款参数:\n")
  cat("- 贷款额度范围:", loan_amount_range[1], "-", loan_amount_range[2], "万元\n")
  cat("- 年利率范围:", interest_rate_range[1]*100, "-", interest_rate_range[2]*100, "%\n")
  cat("- 总预算:", total_budget, "万元\n")
  
  # 初始化结果数据框
  results <- data.frame(
    企业代号 = data$企业代号,
    信誉评级 = data$信誉评级,
    违约概率 = data$违约概率,
    贷款额度 = 0,  # 初始化为0
    贷款利率 = 0,  # 初始化为0
    期望收益 = 0   # 初始化为0
  )
  
  # 为不同信誉评级设置基准利率
  base_rates <- c(
    "A" = 0.06,
    "B" = 0.08,
    "C" = 0.12
  )
  
  # 为不同信誉评级设置贷款额度系数
  loan_factors <- c(
    "A" = 1.0,    # 全额贷款
    "B" = 0.8,    # 80%额度
    "C" = 0.6     # 60%额度
  )
  
  # 计算每个企业的贷款额度和利率
  for (i in 1:nrow(results)) {
    rating <- as.character(results$信誉评级[i])
    default_prob <- results$违约概率[i]
    
    # 根据信誉评级和违约概率调整利率
    base_rate <- base_rates[rating]
    risk_adjustment <- default_prob * 0.05  # 违约概率每增加1%，利率增加0.05%
    final_rate <- base_rate + risk_adjustment
    
    # 确保利率在合理范围内
    final_rate <- max(interest_rate_range[1], min(interest_rate_range[2], final_rate))
    
    # 根据信誉评级和违约概率确定贷款额度
    base_amount <- loan_amount_range[1] + 
      (loan_amount_range[2] - loan_amount_range[1]) * loan_factors[rating]
    
    # 违约概率越高，额度越低
    risk_adjustment_amount <- max(0.3, 1 - default_prob * 2)  # 设置最低额度比例
    final_amount <- base_amount * risk_adjustment_amount
    
    # 确保额度在合理范围内
    final_amount <- max(loan_amount_range[1], min(loan_amount_range[2], final_amount))
    
    results$贷款额度[i] <- round(final_amount, 2)
    results$贷款利率[i] <- round(final_rate, 4)
  }
  
  # 计算期望收益（考虑违约风险和客户流失）
  for (i in 1:nrow(results)) {
    rating <- as.character(results$信誉评级[i])
    loan_amount <- results$贷款额度[i]
    interest_rate <- results$贷款利率[i]
    default_prob <- results$违约概率[i]
    
    # 根据利率和信誉评级预测流失率
    churn_model <- churn_models[[rating]]
    churn_prob <- predict(churn_model, newdata = data.frame(利率 = interest_rate))
    churn_prob <- max(0, min(1, churn_prob))  # 确保在0-1范围内
    
    # 计算基础期望收益
    # 如果企业不违约：收益 = 贷款额度 * 利率
    # 如果企业违约：损失 = 贷款额度
    base_expected_return <- loan_amount * interest_rate * (1 - default_prob) - 
      loan_amount * default_prob
    
    # 考虑客户流失后的调整收益
    adjusted_return <- base_expected_return * (1 - churn_prob)
    
    results$期望收益[i] <- round(adjusted_return, 2)
  }
  
  cat("期望收益计算完成\n")
  cat("- 平均期望收益:", round(mean(results$期望收益), 2), "万元\n")
  cat("- 总期望收益:", round(sum(results$期望收益), 2), "万元\n")
  
  return(results)
}

# 函数：优化信贷策略
optimize_credit_strategy <- function(results, total_budget = 10000) {
  # 在预算约束下优化信贷策略
  # 返回：最优的信贷分配方案
  
  cat("\n=== 优化信贷策略 ===\n")
  
  # 计算收益密度（单位贷款的期望收益）
  results$收益密度 <- results$期望收益 / results$贷款额度
  results$收益密度[is.na(results$收益密度)] <- 0
  
  # 按收益密度降序排序
  sorted_results <- results[order(results$收益密度, decreasing = TRUE), ]
  
  # 初始化分配结果
  allocated_budget <- 0
  allocation <- rep(0, nrow(sorted_results))
  
  # 按收益密度从高到低分配贷款
  for (i in 1:nrow(sorted_results)) {
    if (allocated_budget + sorted_results$贷款额度[i] <= total_budget) {
      allocation[i] <- 1
      allocated_budget <- allocated_budget + sorted_results$贷款额度[i]
    } else {
      # 如果预算不足，尝试分配部分额度
      remaining_budget <- total_budget - allocated_budget
      if (remaining_budget >= 10) {  # 最小贷款额度10万元
        allocation[i] <- remaining_budget / sorted_results$贷款额度[i]
        allocated_budget <- total_budget
      }
      break
    }
  }
  
  # 创建最终分配方案
  final_allocation <- sorted_results
  final_allocation$是否放贷 <- ifelse(allocation > 0, "是", "否")
  final_allocation$实际贷款额度 <- round(final_allocation$贷款额度 * allocation, 2)
  final_allocation$实际期望收益 <- round(final_allocation$期望收益 * allocation, 2)
  
  # 筛选出获得贷款的企业
  funded_companies <- final_allocation[final_allocation$是否放贷 == "是", ]
  
  cat("信贷分配结果:\n")
  cat("- 总预算:", total_budget, "万元\n")
  cat("- 已分配预算:", round(sum(funded_companies$实际贷款额度), 2), "万元\n")
  cat("- 预算使用率:", round(sum(funded_companies$实际贷款额度) / total_budget * 100, 2), "%\n")
  cat("- 获得贷款企业数量:", nrow(funded_companies), "\n")
  cat("- 总期望收益:", round(sum(funded_companies$实际期望收益), 2), "万元\n")
  
  return(funded_companies)
}

# 函数：生成策略报告
generate_strategy_report <- function(funded_companies, total_budget, results_dir) {
  # 生成详细的信贷策略报告
  
  cat("\n=== 生成策略报告 ===\n")
  
  report_file <- paste0(results_dir, "credit_strategy_report.txt")
  sink(report_file)
  
  cat("=== 银行信贷策略报告 ===\n\n")
  
  cat("1. 信贷策略概况\n")
  cat("   总预算:", total_budget, "万元\n")
  cat("   已分配预算:", round(sum(funded_companies$实际贷款额度), 2), "万元\n")
  cat("   预算使用率:", round(sum(funded_companies$实际贷款额度) / total_budget * 100, 2), "%\n")
  cat("   获得贷款企业数量:", nrow(funded_companies), "\n")
  cat("   总期望收益:", round(sum(funded_companies$实际期望收益), 2), "万元\n\n")
  
  cat("2. 按信誉评级统计\n")
  rating_summary <- funded_companies %>%
    group_by(信誉评级) %>%
    summarise(
      企业数量 = n(),
      总贷款额度 = sum(实际贷款额度),
      平均利率 = round(mean(贷款利率) * 100, 2),
      总期望收益 = sum(实际期望收益)
    )
  
  for (i in 1:nrow(rating_summary)) {
    cat("   信誉评级", rating_summary$信誉评级[i], ":\n")
    cat("     - 企业数量:", rating_summary$企业数量[i], "\n")
    cat("     - 总贷款额度:", round(rating_summary$总贷款额度[i], 2), "万元\n")
    cat("     - 平均利率:", rating_summary$平均利率[i], "%\n")
    cat("     - 总期望收益:", round(rating_summary$总期望收益[i], 2), "万元\n")
  }
  
  cat("\n3. 风险控制指标\n")
  cat("   平均违约概率:", round(mean(funded_companies$违约概率) * 100, 2), "%\n")
  cat("   最高违约概率:", round(max(funded_companies$违约概率) * 100, 2), "%\n")
  cat("   最低违约概率:", round(min(funded_companies$违约概率) * 100, 2), "%\n\n")
  
  sink()
  
  # 保存详细的分配结果
  write.csv(funded_companies, 
            paste0(results_dir, "credit_allocation_details.csv"),
            row.names = FALSE, fileEncoding = "GBK")
  
  cat("策略报告已保存至:", report_file, "\n")
  cat("详细分配结果已保存\n")
}

# 函数：绘制策略可视化
plot_strategy_visualization <- function(funded_companies, results_dir) {
  # 绘制信贷策略的可视化图表
  
  cat("\n=== 生成策略可视化 ===\n")
  
  # 1. 按信誉评级的贷款分布
  p1 <- ggplot(funded_companies, aes(x = 信誉评级, fill = 信誉评级)) +
    geom_bar() +
    scale_fill_manual(values = c("A" = COLOR_PALETTE[1], 
                                 "B" = COLOR_PALETTE[2], 
                                 "C" = COLOR_PALETTE[4])) +
    labs(title = "按信誉评级的贷款企业数量分布",
         x = "信誉评级",
         y = "企业数量") +
    theme_minimal()
  
  # 2. 贷款额度与违约概率的关系
  p2 <- ggplot(funded_companies, aes(x = 违约概率, y = 实际贷款额度, color = 信誉评级)) +
    geom_point(alpha = 0.7, size = 3) +
    scale_color_manual(values = c("A" = COLOR_PALETTE[1], 
                                  "B" = COLOR_PALETTE[2], 
                                  "C" = COLOR_PALETTE[4])) +
    labs(title = "贷款额度与违约概率关系",
         x = "违约概率",
         y = "贷款额度（万元）") +
    theme_minimal()
  
  # 3. 期望收益分布
  p3 <- ggplot(funded_companies, aes(x = 实际期望收益, fill = 信誉评级)) +
    geom_histogram(alpha = 0.7, bins = 15) +
    scale_fill_manual(values = c("A" = COLOR_PALETTE[1], 
                                 "B" = COLOR_PALETTE[2], 
                                 "C" = COLOR_PALETTE[4])) +
    labs(title = "期望收益分布",
         x = "期望收益（万元）",
         y = "企业数量") +
    theme_minimal()
  
  # 组合图形
  combined_plot <- (p1 | p2) / p3 +
    plot_annotation(title = "信贷策略分析可视化",
                    theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))
  
  ggsave(paste0(results_dir, "strategy_visualization.png"),
         combined_plot, width = 14, height = 10, dpi = 300)
  
  cat("策略可视化图已保存\n")
}

# 主执行流程
cat("开始制定信贷策略...\n\n")

option_list <- list(
  make_option(c("--budget"), type = "numeric", default = 10000, 
              help = "信贷总预算 (万元) [默认: %default]"),
  
  make_option(c("--min_loan"), type = "numeric", default = 10, 
              help = "单笔贷款最小额度 (万元) [默认: %default]"),
  
  make_option(c("--max_loan"), type = "numeric", default = 100, 
              help = "单笔贷款最大额度 (万元) [默认: %default]"),
  
  make_option(c("--min_rate"), type = "numeric", default = 0.04, 
              help = "贷款年利率下限 (小数) [默认: %default]"),
  
  make_option(c("--max_rate"), type = "numeric", default = 0.15, 
              help = "贷款年利率上限 (小数) [默认: %default]")
)

# 解析命令行参数
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

cat("开始制定信贷策略...\n\n")
cat("=== 当前运行参数 ===\n")
cat(sprintf("总预算: %d 万元\n", opt$budget))
cat(sprintf("贷款额度范围: [%d, %d] 万元\n", opt$min_loan, opt$max_loan))
cat(sprintf("利率范围: [%.2f, %.2f]\n", opt$min_rate, opt$max_rate))
cat("======================\n\n")

# 创建输出目录
results_dir <- "results/credit_strategy/"
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

# 1. 加载数据和模型
loaded_data <- load_data_and_model()
company_data <- loaded_data$data
logistic_model <- loaded_data$model

# 2. 拟合利率-流失率关系
churn_models <- fit_churn_rate_models(results_dir)

# 3. 预测违约概率
company_data_with_probs <- predict_default_probabilities(company_data, logistic_model, results_dir)

# 4. 设置贷款参数
# loan_amount_range <- c(10, 100)      # 10-100万元
# interest_rate_range <- c(0.04, 0.15) # 4%-15%
# loan_term <- 1                       # 1年
# total_budget <- 10000                # 1亿元 = 10000万元
loan_amount_range <- c(opt$min_loan, opt$max_loan)      
interest_rate_range <- c(opt$min_rate, opt$max_rate) 
total_budget <- opt$budget        

# 5. 计算期望收益
company_data_with_probs <- company_data_with_probs[company_data_with_probs$信誉评级 %in% c("A", "B", "C"), ]
results_with_returns <- calculate_expected_return(
  company_data_with_probs, 
  churn_models,
  loan_amount_range,
  interest_rate_range,
  total_budget
)

# 6. 优化信贷策略
funded_companies <- optimize_credit_strategy(results_with_returns, total_budget)

# 7. 生成报告和可视化
generate_strategy_report(funded_companies, total_budget, results_dir)
plot_strategy_visualization(funded_companies, results_dir)

cat("\n=== 信贷策略制定完成 ===\n")
cat("主要输出文件:\n")
cat("- churn_rate_fitting.png: 利率-流失率拟合图\n")
cat("- strategy_visualization.png: 策略可视化图\n")
cat("- credit_strategy_report.txt: 策略报告\n")
cat("- credit_allocation_details.csv: 详细分配结果\n")
cat("\n策略总结:\n")
cat("- 获得贷款企业:", nrow(funded_companies), "家\n")
cat("- 总贷款额度:", round(sum(funded_companies$实际贷款额度), 2), "万元\n")
cat("- 总期望收益:", round(sum(funded_companies$实际期望收益), 2), "万元\n")
cat("- 预算使用率:", round(sum(funded_companies$实际贷款额度) / total_budget * 100, 2), "%\n")
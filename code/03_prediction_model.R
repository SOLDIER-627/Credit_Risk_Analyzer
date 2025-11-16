# 03_prediction_model.R
# 构建预测模型：使用逻辑回归预测企业违约概率

# 加载配置
source("code/00_config.R")

# 函数：加载预处理数据和相关性结果
load_data_and_correlations <- function() {
  # 加载附件1处理后的数据和相关性分析结果
  # 返回：数据和相关性结果
  
  cat("正在加载数据和相关性结果...\n")
  
  # 加载附件1数据
  data_file <- paste0("data/processed/", "processed_company_data_with_credit.csv")
  if (!file.exists(data_file)) {
    stop("预处理数据文件不存在: ", data_file, "\n请先运行01_data_preprocessing.R")
  }
  company_data <- read.csv(data_file, fileEncoding = "GBK")
  
  # 加载相关性结果
  cor_file <- paste0("results/correlation_analysis/", "detailed_correlation_results.csv")
  if (!file.exists(cor_file)) {
    stop("相关性结果文件不存在: ", cor_file, "\n请先运行02_correlation_analysis.R")
  }
  cor_results <- read.csv(cor_file, fileEncoding = "GBK")
  
  cat("数据加载完成\n")
  cat("- 企业数量:", nrow(company_data), "\n")
  cat("- 相关性变量数量:", nrow(cor_results), "\n")
  
  return(list(data = company_data, cor_results = cor_results))
}

# 函数：选择建模变量
select_modeling_variables <- function(data, cor_results, top_n = 8) {
  # 根据相关性选择建模变量
  # 参数：data - 数据, cor_results - 相关性结果, top_n - 选择前n个变量
  # 返回：选择的变量名和数据子集
  
  cat("\n=== 选择建模变量 ===\n")
  
  # 选择相关性最高的top_n个变量（排除目标变量本身）
  top_vars <- cor_results %>%
    filter(变量 != "是否违约数值") %>%
    arrange(desc(绝对值)) %>%
    head(top_n) %>%
    pull(变量)
  
  cat("选择的前", top_n, "个变量:\n")
  for (i in seq_along(top_vars)) {
    cor_value <- cor_results$相关系数[cor_results$变量 == top_vars[i]]
    cat(i, ". ", top_vars[i], " (r = ", round(cor_value, 4), ")\n", sep = "")
  }
  
  # 检查这些变量是否都在数据中
  missing_vars <- setdiff(top_vars, colnames(data))
  if (length(missing_vars) > 0) {
    warning("以下变量在数据中缺失: ", paste(missing_vars, collapse = ", "))
    top_vars <- setdiff(top_vars, missing_vars)
  }
  
  # 创建建模数据集（包含目标变量和选择的特征）
  modeling_vars <- c("是否违约数值", top_vars)
  modeling_data <- data[, modeling_vars, drop = FALSE]
  
  # 移除有缺失值的行
  complete_cases <- complete.cases(modeling_data)
  modeling_data <- modeling_data[complete_cases, ]
  
  cat("最终建模数据集:\n")
  cat("- 样本数量:", nrow(modeling_data), "\n")
  cat("- 特征数量:", ncol(modeling_data) - 1, "\n")
  cat("- 违约率:", round(mean(modeling_data$是否违约数值) * 100, 2), "%\n")
  
  return(list(variables = top_vars, data = modeling_data))
}

# 函数：分割训练集和测试集
split_train_test <- function(data, test_size = 0.2) {
  # 分割数据为训练集和测试集
  # 参数：data - 数据, test_size - 测试集比例
  # 返回：训练集和测试集
  
  cat("\n=== 分割训练集和测试集 ===\n")
  
  set.seed(100)  # 确保可重复性
  
  # 创建分割索引
  train_index <- createDataPartition(data$是否违约数值, 
                                     p = 1 - test_size, 
                                     list = FALSE)
  
  # 分割数据
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  cat("训练集: ", nrow(train_data), "个样本 (", 
      round(nrow(train_data)/nrow(data)*100, 1), "%)\n", sep = "")
  cat("测试集: ", nrow(test_data), "个样本 (", 
      round(nrow(test_data)/nrow(data)*100, 1), "%)\n", sep = "")
  cat("训练集违约率:", round(mean(train_data$是否违约数值) * 100, 2), "%\n")
  cat("测试集违约率:", round(mean(test_data$是否违约数值) * 100, 2), "%\n")
  
  return(list(train = train_data, test = test_data))
}

# 函数：训练逻辑回归模型
train_logistic_model <- function(train_data, variables) {
  # 训练逻辑回归模型
  # 参数：train_data - 训练数据, variables - 特征变量
  # 返回：训练好的模型
  
  cat("\n=== 训练逻辑回归模型 ===\n")
  
  # 构建公式
  formula_str <- paste("是否违约数值 ~", paste(variables, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  cat("模型公式:", formula_str, "\n")
  
  # 训练逻辑回归模型
  model <- glm(formula_obj, 
               data = train_data, 
               family = binomial(link = "logit"))
  
  # 显示模型摘要
  cat("\n模型系数:\n")
  coef_summary <- summary(model)$coefficients
  print(round(coef_summary, 4))
  
  # 检查多重共线性
  if (length(variables) > 1) {
    cat("\n多重共线性诊断 (VIF):\n")
    vif_values <- car::vif(model)
    print(round(vif_values, 2))
  }
  
  return(model)
}

# 函数：模型预测和评估
evaluate_model <- function(model, test_data, results_dir) {
  # 在测试集上评估模型性能
  # 参数：model - 模型, test_data - 测试数据, results_dir - 结果目录
  # 返回：评估结果
  
  cat("\n=== 模型评估 ===\n")
  
  # 预测概率
  predictions_prob <- predict(model, newdata = test_data, type = "response")
  
  # 使用0.5作为分类阈值
  predictions_class <- ifelse(predictions_prob > 0.5, 1, 0)
  
  # 计算评估指标
  actual <- test_data$是否违约数值
  conf_matrix <- table(预测 = predictions_class, 实际 = actual)
  
  # 计算各种指标
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # 计算AUC-ROC
  roc_obj <- roc(actual, predictions_prob)
  auc_value <- auc(roc_obj)
  
  # 打印结果
  cat("混淆矩阵:\n")
  print(conf_matrix)
  
  cat("\n性能指标:\n")
  cat("- 准确率:", round(accuracy, 4), "\n")
  cat("- 精确率:", round(precision, 4), "\n")
  cat("- 召回率:", round(recall, 4), "\n")
  cat("- F1分数:", round(f1_score, 4), "\n")
  cat("- AUC-ROC:", round(auc_value, 4), "\n")
  
  # 绘制ROC曲线
  p_roc <- ggroc(roc_obj, color = COLOR_PALETTE[1], size = 1) +
    geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "gray") +
    labs(title = paste0("ROC曲线 (AUC = ", round(auc_value, 3), ")"),
         x = "1 - 特异度",
         y = "敏感度") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0(results_dir, "roc_curve.png"), 
         p_roc, width = 6, height = 5, dpi = 300)
  
  # 绘制概率分布图
  prob_data <- data.frame(
    概率 = predictions_prob,
    实际标签 = factor(actual, levels = c(0, 1), labels = c("未违约", "违约"))
  )
  
  p_dist <- ggplot(prob_data, aes(x = 概率, fill = 实际标签)) +
    geom_histogram(alpha = 0.7, position = "identity", bins = 20) +
    scale_fill_manual(values = c("未违约" = COLOR_PALETTE[1], "违约" = COLOR_PALETTE[4])) +
    labs(title = "预测概率分布",
         x = "违约概率",
         y = "频数",
         fill = "实际标签") +
    theme_minimal()
  
  ggsave(paste0(results_dir, "probability_distribution.png"), 
         p_dist, width = 8, height = 5, dpi = 300)
  
  return(list(
    predictions_prob = predictions_prob,
    predictions_class = predictions_class,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1_score = f1_score,
    auc = auc_value,
    conf_matrix = conf_matrix,
    roc_obj = roc_obj
  ))
}

# 函数：模型解释和特征重要性
interpret_model <- function(model, variables, results_dir) {
  # 解释模型结果和特征重要性
  # 参数：model - 模型, variables - 特征变量, results_dir - 结果目录
  
  cat("\n=== 模型解释 ===\n")
  
  # 获取系数
  coef_summary <- summary(model)$coefficients
  coef_df <- data.frame(
    变量 = rownames(coef_summary),
    系数 = round(coef_summary[, "Estimate"], 4),
    标准误 = round(coef_summary[, "Std. Error"], 4),
    z值 = round(coef_summary[, "z value"], 4),
    p值 = round(coef_summary[, "Pr(>|z|)"], 4),
    显著性 = ifelse(coef_summary[, "Pr(>|z|)"] < 0.05, "显著", "不显著")
  )
  
  # 绘制特征重要性图（基于系数绝对值）
  importance_df <- coef_df[-1, ]  # 排除截距项
  importance_df$重要性 <- abs(importance_df$系数)
  importance_df <- importance_df[order(importance_df$重要性, decreasing = TRUE), ]
  
  p_importance <- ggplot(importance_df, aes(x = reorder(变量, 重要性), y = 重要性, 
                                            fill = 显著性)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("显著" = COLOR_PALETTE[4], "不显著" = COLOR_PALETTE[1])) +
    coord_flip() +
    labs(title = "特征重要性（基于系数绝对值）",
         x = "变量",
         y = "系数绝对值") +
    theme_minimal()
  
  ggsave(paste0(results_dir, "feature_importance.png"), 
         p_importance, width = 8, height = 6, dpi = 300)
  
  # 保存系数结果
  write.csv(coef_df, 
            paste0(results_dir, "model_coefficients.csv"),
            row.names = FALSE, fileEncoding = "GBK")
  
  cat("模型系数已保存\n")
  cat("特征重要性图已保存\n")
  
  return(coef_df)
}

# 函数：生成模型报告
generate_model_report <- function(model_results, eval_results, coef_df, results_dir) {
  # 生成详细的模型报告
  # 参数：各种模型结果
  
  cat("\n=== 生成模型报告 ===\n")
  
  report_file <- paste0(results_dir, "prediction_model_report.txt")
  sink(report_file)
  
  cat("=== 企业违约预测模型报告 ===\n\n")
  cat("分析时间:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  cat("1. 模型概况\n")
  cat("   模型类型: 逻辑回归\n")
  cat("   特征数量:", length(model_results$variables), "\n")
  cat("   训练集样本:", nrow(model_results$train_data), "\n")
  cat("   测试集样本:", nrow(model_results$test_data), "\n\n")
  
  cat("2. 模型性能\n")
  cat("   准确率:", round(eval_results$accuracy, 4), "\n")
  cat("   精确率:", round(eval_results$precision, 4), "\n")
  cat("   召回率:", round(eval_results$recall, 4), "\n")
  cat("   F1分数:", round(eval_results$f1_score, 4), "\n")
  cat("   AUC-ROC:", round(eval_results$auc, 4), "\n\n")
  
  cat("3. 重要特征系数\n")
  sig_coef <- coef_df[coef_df$显著性 == "显著" & coef_df$变量 != "(Intercept)", ]
  if (nrow(sig_coef) > 0) {
    for (i in 1:min(5, nrow(sig_coef))) {
      cat("   ", sig_coef$变量[i], ": ", sig_coef$系数[i], 
          " (p = ", sig_coef$p值[i], ")\n", sep = "")
    }
  } else {
    cat("   无显著特征\n")
  }
  
  sink()
  
  cat("模型报告已保存至:", report_file, "\n")
}

# 主执行流程
cat("开始构建预测模型...\n\n")

# 创建输出目录
results_dir <- "results/prediction_model/"

# 1. 加载数据和相关性结果
loaded_data <- load_data_and_correlations()
company_data <- loaded_data$data
cor_results <- loaded_data$cor_results

# 2. 选择建模变量
modeling_vars <- select_modeling_variables(company_data, cor_results, top_n = 9)
selected_variables <- modeling_vars$variables
modeling_data <- modeling_vars$data

# 3. 分割训练集和测试集
split_data <- split_train_test(modeling_data, test_size = 0.2)
train_data <- split_data$train
test_data <- split_data$test

# 4. 训练逻辑回归模型
logistic_model <- train_logistic_model(train_data, selected_variables)

# 5. 模型评估
eval_results <- evaluate_model(logistic_model, test_data, results_dir)

# 6. 模型解释
coef_results <- interpret_model(logistic_model, selected_variables, results_dir)

# 7. 生成报告
generate_model_report(
  model_results = list(
    variables = selected_variables,
    train_data = train_data,
    test_data = test_data
  ),
  eval_results = eval_results,
  coef_df = coef_results,
  results_dir = results_dir
)

# 8. 保存模型
saveRDS(logistic_model, paste0(results_dir, "logistic_regression_model.rds"))
cat("\n模型已保存至:", paste0(results_dir, "logistic_regression_model.rds"), "\n")

cat("\n=== 预测模型构建完成 ===\n")
cat("主要输出文件:\n")
cat("- roc_curve.png: ROC曲线\n")
cat("- probability_distribution.png: 概率分布图\n")
cat("- feature_importance.png: 特征重要性图\n")
cat("- model_coefficients.csv: 模型系数\n")
cat("- prediction_model_report.txt: 模型报告\n")
cat("- logistic_regression_model.rds: 训练好的模型\n")
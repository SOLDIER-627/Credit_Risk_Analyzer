import streamlit as st
import pandas as pd
from pathlib import Path

# === 路径配置 ===
# 当前文件所在的目录：.../web
CURRENT_DIR = Path(__file__).resolve().parent
# 项目根目录：.../
ROOT_DIR = CURRENT_DIR.parent
# 结果文件所在目录：.../results
RESULTS_DIR = ROOT_DIR / "results"
RESULTS_CORRELATION_ANALYSIS_DIR = RESULTS_DIR / "correlation_analysis"
RESULTS_CREDIT_STRATEGY_DIR = RESULTS_DIR / "credit_strategy"
RESULTS_PREDICTION_MODEL_DIR = RESULTS_DIR / "prediction_model"

# =============== 一些通用的小工具函数 ===============

def load_csv(path: str):
    p = Path(path)
    if not p.exists():
        st.warning(f"找不到数据文件：{path}（请确认文件是否与 app.py 在同一目录下）")
        return None

    encodings_to_try = ["utf-8", "utf-8-sig", "gbk", "gb2312", "ansi", "latin1"]

    last_err = None
    for enc in encodings_to_try:
        try:
            df = pd.read_csv(p, encoding=enc)
            st.caption(f"（已使用编码 `{enc}` 成功读取 {path}）")
            return df
        except UnicodeDecodeError as e:
            last_err = e
            continue
        except Exception as e:
            last_err = e
            continue

    st.error(f"读取 {path} 失败，尝试的编码有：{encodings_to_try}，最后一个错误：{last_err}")
    return None


def load_txt(path: str):
    """安全读取 txt 报告。"""
    p = Path(path)
    if not p.exists():
        st.warning(f"找不到报告文件：{path}")
        return None
    try:
        return p.read_text(encoding="utf-8", errors="ignore")
    except Exception as e:
        st.error(f"读取 {path} 失败：{e}")
        return None


def show_image(path: str, caption: str = "", use_column_width=True):
    """安全显示图片。"""
    p = Path(path)
    if not p.exists():
        st.warning(f"找不到图片文件：{path}")
        return
    st.image(str(p), caption=caption, use_column_width=use_column_width)


# =============== 页面内容函数 ===============

def page_overview():
    st.title("中小微企业信贷决策分析与建模")
    st.subheader("—— 基于违约预测与资源分配的综合策略研究")

    st.markdown(
        """
        ### 一、项目背景

        - 银行在向 **中小微企业** 发放贷款时面临较大的违约风险；
        - 题目提供了企业的发票数据、信誉评级、历史违约情况以及利率与客户流失关系；
        - 我们希望通过 **数据分析与建模**，为银行设计一套兼顾“收益”和“风险”的信贷决策方案。

        ### 二、研究目标

        1. 对企业发票等数据进行清洗与特征构造，得到企业层面的关键指标；
        2. 分析各指标与违约之间的关系，识别重要风险因子；
        3. 建立企业违约概率预测模型（逻辑回归）；
        4. 在预算约束与利率–流失率关系下，设计信贷资源分配策略。
        """
    )

    st.markdown("### 三、项目整体流程示意")
    show_image(
        "strategy_visualization.png",
        caption="信贷策略与整体流程可视化（示意）"
    )

    # st.markdown(
    #     """
    #     ### 四、页面导航说明
    #
    #     通过左侧侧边栏可以切换查看：
    #
    #     - **项目概览**：背景、目标与整体框架；
    #     - **数据与预处理**：原始数据说明与特征构造步骤；
    #     - **相关性分析**：各变量与违约关系的可视化与统计结果；
    #     - **违约预测模型**：模型结构、性能指标及特征重要性；
    #     - **信贷资源分配策略**：在预算约束下的放贷策略结果与简单交互演示；
    #     - **总结与展望**：关键结论与后续改进方向。
    #     """
    # )


def page_data_preprocess():
    st.header("数据与预处理")

    st.markdown(
        """
        ### 1. 数据来源说明

        - **附件 1**：企业发票及相关交易数据；
        - **附件 2**：企业信誉评级、是否违约等信息；
        - **附件 3**：不同贷款利率对应的客户流失率关系。

        在本项目中，我们基于发票与企业信息构造了企业层面的聚合指标，
        如：**总营收、总支出、毛利润、运营规模、发票数量** 等，并与信誉评级、违约标签进行合并，构建建模数据集。
        """
    )

    st.markdown("### 2. 预处理主要步骤（示意）")
    st.markdown(
        """
        1. **剔除作废发票**：仅保留有效发票记录；
        2. **按企业聚合**：
           - 累计销项金额 → 总营收；
           - 累计进项金额 → 总支出；
           - 总营收 − 总支出 → 毛利润；
           - 发票金额 / 数量等构造运营规模相关指标；
        3. **合并企业信息**：将聚合后的数据与企业信誉评级、是否违约信息进行合并；
        4. **处理缺失与异常值**：对极端值、缺失值进行合理处理；
        5. **划分数据集**：构建建模所需的特征矩阵与标签。
        """
    )

    st.markdown("### 3. 企业层面数据示例")

    # 这里你可以替换为你最终用于建模的企业级 CSV 文件名
    df_example = load_csv(RESULTS_CREDIT_STRATEGY_DIR / "all_companies_default_probabilities.csv")

    if df_example is not None:
        st.caption("下表展示若干企业的示例数据（前 10 行）：")
        st.dataframe(df_example.head(10))
    else:
        st.info("可以将最终用于建模的企业级数据导出为 CSV，并在这里展示前几行示例。")


def page_correlation():
    st.header("相关性分析")

    st.markdown(
        """
        ### 1. 分析目的

        - 探索各企业指标与 **是否违约** 之间的相关关系；
        - 识别对违约风险影响较大的 **关键变量**；
        - 为后续特征选择与模型构建提供依据。
        """
    )

    st.markdown("### 2. 相关性热力图")
    show_image(
        "comprehensive_correlation_heatmap.png",
        caption="主要特征之间及与违约的相关性热力图"
    )

    st.markdown("### 3. 与违约相关性排序条形图")
    show_image(
        "default_correlation_bars.png",
        caption="各变量与违约变量的相关性（示意）"
    )

    st.markdown("### 4. 详细相关性与统计检验结果")

    df_corr = load_csv(RESULTS_CORRELATION_ANALYSIS_DIR / "detailed_correlation_results.csv")
    if df_corr is not None:
        st.subheader("4.1 相关性结果（节选）")
        st.dataframe(df_corr.head(15))

    df_stat = load_csv(RESULTS_CORRELATION_ANALYSIS_DIR / "statistical_test_results.csv")
    if df_stat is not None:
        st.subheader("4.2 统计检验结果（节选）")
        st.dataframe(df_stat.head(15))

    report = load_txt(RESULTS_CORRELATION_ANALYSIS_DIR / "correlation_analysis_report.txt")
    if report is not None:
        st.subheader("4.3 分析结论摘要")
        with st.expander("展开查看文字版分析结论"):
            st.write(report)


def page_model():
    st.header("违约预测模型（逻辑回归）")

    st.markdown(
        """
        ### 1. 模型思路

        - 选用 **逻辑回归模型** 对企业是否违约进行预测；
        - 自变量为企业的各类特征（营收、支出、毛利润、发票数量、信誉评级等）；
        - 因变量为是否违约（0/1）；
        - 输出为 **企业违约概率**，为后续信贷策略提供风险量化依据。
        """
    )

    st.markdown("### 2. 模型性能指标与 ROC 曲线")
    show_image(
        "roc_curve.png",
        caption="违约预测模型 ROC 曲线"
    )

    show_image(
        "probability_distribution.png",
        caption="企业违约概率分布示意"
    )

    st.markdown("### 3. 特征重要性与模型系数")

    show_image(
        "feature_importance.png",
        caption="模型中特征重要性排序（示意）"
    )

    show_image(
        "important_variables_comparison.png",
        caption="部分关键变量重要性对比（示意）"
    )

    df_coef = load_csv(RESULTS_PREDICTION_MODEL_DIR / "model_coefficients.csv")
    if df_coef is not None:
        st.subheader("3.1 模型系数（节选）")
        st.dataframe(df_coef.head(20))

    report = load_txt(RESULTS_PREDICTION_MODEL_DIR / "prediction_model_report.txt")
    if report is not None:
        st.subheader("3.2 模型评估文字报告（摘要）")
        with st.expander("展开查看模型评估报告"):
            st.write(report)


def page_strategy():
    st.header("信贷资源分配策略")

    st.markdown(
        """
        ### 1. 策略设计思路

        - 根据模型输出的 **违约概率**，评估每家企业的风险水平；
        - 结合企业 **信誉评级**、贷款额度上限以及总预算约束；
        - 引入附件 3 的 **利率–客户流失率** 关系，在收益与客户留存之间取得平衡；
        - 通过优化或规则设定，得到一套 **贷款发放与定价策略**。
        """
    )

    st.markdown("### 2. 策略结果整体可视化")
    show_image(
        "credit_rating_analysis.png",
        caption="不同信誉评级企业的违约情况与放贷策略示意"
    )

    show_image(
        "churn_rate_fitting.png",
        caption="利率与客户流失率关系拟合曲线（示意）"
    )

    show_image(
        "strategy_visualization.png",
        caption="整体信贷资源分配策略可视化（示意）"
    )

    st.markdown("### 3. 信贷分配结果数据（分层展示）")

    df_alloc = load_csv(RESULTS_CREDIT_STRATEGY_DIR / "credit_allocation_details.csv")
    if df_alloc is not None:
        st.subheader("3.1 原始分配结果（节选）")
        st.dataframe(df_alloc.head(20))

        # 尝试按“评级”列分组（列名可能需要你根据实际数据修改）
        candidate_rating_cols = ["rating", "信用等级", "评级", "credit_rating"]
        rating_col = None
        for c in candidate_rating_cols:
            if c in df_alloc.columns:
                rating_col = c
                break

        if rating_col is not None:
            st.subheader("3.2 按信誉等级汇总结果（总额统计）")

            # 尝试寻找常见的金额/收益字段
            sum_cols = [c for c in df_alloc.columns if any(
                key in c.lower()
                for key in ["loan", "amount", "额度", "放贷", "credit", "收益", "profit"]
            )]

            if sum_cols:
                grouped = df_alloc.groupby(rating_col)[sum_cols].sum()
                st.dataframe(grouped)
            else:
                st.info(
                    "没有自动识别到金额/收益相关列，请根据你的数据列名手动修改代码中的汇总逻辑。"
                )
        else:
            st.info(
                # TODO:  没有找到明显代表“信誉评级”的列名（例如rating / 信用等级 / 评级 / credit_rating），请根据你的实际列名修改代码中candidate_rating_cols列表。
                "没有找到明显代表“信誉评级”的列名（例如 rating / 信用等级 / 评级 / credit_rating），"
                "请根据你的实际列名修改代码中 candidate_rating_cols 列表。"
            )

    df_extreme = load_csv(RESULTS_CREDIT_STRATEGY_DIR / "extreme_probability_companies.csv")
    if df_extreme is not None:
        st.subheader("3.3 极端违约概率企业（节选）")
        st.dataframe(df_extreme.head(20))

    st.markdown("### 4. 简单交互：违约概率阈值 & 放贷规模（演示）")

    df_prob = load_csv(RESULTS_CREDIT_STRATEGY_DIR / "all_companies_default_probabilities.csv")
    if df_prob is not None:
        # 尝试找到违约概率、贷款金额字段
        prob_col_candidates = ["default_prob", "违约概率", "prob_default", "p_default"]
        loan_col_candidates = ["loan_amount", "贷款额度", "credit_amount", "amount"]

        prob_col = None
        for c in prob_col_candidates:
            if c in df_prob.columns:
                prob_col = c
                break

        loan_col = None
        for c in loan_col_candidates:
            if c in df_prob.columns:
                loan_col = c
                break

        if prob_col is not None:
            st.write("你可以通过调整违约概率阈值，感受“风险控制”与“放贷规模”的变化：")
            thr = st.slider(
                "违约概率阈值（低于该值视为可放贷）",
                min_value=0.0,
                max_value=1.0,
                value=0.5,
                step=0.01,
            )

            lend_df = df_prob[df_prob[prob_col] <= thr]

            st.write(f"在当前阈值下，可放贷企业数量：**{len(lend_df)}**")

            if loan_col is not None:
                total_loan = lend_df[loan_col].sum()
                st.write(f"对应总放贷额度：**{total_loan:,.2f}**")
            else:
                st.info("未识别到贷款额度字段，仅展示企业数量。")

            st.caption("下表为当前阈值下部分可放贷企业（前 20 行）：")
            st.dataframe(lend_df.head(20))
        else:
            # TODO: 未识别到贷款额度字段，仅展示企业数量。
            st.info("未识别到违约概率字段（例如 default_prob / 违约概率），请根据实际数据修改 prob_col_candidates。")

    report = load_txt(RESULTS_CREDIT_STRATEGY_DIR / "credit_strategy_report.txt")
    if report is not None:
        st.subheader("5. 策略效果分析报告（摘要）")
        with st.expander("展开查看信贷策略评估报告"):
            st.write(report)


def page_summary():
    st.header("总结与展望")

    st.markdown(
        """
        ### 1. 主要工作回顾

        1. **数据预处理与特征构造**  
           - 从发票数据与企业信息出发，构造了总营收、总支出、毛利润、运营规模等企业级指标；
           - 合并信誉评级与违约标签，得到建模所需的数据集。

        2. **相关性分析与特征筛选**  
           - 通过相关性分析与统计检验，识别出与违约关系密切的关键变量；
           - 为模型输入特征的选择提供了依据。

        3. **违约预测模型构建**  
           - 使用逻辑回归模型估计企业违约概率；
           - 在验证集上取得了较好的区分能力（通过 ROC 曲线、AUC 等指标进行评估）。

        4. **信贷资源分配策略设计**  
           - 将预测的违约概率与企业信誉评级、预算约束结合；
           - 引入利率–流失率关系，设计在收益与风险之间折衷的信贷决策方案。
        """
    )

    st.markdown(
        """
        ### 2. 关键发现

        - 信誉评级对违约概率具有显著影响，是最重要的风险因子之一；
        - 部分财务指标（如总营收、毛利润）与违约风险呈显著负相关；
        - 在合理设定违约阈值与利率的情况下，可以在控制总体违约率的前提下，显著提升预期收益。
        """
    )

    st.markdown(
        """
        ### 3. 后续改进方向

        - 尝试引入更多模型：如随机森林、XGBoost 等，对比不同模型性能；
        - 增加行业特征、宏观经济指标等，提升模型的稳定性与可解释性；
        - 考虑时间维度，构建动态更新的风险监测与信贷策略调整机制；
        - 将模型部署到业务系统中，实现自动化、可视化的风控决策支持。
        """
    )

    st.success("感谢观看！欢迎老师和同学提出宝贵意见与建议。")


# =============== 主程序入口 ===============

def main():
    st.set_page_config(
        page_title="中小微企业信贷决策分析与建模",
        layout="wide"
    )

    st.sidebar.title("页面导航")
    page = st.sidebar.radio(
        "请选择要查看的内容：",
        (
            "项目概览",
            "数据与预处理",
            "相关性分析",
            "违约预测模型",
            "信贷资源分配策略",
            "总结与展望",
        )
    )

    st.sidebar.markdown("---")

    # TODO: 写上名字学号
    st.sidebar.write("作者：回头填上\n\n课程：统计分析与建模\n")

    if page == "项目概览":
        page_overview()
    elif page == "数据与预处理":
        page_data_preprocess()
    elif page == "相关性分析":
        page_correlation()
    elif page == "违约预测模型":
        page_model()
    elif page == "信贷资源分配策略":
        page_strategy()
    elif page == "总结与展望":
        page_summary()


if __name__ == "__main__":
    main()

# Meta-analysis 数据处理与结果解读要点

本文档总结了 `Validation_Report.xlsx` 与整合输出文件中的各类 sheet 的含义与解读方法，
并以 **p=0.1** 为示例说明。

---

## 一、Summary 类 Sheet 解读

### 1. Indicator_Summary
- **作用**：列出所有指标在不同 SD 填补比例 (p 值) 下的随机效应结果。
- **关键列说明**：
  - **Indicator**：指标名称（如 SOC、DOC、MBC）。
  - **p**：填补 SD 的比例（0.1 表示用 Mean 的 10% 补齐缺失 SD）。
  - **k**：进入合成的研究数。
  - **RE_mean_lnRR**：随机效应 (DL) 模型下的平均效应量（lnRR）。
  - **RE_CI95_L / RE_CI95_U**：95% 置信区间。
  - **I2_%**：异质性比例 I²。
  - **tau2**：研究间方差 τ²。
  - **Q**：Cochran’s Q 统计量。
  - **tau2_REML / REML_mean_lnRR** 等：如果开启 REML，会显示 REML 结果。
- **解读要点**：
  - 看 CI 是否跨 0：不跨 0 表示显著。
  - 横向比较不同 p 值下的结果是否一致，以判断稳健性。

### 2. Summary_p0_1（示例）
- **作用**：给出 **p=0.1** 时所有指标的总体效应结果。
- **关键列说明**：与 Indicator_Summary 相同，但固定在 p=0.1。
- **解读要点**：
  - 用作主分析结果参考。
  - 先看 k 是否足够（<10 代表样本不足，不建议做发表偏倚）。
  - 再看 lnRR 与 95%CI 是否显著。

### 3. Interpretation
- **作用**：自动生成文字解读。
- **生成逻辑**：
  - CI 跨 0 → “效应不显著”；否则指明“提高/降低”。
  - I² <25% → 低；25–50% → 中等；50–75% → 偏高；>75% → 很高。
  - 根据 k 与 I² 给出建议：是否需要分层、元回归、补充样本。
- **解读要点**：作为预实验阶段快速判断的参考，不是最终结论。

---

## 二、DataChecks Sheet 解读

DataChecks 主要用于追踪数据质量。常见检查项：

1. **MissingColumns**  
   - 缺失的必需列名。  
   - **处理**：在原始表格中补齐。

2. **DemoRowsExcluded_StudyID==0**  
   - 被剔除的示例行数（StudyID=0）。  
   - **处理**：不影响分析，正式数据要保证 StudyID≠0。

3. **NonPositiveOrMissing_Mean_E / Mean_C**  
   - 实验组/对照组均值 ≤0 或缺失的行数。  
   - **影响**：lnRR 无法计算。  
   - **处理**：回查原文献，补录或统一单位。

4. **NonPositiveOrMissing_n_E / n_C**  
   - 样本量 ≤0 或缺失。  
   - **影响**：方差 v 无法计算。  
   - **处理**：补充样本量或剔除。

5. **DuplicateRows_onKey**  
   - 基于复合键（Indicator, StudyID, Author_Year, Substitution_ratio(%), Organic fertilizer type, Year, Soil depth(cm)）判定的重复。  
   - **影响**：重复计权。  
   - **处理**：核查后删除或增加区分列。

6. **SD_Imputed_E / C / Total**  
   - 通过 SD= p×Mean 补齐的行数。  
   - **影响**：填补比例过高会降低结果稳健性。  
   - **处理**：敏感性分析（多 p 值）。

7. **Invalid_lnRR_rows_after_fill**  
   - 在补 SD 后仍无法计算 lnRR 的行数。  
   - **影响**：这些行被剔除，减少样本量。  
   - **处理**：回查均值 ≤0 或缺失。

8. **Invalid_v_rows_after_fill_(nan_or<=0)**  
   - 在补 SD 后仍无法计算有效 v 的行数。  
   - **影响**：这些行无法进入加权合成。  
   - **处理**：补齐 SD、n，或考虑换效应量形式（如 SMD）。

---

## 三、森林图在预实验阶段的处理

- **主要用途**：直观检查异常值、权重极端的研究。  
- **是否需要优化规范**：
  - 不必过度美化（颜色、排版等），预实验阶段主要看方向与异常。  
  - 但建议统一一些基本规范：
    - 横轴方向：lnRR > 0 表示增加，< 0 表示降低。
    - 横轴范围合理，避免极端值压缩主区间。
    - 研究标签保持一致（如 “StudyID, Author_Year, Substitution_ratio(%)”）。
- **正式分析阶段**：再优化图形规范（配色、线条、分组汇总）。

---

## 四、读表顺序建议

1. **Summary_p0_1**：快速看主分析结果。  
2. **Indicator_Summary**：横向比较不同 p 的一致性。  
3. **Interpretation**：结合自动解读判断下一步。  
4. **DataChecks**：排查数据问题，决定是否需要补录或清洗。  
5. **MetaEffects_<IND>_pX.xlsx**：研究级数据回溯，核查异常值与权重。

---

# Install and load required packages
if (!require("forestplot")) install.packages("forestplot")
if (!require("dplyr")) install.packages("dplyr")
if (!require("metafor")) install.packages("metafor")
if (!require("writexl")) install.packages("writexl")

library(forestplot)
library(dplyr)
library(metafor)
library(writexl)

# ----------------------------------------------------
# STEP 1: Load and clean data
# ----------------------------------------------------

# Read your CSV file
meta_data <- read.csv("meta_auc.csv", stringsAsFactors = FALSE)

# Clean up whitespace
meta_data <- meta_data %>%
  mutate(
    Study = trimws(Study),
    PredictorType = trimws(PredictorType),
    ModelType = trimws(ModelType),
    MainValidationType = trimws(MainValidationType),
    ValidationMethod = trimws(ValidationMethod),
    StudyDesign = trimws(StudyDesign)
  )

# Calculate Standard Error
meta_data$SE <- (meta_data$UpperCI - meta_data$LowerCI) / (2 * 1.96)

# ----------------------------------------------------
# STEP 2: Perform meta-analysis (Random and Fixed Effects)
# ----------------------------------------------------

# Random effects model (REML)
random_model <- rma(yi = AUC, sei = SE, data = meta_data, method = "REML")

# === PI added (18May2026) ===
pred_random <- predict(random_model)

# (Optional) Print to console
print(random_model)
print(pred_random)

# Fixed effects model (FE)
fixed_model <- rma(yi = AUC, sei = SE, data = meta_data, method = "FE")

# Extract heterogeneity statistics
I2 <- random_model$I2
tau2 <- random_model$tau2
Q_stat <- random_model$QE
Q_pval <- random_model$QEp

# Statistics
heterogeneity_text <- sprintf(" I² = %.1f%%  |  τ² = %.4f  |  Cochran's Q = %.2f (p = %.4f)", 
                              I2, tau2, Q_stat, Q_pval)

# ----------------------------------------------------
# STEP 3: Prepare forest plot data 
# ----------------------------------------------------

# Select columns from original data
meta_data_selected <- meta_data %>%
  select(Study, AUC, LowerCI, UpperCI, PredictorType, ModelType) %>%
  mutate(
    auc_formatted = sprintf("%.3f", AUC),
    ci_formatted = sprintf("[%.3f; %.3f]", LowerCI, UpperCI),
    is_summary = FALSE
  ) %>%
  arrange(AUC)

# Create random effects summary row
random_summary_row <- data.frame(
  Study = "OVERALL SUMMARY (Random Effects)",
  AUC = as.numeric(random_model$b),
  LowerCI = random_model$ci.lb,
  UpperCI = random_model$ci.ub,
  PredictorType = "All",
  ModelType = "All",
  auc_formatted = sprintf("%.3f", random_model$b),
  ci_formatted = sprintf("[%.3f; %.3f]", random_model$ci.lb, random_model$ci.ub),
  is_summary = TRUE,
  stringsAsFactors = FALSE
)

# Create fixed effects summary row
fixed_summary_row <- data.frame(
  Study = "OVERALL SUMMARY (Fixed Effects)",
  AUC = as.numeric(fixed_model$b),
  LowerCI = fixed_model$ci.lb,
  UpperCI = fixed_model$ci.ub,
  PredictorType = "All",
  ModelType = "All",
  auc_formatted = sprintf("%.3f", fixed_model$b),
  ci_formatted = sprintf("[%.3f; %.3f]", fixed_model$ci.lb, fixed_model$ci.ub),
  is_summary = TRUE,
  stringsAsFactors = FALSE
)

# Create a SINGLE heterogeneity row
heterogeneity_row <- data.frame(
  Study = heterogeneity_text,
  AUC = NA,
  LowerCI = NA,
  UpperCI = NA,
  PredictorType = "",
  ModelType = "",
  auc_formatted = "",
  ci_formatted = "",
  is_summary = TRUE,
  stringsAsFactors = FALSE
)

# Combine all rows
plot_data <- rbind(meta_data_selected, random_summary_row, fixed_summary_row, heterogeneity_row)

# ----------------------------------------------------
# STEP 4: Create vectors and label text
# ----------------------------------------------------

# Create vectors for forestplot
mean_vector <- c(NA, plot_data$AUC)
lower_vector <- c(NA, plot_data$LowerCI)
upper_vector <- c(NA, plot_data$UpperCI)

# Create labeltext matrix
labeltext <- cbind(
  c("Study", plot_data$Study),
  c("AUC", plot_data$auc_formatted),
  c("95% CI", plot_data$ci_formatted),
  c("Predictor Type", plot_data$PredictorType),
  c("Model Type", plot_data$ModelType)
)

# Identify summary rows
is_summary <- c(TRUE, plot_data$is_summary)

# Identify heterogeneity row (last row) for italic styling
heterogeneity_row_index <- nrow(plot_data)
row_fontface <- rep(1, nrow(plot_data) + 1)
row_fontface[heterogeneity_row_index + 1] <- 3  # Italic for heterogeneity row

# ----------------------------------------------------
# STEP 5: Create base forest plot
# ----------------------------------------------------

# Create base plot
base_fp <- forestplot(
  labeltext = labeltext,
  mean = mean_vector,
  lower = lower_vector,
  upper = upper_vector,
  
  title = "Forest Plot: Overall Pooled Performance of Models for Breast Cancer Risk Prediction",
  xlab = "Area Under Curve (AUC)",
  zero = 0.5,
  clip = c(0.5, 1.0),
  graph.pos = 3,
  is.summary = is_summary,
  boxsize = 0.15,
  lineheight = "auto",
  colgap = unit(6, "mm"),
  col = fpColors(box = "hotpink3", line = "lightsteelblue4", summary = "red4", zero = "deeppink3"),
  txt_gp = fpTxtGp(
    label = gpar(cex = 0.7, fontface = row_fontface),
    summary = gpar(cex = 0.8, fontface = "bold"),
    xlab = gpar(cex = 1, fontface = "bold"),
    title = gpar(cex = 1.1, fontface = "bold")
  ),
  xticks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
  grid = TRUE,
  fn.ci_norm = fpDrawNormalCI,
  fn.ci_sum = fpDrawSummaryCI,
  lty.ci = 1,
  lwd.ci = 1.5,
  summary = gpar(lwd = 2, col = "red4")
)

# ----------------------------------------------------
# STEP 6: Apply pipe formatting with lines
# ----------------------------------------------------

# Save as PDF
pdf("forest_plot_pipe_format.pdf", width = 14, height = 12)

# Apply formatting using pipe operator WITH lines
final_plot <- base_fp %>%
  fp_add_lines(
    "2" = gpar(lty = 1),  # Line after row 1
    "31" = gpar(lwd = 1, col = "black")  # Line before summary
  ) %>%
  fp_set_style(
    box = "hotpink3",
    line = "lightsteelblue4",
    summary = "red4"
  ) %>%
  fp_set_zebra_style("#f9f9f9")

# Print the plot
print(final_plot)
dev.off()

cat(" Pipe format forest plot saved as 'forest_plot_pipe_format.pdf'\n")

# Save as PNG
png("forest_plot_pipe_format.png", width = 14, height = 12, units = "in", res = 300)

final_plot <- base_fp %>%
  fp_add_lines(
    "2" = gpar(lty = 1),  # Line after row 1
    "31" = gpar(lwd = 1, col = "black")  # Line before summary
  ) %>%
  fp_set_style(
    box = "hotpink3",
    line = "lightsteelblue4",
    summary = "red4"
  ) %>%
  fp_set_zebra_style("#f9f9f9")

print(final_plot)
dev.off()

cat(" Pipe format forest plot saved as 'forest_plot_pipe_format.png'\n")

# Also display the plot in RStudio
print(final_plot)

# ----------------------------------------------------
# STEP 7: Export results to Excel format
# ----------------------------------------------------

# Create a data frame with heterogeneity statistics
heterogeneity_stats <- data.frame(
  Statistic = c("I²", "τ²", "Cochran's Q", "Q-test p-value"),
  Value = c(
    sprintf("%.1f%%", I2),
    sprintf("%.4f", tau2),
    sprintf("%.2f", Q_stat),
    sprintf("%.4f", Q_pval)
  ),
  stringsAsFactors = FALSE
)

# Create a data frame with random effects model results
random_effects_results <- data.frame(
  Parameter = c("Estimate (AUC)", "Standard Error", "CI Lower", "CI Upper", "p-value"),
  Value = c(
    sprintf("%.4f", random_model$b),
    sprintf("%.4f", random_model$se),
    sprintf("%.4f", random_model$ci.lb),
    sprintf("%.4f", random_model$ci.ub),
    sprintf("%.4f", random_model$pval)
  ),
  stringsAsFactors = FALSE
)

# Create a data frame with fixed effects model results
fixed_effects_results <- data.frame(
  Parameter = c("Estimate (AUC)", "Standard Error", "CI Lower", "CI Upper", "p-value"),
  Value = c(
    sprintf("%.4f", fixed_model$b),
    sprintf("%.4f", fixed_model$se),
    sprintf("%.4f", fixed_model$ci.lb),
    sprintf("%.4f", fixed_model$ci.ub),
    sprintf("%.4f", fixed_model$pval)
  ),
  stringsAsFactors = FALSE
)

# Create a data frame with individual study results
study_results <- meta_data_selected %>%
  select(Study, AUC, LowerCI, UpperCI) %>%
  mutate(
    AUC = sprintf("%.4f", AUC),
    LowerCI = sprintf("%.4f", LowerCI),
    UpperCI = sprintf("%.4f", UpperCI),
    `95% CI` = sprintf("[%.4f; %.4f]", as.numeric(LowerCI), as.numeric(UpperCI))
  ) %>%
  select(Study, AUC, `95% CI`)

# Create a list of data frames for Excel export
results_list <- list(
  "Heterogeneity_Statistics" = heterogeneity_stats,
  "Random_Effects_Model" = random_effects_results,
  "Fixed_Effects_Model" = fixed_effects_results,
  "Individual_Study_Results" = study_results,
  "Overall_Summary" = data.frame(
    Metric = c("Random Effects AUC", "Fixed Effects AUC"),
    Value = c(
      sprintf("%.4f (95%% CI: %.4f-%.4f)", random_model$b, random_model$ci.lb, random_model$ci.ub),
      sprintf("%.4f (95%% CI: %.4f-%.4f)", fixed_model$b, fixed_model$ci.lb, fixed_model$ci.ub)
    )
  )
)

# Export to Excel file
write_xlsx(results_list, "meta_analysis_results.xlsx")

cat("\n=============================================\n")
cat("Results exported to Excel file: 'meta_analysis_results.xlsx'\n")
cat("The file contains the following sheets:\n")
cat("  - Heterogeneity_Statistics\n")
cat("  - Random_Effects_Model\n")
cat("  - Fixed_Effects_Model\n")
cat("  - Individual_Study_Results\n")
cat("  - Overall_Summary\n")
cat("=============================================\n\n")

# Also save the plot data for reference
plot_data_for_export <- plot_data %>%
  select(Study, auc_formatted, ci_formatted, PredictorType, ModelType)

write_xlsx(list("Forest_Plot_Data" = plot_data_for_export), "forest_plot_data.xlsx")
cat("Forest plot data exported to: 'forest_plot_data.xlsx'\n")

cat("\n All outputs generated successfully!\n")
cat("Files created:\n")
cat("  - forest_plot_pipe_format.pdf (with lines and italic heterogeneity)\n")
cat("  - forest_plot_pipe_format.png (with lines and italic heterogeneity)\n")
cat("  - meta_analysis_results.xlsx (Excel results)\n")
cat("  - forest_plot_data.xlsx (plot data)\n")


#COOOOPPPYY
# ------------------------------------------------------
# SENSITIVITY ANALYSIS #4: EXCLUDE OUTLIERS (IQR RULE)
# Applied to the FULL dataset (29 studies, meta_auc.csv)
# WITH EXCEL EXPORT AND EXPLICIT LIST OF REMOVED STUDIES
# ------------------------------------------------------

# Load required libraries
library(metafor)
library(dplyr)
library(writexl)

# Load FULL data (all 29 studies)
meta_full <- read.csv("meta_auc.csv", stringsAsFactors = FALSE)

# Calculate SE if not already present
meta_full$SE <- (meta_full$UpperCI - meta_full$LowerCI) / (2 * 1.96)

# ------------------------------------------------------
# RUN ORIGINAL MAIN META-ANALYSIS (for comparison)
# ------------------------------------------------------

random_model_main <- rma(yi = AUC, sei = SE, data = meta_full, method = "REML")
fixed_model_main <- rma(yi = AUC, sei = SE, data = meta_full, method = "FE")

# ------------------------------------------------------
# IQR OUTLIER DETECTION
# ------------------------------------------------------

# Calculate IQR bounds
Q1 <- quantile(meta_full$AUC, 0.25, na.rm = TRUE)
Q3 <- quantile(meta_full$AUC, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

# Identify outliers
meta_full$is_outlier <- meta_full$AUC < lower_bound | meta_full$AUC > upper_bound
meta_full$outlier_direction <- ifelse(meta_full$AUC < lower_bound, "Low outlier",
                                      ifelse(meta_full$AUC > upper_bound, "High outlier", "Not outlier"))

# ------------------------------------------------------
# DISPLAY OUTLIERS IN CONSOLE (with clear formatting)
# ------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("   OUTLIER DETECTION USING IQR RULE\n")
cat("   Applied to FULL dataset (29 studies)\n")
cat("============================================================\n\n")

cat("--- IQR BOUNDS ---\n")
cat(sprintf("  Q1 (25th percentile):  %.4f\n", Q1))
cat(sprintf("  Q3 (75th percentile):  %.4f\n", Q3))
cat(sprintf("  IQR:                   %.4f\n", IQR_val))
cat(sprintf("  Lower bound (Q1 - 1.5*IQR): %.4f\n", lower_bound))
cat(sprintf("  Upper bound (Q3 + 1.5*IQR): %.4f\n\n", upper_bound))

# Get outliers
outliers_list <- meta_full[meta_full$is_outlier, ]

if (nrow(outliers_list) > 0) {
  cat("--- STUDIES REMOVED (OUTLIERS) ---\n")
  cat(sprintf("  Total outliers identified: %d\n\n", nrow(outliers_list)))
  
  for (i in 1:nrow(outliers_list)) {
    cat(sprintf("  [%d] %s\n", i, outliers_list$Study[i]))
    cat(sprintf("      AUC = %.3f (95%% CI: %.3f-%.3f)\n", 
                outliers_list$AUC[i], outliers_list$LowerCI[i], outliers_list$UpperCI[i]))
    cat(sprintf("      Direction: %s\n", outliers_list$outlier_direction[i]))
    cat("\n")
  }
} else {
  cat("--- STUDIES REMOVED (OUTLIERS) ---\n")
  cat("  No outliers identified.\n\n")
}

# ------------------------------------------------------
# LIST STUDIES THAT REMAIN (for transparency)
# ------------------------------------------------------

studies_kept <- meta_full[!meta_full$is_outlier, ]

cat("--- STUDIES KEPT (after outlier removal) ---\n")
cat(sprintf("  Total studies kept: %d\n\n", nrow(studies_kept)))
cat("  List of kept studies:\n")
for (i in 1:nrow(studies_kept)) {
  cat(sprintf("    %s (AUC = %.3f)\n", studies_kept$Study[i], studies_kept$AUC[i]))
}
cat("\n")

# ------------------------------------------------------
# RUN META-ANALYSIS WITHOUT OUTLIERS
# ------------------------------------------------------

# Create filtered dataset (exclude outliers)
meta_full_no_outliers <- meta_full %>% filter(!is_outlier)

# Run meta-analysis without outliers
sens_outliers_model <- rma(yi = AUC, sei = SE, data = meta_full_no_outliers, method = "REML")
sens_outliers_fe_model <- rma(yi = AUC, sei = SE, data = meta_full_no_outliers, method = "FE")

# ------------------------------------------------------
# DISPLAY META-ANALYSIS RESULTS IN CONSOLE
# ------------------------------------------------------

cat("============================================================\n")
cat("   META-ANALYSIS RESULTS\n")
cat("============================================================\n\n")

cat("--- MAIN ANALYSIS (FULL DATASET, 29 studies) ---\n")
cat(sprintf("  Studies (k): %d\n", nrow(meta_full)))
cat(sprintf("  Random Effects AUC: %.3f (95%% CI: %.3f-%.3f)\n", 
            random_model_main$b, random_model_main$ci.lb, random_model_main$ci.ub))
cat(sprintf("  Fixed Effects AUC:  %.3f (95%% CI: %.3f-%.3f)\n", 
            fixed_model_main$b, fixed_model_main$ci.lb, fixed_model_main$ci.ub))
cat(sprintf("  I² = %.1f%%, τ² = %.4f\n\n", random_model_main$I2, random_model_main$tau2))

cat("--- SENSITIVITY #4: AFTER OUTLIER REMOVAL (IQR RULE) ---\n")
cat(sprintf("  Studies removed: %d\n", sum(meta_full$is_outlier)))
cat(sprintf("  Remaining studies (k): %d\n", nrow(meta_full_no_outliers)))
cat(sprintf("  Random Effects AUC: %.3f (95%% CI: %.3f-%.3f)\n", 
            sens_outliers_model$b, sens_outliers_model$ci.lb, sens_outliers_model$ci.ub))
cat(sprintf("  Fixed Effects AUC:  %.3f (95%% CI: %.3f-%.3f)\n", 
            sens_outliers_fe_model$b, sens_outliers_fe_model$ci.lb, sens_outliers_fe_model$ci.ub))
cat(sprintf("  I² = %.1f%%, τ² = %.4f\n", sens_outliers_model$I2, sens_outliers_model$tau2))

# ------------------------------------------------------
# EXPORT RESULTS TO EXCEL
# ------------------------------------------------------

# Prepare data frames for Excel export
if (nrow(outliers_list) > 0) {
  outliers_export <- data.frame(
    Removed_Study = outliers_list$Study,
    AUC = outliers_list$AUC,
    CI_Lower = outliers_list$LowerCI,
    CI_Upper = outliers_list$UpperCI,
    Outlier_Direction = outliers_list$outlier_direction,
    Reason_for_Removal = sprintf("AUC %.3f is outside IQR bounds [%.3f, %.3f]", 
                                 outliers_list$AUC, lower_bound, upper_bound),
    IQR_Lower_Bound = lower_bound,
    IQR_Upper_Bound = upper_bound
  )
} else {
  outliers_export <- data.frame(
    Message = "No outliers identified using IQR rule",
    IQR_Lower_Bound = lower_bound,
    IQR_Upper_Bound = upper_bound
  )
}

# List of kept studies
kept_export <- data.frame(
  Kept_Study = studies_kept$Study,
  AUC = studies_kept$AUC,
  CI_Lower = studies_kept$LowerCI,
  CI_Upper = studies_kept$UpperCI
)

# Comparison of results (main vs without outliers)
comparison_export <- data.frame(
  Analysis = c("Main analysis (all 29 studies)", 
               "Sensitivity #4: Without outliers (IQR rule)"),
  Studies_k = c(nrow(meta_full), nrow(meta_full_no_outliers)),
  Studies_Removed = c(0, sum(meta_full$is_outlier)),
  RE_AUC = c(sprintf("%.3f", random_model_main$b), sprintf("%.3f", sens_outliers_model$b)),
  RE_CI_Lower = c(sprintf("%.3f", random_model_main$ci.lb), sprintf("%.3f", sens_outliers_model$ci.lb)),
  RE_CI_Upper = c(sprintf("%.3f", random_model_main$ci.ub), sprintf("%.3f", sens_outliers_model$ci.ub)),
  RE_I_Squared = c(sprintf("%.1f%%", random_model_main$I2), sprintf("%.1f%%", sens_outliers_model$I2)),
  RE_Tau_Squared = c(sprintf("%.4f", random_model_main$tau2), sprintf("%.4f", sens_outliers_model$tau2)),
  FE_AUC = c(sprintf("%.3f", fixed_model_main$b), sprintf("%.3f", sens_outliers_fe_model$b)),
  FE_CI_Lower = c(sprintf("%.3f", fixed_model_main$ci.lb), sprintf("%.3f", sens_outliers_fe_model$ci.lb)),
  FE_CI_Upper = c(sprintf("%.3f", fixed_model_main$ci.ub), sprintf("%.3f", sens_outliers_fe_model$ci.ub))
)

# IQR parameters export
iqr_parameters_export <- data.frame(
  Parameter = c("Q1 (25th percentile)", "Q3 (75th percentile)", "IQR", 
                "Lower bound (Q1 - 1.5*IQR)", "Upper bound (Q3 + 1.5*IQR)"),
  Value = c(sprintf("%.4f", Q1), sprintf("%.4f", Q3), sprintf("%.4f", IQR_val),
            sprintf("%.4f", lower_bound), sprintf("%.4f", upper_bound))
)

# Create a list of data frames to export
export_list <- list(
  "IQR_Parameters" = iqr_parameters_export,
  "Studies_Removed_Outliers" = outliers_export,
  "Studies_Kept_After_Removal" = kept_export,
  "Comparison_Main_vs_No_Outliers" = comparison_export,
  "Full_Dataset_with_Outlier_Flag" = meta_full[, c("Study", "AUC", "LowerCI", "UpperCI", "is_outlier", "outlier_direction")]
)

# Write to Excel
write_xlsx(export_list, "outlier_analysis_full_dataset.xlsx")

cat("\n")
cat("============================================================\n")
cat("   EXPORT COMPLETE\n")
cat("============================================================\n")
cat("✅ Results exported to 'outlier_analysis_full_dataset.xlsx'\n\n")
cat("   Excel file contains the following sheets:\n")
cat("     1. IQR_Parameters - bounds used for outlier detection\n")
cat("     2. Studies_Removed_Outliers - list of studies removed\n")
cat("     3. Studies_Kept_After_Removal - list of studies kept\n")
cat("     4. Comparison_Main_vs_No_Outliers - main vs sensitivity results\n")
cat("     5. Full_Dataset_with_Outlier_Flag - all studies with flags\n")
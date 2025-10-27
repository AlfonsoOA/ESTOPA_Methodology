# ==============================================================================
# ESTOPA ANALYSIS SCRIPT
# ==============================================================================

# 0. INITIAL CONFIGURATION AND DATA LOADING
# ------------------------------------------------------------------------------
# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstatix) 
library(ggpubr)
library(stringr) 
library(psych) 
library(scales) 

# --- DEFINE PATHS ---
base_path <- "C:/Users/Alfonso/Desktop/AlfonsoOA_MSI/2Docencia/8b_Trabajo-ESTOPA/0_JCE" 
data_file <- file.path(base_path, "2_SupportingInformation2_TestData.xlsx") # Achievement data
bloom_file <- file.path(base_path, "BloomData.xlsx") # Bloom map
survey_file <- file.path(base_path, "1_SupportingInformation1_SurveyResults_ESTOPA.xlsx") # Survey results
output_dir <- file.path(base_path, "ESTOPA_analysis") # Output folder for plots and reports

# Create the output folder if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  cat(paste("Output folder created at:", output_dir, "\n"))
}

# Load data
cat("\n--- 0. LOADING DATA ---\n")
tryCatch({
  data_achievements <- read_excel(data_file)
  bloom_map_raw <- read_excel(bloom_file)
  data_survey_raw <- read_excel(survey_file) 
}, error = function(e) {
  cat(paste("CRITICAL ERROR: Could not load one or more Excel files. Please verify the base path and file names. Error:", e$message, "\n"))
  stop("Execution stopped due to file loading errors.")
})


# ==============================================================================
# PART 1: ACHIEVEMENT AND BLOOM ANALYSIS
# ==============================================================================
cat("\n--- 1. ACHIEVEMENT AND BLOOM ANALYSIS ---\n")

# 1. PRE-PROCESSING AND DATA CLEANING (ACHIEVEMENTS)
data_achievements$AcademicYear <- as.factor(data_achievements$AcademicYear)
data_achievements <- data_achievements %>%
  mutate(ESTOPA_Group = ifelse(AcademicYear == "2024/2025", "ESTOPA (2024/2025)", "Pre-ESTOPA")) %>%
  mutate(across(starts_with("Q"), as.numeric),
         Call = as.numeric(Call))

# B. Preparing the Bloom map
bloom_map <- bloom_map_raw %>%
  rename(bloom_level_full = `Bloom classification`) %>%
  mutate(bloom_level = str_extract(bloom_level_full, "^B[1-6]")) %>%
  mutate(
    AcademicYear = sub("_(1|2)\\.Q.*", "", Code),
    Call = as.numeric(sub(".*_([1|2])\\..*", "\\1", Code)),
    question = str_extract(Code, "Q[0-9]+")
  ) %>%
  select(AcademicYear, Call, question, bloom_level)

# C. Creating the long format and dynamic join
data_long <- data_achievements %>%
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "question",
    values_to = "correct"
  ) %>%
  filter(!is.na(correct)) %>%
  left_join(bloom_map, by = c("AcademicYear", "Call", "question")) %>%
  filter(!is.na(bloom_level)) %>% 
  mutate(bloom_level = factor(bloom_level, levels = c("B1", "B2", "B3", "B4", "B5", "B6"))) %>%
  mutate(bloom_level = droplevels(bloom_level))


# 2. QUANTITATIVE ANALYSIS (Global achievements)
summary_estopa_means <- data_achievements %>%
  group_by(ESTOPA_Group) %>%
  summarise(
    Mean_MarkRelated = mean(MarkRelated, na.rm = TRUE),
    SD_MarkRelated = sd(MarkRelated, na.rm = TRUE),
    N = n(),
    .groups = 'drop'
  )
test_mw_estopa <- wilcox.test(MarkRelated ~ ESTOPA_Group, data = data_achievements)
d_cohen_global <- cohens_d(MarkRelated ~ ESTOPA_Group, data = data_achievements)$effsize

cor_segmented <- data_achievements %>%
  group_by(ESTOPA_Group) %>%
  do(spearman = cor.test(.$MarkRelated, .$ExamMarks, method = "spearman")) %>%
  mutate(rho = spearman$estimate, p.value = spearman$p.value) %>% 
  select(ESTOPA_Group, rho, p.value)

annual_percentage <- data_achievements %>%
  group_by(AcademicYear) %>%
  summarise(
    `% Correct` = mean(MarkRelated, na.rm = TRUE) * 10,
    SD = sd(MarkRelated, na.rm = TRUE) * 10,
    N = n(),
    .groups = 'drop'
  )


# 3. ANALYSIS BY BLOOM LEVEL
data_bloom_trend <- data_long %>%
  group_by(AcademicYear, bloom_level) %>%
  summarise(
    mean_correct = mean(correct, na.rm = TRUE),
    sd_correct = sd(correct, na.rm = TRUE),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    SEM_correct = sd_correct / sqrt(N),
    `%_Correct` = mean_correct * 100,
    `%_SEM` = SEM_correct * 100
  )

test_mw_bloom <- data_long %>%
  group_by(bloom_level) %>%
  filter(sum(ESTOPA_Group == "ESTOPA (2024/2025)") >= 2 & 
           sum(ESTOPA_Group == "Pre-ESTOPA") >= 2) %>% 
  wilcox_test(correct ~ ESTOPA_Group, detailed = TRUE) %>%
  select(bloom_level, p, method) %>%
  rename(`p-value` = p)

summary_bloom_means <- data_long %>%
  group_by(ESTOPA_Group, bloom_level) %>%
  summarise(Mean = mean(correct, na.rm = TRUE) * 100, N = n(), .groups = 'drop') %>%
  pivot_wider(names_from = ESTOPA_Group, values_from = c(Mean, N), values_fill = NA) %>%
  right_join(test_mw_bloom, by = "bloom_level") %>%
  mutate(Difference = `Mean_ESTOPA (2024/2025)` - `Mean_Pre-ESTOPA`) %>%
  select(bloom_level, starts_with("Mean"), starts_with("N_"), Difference, `p-value`)


# 4. GENERATION AND SAVING OF PLOTS (ACHIEVEMENTS)
# ... (Plot generation code 01, 02, 03) ...

cat("\n--- 4. GENERATING AND SAVING ACHIEVEMENT PLOTS ---\n")

plot_estopa_bars <- ggplot(summary_estopa_means, aes(x = ESTOPA_Group, y = Mean_MarkRelated)) +
  geom_bar(stat = "identity", fill = c("#66c2a5", "#fc8d62"), alpha = 0.8) +
  geom_errorbar(aes(ymin = Mean_MarkRelated - SD_MarkRelated, ymax = Mean_MarkRelated + SD_MarkRelated), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(
    title = "Mean Achievement Score (0-10) by Group",
    x = "Application Group",
    y = "Mean Score (MarkRelated, 0-10)",
    caption = paste("P-value (Mann-Whitney U Test):", round(test_mw_estopa$p.value, 4), 
                    "| Cohen's d:", round(d_cohen_global, 2))
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 10))

ggsave(file.path(output_dir, "01_Bars_ESTOPA_vs_PreESTOPA.png"), plot_estopa_bars, width = 6, height = 5)

plot_bloom_trends <- ggplot(data_bloom_trend, aes(x = AcademicYear, y = `%_Correct`, group = bloom_level, color = bloom_level)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = `%_Correct` - `%_SEM`, ymax = `%_Correct` + `%_SEM`), width = 0.2) + 
  labs(
    title = "Trend of Mean Percentage of Correct Answers by Bloom Level",
    x = "Academic Year",
    y = "Mean Percentage of Correct Answers (%)",
    color = "Bloom Level"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100))

ggsave(file.path(output_dir, "02_Trend_Bloom_Annual.png"), plot_bloom_trends, width = 8, height = 6)

plot_bloom_boxplot <- data_long %>% 
  ggplot(aes(x = bloom_level, y = correct, fill = ESTOPA_Group)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Correct Answers by Bloom Level and ESTOPA Group",
    x = "Bloom Level",
    y = "Correct Answer (0 = Incorrect, 1 = Correct)",
    fill = "Group"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 1))

ggsave(file.path(output_dir, "03_Boxplot_Bloom_Achievements.png"), plot_bloom_boxplot, width = 7, height = 5)


# ==============================================================================
# PART 2: ACTIVITY PERCEPTION ANALYSIS (Survey)
# ==============================================================================

cat("\n--- 6. ACTIVITY PERCEPTION ANALYSIS ---\n")

# 6.1. PRE-PROCESSING AND DIMENSION DEFINITION
# ------------------------------------------------------------------------------
# 1. Definition of dimensions (Q1 and Q10 excluded from dimensions)
dimension_map <- list(
  TLP = c("Q2", "Q4", "Q9"), # Teaching and Learning Process
  ASA = c("Q6", "Q7"),      # Activity Structure and Suitability
  AC = c("Q3", "Q5", "Q8")  # Activity Characteristics
)

# Mapping table for the join
question_dimension_map <- bind_rows(
  data.frame(question = dimension_map$TLP, Dimension = "TLP_Score"),
  data.frame(question = dimension_map$ASA, Dimension = "ASA_Score"),
  data.frame(question = dimension_map$AC, Dimension = "AC_Score")
)


# 2. Creating the Long Format for INDIVIDUAL QUESTIONS
data_survey_long <- data_survey_raw %>%
  select(starts_with("Q")) %>%
  # Filter only the questions relevant to the 3 main dimensions PLUS Q1
  select(all_of(c(unique(question_dimension_map$question), "Q1"))) %>% 
  mutate(across(everything(), as.numeric)) %>%
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "question",
    values_to = "Score"
  ) %>%
  filter(!is.na(Score)) %>%
  # Add the dimension to each question (Q1 will result in NA for Dimension)
  left_join(question_dimension_map, by = "question")

# 3. Calculation of Cronbach's Alpha for the report 
data_survey_alpha <- data_survey_raw %>%
  select(starts_with("Q")) %>% 
  mutate(across(starts_with("Q"), as.numeric)) %>%
  select(-Q1, -Q10)

alpha_results <- list()
for (dim_name in names(dimension_map)) {
  q_cols <- dimension_map[[dim_name]]
  data_subset <- data_survey_alpha %>% select(all_of(q_cols))
  data_subset_complete <- data_subset[complete.cases(data_subset), ]
  
  if (ncol(data_subset_complete) > 1 && nrow(data_subset_complete) > 1) {
    alpha_result <- psych::alpha(data_subset_complete, check.keys = FALSE) 
    alpha_results[[dim_name]] <- alpha_result$total$raw_alpha
  } else {
    alpha_results[[dim_name]] <- NA
  }
}


# 6.2. CALCULATION OF RELATIVE FREQUENCIES BY RANGE (PER QUESTION)
# ------------------------------------------------------------------------------

# Define range limits and labels
breaks <- c(0, 2.5, 5.0, 7.5, 10.1) 
labels <- c("0-2.49", "2.5-4.99", "5-7.49", "7.5-10.0")

# Create score ranges and calculate relative frequency per QUESTION
# Filter to only include the dimension questions (excluding Q1)
data_survey_ranges_individual <- data_survey_long %>%
  filter(!is.na(Dimension)) %>% 
  mutate(Range = cut(
    Score,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = FALSE 
  )) %>%
  group_by(Dimension, question, Range, .drop = FALSE) %>%
  summarise(
    N = n(),
    .groups = 'drop_last'
  ) %>%
  mutate(
    Total_Q = sum(N),
    Relative_Freq = N / Total_Q
  ) %>%
  ungroup() %>%
  filter(Total_Q > 0)


# 6.3. GENERATION OF BAR PLOTS BY DIMENSION (INDIVIDUAL QUESTIONS)
# ------------------------------------------------------------------------------

cat("\n--- 7. GENERATING AND SAVING PERCEPTION PLOTS BY QUESTION ---\n")

# Reorder questions within each dimension for the X-axis
order_tlp <- c("Q2", "Q4", "Q9")
order_asa <- c("Q6", "Q7")
order_ac <- c("Q3", "Q5", "Q8")

dim_titles <- c(
  "TLP_Score" = "Perception: Teaching and Learning Process (TLP) by Question (Q2, Q4, Q9)",
  "ASA_Score" = "Perception: Activity Structure and Suitability (ASA) by Question (Q6, Q7)",
  "AC_Score" = "Perception: Activity Characteristics (AC) by Question (Q3, Q5, Q8)"
)
dim_file_names <- c(
  "TLP_Score" = "04_Bars_Perception_TLP_Questions.png",
  "ASA_Score" = "05_Bars_Perception_ASA_Questions.png",
  "AC_Score" = "06_Bars_Perception_AC_Questions.png"
)

# Iteration to generate and save the 3 plots
for (dim_code in names(dim_titles)) {
  plot_data <- data_survey_ranges_individual %>% filter(Dimension == dim_code)
  
  alpha_key <- sub("_Score", "", dim_code)
  
  if (nrow(plot_data) == 0) {
    cat(paste("Warning: No data for dimension", dim_code, ". Skipping plot generation.\n"))
    next
  }
  
  # Define the specific question order and label for the current dimension
  if (dim_code == "TLP_Score") {
    q_order <- order_tlp
    x_axis_label <- "Question (Q2, Q4, Q9)"
  } else if (dim_code == "ASA_Score") {
    q_order <- order_asa
    x_axis_label <- "Question (Q6, Q7)"
  } else { # AC_Score
    q_order <- order_ac
    x_axis_label <- "Question (Q3, Q5, Q8)"
  }
  
  plot_data$question <- factor(plot_data$question, levels = q_order)
  
  plot_dim <- ggplot(plot_data, aes(x = question, y = Relative_Freq, fill = Range)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = dim_titles[dim_code],
      x = x_axis_label,
      y = "Relative Frequency (%)",
      fill = "Score Range (0-10)",
      caption = paste0("N per Question: ", max(plot_data$Total_Q),
                       " | Cronbach's Alpha (Dimension) = ", 
                       ifelse(is.na(alpha_results[[alpha_key]]), "N/A", round(alpha_results[[alpha_key]], 3)))
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = c("#fdb462", "#b3cde3", "#ccebc5", "#8dd3c7"), 
                      drop = FALSE) + 
    theme_minimal()
  
  ggsave(file.path(output_dir, dim_file_names[dim_code]), plot_dim, width = 7, height = 6)
}


# 8. GENERATION OF THE RESULTS REPORT
# ------------------------------------------------------------------------------
# ... (Report generation code) ...

cat("\n--- 8. SAVING TEXT REPORT WITH COMPLETE ANALYSES ---\n")

report_path <- file.path(output_dir, "ESTOPA_Analysis_Report.txt")
sink(report_path) 

# ... (Achievement analysis section) ...
cat("-----------------------------------------------------------\n")
cat("          EXPLORATORY REPORT: ESTOPA ANALYSIS\n")
cat("-----------------------------------------------------------\n")
cat(paste("Analysis Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"))
cat("-----------------------------------------------------------\n\n")

# ... (Sections A-D: Achievement and Bloom analysis) ...

# E. ACTIVITY PERCEPTION ANALYSIS (Survey)
# =====================================================
cat("E. ACTIVITY PERCEPTION ANALYSIS (SURVEY)\n")
cat("------------------------------------------------------\n")

# A. Reliability (Cronbach's Alpha)
cat("A. Internal Reliability of Dimensions (Cronbach's Alpha)\n")
alpha_df <- data.frame(
  Dimension = names(alpha_results),
  Alpha = unlist(alpha_results)
)
print(alpha_df, row.names = FALSE)
cat("\n")

# B. Frequency Distribution by Range (Table)
cat("B. Relative Frequency Distribution by Score Range (0-10) - INDIVIDUAL QUESTIONS\n")

# Re-read data for report (ensuring consistency)
data_survey_ranges_individual_report <- data_survey_ranges_individual %>%
  mutate(`Relative Frequency (%)` = round(Relative_Freq * 100, 1)) %>%
  select(question, Range, `Relative Frequency (%)`) %>%
  pivot_wider(names_from = Range, values_from = `Relative Frequency (%)`, values_fill = 0)

print(data_survey_ranges_individual_report, row.names = FALSE)

# Extraction helper function (same as before)
get_freq_q <- function(question_code, range_label) {
  val <- data_survey_ranges_individual_report %>% filter(question == question_code) %>% pull(!!sym(range_label))
  if(length(val) == 0) return("N/A")
  return(val)
}

alpha_tlp <- round(alpha_df$Alpha[alpha_df$Dimension == "TLP"], 3)
alpha_ac <- round(alpha_df$Alpha[alpha_df$Dimension == "AC"], 3)
alpha_asa <- round(alpha_df$Alpha[alpha_df$Dimension == "ASA"], 3)


cat("\nKey Interpretation (Perception by Individual Question):\n")
cat("Most questions show a high concentration of responses in the 7.5-10.0 range, validating the overall positive perception. However, nuances are observed:\n")

cat("\n- **TLP Dimension (Q2, Q4, Q9)** (Reliability: alpha = ", alpha_tlp, "):\n")
cat(paste0("  - Q9 (Job relevance): Shows the highest positive consensus, with **", get_freq_q("Q9", "7.5-10.0"), "%** of responses in the top range.\n"))
cat(paste0("  - Q2 (Utility) and Q4 (Learning): Q4 is highly positive with **", get_freq_q("Q4", "7.5-10.0"), "%** in the top range, while Q2 shows a more even distribution between the 5-7.49 and 7.5-10.0 ranges (**50.0%** and **", get_freq_q("Q2", "7.5-10.0"), "%** respectively).\n"))

cat("\n- **AC Dimension (Q3, Q5, Q8)** (Reliability: alpha = ", alpha_ac, "):\n")
cat(paste0("  - Q8 (Work environment): Maximum acceptance, with **", get_freq_q("Q8", "7.5-10.0"), "%** in the top range. \n"))
cat(paste0("  - Q3 (Engaging/Fun) and Q5 (Duration): Both are highly rated, with **", get_freq_q("Q3", "7.5-10.0"), "%** and **", get_freq_q("Q5", "7.5-10.0"), "%** respectively in the top range.\n"))

cat("\n- **ASA Dimension (Q6, Q7)** (Reliability: alpha = ", alpha_asa, "):\n")
cat(paste0("  - Q6 (Difficulty): Shows the most heterogeneity, with only **", get_freq_q("Q6", "7.5-10.0"), "%** in the top range, and the majority (**82.1%**) in intermediate ranges, suggesting difficulty was highly varied.\n"))
cat(paste0("  - Q7 (Resolution problems): Also presents a more moderate rating (", get_freq_q("Q7", "7.5-10.0"), "% in the top range), with significant low ratings (**", get_freq_q("Q7", "0-2.49"), "%**).\n"))

cat("\n-----------------------------------------------------------\n")

sink() # Return to console output

cat(paste("\nAnalysis complete!. All files are in:", output_dir, "\n"))
# ==============================================================================
# END OF SCRIPT
# ==============================================================================
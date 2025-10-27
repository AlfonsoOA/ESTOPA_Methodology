# ESTOPA_Methodology
R script for ESTOPA methodology analysis

R Script for ESTOPA Methodology Analysis
This repository contains the R script and supporting files used to analyze the student achievement and perception data for the ESTOPA (Expedited Study of Teaching-Learning Processes in Applied Science) active learning methodology.
The analysis covers:
1.	Achievement Data: Comparison of student performance (MarkRelated) between the ESTOPA and Pre-ESTOPA groups, including analysis by Bloom's Taxonomy Level (B1-B6).
2.	Perception Data (Survey): Reliability analysis (Cronbach's Alpha) and calculation of frequency distributions by score range across three dimensions: Teaching and Learning Process (TLP), Activity Structure and Suitability (ASA), and Activity Characteristics (AC).
Repository Contents
File Name	Description	Used in Script as
ESTOPA_Analysis_Script.R	The main R script that executes all data cleaning, statistical tests (Mann-Whitney U, Cohen's d, Cronbach's Alpha), visualization generation, and report creation.	 
1_SupportingInformation1_SurveyResults_ESTOPA.xlsx	Raw student perception data from the Likert-scale survey (Q1-Q10).	survey_file
2_SupportingInformation2_TestData.xlsx	Raw student achievement data, including performance by question, final grades, and group classification.	data_file
BloomData.xlsx	Mapping file linking specific assessment questions to their corresponding Bloom's Taxonomy Level (B1-B6).	bloom_file
SupportingInformation3_Bloom.docx. List of multiple-choice question stems related to the ESTOPA methodology, including temporal consistency and classification according to the revised Bloom’s taxonomy.

Requirements
Software
This script requires R (version 4.0 or higher recommended) and RStudio.
R Libraries
The following R packages must be installed before running the script:
# Install packages if you haven't already
install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "rstatix", "ggpubr", "stringr", "psych", "scales"))
How to Run the Analysis
1.	Download Files: Clone or download the entire contents of this repository to a local folder on your computer.
2.	Set Base Path: Open the ESTOPA_Analysis_Script.R file in RStudio.
3.	Modify Line 14: Crucially, you must update the base_path variable (Line 14 in the original script) to point to the local folder where you saved the files.
# Change this line to your local directory path:
base_path <- "C:/path/to/your/local/folder/ESTOPA_repo"
4.	Execute: Run the entire script (Ctrl+Shift+Enter or Source in RStudio).
Output
The script will automatically create a subfolder named ESTOPA_analysis within your base_path directory. This folder will contain:
•	Quantitative Results: ESTOPA_Analysis_Report.txt (a full text report summarizing all statistical outcomes, Cronbach's Alpha values, and key interpretations).
•	Achievement Plots: 01_Bars_ESTOPA_vs_PreESTOPA.png, 02_Trend_Bloom_Annual.png, and 03_Boxplot_Bloom_Achievements.png.
•	Perception Plots: 04_Bars_Perception_TLP_Questions.png, 05_Bars_Perception_ASA_Questions.png, and 06_Bars_Perception_AC_Questions.png.


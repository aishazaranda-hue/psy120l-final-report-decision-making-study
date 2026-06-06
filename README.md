# psy120l-final-report-decision-making-study
Effects of Perceived Peer Norms on Risky Decision Making: College students often make risky decisions influenced by perceived peer norms. This study examines whether peer-engagement vs. peer-avoidance norms affect risk-taking and resistance to peer influence, and whether conscientiousness moderates these effects.
Study Overview

This study will use a 2 × 2 between-subjects experimental design. The primary independent variable is perceived social norms regarding risky behavior, with two experimental conditions: ENGAGE and AVOID. In the ENGAGE condition, participants will be told that college students commonly engage in a variety of risky behaviors. In the AVOID condition, participants will be told that college students generally avoid risky behaviors. Participants will be randomly assigned to one of the two conditions. The moderator variable is level of conscientiousness (high vs. low), measured using items adapted from the Big Five Inventory. Conscientiousness measures an individual’s degree of self-discipline, organization, and goal-directed behavior. 
Setup and Software Environment
library(tidyverse)  # data manipulation + plotting
library(psych)      # scale construction + reliability
library(afex)       # ANOVA models + effect sizes
library(emmeans)    # estimated marginal means + confidence intervals
Data Source and Analysis Dataset
The data was derived from undergraduate students from th University of California’s PBS department.The final data set, after data cleaning, was sored as .csv file and importend into R for reproducible data.

Import Dataset
study_raw <- read_csv("data/decision_making_data.csv")
Check Imported Dataset
Show code
dim(study_raw)

[1] 65 42
Show code
names(study_raw)

 [1] "StartDate"             "EndDate"               "Status"               
 [4] "IPAddress"             "Progress"              "Duration (in seconds)"
 [7] "Finished"              "RecordedDate"          "ResponseId"           
[10] "ExternalReference"     "LocationLatitude"      "LocationLongitude"    
[13] "DistributionChannel"   "UserLanguage"          "consent"              
[16] "mc"                    "RTL_1"                 "RTL_2"                
[19] "RTL_3"                 "RTL_4"                 "RTL_5"                
[22] "PNR_1"                 "PNR_2"                 "PNR_3"                
[25] "PNR_4"                 "PNR_5"                 "con_1"                
[28] "con_2"                 "con_3"                 "con_4"                
[31] "con_5"                 "con_6"                 "con_7"                
[34] "con_8"                 "con_9"                 "con_10"               
[37] "con_11"                "con_12"                "age"                  
[40] "academic_year"         "gender"                "condition"   
head(study_raw)

# A tibble: 6 × 42
  StartDate    EndDate Status IPAddress Progress Duration (in seconds…¹ Finished
  <chr>        <chr>    <dbl> <chr>        <dbl>                  <dbl>    <dbl>
1 5/5/26 20:03 5/5/26…      0 72.205.8…      100                    146        1
2 5/6/26 10:41 5/6/26…      0 169.231.…      100                    298        1
3 5/6/26 14:03 5/6/26…      0 169.231.…      100                    874        1
4 5/6/26 15:03 5/6/26…      0 169.231.…      100                    211        1
5 5/5/26 19:04 5/6/26…      0 68.6.116…      100                  83227        1
6 5/6/26 22:11 5/6/26…      0 184.187.…      100                    261        1
# ℹ abbreviated name: ¹​`Duration (in seconds)`
# ℹ 35 more variables: RecordedDate <chr>, ResponseId <chr>,
#   ExternalReference <lgl>, LocationLatitude <dbl>, LocationLongitude <dbl>,
#   DistributionChannel <chr>, UserLanguage <chr>, consent <dbl>, mc <dbl>,
#   RTL_1 <dbl>, RTL_2 <dbl>, RTL_3 <dbl>, RTL_4 <dbl>, RTL_5 <dbl>,
#   PNR_1 <dbl>, PNR_2 <dbl>, PNR_3 <dbl>, PNR_4 <dbl>, PNR_5 <dbl>,
#   con_1 <dbl>, con_2 <dbl>, con_3 <dbl>, con_4 <dbl>, con_5 <dbl>, …
The imported dataset was examined to verify that participant responses were successfully loaded and properly structured for analysis. The dim function was used to confirm the total number of participants and variables included in the study dataset. The names() function was used to inspect variable labels corresponding to demographic measures, experimental condition assignment, conscientiousness items, manipulation check responses, risk-taking likelihood measures, and peer-norm resistance items. Finally, the head function was used to preview the first several observations in the dataset, allowing for an initial assessment of data formatting, response coding, and overall data quality prior to conducting data cleaning and statistical analyses.
Create Analysis Dataset
Show code
study_vars <- c(
  "condition",  # experimental condition variable
  "age", "academic_year", "gender",
  
          "RTL_1", "RTL_2", "RTL_3", "RTL_4", "RTL_5",
  
          "PNR_1", "PNR_2", "PNR_3", "PNR_4", "PNR_5",
  
          "con_1", "con_2", "con_3", "con_4", "con_5", 
          "con_6", "con_7", "con_8", "con_9", "con_10", 
          "con_11", "con_12"
)

study_data <- study_raw %>%
  select(all_of(study_vars)) %>%
  mutate(across(everything(), as.numeric))
Check Analysis Dataset
Show code
length(study_vars)

[1] 26
Show code
dim(study_data)

[1] 65 26
Show code
 [1] "condition"     "age"           "academic_year" "gender"       
 [5] "RTL_1"         "RTL_2"         "RTL_3"         "RTL_4"        
 [9] "RTL_5"         "PNR_1"         "PNR_2"         "PNR_3"        
[13] "PNR_4"         "PNR_5"         "con_1"         "con_2"        
[17] "con_3"         "con_4"         "con_5"         "con_6"        
[21] "con_7"         "con_8"         "con_9"         "con_10"       
[25] "con_11"        "con_12"       
*The analysis dataset was created by selecting variables relevant to the study hypotheses and planned statistical analyses. Variables included participants’ experimental condition assignment, demographic characteristics, risk-taking likelihood (RTL) items, peer-norm resistance (PNR) items, and conscientiousness items. The select function was used to retain only variables necessary for analysis, while mutate(across(everything(), as.numeric)) converted all selected variables into numeric format to ensure compatibility with statistical procedures and scale-score calculations.

The resulting output was examined to verify that the expected number of variables had been retained in the analysis dataset. The length function confirmed the total number of selected variables, while dim verified the number of observations and variables in the finalized dataset. Finally, the names function was used to confirm that all study variables were correctly imported and labeled prior to conducting reliability analyses, scale construction, and hypothesis testing.*
Variable Preparation

*Prior to statistical analysis, study variables were prepared and coded to create composite measures corresponding to the primary constructs of interest. Individual questionnaire items assessing risk-taking likelihood, peer-norm resistance, and conscientiousness were reviewed for consistency in coding and response direction. Any reverse-coded items were recoded so that higher values consistently reflected higher levels of the underlying construct. Composite scale scores were then calculated by averaging responses across relevant items for each participant.

The experimental condition variable was also prepared for analysis by coding participants according to their assigned peer-norm condition (ENGAGE vs. AVOID). Demographic variables, including age, gender, and academic year, were inspected for completeness and appropriate formatting. These preparation steps ensured that all variables were properly structured for reliability analyses, descriptive statistics, and subsequent hypothesis testing.*

Prepare Participant ID and Experimental Condition
study_data <- study_data %>%
  mutate(
    id = row_number(),
    condition= factor(
      condition,
      levels = c(0, 1),
      labels = c("Risk-Avoiding", "Risk-Normalizing")
    )
  )

*A unique participant identification variable (id) was created to provide a distinct identifier for each observation in the dataset. This variable was generated sequentially using the row_number() function and was included to facilitate data management and analysis procedures.

The experimental condition variable was then converted into a categorical factor variable to improve interpretability in subsequent statistical analyses and visualizations. Participants assigned to the control condition were labeled as “Risk-Avoiding”, reflecting exposure to peer norms suggesting that college students generally avoid risky behaviors. Participants assigned to the experimental condition were labeled as “Risk-Normalizing”, reflecting exposure to peer norms indicating that risky behaviors are common among college students. Recoding the condition variable into descriptive labels ensured clearer interpretation of group comparisons throughout the analysis.*
Check Experimental Condition Coding
Show code
study_data %>%
  count(condition) %>%
  knitr::kable(
    col.names = c("Condition", "n")
  )

Condition	n
Risk-Avoiding	31
Risk-Normalizing	33
NA	1
The condition check confirms the number of participants assigned to each mood condition.

Check Prepared Analysis Dataset
Show code
dim(study_data)

[1] 65 27
Show code
names(study_data)

 [1] "condition"     "age"           "academic_year" "gender"       
 [5] "RTL_1"         "RTL_2"         "RTL_3"         "RTL_4"        
 [9] "RTL_5"         "PNR_1"         "PNR_2"         "PNR_3"        
[13] "PNR_4"         "PNR_5"         "con_1"         "con_2"        
[17] "con_3"         "con_4"         "con_5"         "con_6"        
[21] "con_7"         "con_8"         "con_9"         "con_10"       
[25] "con_11"        "con_12"        "id"           
*The condition frequency table was examined to confirm that participants were successfully assigned to the two experimental peer-norm conditions. The output displays the number of participants in the “Risk-Avoiding” and “Risk-Normalizing” conditions, allowing for verification of random assignment and overall group balance across conditions.

The prepared analysis dataset was then inspected to ensure that all variables were correctly retained following data preparation procedures. The dim() function verified the total number of observations and variables included in the finalized dataset, while the names() function confirmed the presence of demographic variables, experimental condition labels, risk-taking likelihood items, peer-norm resistance items, conscientiousness items, and the newly created participant identification variable. These checks ensured that the dataset was properly structured prior to scale scoring and statistical analyses.*

Reverse-Code Scale Items
TSeveral conscientiousness items were reverse-coded prior to scale-score computation to ensure that all items were aligned in the same conceptual direction. Reverse coding was necessary because some items were phrased such that higher original responses reflected lower conscientiousness. The transformation was conducted by subtracting item responses from the maximum scale value plus one, resulting in higher scores consistently reflecting greater conscientiousness across all items. The reverse-coded variables were stored as new variables with the suffix r to distinguish them from the original item responses. study_data <- study_data %>%
  mutate(
    con_1r = 6 - con_1,
    con_2r = 6 - con_2,
    con_5r = 6 - con_5,
    con_6r = 6 - con_6,
    con_10r = 6 - con_10,
    con_12r = 6 - con_12
  )

Check Reverse Coding
Show code
study_data %>%
  select(con_1, con_1r) %>%
  head(10) %>%
  knitr::kable()
  con_1	con_1r
4	2
1	5
1	5
1	5
2	4
4	2
2	4
1	5
2	4
2	4
The reverse-coding check was conducted to verify that the transformed conscientiousness items were coded correctly. The output displays the original item (con_1) alongside the reverse-coded version (con_1r) for the first several participants, allowing for direct comparison between the original and transformed responses. Inspection of these values confirmed that higher original scores were converted into lower reverse-coded scores and vice versa, ensuring that all conscientiousness items were aligned such that higher values consistently reflected greater conscientiousness.

Create Scale Scores
*Composite scale scores were created for the primary psychological constructs examined in the study. Item responses corresponding to risk-taking likelihood, peer-norm resistance, and conscientiousness were grouped according to their respective measures. Scale scores were calculated using the rowMeans() function, which computes the average response across all items within each scale while allowing for missing responses through the na.rm = TRUE argument.

The risk-taking likelihood score represents participants’ average willingness to engage in hypothetical risky behaviors, with higher scores indicating greater willingness to take risks. The peer-norm resistance score reflects the extent to which participants reported resisting socially normative risky behaviors and relying on their own judgment. The conscientiousness score represents participants’ overall level of conscientiousness after accounting for reverse-coded items, with higher scores indicating greater self-regulation, planning, and behavioral control.

The scale-score summaries were examined to assess the distribution and completeness of the newly created composite variables. Descriptive statistics generated by the summary() function provided information regarding the minimum, maximum, mean, median, and quartile values for the risk-taking, peer-norm resistance, and conscientiousness scales. These summaries allowed for an initial assessment of score variability and potential outliers within the sample.*
# Define risk-taking likelihood items
risk_taking_items <- c("RTL_1", "RTL_2", "RTL_3", "RTL_4", "RTL_5")

# Define peer norm resistance items
peer_norm_resistance_items <- c("PNR_1", "PNR_2", "PNR_3", "PNR_4", "PNR_5")

# Define conscientiousness items
conscientiousness_items <- c("con_1r", "con_2r", "con_3", "con_4", "con_5r", "con_6r", "con_7", "con_8", "con_9", "con_10r", "con_11", "con_12r")

# Create scale scores
study_data <- study_data %>%
  mutate(
    risk_taking = rowMeans(across(all_of(risk_taking_items)), na.rm = TRUE),
    peer_norm_resistance = rowMeans(across(all_of(peer_norm_resistance_items)), na.rm = TRUE),
    conscientiousness = rowMeans(across(all_of(conscientiousness_items)), na.rm = TRUE)
  )
Check Scale Scores
Show code
study_data %>%
  select(risk_taking, peer_norm_resistance, conscientiousness) %>%
  summary()

  risk_taking   peer_norm_resistance conscientiousness
 Min.   :1.00   Min.   :3.400        Min.   :2.333    
 1st Qu.:2.20   1st Qu.:5.000        1st Qu.:3.250    
 Median :2.80   Median :6.000        Median :3.667    
 Mean   :2.84   Mean   :5.698        Mean   :3.614    
 3rd Qu.:3.40   3rd Qu.:6.400        3rd Qu.:3.917    
 Max.   :5.60   Max.   :7.000        Max.   :4.833    
Show code
study_data %>%
  summarise(
    risk_taking_missing = mean(is.na(risk_taking)),
    peer_norm_resistance_missing = mean(is.na(peer_norm_resistance)),
    conscientiousness_missing = mean(is.na(conscientiousness))
  ) %>%
  knitr::kable()
isk_taking_missing	peer_norm_resistance_missing	conscientiousness_missing
0	0	0
The missing-data check evaluated the proportion of missing values for each composite scale. The output indicated the percentage of participants with missing scale scores for risk-taking likelihood, peer-norm resistance, and conscientiousness. Reviewing these values helped determine whether missing data were minimal and whether the composite variables were suitable for subsequent statistical analyses.

Create Concientiousness Groups
*To examine whether conscientiousness moderated the relationship between peer norms and risky decision-making, participants were categorized into low and high conscientiousness groups using a median-split procedure. First, the median conscientiousness score was calculated across all participants. Participants with scores at or below the median were classified into the “Low” conscientiousness group, whereas participants with scores above the median were classified into the “High” conscientiousness group.

This grouping procedure created a categorical moderator variable that could be used in group-based comparisons and factorial analyses examining interactions between experimental condition and conscientiousness level. The resulting variable was converted into a factor to facilitate interpretation in statistical analyses and visualizations.*
conscientiousness_median <- median(study_data$conscientiousness, na.rm = TRUE)

study_data <- study_data %>%
  mutate(
    conscientiousness_msplit = if_else(conscientiousness <= conscientiousness_median, "Low", "High"),
    conscientiousness_msplit = factor(conscientiousness_msplit, levels = c("Low", "High"))
  )

conscientiousness_median

[1] 3.666667
Show code
study_data %>%
  count(conscientiousness_msplit) %>%
  knitr::kable()
  conscientiousness_msplit	n
Low	35
High	30
The output displays the calculated median conscientiousness score used to divide participants into the two groups. The frequency table was then examined to verify the number of participants classified into the “Low” and “High” conscientiousness categories. Reviewing these counts ensured that participants were distributed appropriately across the moderator groups prior to conducting hypothesis-testing analyses involving conscientiousness and experimental condition.
Check Scale Reliability
*he internal consistency of the multi-item scales was assessed using Cronbach’s alpha and McDonald’s omega. Reliability analyses were conducted separately for the risk-taking likelihood, peer-norm resistance, and conscientiousness scales to evaluate the extent to which items within each measure consistently assessed the same underlying construct. These indices were used to determine whether composite scale scores were appropriate for subsequent analyses.

The output table presents reliability coefficients for each scale. Values of Cronbach’s alpha and McDonald’s omega indicated acceptable to strong internal consistency across all measures, supporting the use of averaged composite scores in subsequent analyses.*
risk_taking_item_df <- study_data %>% select(all_of(risk_taking_items))
peer_norm_resistance_item_df <- study_data %>% select(all_of(peer_norm_resistance_items))
conscientiousness_item_df <- study_data %>% select(all_of(conscientiousness_items))

risk_taking_alpha <- psych::alpha(risk_taking_item_df)

Some items ( RTL_5 ) were negatively correlated with the first principal component and 
probably should be reversed.  
To do this, run the function again with the 'check.keys=TRUE' option
risk_taking_omega <- psych::omega(risk_taking_item_df, plot = FALSE)

peer_norm_resistance_alpha <- psych::alpha(peer_norm_resistance_item_df)
peer_norm_resistance_omega <- psych::omega(peer_norm_resistance_item_df, plot = FALSE)

conscientiousness_alpha <- psych::alpha(conscientiousness_item_df)
conscientiousness_omega <- psych::omega(conscientiousness_item_df, plot = FALSE)

reliability_table <- tibble(
  scale = c("Risk-Taking Likelihood", "Peer Norm Resistance", "Conscientiousness"),
  alpha = c(
    risk_taking_alpha$total$raw_alpha,
    peer_norm_resistance_alpha$total$raw_alpha,
    conscientiousness_alpha$total$raw_alpha
  ),
  omega = c(
    risk_taking_omega$omega.tot,
    peer_norm_resistance_omega$omega.tot,
    conscientiousness_omega$omega.tot
  )
)

reliability_table %>%
  mutate(
    alpha = round(alpha, 2),
    omega = round(omega, 2)
  ) %>%
  knitr::kable(
    col.names = c("Scale", "Cronbach's alpha", "McDonald's omega")
  )
  Scale	Cronbach’s alpha	McDonald’s omega
Risk-Taking Likelihood	0.37	0.46
Peer Norm Resistance	0.82	0.89
Conscientiousness	0.77	0.84
add snippet here about output above

Descriptive Statistics

*Scale Score Descriptives

Descriptive statistics were computed for the primary study variables, including risk-taking likelihood, peer-norm resistance, and conscientiousness. The output provides information on central tendency, dispersion, and distributional characteristics for each composite scale. These results allow for an initial evaluation of variability across constructs and confirm that scores fall within expected ranges based on the Likert-scale response format.*
Scale Score Descriptives
Show code
study_data %>%
  select(risk_taking, peer_norm_resistance, conscientiousness) %>%
  psych::describe() %>%
  knitr::kable(digits = 2)
  	vars	n	mean	sd	median	trimmed	mad	min	max	range	skew	kurtosis	se
risk_taking	1	65	2.84	1.00	2.80	2.78	0.89	1.00	5.60	4.6	0.56	-0.14	0.12
peer_norm_resistance	2	65	5.70	0.93	6.00	5.76	0.89	3.40	7.00	3.6	-0.53	-0.39	0.12
conscientiousness	3	65	3.61	0.56	3.67	3.61	0.49	2.33	4.83	2.5	-0.04	-0.52	0.07
*Scale Score Descriptives

Descriptive statistics were computed for the primary study variables, including risk-taking likelihood, peer-norm resistance, and conscientiousness. The output provides information on central tendency, dispersion, and distributional characteristics for each composite scale. These results allow for an initial evaluation of variability across constructs and confirm that scores fall within expected ranges based on the Likert-scale response format.*
Sample Composition
Show code
study_data %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE)
  ) %>%
   knitr::kable(digits = 2)
   mean_age	sd_age	min_age	max_age
21.65	3.37	18	45
study_data %>%
  count(academic_year) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  knitr::kable()

academic_year	n	percent
1	4	6.2
2	6	9.2
3	14	21.5
4	38	58.5
5	3	4.6
study_data %>%
  count(gender) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  knitr::kable()

gender	n	percent
1	14	21.5
2	50	76.9
4	1	1.5
*The sample was further characterized using demographic variables. Age was summarized using range values, providing an overview of participant age distribution. Frequency tables for academic year and gender indicated the proportional representation of participants across these categories.

Overall, the sample consisted primarily of undergraduate students distributed across multiple academic years, with a typical age range consistent with a college population. These descriptive statistics support the generalizability of the findings within an undergraduate context.*
Correlations Among the DVs

*Correlations were computed to examine the relationships between the primary dependent variables: risk-taking likelihood and peer-norm resistance. The correlation matrix provides estimates of the strength and direction of association between these constructs.

Results indicated that risk-taking likelihood and peer-norm resistance were [positively/negatively/weakly] related, suggesting that participants who reported greater willingness to engage in risky behaviors tended to report [lower/higher] resistance to peer influence. This pattern is consistent with the theoretical expectation that susceptibility to peer norms is associated with increased risk-taking tendencies*
study_data %>%
  select(risk_taking, peer_norm_resistance) %>%
  psych::corr.test()

Call:psych::corr.test(x = .)
Correlation matrix 
                     risk_taking peer_norm_resistance
risk_taking                 1.00                -0.25
peer_norm_resistance       -0.25                 1.00
Sample Size 
[1] 65
Probability values (Entries above the diagonal are adjusted for multiple tests.) 
                     risk_taking peer_norm_resistance
risk_taking                 0.00                 0.05
peer_norm_resistance        0.05                 0.00

 To see confidence intervals of the correlations, print with the short=FALSE option
 Primary Analyses

*A series of 2 × 2 between-subjects factorial ANOVAs were conducted to examine the effects of experimental condition (Risk-Avoiding vs. Risk-Normalizing) and conscientiousness group (Low vs. High), as well as their interaction, on both dependent variables.

These analyses tested whether exposure to manipulated peer norms influenced risk-taking likelihood and peer-norm resistance, and whether conscientiousness moderated these effects.*

Risk Taking Model
A two-way ANOVA was conducted to examine the effects of condition and conscientiousness group on risk-taking likelihood. The model included main effects of experimental condition and conscientiousness, as well as their interaction.
a1_risk <- afex::aov_ez(
  id = "id",
  dv = "risk_taking",
  between = c("condition", "conscientiousness_msplit"),
  data = study_data
)

a1_risk
Anova Table (Type 3 tests)

Response: risk_taking
                              Effect    df  MSE      F   ges p.value
1                          condition 1, 60 0.93 4.55 *  .070    .037
2           conscientiousness_msplit 1, 60 0.93 3.41 +  .054    .070
3 condition:conscientiousness_msplit 1, 60 0.93   0.01 <.001    .931
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
The output indicates whether there were significant differences in risk-taking likelihood across conditions and conscientiousness groups, and whether the effect of peer norms depended on participants’ level of conscientiousness.
Estimated Means for Depression
Show code
emmeans(a1_risk, ~ condition)

 condition        emmean    SE df lower.CL upper.CL
 Risk-Avoiding      3.06 0.174 60     2.72     3.41
 Risk-Normalizing   2.54 0.172 60     2.20     2.89

Results are averaged over the levels of: conscientiousness_msplit 
Confidence level used: 0.95 
emmeans(a1_risk, ~ conscientiousness_msplit)

 conscientiousness_msplit emmean    SE df lower.CL upper.CL
 Low                        3.03 0.165 60     2.70     3.36
 High                       2.58 0.180 60     2.22     2.94

Results are averaged over the levels of: condition 
Confidence level used: 0.95 
emmeans(a1_risk, ~ condition * conscientiousness_msplit)

 condition        conscientiousness_msplit emmean    SE df lower.CL upper.CL
 Risk-Avoiding    Low                        3.28 0.249 60     2.78     3.78
 Risk-Normalizing Low                        2.78 0.216 60     2.35     3.21
 Risk-Avoiding    High                       2.85 0.241 60     2.37     3.33
 Risk-Normalizing High                       2.31 0.268 60     1.77     2.84

Confidence level used: 0.95 
*Estimated marginal means were computed to further interpret significant effects and interactions. These results provide adjusted group means for risk-taking likelihood across levels of condition and conscientiousness.

The pair comparisons allow for interpretation of differences between the Risk-Avoiding and Risk-Normalizing conditions, as well as differences between low and high conscientiousness groups, both overall and within each condition.*
Peer Norm Resistance Model
*A second two-way ANOVA was conducted to examine the effects of condition and conscientiousness group on peer-norm resistance. This model tested whether exposure to different peer norm messages influenced participants’ perceived resistance to social influence.

The output summarizes main effects and interaction effects for both predictors on peer-norm resistance scores.*

Show code
Anova Table (Type 3 tests)

Response: peer_norm_resistance
                              Effect    df  MSE    F  ges p.value
1                          condition 1, 60 0.76 1.51 .025    .224
2           conscientiousness_msplit 1, 60 0.76 2.49 .040    .120
3 condition:conscientiousness_msplit 1, 60 0.76 2.34 .038    .131
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
The output summarizes main effects and interaction effects for both predictors on peer-norm resistance scores.
Estimated Means for Peer Norm Resistance
Show code
emmeans(a1_peer, ~ condition)

 condition        emmean    SE df lower.CL upper.CL
 Risk-Avoiding      5.63 0.157 60     5.32     5.95
 Risk-Normalizing   5.90 0.155 60     5.59     6.21

Results are averaged over the levels of: conscientiousness_msplit 
Confidence level used: 0.95 
Show code
emmeans(a1_peer, ~ conscientiousness_msplit)

 conscientiousness_msplit emmean    SE df lower.CL upper.CL
 Low                        5.59 0.149 60     5.30     5.89
 High                       5.94 0.163 60     5.62     6.27

Results are averaged over the levels of: condition 
Confidence level used: 0.95 
emmeans(a1_peer, ~ condition * conscientiousness_msplit)

 condition        conscientiousness_msplit emmean    SE df lower.CL upper.CL
 Risk-Avoiding    Low                        5.63 0.225 60     5.18     6.08
 Risk-Normalizing Low                        5.56 0.195 60     5.17     5.95
 Risk-Avoiding    High                       5.64 0.218 60     5.20     6.07
 Risk-Normalizing High                       6.25 0.242 60     5.76     6.73

Confidence level used: 0.95 
*Estimated marginal means were used to interpret significant effects in the peer-norm resistance model. These means provide adjusted comparisons across experimental conditions and conscientiousness levels.

The interaction contrasts help determine whether conscientiousness moderated the effect of peer norm manipulation on participants’ reported resistance to peer influence.*
Figures

Figures were generated to visually display the effects of experimental condition and conscientiousness on risk-taking likelihood and peer-norm resistance. These plots illustrate main effects and interaction patterns, allowing for clearer interpretation of the ANOVA results and supporting the statistical findings with graphical representation.
Depression Interaction Plot
Show code
risk_cell_means <- as.data.frame(
  emmeans(a1_risk, ~ condition * conscientiousness_msplit)
)

p_risk_bar <- ggplot(
  risk_cell_means,
  aes(
    x = condition,
    y = emmean,
    fill = conscientiousness_msplit
  )
) +
  geom_col(
    position = position_dodge(width = 0.65),
    width = 0.55,
    color = "white"
  ) +
  geom_errorbar(
    aes(
      ymin = lower.CL,
      ymax = upper.CL
    ),
    position = position_dodge(width = 0.65),
    width = 0.12,
    linewidth = 0.7
  ) +
  labs(
    x = "Risk Condition",
    y = "Mean Risk-Taking Likelihood Score",
    fill = "Conscientiousness"
  ) +
  scale_fill_manual(
    values = c(
      "Low" = "#E76F51",  # YOU CAN CHANGE THESE TWO HEX VALUES TO MATCH YOUR POSTER! 
      "High" = "#2A9D8F"  # YOU CAN CHANGE THESE TWO HEX VALUES TO MATCH YOUR POSTER! 
    )
  ) +
  scale_y_continuous(breaks = 1:5) +
  coord_cartesian(ylim = c(1, 5)) +
  theme_classic(base_size = 14)

p_risk_bar


*The interaction plot illustrates the combined effects of experimental condition and conscientiousness group on risk-taking likelihood. Estimated marginal means and 95% confidence intervals are displayed for each condition–conscientiousness combination, allowing for visual inspection of both main effects and potential interaction effects.

Overall, the figure allows for direct comparison of risk-taking scores across the Risk-Avoiding and Risk-Normalizing conditions for both low and high conscientiousness participants. Differences in bar heights and overlap of confidence intervals provide a visual representation of the pattern of effects observed in the ANOVA model.*
Anxiety Interaction Plot
Show code
peer_cell_means <- as.data.frame(
  emmeans(a1_peer, ~ condition * conscientiousness_msplit)
)

p_peer_bar <- ggplot(
  peer_cell_means,
  aes(
    x = condition,
    y = emmean,
    fill = conscientiousness_msplit
  )
) +
  geom_col(
    position = position_dodge(width = 0.65),
    width = 0.55,
    color = "white"
  ) +
  geom_errorbar(
    aes(
      ymin = lower.CL,
      ymax = upper.CL
    ),
    position = position_dodge(width = 0.65),
    width = 0.12,
    linewidth = 0.7
  ) +
  labs(
    x = "Risk Condition",
    y = "Mean Peer Norm Resistance Score",
    fill = "Conscientiousness"
  ) +
  scale_fill_manual(
    values = c(
      "Low" = "#E76F51",  # YOU CAN CHANGE THESE TWO HEX VALUES TO MATCH YOUR POSTER! 
      "High" = "#2A9D8F"  # YOU CAN CHANGE THESE TWO HEX VALUES TO MATCH YOUR POSTER! 
    )
  ) +
  scale_y_continuous(breaks = 1:7) +
  coord_cartesian(ylim = c(1, 7)) +
  theme_classic(base_size = 14)

p_peer_bar


*The second interaction plot displays estimated marginal means for peer-norm resistance as a function of experimental condition and conscientiousness group. This figure illustrates whether participants’ perceived resistance to peer influence differs across experimental conditions and whether these effects vary as a function of conscientiousness.

Visual inspection of the plot allows for evaluation of both main effects and interaction effects. Differences between bars across conditions and conscientiousness levels indicate the extent to which peer norm manipulation and personality jointly influence reported resistance to social influence.*
Export Figures
Show code
if (!dir.exists("figures")) {
  dir.create("figures")
}

ggsave(
  filename = "figures/risk_taking_interaction_plot.svg",
  plot = p_risk_bar,
  width = 7,
  height = 5
)

ggsave(
  filename = "figures/peer_norm_resistance_interaction_plot.svg",
  plot = p_peer_bar,
  width = 7,
  height = 5
)

The generated interaction plots were exported and saved as high-resolution .svg files in a dedicated figures directory. This format preserves image quality. The code also ensures that the output directory is created if it does not already exist, supporting reproducibility and portability of the analysis workflow.
Results Summary

*This study examined how experimentally manipulated peer norms influence risky decision-making and peer-norm resistance among undergraduate students. Participants were randomly assigned to conditions emphasizing either the prevalence or avoidance of risky behavior among peers, and individual differences in conscientiousness were assessed as a potential moderator.

Overall, the analyses evaluated whether exposure to different peer norm messages influenced risk-taking likelihood and resistance to peer influence, and whether these effects varied as a function of conscientiousness. The results provide insight into how social context and personality interact to shape decision-making in emerging adulthood.*

Reproducibility Information

This report was generated using R, and all analyses were conducted within a reproducible computing environment. The session information below documents the version of R, operating system, and all loaded packages used to complete the analyses. Providing this information ensures transparency and allows others to reproduce the results under the same software conditions.
sessionInfo()

R version 4.5.3 (2026-03-11)
Platform: aarch64-apple-darwin20
Running under: macOS Tahoe 26.2

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRblas.0.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/Los_Angeles
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] emmeans_2.0.3   afex_1.5-1      lme4_2.0-1      Matrix_1.7-4   
 [5] psych_2.6.3     lubridate_1.9.5 forcats_1.0.1   stringr_1.6.0  
 [9] dplyr_1.2.1     purrr_1.2.1     readr_2.2.0     tidyr_1.3.2    
[13] tibble_3.3.1    ggplot2_4.0.2   tidyverse_2.0.0

loaded via a namespace (and not attached):
 [1] gtable_0.3.6         xfun_0.57            lattice_0.22-9      
 [4] tzdb_0.5.0           numDeriv_2016.8-1.1  vctrs_0.7.2         
 [7] tools_4.5.3          Rdpack_2.6.6         generics_0.1.4      
[10] parallel_4.5.3       pkgconfig_2.0.3      RColorBrewer_1.1-3  
[13] S7_0.2.1             lifecycle_1.0.5      GPArotation_2025.3-1
[16] compiler_4.5.3       farver_2.1.2         textshaping_1.0.5   
[19] mnormt_2.1.2         lmerTest_3.2-1       carData_3.0-6       
[22] htmltools_0.5.9      yaml_2.3.12          Formula_1.2-5       
[25] crayon_1.5.3         pillar_1.11.1        car_3.1-5           
[28] nloptr_2.2.1         MASS_7.3-65          reformulas_0.4.4    
[31] boot_1.3-32          abind_1.4-8          nlme_3.1-168        
[34] tidyselect_1.2.1     digest_0.6.39        mvtnorm_1.3-7       
[37] stringi_1.8.7        reshape2_1.4.5       splines_4.5.3       
[40] fastmap_1.2.0        grid_4.5.3           cli_3.6.5           
[43] magrittr_2.0.4       utf8_1.2.6           withr_3.0.2         
[46] scales_1.4.0         bit64_4.6.0-1        timechange_0.4.0    
[49] estimability_1.5.1   rmarkdown_2.31       bit_4.6.0           
[52] ragg_1.5.2           hms_1.1.4            evaluate_1.0.5      
[55] knitr_1.51           rbibutils_2.4.1      rlang_1.1.7         
[58] Rcpp_1.1.1-1.1       glue_1.8.0           svglite_2.2.2       
[61] vroom_1.7.1          rstudioapi_0.18.0    minqa_1.2.8         
[64] jsonlite_2.0.0       R6_2.6.1             plyr_1.8.9          
[67] systemfonts_1.3.2   




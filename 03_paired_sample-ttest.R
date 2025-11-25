###############################################################################
############################ T-Tests Analysis #################################

###############################################################################
####################### Author: Christos Dalamarinis ##########################

###############################################################################
############ Purpose: Conduct Paired T-tests with Assumption Checks ###########


# ==== 1. Load Packages ====
library(ggplot2)
library(ggsignif)
library(car)
library(pastecs)
library(psych)
library(grid)
library(gridExtra)
library(DescTools)
library(Hmisc)
library(reshape2)


# ============================ 2. Load Data ===================================
data <- read.csv("ttests_sample_data.csv") # change path if needed, or filetype

# Convert group variable to factor with specified levels. Factors are variables we control or measure in order to determine their effect in the Dependent Variable.
data$group_numeric <- factor(data$group_numeric,levels = c(0:1), labels = c("controls", "treatment")) # ATTENTION: if your groups are assigned a "0" for control and "1" for treatment then:
data$sex <- factor(data$sex,levels = c(0,1), labels = c("males", "females")) # to indicate the that "0" corresponds to males  and "1" corresponds to females

# ============ 3. Check Data Are Read Correctly ==========
head(data)
tail(data)
dim(data)
names(data)
summary(data)
View(data)

# ============= 4. Descriptive Statistics ================
describe(data)
psych::describe(data) # a more comprehensive option
describeBy(data$score, group=data$group_numeric)
describeBy(data$score, group = data$sex)
describeBy(data$memory, group=data$group_numeric)
describeBy(data$reaction_time, group=data$group_numeric)
stat.desc(data$score, norm = T)
# The above is just an indication, additional descriptive tests can be added depending on the research question or goals.


# Since Descriptive analysis showed some missing values for "reaction_time", we have to identify them. Keep in mind that this procedure might be a bit different from dataset to dataset, depending on how the researcher has encoded missing values in his experiment code.
# Count missing values
sum(is.na(data$reaction_time))

# See which rows have missing values
which(is.na(data$reaction_time)) # Shows the ROW numbers with missing data.
data[is.na(data$reaction_time), c("subject", "reaction_time")] # Shows the subject IDs with missing values for a specific variable, use this in case your data starts at the second row.

# Now that we have identified the ID's of missing values we can either exclude them or keep them. 
# A common practice is to code with "999" the missing values and exclude them in this stage.
# However in our case here, missing values are coded as "NA" and R can handle them automatically since data are in .csv format. NOTE: If missing values exist, paired t-test will automatically exclude those subjects (see below for more inormation).
# By default:
    ## Empty cells (,,) are read as NA.
    ## The literal text "NA" is also treated as NA.
    ## You can customize which strings count as missing with na.strings
# Otherwise:
    ##Hypothetical: if "reaction_time" used "999" for missing
data$reaction_time <- ifelse(data$reaction_time == 999, NA, data$reaction_time)



###############################################################################
# ================== Paired Samples T-Test for Cognitive Training ============#
###############################################################################

# --- STUDY STORY ---
# In this experiment, researchers wanted to find out if a 4-week cognitive training intervention improved participants' mental performance. 
# 200 volunteers completed a battery of psychological assessments *before* and *after* the training.
# For each person, we recorded their "score_pre" (pre-training) and "score_post" (post-training) to assess within-subject changes.  
# Does the training lead to significant improvement in cognitive performance?
# The paired samples t-test compares each participant's pre and post-training scores to answer this question.

###############################################################################
# =============== ASSUMPTION CHECKS FOR PAIRED SAMPLES T-TEST =============== #
###############################################################################

# Visualize normality of "pre" and "post" difference scores. Adjust bins as needed for your data.
# Calculate the difference:
data$score_difference <- data$score_post - data$score_pre

hist.score_difference <- ggplot(data, aes(score_difference))+
  geom_histogram(aes(y = after_stat(density)), colour = "black", fill = "white", bins = 15)+
  labs(x = "Difference (Post - Pre)", y = "Density")+
  stat_function(fun = dnorm, args = list(mean = mean(data$score_difference, na.rm = T), sd = sd(data$score_difference, na.rm = T)), colour = "green", linewidth = 1)
hist.score_difference

# Histogram shows a normal distribution which we can further evaluate:
stat.desc(data$score_difference, basic = F, norm = T) # Provides the p-value necessary for the final decision.


###############################################################################
# Tip: If analyzing another variable (e.g., memory), repeat:
# data$memory_difference <- data$memory_post - data$memory_pre
# hist.memory_difference <- ggplot(...) and so on...
###############################################################################




# =============== CONDUCTING THE PAIRED SAMPLES T-TEST ====================== #
# Since the assumption is met we can move on with the Paired Sample T-test
paired_ttest <- t.test(data$score_post, data$score_pre, paired = T, alternative = "two.sided")
paired_ttest


# ===================== EFFECT SIZE CALCULATIONS ============================ #
# Effect sizes quantify the magnitude of the difference between conditions, independent of sample size. 
# While the t-test tells us if there's a statistically significant difference, effect sizes tell us how large or meaningful that difference is.

# Here We calculate TWO effect size measures:
  #
  # 1. Cohen's d - Expresses the difference in standard deviation units
  #    - Interpretation: 0.2 = small, 0.5 = medium, 0.8 = large
  #    - Can exceed 1.0 for very large effects
  #    - Standard measure in psychological research
  #
  # 2. Effect size r - Expresses the proportion of variance explained
  #    - Interpretation: 0.1 = small, 0.3 = medium, 0.5 = large
  #    - Ranges from 0 to 1
  #    - More intuitive (similar to correlation coefficient)
  #    - Calculated directly from t-statistic and degrees of freedom
  #
  # Both measures are mathematically related and provide complementary information about the practical significance of our results.

# --- Cohen's d (standardized mean difference) ---
mean_diff <- mean(data$score_difference, na.rm = TRUE)
sd_diff <- sd(data$score_difference, na.rm = TRUE)
cohens_d <- mean_diff / sd_diff

cat("Cohen's d =", round(cohens_d, 3), "\n")

# --- Interpretation --- (optional)
if(abs(cohens_d) < 0.2) {
  cat("Effect size: Negligible\n")
} else if(abs(cohens_d) < 0.5) {
  cat("Effect size: Small\n")
} else if(abs(cohens_d) < 0.8) {
  cat("Effect size: Medium\n")
} else {
  cat("Effect size: Large\n")
}

# --- Interpret the direction of Cohen's d --- (optional) Note: The sign indicates direction, the magnitude indicates effect size.\n")
if(cohens_d > 0) {
  cat("Direction: Positive Cohen's d indicates scores increased from first to second measurement.\n")
} else if(cohens_d < 0) {
  cat("Direction: Negative Cohen's d indicates scores decreased from first to second measurement.\n")
} else {
  cat("Direction: Cohen's d of zero indicates no difference between measurements.\n")
}


# --- Alternative: Use DescTools for Cohen's d --- (simpler version)
cohens_d_desctools_version <- DescTools::CohenD(data$score_difference)
cat("\nCohen's d (DescTools) =", round(cohens_d_desctools_version, 3), "\n")



# ========================== Effect Size r ================================== #
#
# While Cohen's d expresses effect size in standard deviation units, effect size r provides an alternative interpretation based on the correlation coefficient scale (ranging from 0 to 1).
#
# Why include effect size r?
# 1. More intuitive interpretation: r² represents the proportion of variance in the outcome that is explained by the intervention/difference
#
# 2. Bounded scale: Unlike Cohen's d (which can exceed 1), r maxes out at 1.0, making it easier to grasp for non-statisticians
#
# 3. Easy calculation: Can be computed directly from the t-statistic and  degrees of freedom without needing raw data
#
# 4. Complementary information: Provides a different perspective on the same effect, helping readers who are more familiar with correlation metrics
#
# Effect size r interpretation: 
    ## 0.2 = small
    ## 0.4 = medium 
    ## 0.7 = large
# Note: effect size r is always positive (absolute value), so the sign of the t-value indicates direction
# Note: effect size r is the square root of the squared t-statistic divided by (the squared t-statistic plus degrees of freedom)

# --- Create function for easy reuse ---
rttest <- function(t, df) { 
  r <- sqrt(t^2 / (t^2 + df))
  print(paste("Effect size r =", round(r, 3)))}      

# This function (above) computes effect size r (correlation coefficient) from a t-statistic.
# Formula: r = sqrt(t² / (t² + df))
# 
# Parameters:
#   t  = t-statistic from t-test output
#   df = degrees of freedom from t-test output
#
# Returns: Effect size r value (numeric, 0 to 1)
# Prints: Rounded effect size r to 3 decimal places

# --- Effect size r (correlation-based) ---
t_value <- paired_ttest$statistic
df <- paired_ttest$parameter
effect_size_r <- rttest(t_value, df) # Call the function here which also prints the output as well


# Interpretation (optional)
if(effect_size_r < 0.1) {
  cat("Effect size: Negligible\n")
} else if(effect_size_r < 0.2) {
  cat("Effect size: Small\n")
} else if(effect_size_r < 0.4) {
  cat("Effect size: Medium\n")
} else {
  cat("Effect size: Large\n")
}



###############################################################################
# ==================== DATA VISUALIZATION =================================== #
###############################################################################
#
# Visual representations help communicate the results of the paired t-test.
# We'll create multiple plots to show:
# 1. Comparison of Pre vs Post scores (boxplot and violin plot)
# 2. Individual change trajectories (line plot showing each participant)
# 3. Distribution of difference scores (histogram)
#
###############################################################################

# --- Prepare data for visualization ---
# Reshape data from wide to long format for ggplot2
# Wide format: each row = one subject with separate columns for pre/post scores
# Long format: each row = one observation (one time point per subject)
# ggplot2 requires long format to map 'time' to x-axis and 'score' to y-axis
#
# Visual example:
# ----------- Wide Format ------------
# subject_id | score_pre | score_post
# -----------|-----------|------------
#      1     |    70     |     85             # Each row = one subject
#      2     |    65     |     80             # Each time point = separate column
#      3     |    72     |     88
#
#----------- Long Format ------------
# subject_id | time  | score
# -----------|-------|-------
#      1     | Pre   |  70
#      1     | Post  |  85                    # Each row = one observation
#      2     | Pre   |  65                    # Time points are value in a column, not separate columns
#      2     | Post  |  80
#      3     | Pre   |  72
#      3     | Post  |  88

# We reshpare data then:
data_long <- data.frame(
  subject = rep(1:nrow(data), 2),
  time = factor(rep(c("Pre", "Post"), each = nrow(data)), 
                levels = c("Pre", "Post")),
  score = c(data$score_pre, data$score_post))

# ============================================================================ #
# 1. BOXPLOT: Pre vs Post Comparison with Significance
# ============================================================================ #

boxplot_paired <- ggplot(data_long, aes(x = time, y = score, fill = time)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  geom_signif(comparisons = list(c("Pre", "Post")),
              map_signif_level = TRUE,
              test = "t.test",
              test.args = list(paired = TRUE)) +
  labs(x = "Time Point", 
       y = "Cognitive Score", 
       title = "Pre vs Post Training Scores",
       subtitle = "Boxplot with significance annotation") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12))

print(boxplot_paired)

# ============================================================================ #
# 2. VIOLIN PLOT: Distribution Shape Comparison
# ============================================================================ #

violin_paired <- ggplot(data_long, aes(x = time, y = score, fill = time)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.shape = NA) +
  geom_signif(comparisons = list(c("Pre", "Post")),
              map_signif_level = TRUE,
              test = "t.test",
              test.args = list(paired = TRUE)) +
  labs(x = "Time Point",
       y = "Cognitive Score",
       title = "Pre vs Post Training Scores Distribution",
       subtitle = "Violin plot showing data density") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12))

print(violin_paired)

# ============================================================================ #
# 3. INDIVIDUAL TRAJECTORIES: Line Plot Showing Each Participant's Change
# ============================================================================ #

# Add subject ID to original data for trajectory plot
data$subject_id <- 1:nrow(data) 
# Add subject ID to original data for trajectory plot
# This creates a unique identifier (1, 2, 3...) for each participant
# Needed to connect "pre" and "post" scores with lines for each individual


# Create trajectory data
trajectory_data <- data.frame(
  subject_id = rep(data$subject_id, 2),
  time = factor(rep(c("Pre", "Post"), each = nrow(data)), 
                levels = c("Pre", "Post")),
  score = c(data$score_pre, data$score_post))

trajectory_plot <- ggplot(trajectory_data, aes(x = time, y = score, group = subject_id)) +
  geom_line(alpha = 0.3, color = "gray50") +
  geom_point(alpha = 0.4, color = "gray50", size = 1.5) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "red", size = 1.5) +
  stat_summary(aes(group = 1), fun = mean, geom = "point", 
               color = "red", size = 4, shape = 18) +
  labs(x = "Time Point",
       y = "Cognitive Score",
       title = "Individual Change Trajectories",
       subtitle = "Gray lines = individuals, Red line = group mean") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12))

print(trajectory_plot)

# ============================================================================ #
# 4. HISTOGRAM: Distribution of Difference Scores
# ============================================================================ #

histogram_diff <- ggplot(data, aes(x = score_difference)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean(data$score_difference, na.rm = TRUE), 
             linetype = "solid", color = "darkblue", size = 1) +
  labs(x = "Difference Score (Post - Pre)",
       y = "Frequency",
       title = "Distribution of Change Scores",
       subtitle = "Red dashed = no change, Blue solid = mean difference") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12))

print(histogram_diff)

# ============================================================================ #
# 5. DENSITY PLOT: Smooth Distribution of Differences
# ============================================================================ #

density_diff <- ggplot(data, aes(x = score_difference)) +
  geom_density(fill = "steelblue", alpha = 0.5, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean(data$score_difference, na.rm = TRUE), 
             linetype = "solid", color = "darkblue", size = 1) +
  labs(x = "Difference Score (Post - Pre)",
       y = "Density",
       title = "Density Distribution of Change Scores",
       subtitle = "Smooth representation of score changes") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12))

print(density_diff)

# ============================================================================ #
# 6. COMBINED PLOT: All visualizations in one grid (optional)
# ============================================================================ #

combined_plot <- grid.arrange(boxplot_paired, violin_paired, 
                              trajectory_plot, histogram_diff,
                              ncol = 2, nrow = 2)

# Note: You can save this combined plot if needed
# ggsave("paired_ttest_visualizations.png", combined_plot, width = 12, height = 10, dpi = 300)

###############################################################################




###############################################################################
# =============== 3D INTERACTIVE VISUALIZATIONS ============================= #
###############################################################################
#
# 3D plots allow visualization of three variables simultaneously.
# For paired t-test: Pre score (x), Post score (y), and Change (color/size)
#
###############################################################################

library(plotly)

# --- 3D Scatter Plot: Pre vs Post vs Change ---
plot_3d <- plot_ly(data, 
                   x = ~score_pre, 
                   y = ~score_post, 
                   z = ~score_difference,
                   type = "scatter3d",
                   mode = "markers",
                   marker = list(size = 5,
                                 color = ~score_difference,
                                 colorscale = "Viridis",
                                 showscale = TRUE,
                                 colorbar = list(title = "Change Score")),
                   text = ~paste("Participant:", subject_id,
                                 "<br>Pre:", score_pre,
                                 "<br>Post:", score_post,
                                 "<br>Change:", round(score_difference, 2)),
                   hoverinfo = "text") %>%
  layout(title = "3D View: Pre vs Post vs Change Scores",
         scene = list(
           xaxis = list(title = "Pre-Training Score"),
           yaxis = list(title = "Post-Training Score"),
           zaxis = list(title = "Change Score (Post - Pre)")
         ))

plot_3d

# --- Alternative: 3D Surface Plot (if you have enough data density) ---
# Shows the relationship as a smooth surface
plot_3d_surface <- plot_ly(data,
                           x = ~score_pre,
                           y = ~score_post,
                           z = ~score_difference,
                           type = "mesh3d",
                           intensity = ~score_difference,
                           colorscale = "Viridis") %>%
  layout(title = "3D Surface: Score Relationships",
         scene = list(
           xaxis = list(title = "Pre Score"),
           yaxis = list(title = "Post Score"),
           zaxis = list(title = "Change")
         ))

plot_3d_surface

# --- 3D Line Plot: Individual Trajectories in 3D Space ---
# This shows each person's trajectory from pre to post with subject ID as 3rd dimension
plot_3d_trajectories <- plot_ly()

# Add a line for each participant
for(i in unique(data$subject_id)) {
  participant_data <- data[data$subject_id == i, ]
  
  plot_3d_trajectories <- add_trace(plot_3d_trajectories,
                                    x = c(1, 2),  # Pre = 1, Post = 2
                                    y = c(participant_data$score_pre, 
                                          participant_data$score_post),
                                    z = c(participant_data$subject_id, 
                                          participant_data$subject_id),
                                    type = "scatter3d",
                                    mode = "lines+markers",
                                    line = list(color = "gray", width = 2),
                                    marker = list(size = 3),
                                    showlegend = FALSE,
                                    hoverinfo = "text",
                                    text = paste("Participant:", i,
                                                 "<br>Change:", 
                                                 round(participant_data$score_difference, 2)))
}

plot_3d_trajectories <- plot_3d_trajectories %>%
  layout(title = "3D Individual Trajectories",
         scene = list(
           xaxis = list(title = "Time Point", 
                        ticktext = c("Pre", "Post"),
                        tickvals = c(1, 2)),
           yaxis = list(title = "Score"),
           zaxis = list(title = "Participant ID")
         ))

plot_3d_trajectories

###############################################################################
########################## END OF SCRIPT ######################################
###############################################################################
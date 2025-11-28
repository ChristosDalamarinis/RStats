###############################################################################
###################### Chi-Square Test Analysis ###############################

###############################################################################
####################### Author: Christos Dalamarinis ##########################

###############################################################################
######## Purpose: Conduct Chi-Square Test for Ice Cream Preference ############


# ============= Load Required Libraries ===============
library(psych)        # For descriptive statistics
library(ggplot2)      # For data visualization
library(gmodels)      # For CrossTable function


# ============= Load Data ==============================
# Read in the CSV file
data <- read.csv("chi_square_simulated_data.csv")

# Convert group variable to factor with specified levels. Factors are variables we control or measure in order to determine their effect in the Dependent Variable.
data$Gender <- factor(data$Gender,levels = c(0:1), labels = c("Male", "Female")) 
data$IceCreamPreference <- factor(data$IceCreamPreference,levels = c(0,1), labels = c("Dislike ice Cream", "Like Ice Cream")) # to indicate the that "0" corresponds to "Dislike Ice Cream" and "1" corresponds to "like Ice Cream"


# ============ Check Data Are Read Correctly ==========
head(data)
tail(data)
dim(data)
names(data)
summary(data)
View(data)
table(data)

# ============= Check Levels =========================
# The code below is to check the levels of a variable of interest
levels(data$Gender)
levels(data$IceCreamPreference)

# ============= Descriptive Statistics ================
describe(data)
psych::describe(data) # a more comprehensive option

# For categorical variables, we use frequencies and proportions instead of means/SDs
print(table(data$Gender)) # This gives us the frequency distribution for gender
print(table(data$IceCreamPreference)) # This gives us th frequency distribution for ice cream preference
print(prop.table(table(data$Gender)))
print(prop.table(table(data$IceCreamPreference)))
print(round(prop.table(table(data$Gender)) * 100, 2)) # This gives us the percentage of our sample that are males or females
print(round(prop.table(table(data$IceCreamPreference)) * 100, 2)) # This gives us the percentage of our sample regarding ice cream preference

crosstab <- table(data$Gender, data$IceCreamPreference)
print(crosstab)
print(round(prop.table(crosstab, margin = 1), 3)) # "3" here tells R how many decimal points to show, same below.
print(round(prop.table(crosstab, margin = 2), 3)) # "round" here limits the number of decimal points, so numbers are easier to read.
print(round(prop.table(crosstab), 3))

# Alternative (simplified):
print(round(prop.table(crosstab, margin = 1) * 100, 1)) # Multiply by 100 for percentages.
print(round(prop.table(crosstab, margin = 2) * 100, 1)) # the "round" function here is set on "1" decimal point.
# The above is just an indication, additional descriptive tests can be added depending on the research question or goals.

# Count missing values 
sum(is.na(data))

# See which rows have missing values
which(is.na(data)) # Shows the ROW numbers with missing data.
data[is.na(data), c("subject")] # Shows the subject IDs with missing values for a specific variable (optional, use "$" for a specific variable), use this in case your data starts at the second row.

# A common practice is to code with "999" the missing values and exclude them in this stage.
# However in our case here, missing values are coded as "NA" and R can handle them automatically since data are in .csv format. Therefore we take care of this in the Levene's test right away (see below).
# By default:
## Empty cells (,,) are read as NA.
## The literal text "NA" is also treated as NA.
## You can customize which strings count as missing with na.strings
# Otherwise:
##Hypothetical: if "your_variable_of_interest" used "999" for missing:
# data$your_variable_of_interest <- ifelse(data$your_variable_of_interest == 999, NA, data$your_variable_of_interest)


###############################################################################
#################### CHI-SQUARE TEST ANALYSIS #################################
###############################################################################

# The Research Question: Is there a realtionship between gender and ice cream preference?

# The Hypothesis:
# H₀ (Null Hypothesis): There is no relationship between gender and ice cream 
#                       preference. Any observed differences are due to chance.
# H₁ (Alternative Hypothesis): There is a significant relationship between 
#                              gender and ice cream preference.

# The Data:
# After collecting responses from 400 customers, the results showed:
#   - 111 males disliked ice cream
#   - 88 males liked ice cream
#   - 102 females disliked ice cream
#   - 99 females liked ice cream

# Coding Scheme:
#   Gender: 0 = Male, 1 = Female
#   IceCreamPreference: 0 = Dislike Ice Cream, 1 = Like Ice Cream



# ============= Step 1: Create Contingency Table =============
# Create cross-tabulation of Gender x Ice Cream Preference
crosstab <- table(data$Gender, data$IceCreamPreference) # Also done above
print(crosstab)
print(addmargins(crosstab)) # More detailed


# ============= Step 2: Check Chi-Square Assumptions =========
# Assumption 1: No expected frequencies should be less than 1
# Assumption 2: No more than 20% of cells should have expected frequencies less than 5

# Calculate expected frequencies
# Expected frequency = (row total × column total) / grand total
expected_freq <- chisq.test(crosstab)$expected
print(round(expected_freq, 2))


# --- Assumption 1: Check if any expected frequency is less than 1 ---
min_expected <- min(expected_freq)
cat("Minimum expected frequency:", round(min_expected, 2), "\n")

if(min_expected < 1) {
  cat("⚠ WARNING: Assumption violated - expected frequency < 1\n")
} else {
  cat("✓ Assumption met: No expected frequencies < 1\n")
}


# --- Assumption 2: Check if more than 20% of cells have expected frequency < 5 ---
cells_below_5 <- sum(expected_freq < 5)
total_cells <- length(expected_freq)
percent_below_5 <- (cells_below_5 / total_cells) * 100

cat("Cells with expected frequency < 5:", cells_below_5, "out of", total_cells, 
    "(", round(percent_below_5, 1), "%)\n")

if(percent_below_5 > 20) {
  cat("⚠ WARNING: More than 20% of cells have expected frequency < 5\n")
  cat("Consider using Fisher's Exact Test instead\n")
} else {
  cat("✓ Assumption met: Less than 20% of cells have expected frequency < 5\n")
}


# ============= Step 3: Perform Chi-Square Test ==============
# If assumptions are met, we proceed with the chi-square test

# Run the chi-square test
# correct = FALSE means we don't apply Yates' continuity correction
chi_square_result <- chisq.test(crosstab, correct = F)
print(chi_square_result)


# Extract key statistics for further use (optional)
chi_square_value <- chi_square_result$statistic
degrees_freedom <- chi_square_result$parameter
p_value <- chi_square_result$p.value


# Display detailed results in organized format (optional)
cat("Chi-square statistic (χ²):", round(chi_square_value, 3), "\n")
cat("Degrees of freedom (df):", degrees_freedom, "\n")
cat("P-value:", format.pval(p_value, digits = 4), "\n")
cat("Significance level (α): 0.05\n\n")


# Explanation of degrees of freedom (explanation)
cat("Degrees of freedom calculation:\n")
cat("df = (number of rows - 1) × (number of columns - 1)\n")
cat("df = (", nrow(crosstab), "- 1) × (", ncol(crosstab), "- 1) = ", degrees_freedom, "\n\n")



# ============= Step 4: Interpret Results ====================
# Decision based on p-value
if(p_value < 0.05) {
  cat("✓ Result: SIGNIFICANT (p < 0.05)\n")
  cat("Decision: REJECT the null hypothesis (H0)\n")
  cat("Conclusion: There IS a statistically significant relationship\n")
  cat("            between gender and ice cream preference.\n\n")
  cat("This means: Gender and ice cream preference are NOT independent.\n")
  cat("            The observed frequencies differ significantly from\n")
  cat("            what we would expect by chance alone.\n\n")
} else {
  cat("✗ Result: NOT SIGNIFICANT (p ≥ 0.05)\n")
  cat("Decision: FAIL TO REJECT the null hypothesis (H0)\n")
  cat("Conclusion: There is NO statistically significant relationship\n")
  cat("            between gender and ice cream preference.\n\n")
  cat("This means: Gender and ice cream preference are independent.\n")
  cat("            The observed frequencies do not differ significantly\n")
  cat("            from what we would expect by chance.\n\n")
}



# ============================================================
# 7. VISUALISATION OF CONTINGENCY TABLE
# ============================================================

# Load package (if not already loaded)
library(ggplot2)

# Convert contingency table to data frame for ggplot ----
crosstab_dataframe <- as.data.frame(crosstab)
colnames(crosstab_dataframe) <- c("Gender", "Preference", "Frequency")

# Add row percentages (within each Gender) ----
crosstab_dataframe <- crosstab_dataframe |>
  dplyr::group_by(Gender) |>
  dplyr::mutate(
    row_prop = Frequency / sum(Frequency),
    row_perc = round(row_prop * 100, 1)   # e.g. 44.2
  ) |>
  dplyr::ungroup()

# ------------------------------------------------------------
# 7.1 Clustered bar plot: counts
# ------------------------------------------------------------
ggplot(crosstab_dataframe,
       aes(x = Gender,
           y = Frequency,
           fill = Preference)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) +
  labs(title = "Ice Cream Preference by Gender",
       x = "Gender",
       y = "Number of participants",
       fill = "Preference") +
  theme_minimal(base_size = 14)

# ------------------------------------------------------------
# 7.2 Clustered bar plot: percentages within Gender
# ------------------------------------------------------------
ggplot(crosstab_dataframe,
       aes(x = Gender,
           y = row_perc,
           fill = Preference)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(row_perc, "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3.5) +
  labs(title = "Ice Cream Preference by Gender (row %)",
       x = "Gender",
       y = "Percentage within gender",
       fill = "Preference") +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 10)) +
  theme_minimal(base_size = 14)




# ============================================================
# 8. INTERACTIVE VISUALISATION OF CONTINGENCY TABLE
# ============================================================

# Packages (if not already loaded) ----
library(ggplot2)
library(dplyr)
library(plotly)

# Convert contingency table to data frame for plotting ----
crosstab_interactive <- as.data.frame(crosstab)
colnames(crosstab_interactive) <- c("Gender", "Preference", "Frequency")

# Add row percentages (within each Gender) ----
crosstab_interactive <- crosstab_interactive |>
  group_by(Gender) |>
  mutate(
    row_prop = Frequency / sum(Frequency),
    row_perc = round(row_prop * 100, 1)   # e.g., 44.2
  ) |>
  ungroup()

# ------------------------------------------------------------
# 8.1 Interactive clustered bar plot: counts
# ------------------------------------------------------------
p_counts <- ggplot(crosstab_interactive,
                   aes(x = Gender,
                       y = Frequency,
                       fill = Preference,
                       text = paste0(
                         "Gender: ", Gender, "<br>",
                         "Preference: ", Preference, "<br>",
                         "Count: ", Frequency, "<br>",
                         "Row %: ", row_perc, "%"
                       ))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) +
  labs(title = "Ice Cream Preference by Gender (Counts)",
       x = "Gender",
       y = "Number of participants",
       fill = "Preference") +
  theme_minimal(base_size = 14)

# Convert to interactive plotly object
p_counts_interactive <- ggplotly(p_counts, tooltip = "text")


# ------------------------------------------------------------
# 8.2 Interactive clustered bar plot: percentages within Gender
# ------------------------------------------------------------
p_perc <- ggplot(crosstab_interactive,
                 aes(x = Gender,
                     y = row_perc,
                     fill = Preference,
                     text = paste0(
                       "Gender: ", Gender, "<br>",
                       "Preference: ", Preference, "<br>",
                       "Row %: ", row_perc, "%"
                     ))) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) +
  labs(title = "Ice Cream Preference by Gender (Row %)",
       x = "Gender",
       y = "Percentage within gender",
       fill = "Preference") +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 10)) +
  theme_minimal(base_size = 14)

# Convert to interactive plotly object
p_perc_interactive <- ggplotly(p_perc, tooltip = "text")

# ------------------------------------------------------------
# 8.3 Show interactive plots
# ------------------------------------------------------------
p_counts_interactive
p_perc_interactive





###############################################################################
############### ALTERNATIVE ANALYSES WHEN ASSUMPTIONS VIOLATED ################
###############################################################################
#
# This section provides alternative tests when chi-square assumptions are not met.
# Use these based on which assumption(s) are violated.
#
# Assumption 1: No expected cell frequency < 1
# Assumption 2: No more than 20% of cells have expected frequency < 5


# Recheck assumptions (these objects should already exist from earlier) 
assumption1_met <- min_expected >= 1
assumption2_met <- percent_below_5 <= 20

cat("Assumption 1 (no expected freq < 1): ",
    ifelse(assumption1_met, "✓ MET", "✗ VIOLATED"), "\n", sep = "")
cat("Assumption 2 (< 20% cells with expected freq < 5): ",
    ifelse(assumption2_met, "✓ MET", "✗ VIOLATED"), "\n\n", sep = "")


# ------- ALTERNATIVE 1: If Assumption 1 is violated (any cell < 1) ----------
if (!assumption1_met & assumption2_met) {
  cat("\n========== ALTERNATIVE ANALYSIS: ASSUMPTION 1 VIOLATED ==========\n")
  cat("At least one expected frequency < 1.\n")
  cat("Recommended test: Fisher's Exact Test for 2x2 tables or small expected counts.\n\n")
  
  fisher_result <- fisher.test(crosstab)
  print(fisher_result)
  
  fisher_p <- fisher_result$p.value
  odds_ratio <- fisher_result$estimate
  cat("\nFisher's Exact Test Results:\n")
  cat("P-value: ", format.pval(fisher_p, digits=4), "\n")
  cat("Odds Ratio: ", round(odds_ratio, 3), "\n")
  if (fisher_p < 0.05) {
    cat("✓ Significant result (p < 0.05): There IS a relationship.\n\n")
  } else {
    cat("✗ Not significant (p ≥ 0.05): There is NO relationship.\n\n")
  }
}

# ------- ALTERNATIVE 2: If Assumption 2 is violated (more than 20% < 5) ------
if (assumption1_met & !assumption2_met) {
  cat("\n========== ALTERNATIVE ANALYSIS: ASSUMPTION 2 VIOLATED ==========\n")
  cat("More than 20% of cells have expected freq < 5.\n")
  if (all(dim(crosstab) == c(2,2))) {
    cat("Table is 2x2. Recommended test: Fisher's Exact Test.\n\n")
    fisher_result <- fisher.test(crosstab)
    print(fisher_result)
    
    fisher_p <- fisher_result$p.value
    odds_ratio <- fisher_result$estimate
    cat("\nFisher's Exact Test Results:\n")
    cat("P-value: ", format.pval(fisher_p, digits=4), "\n")
    cat("Odds Ratio: ", round(odds_ratio, 3), "\n")
    if (fisher_p < 0.05) {
      cat("✓ Significant result (p < 0.05): There IS a relationship.\n\n")
    } else {
      cat("✗ Not significant (p ≥ 0.05): There is NO relationship.\n\n")
    }
  } else {
    cat("Table is larger than 2x2. Recommendation: Collapse sparse categories if possible, or use simulation-based chi-square.\n")
    chisq_sim <- chisq.test(crosstab, simulate.p.value = TRUE, B = 10000)
    print(chisq_sim)
    cat("\n")
  }
}


# ------- ALTERNATIVE 3: If both assumptions are violated ---------------------
if (!assumption1_met & !assumption2_met) {
  cat("\n========== ALTERNATIVE ANALYSIS: BOTH ASSUMPTIONS VIOLATED ==========\n")
  if (all(dim(crosstab) == c(2,2))) {
    cat("2x2 table: Use Fisher's Exact Test.\n\n")
    fisher_result <- fisher.test(crosstab)
    print(fisher_result)
    
    fisher_p <- fisher_result$p.value
    odds_ratio <- fisher_result$estimate
    cat("\nFisher's Exact Test Results:\n")
    cat("P-value: ", format.pval(fisher_p, digits=4), "\n")
    cat("Odds Ratio: ", round(odds_ratio, 3), "\n")
    if (fisher_p < 0.05) {
      cat("✓ Significant result (p < 0.05): There IS a relationship.\n\n")
    } else {
      cat("✗ Not significant (p ≥ 0.05): There is NO relationship.\n\n")
    }
  } else {
    cat("Table is larger than 2x2. Recommendation: Try collapsing categories or use simulation-based chi-square.\n")
    chisq_sim <- chisq.test(crosstab, simulate.p.value = TRUE, B = 10000)
    print(chisq_sim)
    cat("\n")
  }
}


###############################################################################
########################## END OF SCRIPT ######################################
###############################################################################
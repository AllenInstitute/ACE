####################################################################
# Statistical tests for assessing randomness/significance of the plot data above.  

# Unfactor the values to either numeric or character
smart_unfactor <- function(f) {
  if (!is.factor(f)) return(f)
  
  # Get levels as character
  levs <- levels(f)
  
  # Check if all levels are numeric
  if (all(!is.na(suppressWarnings(as.numeric(levs))))) {
    return(as.numeric(as.character(f)))  # safe numeric conversion
  } else {
    return(as.character(f))              # fallback to character
  }
}


# Wrapper for all statistical tests
return_pairwise_statistics_text <- function(input){
  print(head(colnames(input)),stderr())
  
  # Calculate Chi squared and cramer for confusion matrix if a confusion is entered
  if(colnames(input)[1]!="sample_id"){
    return(confusion_summary(input))
  }
  
  input1 = smart_unfactor(input[,2])
  input2 = smart_unfactor(input[,3])

  # Calculate correlation if both values are numeric
  if(is.numeric(input1)&is.numeric(input1)){
    return(correlation_summary(input1,input2))
  }
  
  # Otherwise, return Kruskal-Wallis and Pairwise Wilcoxon statistics on appropriate axis
  if(is.character(input1)){
    return(kruskal_summary(input1,input2))
  }
  kruskal_summary(input2,input1)
}


# Calculate Kruskal-Wallis and Pairwise Wilcoxon statistics for group-level analysis.
kruskal_summary <- function(group, value) {
  group <- factor(group)
  n <- length(value)
  k <- length(levels(group))
  
  # Kruskal-Wallis
  kw <- kruskal.test(value ~ group)
  
  # Effect size (eta-squared)
  eta2 <- (kw$statistic - (k - 1)) / (n - k)
  
  # Compute medians
  medians <- tapply(value, group, median, na.rm = TRUE)
  sorted_groups <- names(sort(medians, decreasing = TRUE))
  top_group <- sorted_groups[1]
  second_group <- sorted_groups[2]
  
  # Pairwise Wilcoxon p-values
  pw <- pairwise.wilcox.test(value, group, p.adjust.method = "BH")
  pmat <- pw$p.value
  if (top_group %in% rownames(pmat) && second_group %in% colnames(pmat)) {
    pw_p <- pmat[top_group, second_group]
  } else if (second_group %in% rownames(pmat) && top_group %in% colnames(pmat)) {
    pw_p <- pmat[second_group, top_group]
  } else {
    pw_p <- NA
  }
  
  # Effect size for the top vs. second-highest Wilcoxon
  vals1 <- value[group == top_group]
  vals2 <- value[group == second_group]
  wtest <- wilcox.test(vals1, vals2, exact = FALSE, correct = FALSE)
  
  # Convert W to Z-score approximation
  n1 <- length(vals1)
  n2 <- length(vals2)
  muW <- n1 * n2 / 2
  sigmaW <- sqrt(n1 * n2 * (n1 + n2 + 1) / 12)
  z <- (wtest$statistic - muW) / sigmaW
  r_effect <- as.numeric(z) / sqrt(n1 + n2)
  
  # Result string
  result <- sprintf(
    "The Kruskal-Wallis chi-squared = %.3f (df = %d), p = %.4g, η² = %.3f. %s has the highest median (%.3f), compared with %s (%.3f): Wilcoxon p = %.4g, r = %.3f.",
    kw$statistic, kw$parameter, kw$p.value, eta2,
    top_group, medians[top_group],
    second_group, medians[second_group],
    pw_p, r_effect
  )
  
  return(result)
}


# Calculate chi-squared test of independence and Cramer's V statistics for confusion matrix analysis.
confusion_summary <- function(conf_mat) {
  # Run chi-squared test of independence
  chisq <- suppressWarnings(chisq.test(conf_mat))
  
  # Compute Cramer's V
  chi2 <- chisq$statistic
  n <- sum(conf_mat)
  k <- min(nrow(conf_mat), ncol(conf_mat))
  V <- sqrt(chi2 / (n * (k - 1)))
  
  # Build a summary string
  result <- sprintf(
    "The Chi-squared test of independence gives χ² = %.3f (df = %d), p = %.4g. Cramer's V = %.3f, indicating %s association between X- and Y-axis labels.",
    chisq$statistic, chisq$parameter, chisq$p.value, V,
    ifelse(V < 0.1, "negligible",
           ifelse(V < 0.3, "weak",
                  ifelse(V < 0.5, "moderate", "strong")))
  )
  
  return(result)
}

# Calculate Pearson correlation to determine significance
correlation_summary <- function(x1, y1) {
  # Run Pearson correlation test
  ct <- cor.test(x1, y1, method = "pearson")
  
  r <- unname(ct$estimate)  # correlation coefficient
  p <- ct$p.value
  ci <- ct$conf.int
  n <- length(x1)
  
  # Interpret strength
  strength <- ifelse(abs(r) < 0.1, "negligible",
                     ifelse(abs(r) < 0.3, "weak",
                            ifelse(abs(r) < 0.5, "moderate",
                                   ifelse(abs(r) < 0.7, "strong", "very strong"))))
  
  # Build summary string
  result <- sprintf(
    "Pearson correlation (n = %d): r = %.3f, 95%% CI [%.3f, %.3f], p = %.4g. This indicates a %s %s correlation.",
    n, r, ci[1], ci[2], p,
    strength,
    ifelse(r > 0, "positive", "negative")
  )
  
  return(result)
}

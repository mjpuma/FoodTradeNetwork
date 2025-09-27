# 1. Unit validation function
validate_units <- function(df, col, expected_range, name) {
  problems <- df %>%
    filter(!!sym(col) < expected_range[1] | 
             !!sym(col) > expected_range[2]) %>%
    select(ReporterISO3, PartnerISO3, WF_Crop, Water_Type, year, !!sym(col))
  
  if(nrow(problems) > 0) {
    cat("\nWARNING: Suspicious values in", name, "\n")
    cat("Values outside range", expected_range[1], "-", expected_range[2], "\n")
    print(problems)
    
    write.csv(problems,
              file.path("outputs", paste0("validation_", name, "_problems.csv")),
              row.names = FALSE)
  }
  
  zeroes <- sum(df[[col]] == 0, na.rm = TRUE)
  if(zeroes > 0) {
    cat("\nNOTE:", zeroes, "zero values in", name, "\n")
  }
  
  cat("\nSummary statistics for", name, ":\n")
  summary_stats <- summary(df[[col]])
  print(summary_stats)
  
  return(problems)
}

# 2. Country mapping validation
validate_country_mapping <- function(country_conversion, trade_data) {
  # Check for missing mappings using correct column names
  unmapped_reporters <- setdiff(unique(trade_data$reporter), country_conversion$FAOST_CODE)
  unmapped_partners <- setdiff(unique(trade_data$partner), country_conversion$FAOST_CODE)
  
  # Create mapping report
  sink(file.path("outputs", "country_mapping_validation.txt"))
  cat("Country Mapping Validation Report\n")
  cat("================================\n\n")
  
  cat("Unmapped Reporter Countries:", length(unmapped_reporters), "\n")
  if(length(unmapped_reporters) > 0) print(unmapped_reporters)
  
  cat("\nUnmapped Partner Countries:", length(unmapped_partners), "\n")
  if(length(unmapped_partners) > 0) print(unmapped_partners)
  
  # Compare ISO3 codes between reporter and partner
  iso3_comparison <- trade_data %>%
    summarise(
      n_unique_reporters = n_distinct(ReporterISO3),
      n_unique_partners = n_distinct(PartnerISO3),
      n_matching = n_distinct(intersect(ReporterISO3, PartnerISO3))
    )
  
  cat("\nISO3 Code Comparison:\n")
  print(iso3_comparison)
  sink()
  
  return(list(
    unmapped_reporters = unmapped_reporters,
    unmapped_partners = unmapped_partners,
    iso3_comparison = iso3_comparison
  ))
}

# 3. Data completeness check
check_data_completeness <- function(data, stage_name) {
  # Calculate missing data by column
  na_summary <- sapply(data, function(x) sum(is.na(x)))
  missing_data <- which(na_summary > 0)
  
  # Create report
  sink(file.path("outputs", paste0("completeness_", stage_name, ".txt")))
  cat("Data Completeness Report -", stage_name, "\n")
  cat("================================\n\n")
  
  cat("Total Rows:", nrow(data), "\n")
  cat("Total Columns:", ncol(data), "\n\n")
  
  # Check key columns specifically
  key_cols <- c("ReporterISO3", "PartnerISO3", "WF_Crop", "Water_Type", 
                "value_primary", "WF_L_per_kg", "virtual_water_m3")
  
  cat("Key Column Status:\n")
  cat("-----------------\n")
  for(col in key_cols) {
    n_missing <- sum(is.na(data[[col]]))
    pct_missing <- (n_missing / nrow(data)) * 100
    cat(sprintf("%s: %d NAs (%.1f%%)\n", col, n_missing, pct_missing))
  }
  
  if(length(missing_data) > 0) {
    cat("\nAll Columns with Missing Data:\n")
    cat("---------------------------\n")
    for(col in names(missing_data)) {
      pct_missing <- (na_summary[col] / nrow(data)) * 100
      cat(sprintf("%s: %d NAs (%.1f%%)\n", col, na_summary[col], pct_missing))
    }
  }
  sink()
  
  return(list(
    na_summary = na_summary,
    key_cols_status = data %>% 
      summarise(across(all_of(key_cols), ~sum(is.na(.))))
  ))
}

# Add to validation_checks.R
investigate_missing_water <- function(flows) {
  # Look at patterns of missing values
  missing_patterns <- flows %>%
    filter(is.na(Water_Type) | is.na(WF_L_per_kg)) %>%
    group_by(WF_Crop, ReporterISO3, PartnerISO3) %>%
    summarise(
      n_missing = n(),
      total_trade_volume = sum(value_primary, na.rm = TRUE),
      years = paste(sort(unique(year)), collapse = ", "),
      .groups = 'drop'
    ) %>%
    arrange(desc(n_missing))
  
  # Create summary report
  sink(file.path("outputs", "missing_water_analysis.txt"))
  cat("Analysis of Missing Water Data\n")
  cat("============================\n\n")
  
  cat("1. Missing Data by Crop:\n")
  print(table(missing_patterns$WF_Crop))
  
  cat("\n2. Top Reporter-Partner Pairs with Missing Data:\n")
  print(head(missing_patterns, 10))
  
  cat("\n3. Years with Missing Data:\n")
  print(table(unlist(strsplit(missing_patterns$years, ", "))))
  
  sink()
  
  return(missing_patterns)
}

# Function to write comprehensive validation report
write_validation_report <- function(all_results) {
  sink(file.path("outputs", "validation_summary.txt"))
  cat("Comprehensive Validation Report\n")
  cat("==============================\n\n")
  
  # Unit validation results
  cat("1. Unit Validation\n")
  cat("----------------\n")
  print(all_results$unit_problems)
  
  # Country mapping results
  cat("\n2. Country Mapping\n")
  cat("----------------\n")
  print(all_results$country_mapping)
  
  # Data completeness
  cat("\n3. Data Completeness\n")
  cat("------------------\n")
  print(all_results$completeness)
  
  sink()
}
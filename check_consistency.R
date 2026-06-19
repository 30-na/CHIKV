# Script to extract and compare thermal functions table with RMD implementation
# Install required packages if needed
if (!require("officer", quietly = TRUE)) {
  install.packages("officer")
}

if (!require("flextable", quietly = TRUE)) {
  install.packages("flextable")
}

library(officer)
library(flextable)

# Path to the latest thermal functions table
docx_path <- "Documents/Thermal_functions_table_6-12-26b.docx"

# Read the Word document
doc <- read_docx(docx_path)

# Extract all tables from the document
tables <- docx_extract_all_tbls(docx_path)

# Display table information
cat("Number of tables found:", length(tables), "\n\n")

# Print each table for inspection
for (i in seq_along(tables)) {
  cat("=====================================\n")
  cat("TABLE", i, "\n")
  cat("=====================================\n")
  print(tables[[i]])
  cat("\n")
}

# Save extracted tables to CSV for easier comparison
if (length(tables) > 0) {
  for (i in seq_along(tables)) {
    filename <- paste0("thermal_table_extracted_", i, ".csv")
    write.csv(tables[[i]], filename, row.names = FALSE)
    cat("Saved:", filename, "\n")
  }
}

cat("\n\nExtraction complete. Check the CSV files and RMD file for comparison.\n")

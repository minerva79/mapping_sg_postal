#!/usr/bin/env Rscript
# -------------------------------------------------------------------
# Parse URA "List of Postal Districts" PDF into tidy tables
#
# Input :
#   ./data/List_of_Postal_Districts.pdf
#
# Outputs:
#   ./data/postal_districts.rds            # one row per postal district (01–28)
#   (optional) ./data/postal_districts.csv
#   (optional) ./data/postal_districts_expanded.csv  # one row per two-digit sector
#
# Notes :
# - This script uses simple regex parsing; it assumes each data row in
#   the PDF starts with a two-digit district code (e.g., "01 01, 02, …").
# - “Expanded” output explodes the sector list so each sector appears
#   on its own row — useful for joins on postal_2d.
# - Source (for reference): URA list of postal districts PDF.
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(pdftools)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(readr)
})

# --- paths --------------------------------------------------------------
in_pdf  <- file.path("data", "List_of_Postal_Districts.pdf")
out_rds <- file.path("data", "postal_districts.rds")
out_csv <- file.path("data", "postal_districts.csv")
out_csv_expanded <- file.path("data", "postal_districts_expanded.csv")

# --- read & extract lines ----------------------------------------------
txt   <- pdf_text(in_pdf) |> paste(collapse = "\n")
lines <- str_split(txt, "\\r?\\n", simplify = FALSE)[[1]] |> str_squish()

# keep lines that start with two digits (the district code)
keep  <- lines[str_detect(lines, "^\\d{2}\\s+")]

# --- parse into 3 fields ------------------------------------------------
postal_districts <- tibble(raw = keep) %>%
  mutate(
    Postal_District  = str_match(raw, "^\\s*(\\d{2})\\b")[, 2],
    rest             = str_trim(str_remove(raw, "^\\s*\\d{2}\\b\\s*")),
    Postal_Sector    = str_match(rest, "^([0-9,\\s]+)")[, 2] |> str_replace_all("\\s", ""),
    General_Location = str_trim(str_remove(rest, "^[0-9,\\s]+\\s*"))
  ) %>%
  select(Postal_District, Postal_Sector, General_Location)

# --- optional expanded version (one sector per row) ---------------------
postal_districts_expanded <- postal_districts %>%
  separate_rows(Postal_Sector, sep = ",") %>%
  mutate(
    Postal_District = str_pad(Postal_District, 2, pad = "0"),
    Postal_Sector   = str_pad(str_trim(Postal_Sector), 2, pad = "0")
  ) %>%
  arrange(as.integer(Postal_District), as.integer(Postal_Sector))

# --- save ---------------------------------------------------------------
saveRDS(postal_districts, out_rds)
# Uncomment if you also want CSVs:
# write_csv(postal_districts,         out_csv)
# write_csv(postal_districts_expanded, out_csv_expanded)

cat("Wrote:", out_rds, "\n")
# cat("Wrote:", out_csv, "and", out_csv_expanded, "\n")

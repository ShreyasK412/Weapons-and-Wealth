## Clean data for three research questions:
## - RQ1: Wealth & Spending (GDP per capita + military % of GDP)
## - RQ2: Regional Arms Races (spending + arms imports)
## - RQ3: Global Conflicts (event-study-friendly panel)

library(tidyverse)
library(readxl)
library(countrycode)

# Create output directories
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)


# Read SIPRI Excel file (multiple sheets)
sipri_sheets <- excel_sheets("data/raw/SIPRI-Milex-data-1949-2024.xlsx")
print("Available sheets in SIPRI file:")
print(sipri_sheets)

# Read % of GDP sheet (usually most complete)
sipri_gdp_pct <- read_excel(
  "data/raw/SIPRI-Milex-data-1949-2024.xlsx",
  sheet = "Share of GDP",  # adjust if different
  skip = 5                 # skip header rows
)

# Read constant USD sheet
sipri_constant_usd <- read_excel(
  "data/raw/SIPRI-Milex-data-1949-2024.xlsx",
  sheet = "Constant (2023) US$",  # adjust if different
  skip = 5
)


wb_ms_raw <- read_csv(
  "data/raw/API_MS.MIL.XPND.GD.ZS_DS2_en_csv_v2_2497.csv",
  skip = 4
)

wb_ms_clean <- wb_ms_raw %>%
  select(
    country_name = `Country Name`,
    country_code = `Country Code`,
    starts_with("19"),
    starts_with("20")
  ) %>%
  filter(!is.na(country_code)) %>%
  pivot_longer(
    cols = starts_with(c("19", "20")),
    names_to = "year",
    values_to = "spending_pct_gdp_wb"
  ) %>%
  mutate(
    year = as.integer(year),
    spending_pct_gdp_wb = as.numeric(spending_pct_gdp_wb),
    iso3c = country_code
  ) %>%
  # Drop aggregates like "World", income groups, etc.
  filter(!str_detect(country_name, "income|World|IBRD|IDA|region|&"))


gdp_pc_raw <- read_csv(
  "data/raw/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_2518.csv",
  skip = 4
)

gdp_pc_clean <- gdp_pc_raw %>%
  select(
    country_name = `Country Name`,
    country_code = `Country Code`,
    starts_with("19"),
    starts_with("20")
  ) %>%
  filter(!is.na(country_code)) %>%
  pivot_longer(
    cols = starts_with(c("19", "20")),
    names_to = "year",
    values_to = "gdp_per_capita_usd"
  ) %>%
  mutate(
    year = as.integer(year),
    gdp_per_capita_usd = as.numeric(gdp_per_capita_usd),
    iso3c = country_code
  ) %>%
  filter(!str_detect(country_name, "income|World|IBRD|IDA|region|&")) %>%
  # keep only what we need for joins (avoid duplicate country_name columns)
  select(iso3c, year, gdp_per_capita_usd)


arms_top <- read_csv(
  "data/raw/import-export-top_1950-2025.csv",
  skip = 10  # skip metadata header rows
)

arms_imports_clean <- arms_top %>%
  # Remove completely empty columns
  select(where(~ !all(is.na(.)))) %>%
  # Drop rank/share columns; keep Recipient + yearly columns
  select(-contains("Rank"), -contains("Share")) %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  # columns that are 4-digit years
    names_to = "year",
    values_to = "arms_imports_tiv"
  ) %>%
  mutate(
    year = as.integer(year),
    arms_imports_tiv = as.numeric(arms_imports_tiv)
  ) %>%
  rename(country_arms = Recipient)


# 5.1 SIPRI % of GDP
sipri_gdp_clean <- sipri_gdp_pct %>%
  # assume first column is country name
  rename(country_sipri = 1) %>%
  pivot_longer(
    cols = -country_sipri,
    names_to = "year",
    values_to = "spending_pct_gdp_sipri"
  ) %>%
  mutate(
    year = as.integer(year),
    spending_pct_gdp_sipri = as.numeric(spending_pct_gdp_sipri)
  ) %>%
  filter(!is.na(country_sipri)) %>%
  mutate(
    iso3c = countrycode(
      country_sipri,
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE
    )
  )

# 5.2 SIPRI constant USD
sipri_usd_clean <- sipri_constant_usd %>%
  rename(country_sipri = 1) %>%
  pivot_longer(
    cols = -country_sipri,
    names_to = "year",
    values_to = "spending_constant_usd_sipri"
  ) %>%
  mutate(
    year = as.integer(year),
    spending_constant_usd_sipri = as.numeric(spending_constant_usd_sipri)
  ) %>%
  mutate(
    iso3c = countrycode(
      country_sipri,
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE
    )
  ) %>%
  # avoid duplicate country_sipri columns on joins – use the one from sipri_gdp_clean
  select(-country_sipri)

# 5.3 Arms imports ISO3C
arms_imports_clean <- arms_imports_clean %>%
  mutate(
    iso3c = countrycode(
      country_arms,
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE
    )
  )

add_regions <- function(df) {
  df %>%
    mutate(
      region = countrycode(
        iso3c,
        origin = "iso3c",
        destination = "un.region.name",
        warn = FALSE
      ),
      subregion = countrycode(
        iso3c,
        origin = "iso3c",
        destination = "un.regionsub.name",
        warn = FALSE
      )
    ) %>%
    mutate(
      region_custom = case_when(
        iso3c %in% c("SAU", "IRN", "IRQ", "ARE", "KWT", "QAT", "BHR", "OMN", "YEM") ~ "Middle East (Gulf)",
        iso3c %in% c("ISR", "PSE", "JOR", "LBN", "SYR") ~ "Middle East (Levant)",
        iso3c %in% c("EGY", "LBY", "TUN", "DZA", "MAR") ~ "North Africa",
        TRUE ~ region
      )
    )
}


rq1_wealth_spending <- sipri_gdp_clean %>%
  full_join(wb_ms_clean, by = c("iso3c", "year")) %>%
  full_join(sipri_usd_clean, by = c("iso3c", "year")) %>%
  full_join(gdp_pc_clean, by = c("iso3c", "year")) %>%
  mutate(
    country = coalesce(country_sipri, country_name),
    spending_pct_gdp = coalesce(spending_pct_gdp_sipri, spending_pct_gdp_wb),
    data_source = case_when(
      !is.na(spending_pct_gdp_sipri) ~ "SIPRI",
      !is.na(spending_pct_gdp_wb) ~ "World Bank",
      TRUE ~ NA_character_
    )
  ) %>%
  add_regions() %>%
  filter(!is.na(iso3c), year >= 1960) %>%
  select(
    country,
    iso3c,
    year,
    region,
    subregion,
    region_custom,
    spending_pct_gdp,
    spending_pct_gdp_sipri,
    spending_pct_gdp_wb,
    spending_constant_usd_sipri,
    gdp_per_capita_usd
  )

write_csv(rq1_wealth_spending, "data/clean/rq1_wealth_spending.csv")
write_rds(rq1_wealth_spending, "data/clean/rq1_wealth_spending.rds")


rq2_arms_races <- sipri_gdp_clean %>%
  full_join(sipri_usd_clean, by = c("iso3c", "year")) %>%
  full_join(arms_imports_clean, by = c("iso3c", "year")) %>%
  mutate(
    country = coalesce(country_sipri, country_arms)
  ) %>%
  add_regions() %>%
  filter(!is.na(iso3c), year >= 1960) %>%
  select(
    country,
    iso3c,
    year,
    region,
    subregion,
    region_custom,
    spending_pct_gdp_sipri,
    spending_constant_usd_sipri,
    arms_imports_tiv
  )

write_csv(rq2_arms_races, "data/clean/rq2_arms_races.csv")
write_rds(rq2_arms_races, "data/clean/rq2_arms_races.rds")


rq3_conflicts <- sipri_gdp_clean %>%
  full_join(sipri_usd_clean, by = c("iso3c", "year")) %>%
  full_join(wb_ms_clean, by = c("iso3c", "year")) %>%
  full_join(arms_imports_clean, by = c("iso3c", "year")) %>%
  mutate(
    country = coalesce(country_sipri, country_name, country_arms),
    spending_pct_gdp = coalesce(spending_pct_gdp_sipri, spending_pct_gdp_wb)
  ) %>%
  add_regions() %>%
  filter(!is.na(iso3c), year >= 1960) %>%
  select(
    country,
    iso3c,
    year,
    region,
    subregion,
    region_custom,
    spending_pct_gdp,
    spending_pct_gdp_sipri,
    spending_pct_gdp_wb,
    spending_constant_usd_sipri,
    arms_imports_tiv
  )

write_csv(rq3_conflicts, "data/clean/rq3_conflicts.csv")
write_rds(rq3_conflicts, "data/clean/rq3_conflicts.rds")

cat("\n=== CLEANING COMPLETE ===\n")
cat("RQ1 rows:", nrow(rq1_wealth_spending), "\n")
cat("RQ2 rows:", nrow(rq2_arms_races), "\n")
cat("RQ3 rows:", nrow(rq3_conflicts), "\n")

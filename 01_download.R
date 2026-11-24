# ============================================================
# Singapore GDP growth forecasting project
# (Download the dataset)
# ============================================================
# Run from the project root (open singapore-gdp-forecasting.Rproj in RStudio,
# or setwd() to the repo folder once). Nothing here depends on RStudio.
#
# Writes:
#   data/raw/<series>.csv              each series exactly as downloaded
#   data/processed/sg_quarterly.csv    merged quarterly panel used by 02_forecast.R
#   data/processed/vintage.txt         download date (GDP gets revised, so record it)
#
# All sources are free and need no API key:
#   SingStat Table Builder  - real GDP, seasonally adjusted, chained 2015 dollars
#   data.gov.sg (MOM)       - overall unemployment rate, seasonally adjusted
#   FRED                    - US and euro-area real GDP, Fed funds rate,
#                             BIS broad nominal effective exchange rate (NEER) for Singapore
#   SingStat (optional)     - CPI; see CPI_TABLE_ID below
# ============================================================

required <- c("httr", "jsonlite", "dplyr", "tidyr", "readr")
to_install <- required[!required %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(readr)
})

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Series identifiers
# ------------------------------------------------------------
# "GDP In Chained (2015) Dollars, By Industry (SSIC 2020), Quarterly, Seasonally Adjusted".
# Series 1 in this table is total GDP; the rest are industries.
GDP_TABLE_ID <- "M015662"

# MOM "Overall Unemployment Rate, Quarterly" on data.gov.sg (from 1992Q1).
UNEMP_DATASET_ID <- "d_ca32584c91ee07d091a4ce75fa868414"

# CPI: SingStat rebased the CPI to 2024 = 100 in 2025, which changed table IDs.
# Find the current table in Table Builder by searching
#   "Consumer Price Index (CPI), 2024 As Base Year, Monthly"
# and paste its ID (starts with "M") here. Series 1 should be "All Items".
# If left as NA, the script skips CPI and 02_forecast.R drops inflation from the models.
CPI_TABLE_ID <- NA

FRED_SERIES <- c(
  us_gdp   = "GDPC1",              # US real GDP, chained 2017 dollars, SAAR, quarterly
  ea_gdp   = "CLVMNACSCAB1GQEA19", # Euro area (19) real GDP, chained 2010 euros, SA, quarterly
  fedfunds = "FEDFUNDS",           # Effective federal funds rate, monthly
  neer     = "NBSGBIS"             # BIS broad NEER for Singapore, 2020 = 100, monthly
)

UA <- user_agent("singapore-gdp-forecasting (github portfolio project)")

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

# Turn period labels into the first day of the period:
#   "1975 1Q" -> 1975-01-01, "2024 Jan" -> 2024-01-01, "1992-03" -> 1992-03-01
parse_period <- function(x) {
  x <- trimws(x)
  out <- rep(as.Date(NA), length(x))

  # Each branch is guarded with any(): sprintf/paste0 on an empty vector can
  # return a length-1 string, which would make as.Date fail.
  q <- grepl("^\\d{4} [1-4]Q$", x)
  if (any(q)) out[q] <- as.Date(sprintf("%s-%02d-01", substr(x[q], 1, 4),
                                        (as.integer(substr(x[q], 6, 6)) - 1) * 3 + 1))

  m <- grepl("^\\d{4} [A-Za-z]{3}$", x)
  if (any(m)) out[m] <- as.Date(sprintf("%s-%02d-01", substr(x[m], 1, 4),
                                        match(substr(x[m], 6, 8), month.abb)))

  ym <- grepl("^\\d{4}-\\d{2}$", x)
  if (any(ym)) out[ym] <- as.Date(paste0(x[ym], "-01"))

  if (any(is.na(out))) {
    stop("Unrecognised period labels: ", paste(head(x[is.na(out)]), collapse = ", "))
  }
  out
}

# First day of the quarter containing each date.
quarter_start <- function(d) {
  as.Date(sprintf("%s-%02d-01", format(d, "%Y"),
                  (as.integer(format(d, "%m")) - 1) %/% 3 * 3 + 1))
}

# Parse a SingStat Table Builder JSON response into a tibble(date, value).
parse_singstat_json <- function(txt, series_no = 1) {
  js <- fromJSON(txt, simplifyVector = FALSE)
  rows <- js$Data$row
  if (length(rows) == 0) stop("SingStat response contained no rows")

  row <- Filter(function(r) as.character(r$seriesNo) == as.character(series_no), rows)
  if (length(row) == 0) stop("Series number ", series_no, " not found in table")
  row <- row[[1]]
  message("  SingStat series: ", row$rowText, " [", row$uoM, "]")

  tibble(
    period = vapply(row$columns, function(c) c$key, character(1)),
    value  = suppressWarnings(as.numeric(
      vapply(row$columns, function(c) as.character(c$value), character(1))))
  ) %>%
    mutate(date = parse_period(period)) %>%
    filter(!is.na(value)) %>%
    arrange(date) %>%
    select(date, value)
}

get_singstat <- function(table_id, series_no = 1) {
  url <- sprintf(
    "https://tablebuilder.singstat.gov.sg/api/table/tabledata/%s?seriesNoORrowNo=%s",
    table_id, series_no)
  resp <- GET(url, UA, timeout(60))
  stop_for_status(resp, task = paste("download SingStat table", table_id))
  parse_singstat_json(content(resp, as = "text", encoding = "UTF-8"), series_no)
}

get_mom_unemployment <- function(dataset_id) {
  url <- sprintf(
    "https://data.gov.sg/api/action/datastore_search?resource_id=%s&limit=1000",
    dataset_id)
  resp <- GET(url, UA, timeout(60))
  stop_for_status(resp, task = "download MOM unemployment from data.gov.sg")
  js <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))

  # "period" is the quarter-end month ("1992-03" = 1992Q1).
  js$result$records %>%
    as_tibble() %>%
    filter(residential_status == "overall") %>%
    transmute(date  = quarter_start(parse_period(period)),
              value = suppressWarnings(as.numeric(seasonally_adj_unemp_rate))) %>%
    filter(!is.na(value)) %>%
    arrange(date)
}

get_fred <- function(series_id) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id)
  resp <- GET(url, UA, timeout(60))
  stop_for_status(resp, task = paste("download FRED series", series_id))
  df <- read_csv(content(resp, as = "text", encoding = "UTF-8"),
                 show_col_types = FALSE, na = c("", "."))
  # The date column is "observation_date" (current) or "DATE" (older files).
  names(df)[1:2] <- c("date", "value")
  df %>%
    mutate(date = as.Date(date), value = as.numeric(value)) %>%
    filter(!is.na(value))
}

# Average a monthly series to quarters, keeping only complete quarters.
to_quarterly_mean <- function(df) {
  df %>%
    mutate(date = quarter_start(date)) %>%
    group_by(date) %>%
    summarise(n = n(), value = mean(value), .groups = "drop") %>%
    filter(n == 3) %>%
    select(-n)
}

save_raw <- function(df, name) {
  write_csv(df, file.path("data/raw", paste0(name, ".csv")))
  message("  saved data/raw/", name, ".csv  (", nrow(df), " obs, ",
          format(min(df$date)), " to ", format(max(df$date)), ")")
  df
}

# ------------------------------------------------------------
# Download
# ------------------------------------------------------------
message("Singapore real GDP (SingStat ", GDP_TABLE_ID, ")")
gdp <- get_singstat(GDP_TABLE_ID) %>% save_raw("sg_gdp_sa")

message("Singapore unemployment rate (MOM via data.gov.sg)")
unemp <- get_mom_unemployment(UNEMP_DATASET_ID) %>% save_raw("sg_unemployment_sa")

cpi <- NULL
if (!is.na(CPI_TABLE_ID)) {
  message("Singapore CPI (SingStat ", CPI_TABLE_ID, ")")
  cpi <- get_singstat(CPI_TABLE_ID) %>% save_raw("sg_cpi")
} else {
  message("CPI_TABLE_ID not set: skipping CPI (inflation will be left out of the models)")
}

fred <- list()
for (nm in names(FRED_SERIES)) {
  message("FRED ", FRED_SERIES[[nm]], " (", nm, ")")
  fred[[nm]] <- get_fred(FRED_SERIES[[nm]]) %>% save_raw(paste0("fred_", nm))
}

# ------------------------------------------------------------
# Build the quarterly panel
# ------------------------------------------------------------
# Growth rates are quarter-on-quarter, annualised, in percent:
#   400 * log(x_t / x_{t-1})
# Log differences are used so growth rates add up across quarters; for the
# range of Singapore's growth they are within ~0.5pp of the compounded rate.
qoq_ann <- function(x) 400 * (log(x) - log(dplyr::lag(x)))

panel <- gdp %>%
  transmute(date, sg_gdp = value) %>%
  left_join(unemp %>% transmute(date, unemp = value), by = "date") %>%
  left_join(fred$us_gdp %>% transmute(date, us_gdp = value), by = "date") %>%
  left_join(fred$ea_gdp %>% transmute(date, ea_gdp = value), by = "date") %>%
  left_join(to_quarterly_mean(fred$fedfunds) %>% transmute(date, fedfunds = value), by = "date") %>%
  left_join(to_quarterly_mean(fred$neer) %>% transmute(date, neer = value), by = "date")

if (!is.null(cpi)) {
  # The CPI table is monthly; average it to quarters like the other monthly series.
  panel <- panel %>%
    left_join(to_quarterly_mean(cpi) %>% transmute(date, cpi = value), by = "date")
}

panel <- panel %>%
  arrange(date) %>%
  mutate(
    year      = as.integer(format(date, "%Y")),
    quarter   = (as.integer(format(date, "%m")) - 1) %/% 3 + 1,
    gdp_g     = qoq_ann(sg_gdp),   # target: Singapore real GDP growth, q/q annualised
    us_g      = qoq_ann(us_gdp),
    ea_g      = qoq_ann(ea_gdp),
    neer_g    = qoq_ann(neer),     # positive = S$ appreciation
    d_unemp   = unemp - dplyr::lag(unemp)
  )

if ("cpi" %in% names(panel)) {
  # Year-on-year, because the headline CPI is not seasonally adjusted.
  panel <- panel %>% mutate(infl = 100 * (cpi / dplyr::lag(cpi, 4) - 1))
}

panel <- panel %>% relocate(date, year, quarter)

write_csv(panel, "data/processed/sg_quarterly.csv")
writeLines(c(paste("Downloaded:", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
             paste("Latest GDP quarter:", format(max(gdp$date)))),
           "data/processed/vintage.txt")

message("\nWrote data/processed/sg_quarterly.csv: ", nrow(panel), " quarters, ",
        format(min(panel$date)), " to ", format(max(panel$date)))
message("Quarters with every regressor available start at ",
        format(min(panel$date[complete.cases(panel)])))

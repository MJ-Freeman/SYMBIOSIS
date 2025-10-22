

 
library(tidyverse) 
library(readxl)
library(janitor) 
library(glue)



DATA_PATH <- "N:/Project work/SYMBIOSIS/data/SYMBIOSIS-analytics-uncoded-02-10-2025.xlsx"
SHEET <- 1

SECTOR_COL <- "3. Which sector of activity does your work fall under?Please select one of the following."
SECTOR_VALUE <- "Railway"

HABITAT_MON <- "6. Does your organisation carry out formal habitat monitoring?"
BIO_MON     <- "32. Does your organisation carry out formal biodiversity monitoring?"

PLAN_HAB <- "8. Does your organisation plan to implement a formal habitat monitoring programme in the future?"
PLAN_BIO <- "34. Does your organisation plan to implement a formal biodiversity monitoring programme in the future?"

HAB_FREQ <- "27. How frequently is habitat monitoring conducted/updated?"
HAB_FREQ_OTHER <- "28. You selected \"Other regular time period\". Please specify:"
BIO_FREQ <- "55. How frequently is biodiversity monitoring conducted/updated?"
BIO_FREQ_OTHER <- "56. You selected \"Other regular time period\". Please specify."

OUTDIR <- "output"
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

# Load and clean
raw <- read_excel(DATA_PATH, sheet = SHEET)
# Normalise column names
names(raw) <- names(raw) |>
  stringr::str_replace_all("’", "'") |>
  stringr::str_replace_all("\\s+", " ") |>
  stringr::str_trim()
# Prevent hidden errors
needed <- c(SECTOR_COL, HABITAT_MON, BIO_MON, PLAN_HAB, PLAN_BIO,
            HAB_FREQ, HAB_FREQ_OTHER, BIO_FREQ, BIO_FREQ_OTHER)
missing <- setdiff(needed, names(raw))
if (length(missing)) {
  stop("These columns were not found (after normalising headers):\n",
       paste(missing, collapse = "\n"))
}

# Filter Railway respondents 
d <- raw %>% dplyr::filter(.data[[SECTOR_COL]] == SECTOR_VALUE)

n_rail <- nrow(d)
message("N railway respondents: ", n_rail)

# Helper Functions
clean_text <- function(x) {
  x |>
    as.character() |>
    stringr::str_trim() |>                 # trim leading/trailing spaces
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_replace_all("’", "'") |>  # normalise curly apostrophes
    stringr::str_to_lower()
}

yn_true <- function(x) {
  x <- clean_text(x)
  x %in% c("yes","y","true","t")
}

yn_false <- function(x) {
  x <- clean_text(x)
  x %in% c("no","n","false","f")
}

count_pct <- function(v) {
  tibble(value = v) |>
    mutate(value = clean_text(value),
           value = dplyr::if_else(is.na(value) | value == "", "missing", value)) |>
    mutate(value = forcats::fct_infreq(factor(value))) |>
    count(value, name = "n") |>
    mutate(pct = round(100 * n / sum(n), 1))
}

save_csv <- function(x, name) write_csv(x, file.path(OUTDIR, name))

# Monitoring Summary
hab_yes  <- mean(yn_true(d[[HABITAT_MON]]), na.rm = TRUE)
bio_yes  <- mean(yn_true(d[[BIO_MON]]),     na.rm = TRUE)
both_yes <- mean(yn_true(d[[HABITAT_MON]]) & yn_true(d[[BIO_MON]]), na.rm = TRUE)

summary_monitoring <- tibble(
  metric = c("Habitat monitoring", "Biodiversity monitoring", "Both"),
  pct    = round(100 * c(hab_yes, bio_yes, both_yes), 1),
  n      = n_rail
)
save_csv(summary_monitoring, "summary_monitoring.csv")

# Auto-detect whether the plan questions were answered only by "No" respondents or by everyone
hab_plan_by_all <- any(!is.na(d[[PLAN_HAB]]) & !yn_false(d[[HABITAT_MON]]), na.rm = TRUE)
bio_plan_by_all <- any(!is.na(d[[PLAN_BIO]]) & !yn_false(d[[BIO_MON]]),     na.rm = TRUE)

plans_hab_df <- if (hab_plan_by_all) d else d %>% filter(yn_false(.data[[HABITAT_MON]]))
plans_bio_df <- if (bio_plan_by_all) d else d %>% filter(yn_false(.data[[BIO_MON]]))

# Calculate plan % among the selected base (either all, or only current "No")
plans_summary <- tibble(
  domain       = c("Habitat","Biodiversity"),
  base_rule    = c(if (hab_plan_by_all) "All railway respondents" else "Only those not currently monitoring",
                   if (bio_plan_by_all) "All railway respondents" else "Only those not currently monitoring"),
  n_base       = c(nrow(plans_hab_df), nrow(plans_bio_df)),
  plan_yes_pct = round(100 * c(mean(yn_true(plans_hab_df[[PLAN_HAB]]), na.rm = TRUE),
                               mean(yn_true(plans_bio_df[[PLAN_BIO]]), na.rm = TRUE)), 1),
  plan_dk_pct  = round(100 * c(
    mean(clean_text(plans_hab_df[[PLAN_HAB]]) %in% c("i don't know","i dont know"), na.rm = TRUE),
    mean(clean_text(plans_bio_df[[PLAN_BIO]]) %in% c("i don't know","i dont know"), na.rm = TRUE)
  ), 1)
)
save_csv(plans_summary, "plans_summary.csv")

# Frequency Summary - both "overall" and "yes only" summaries
summarise_frequency <- function(df, mon_col, freq_col, label) {
  
  all_responses <- count_pct(df[[freq_col]])
  all_responses$type <- "All respondents"
  
  yes_idx <- yn_true(df[[mon_col]])
  yes_idx[is.na(yes_idx)] <- FALSE
  df_yes <- df[yes_idx, , drop = FALSE]
  
  if (nrow(df_yes) > 0) {
    yes_responses <- count_pct(df_yes[[freq_col]])
  } else {
    yes_responses <- tibble(value = factor("no data"), n = 0, pct = 0)
  }
  yes_responses$type <- "Only those currently monitoring"
  
  combined <- bind_rows(all_responses, yes_responses) %>%
    mutate(
      domain  = label,
      n_total = nrow(df),
      n_yes   = sum(yes_idx, na.rm = TRUE)
    ) %>%
    select(domain, type, n_total, n_yes, value, n, pct)
  
  combined
}

freq_hab_combined <- summarise_frequency(d, HABITAT_MON, HAB_FREQ, "Habitat")
freq_bio_combined <- summarise_frequency(d, BIO_MON,    BIO_FREQ,  "Biodiversity")

freq_combined <- bind_rows(freq_hab_combined, freq_bio_combined)
save_csv(freq_combined,        "freq_combined.csv")
save_csv(freq_hab_combined,    "freq_hab_combined.csv")
save_csv(freq_bio_combined,    "freq_bio_combined.csv")















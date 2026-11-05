# R/02_build_site_data.R
#
# Reads the case study responses from the local OneDrive-synced folder, saves
# date-stamped copies to data/raw/ for the audit trail, then processes them and
# writes the public CSV.
#
# There are two sources:
#   - The spreadsheet for the original form. That form no longer exists, so this
#     file no longer changes.
#   - The workbook for the current form, which is owned by a Microsoft 365 group.
#
# BEFORE RUNNING: open the current form in Microsoft Forms and open its responses
# workbook in the browser. Forms only copies new responses into the workbook when
# it is opened, so check the "Last modified" message below once it has synced.
#
# Requires: readxl, dplyr, stringr, readr, janitor

library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(janitor)

# ---------------------------------------------------------------------------
# Source files (OneDrive-synced SharePoint locations)
# ---------------------------------------------------------------------------

# Finds the OneDrive folder of whoever is running the script.
ONEDRIVE_ROOT <- normalizePath(
  Sys.getenv(
    "OneDriveCommercial",
    unset = file.path("C:/Users", Sys.getenv("USERNAME"), "OneDrive - United Nations")
  ),
  winslash = "/",
  mustWork = FALSE
)

ORIGINAL_FORM_FILE <- "SD Documents/Governance/ESCAP Committee (CST)/DASH Teams/AI for Official Statistics/Template development/Template responses/AI for Official Statistics_ Case Study Submission(1-10).xlsx"

# The current form's responses workbook. Do not move or rename it, or Forms will
# stop syncing to it.
CURRENT_FORM_FILE <- "Documents - ESCAP-SD NEW/AI for Official Statistics_ Case Study Submission.xlsx"

source_files <- c(original = file.path(ONEDRIVE_ROOT, ORIGINAL_FORM_FILE))

if (is.na(CURRENT_FORM_FILE)) {
  message("CURRENT_FORM_FILE is not set, so only the original form's responses will be read.")
} else {
  source_files <- c(source_files, current = file.path(ONEDRIVE_ROOT, CURRENT_FORM_FILE))
}

# ---------------------------------------------------------------------------
# Check source files exist and report when they were last modified
# ---------------------------------------------------------------------------

for (label in names(source_files)) {
  path <- source_files[[label]]
  
  if (!file.exists(path)) {
    stop("Source file not found (", label, "):\n  ", path,
         "\nCheck the file path and that OneDrive is synced.")
  }
  
  message("Source file (", label, "): ", basename(path),
          "\nLast modified: ", format(file.mtime(path), "%Y-%m-%d %H:%M:%S"))
}

# ---------------------------------------------------------------------------
# Save date-stamped archive copies to data/raw/
# ---------------------------------------------------------------------------

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Stops with a clear message if a copy fails. file.copy() can otherwise leave an
# empty file behind, for example when the source is open in Excel or is still
# downloading from OneDrive.
copy_checked <- function(from, to) {
  for (attempt in 1:3) {
    copied <- suppressWarnings(file.copy(from, to, overwrite = TRUE))
    
    if (isTRUE(copied) && isTRUE(file.size(to) > 0) && isTRUE(file.size(to) == file.size(from))) {
      return(invisible(TRUE))
    }
    
    Sys.sleep(3)
  }
  
  stop(
    "Could not make a complete copy of:\n  ", from,
    "\nSource size: ", file.size(from), " bytes. Copy size: ", file.size(to), " bytes.",
    "\nClose the file if it is open in desktop Excel, set it to 'Always keep on this device' in File Explorer, and run again."
  )
}

# The working copies go in R's temporary folder, because this repository may
# itself be synced by OneDrive, which can lock new files while uploading them.
working_paths <- setNames(
  file.path(tempdir(), paste0("forms_responses_", names(source_files), ".xlsx")),
  names(source_files)
)

for (label in names(source_files)) {
  archive_path <- file.path("data/raw", paste0("forms_responses_", label, "_", timestamp, ".xlsx"))
  
  copy_checked(source_files[[label]], archive_path)
  copy_checked(source_files[[label]], working_paths[[label]])
  
  message("Archived to: ", archive_path)
}

# ---------------------------------------------------------------------------
# Load and validate columns
# ---------------------------------------------------------------------------

# If MS Forms question wording changes, clean_names() produces a different
# column name and the mapping below silently produces NAs. This check catches
# that before it causes a silent failure on the published site.

expected_cols <- c(
  "id",
  "initiative_name_title_for_this_case_study",
  "permissions_for_use_and_sharing",
  "year_s_during_which_the_initiative_was_active_please_select_all_years_in_which_the_project_was_active_up_to_the_present_if_ongoing",
  "which_statistical_domain_s_this_initiative_cover_please_select_all_applicable_options",
  "gsbpm_phase_s_where_ai_is_applied_within_this_initiative",
  "type_s_of_ai_used",
  "models_or_algorithms_used_if_known",
  "name_of_lead_organisation_for_case_study",
  "type_of_organisation",
  "main_purpose_of_the_ai_initiative",
  "implementation_approach",
  "types_of_tools_used",
  "current_maturity_implementation_status",
  "what_is_the_business_need_or_problem_being_addressed_through_this_initiative",
  "briefly_describe_how_ai_is_being_used_to_address_this_need",
  "key_benefits_observed_so_far_if_any",
  "main_challenges_or_risks_encountered",
  "does_this_ai_initiative_involve_the_use_of_any_personal_or_sensitive_data",
  "what_governance_or_ethical_measures_have_been_applied",
  "brief_note_on_ethics_bias_or_accountability_considerations",
  "please_provide_links_to_related_resources_and_further_information_and_context_where_available"
)

# Every column is read as text so the two files can be combined safely.
read_responses <- function(path) {
  readxl::read_excel(path, col_types = "text") %>%
    janitor::clean_names() %>%
    # The new form restores the word "does" missing from this question's
    # heading in the original spreadsheet.
    rename(any_of(c(
      which_statistical_domain_s_this_initiative_cover_please_select_all_applicable_options =
        "which_statistical_domain_s_does_this_initiative_cover_please_select_all_applicable_options"
    )))
}

responses <- lapply(names(working_paths), function(label) {
  df <- read_responses(working_paths[[label]])
  
  missing_cols <- setdiff(expected_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "The following expected columns were not found in the ", label, " file after clean_names().\n",
      "This usually means a Forms question was reworded. Update the column names below to match.\n\n",
      "Missing:\n  ", paste(missing_cols, collapse = "\n  ")
    )
  }
  
  message("Column validation passed (", label, "): ", nrow(df), " response(s).")
  df
})

raw <- bind_rows(responses)

# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

# Normalise MS Forms multi-select strings.
# Handles two formats produced by different submission routes:
#   Forms workbook or manual export: "Value A; Value B; Value C;"
#   Power Automate export:           ["Value A","Value B","Value C"]
norm_multi <- function(x) {
  x <- as.character(x)
  
  vapply(x, function(val) {
    if (is.na(val) || val == "NA" || trimws(val) == "") return(NA_character_)
    
    val <- trimws(val)
    
    # JSON array format: ["Value A","Value B"]
    if (startsWith(val, "[")) {
      matches <- regmatches(val, gregexpr('"([^"]*)"', val, perl = TRUE))[[1]]
      parts   <- gsub('^"|"$', "", matches)
      parts   <- trimws(parts)
      parts   <- parts[nzchar(parts)]
      return(paste(parts, collapse = ";"))
    }
    
    # Semicolon-delimited format: "Value A; Value B; Value C;"
    val <- str_replace_all(val, "\\s*;\\s*", ";")
    val <- str_replace_all(val, ";+$", "")
    str_squish(val)
    
  }, character(1))
}

# Publish if the response indicates permission to include in the regional
# repository. Matches both publishable options:
#   "...may be included only in the ESCAP/DASH regional repository"
#   "...may be included in the ESCAP/DASH regional repository and shared..."
has_repo_permission <- function(x) {
  x <- as.character(x)
  str_detect(x, regex("ESCAP/DASH regional repository", ignore_case = TRUE))
}

slugify <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "-") %>%
    str_replace_all("(^-|-$)", "")
}

# ---------------------------------------------------------------------------
# Processing
# ---------------------------------------------------------------------------

cs <- raw %>%
  mutate(
    initiative_title = str_squish(as.character(initiative_name_title_for_this_case_study)),
    lead_org         = str_squish(as.character(name_of_lead_organisation_for_case_study)),
    permission_ok    = has_repo_permission(permissions_for_use_and_sharing),
    
    # All multi-select fields normalised — handles both old (semicolon) and
    # new (JSON array) formats produced by different submission routes
    years_active            = norm_multi(year_s_during_which_the_initiative_was_active_please_select_all_years_in_which_the_project_was_active_up_to_the_present_if_ongoing),
    domains                 = norm_multi(which_statistical_domain_s_this_initiative_cover_please_select_all_applicable_options),
    gsbpm_phases            = norm_multi(gsbpm_phase_s_where_ai_is_applied_within_this_initiative),
    ai_types                = norm_multi(type_s_of_ai_used),
    purpose_norm            = norm_multi(main_purpose_of_the_ai_initiative),
    implementation_norm     = norm_multi(implementation_approach),
    tools_norm              = norm_multi(types_of_tools_used),
    governance_norm         = norm_multi(what_governance_or_ethical_measures_have_been_applied),
    
    case_id = paste0(as.character(id), "-", slugify(initiative_title))
  )

# PUBLIC dataset (no PII)
cs_public <- cs %>%
  filter(permission_ok) %>%
  transmute(
    case_id,
    title                   = initiative_title,
    lead_organisation       = lead_org,
    organisation_type       = type_of_organisation,
    years_active,
    statistical_domains     = domains,
    gsbpm_phases,
    ai_types,
    models_or_algorithms    = models_or_algorithms_used_if_known,
    purpose                 = purpose_norm,
    implementation_approach = implementation_norm,
    tools_used              = tools_norm,
    maturity                = current_maturity_implementation_status,
    business_need           = what_is_the_business_need_or_problem_being_addressed_through_this_initiative,
    description             = briefly_describe_how_ai_is_being_used_to_address_this_need,
    benefits                = key_benefits_observed_so_far_if_any,
    challenges              = main_challenges_or_risks_encountered,
    personal_sensitive_data = does_this_ai_initiative_involve_the_use_of_any_personal_or_sensitive_data,
    governance_measures     = governance_norm,
    ethics_note             = brief_note_on_ethics_bias_or_accountability_considerations,
    links                   = please_provide_links_to_related_resources_and_further_information_and_context_where_available
  )

# ---------------------------------------------------------------------------
# Write output and confirm
# ---------------------------------------------------------------------------

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(cs_public, "data/processed/case_studies_public.csv")

message(
  "Done. ", nrow(cs), " total response(s); ",
  sum(cs$permission_ok, na.rm = TRUE), " with repository permission; ",
  nrow(cs_public), " row(s) written to data/processed/case_studies_public.csv."
)
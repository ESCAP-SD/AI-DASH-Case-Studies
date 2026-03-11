# R/02_build_site_data.R
#
# Reads the latest case study responses from the local OneDrive-synced folder,
# saves a date-stamped copy to data/raw/ for the audit trail, then processes
# and writes the public CSV.
#
# The source file is the OneDrive-synced copy of the SharePoint file.
# As long as OneDrive is running and synced, this will always reflect the
# latest version without any manual downloading.
#
# Requires: readxl, dplyr, stringr, readr, janitor

library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(janitor)

# ---------------------------------------------------------------------------
# Source file (OneDrive-synced SharePoint location)
# ---------------------------------------------------------------------------

SOURCE_FILE <- "C:/Users/RTONKIN/OneDrive - United Nations/SD Documents/Governance/ESCAP Committee (CST)/DASH Teams/AI for Official Statistics/Template development/Template responses/AI for Official Statistics_ Case Study Submission(1-10).xlsx"

# ---------------------------------------------------------------------------
# Check source file exists and report when it was last modified
# ---------------------------------------------------------------------------

if (!file.exists(SOURCE_FILE)) {
  stop("Source file not found:\n  ", SOURCE_FILE,
       "\nCheck the file path and that OneDrive is synced.")
}

message("Source file: ", basename(SOURCE_FILE),
        "\nLast modified: ", format(file.mtime(SOURCE_FILE), "%Y-%m-%d %H:%M:%S"))

# ---------------------------------------------------------------------------
# Save a date-stamped archive copy to data/raw/
# ---------------------------------------------------------------------------

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

timestamp    <- format(Sys.time(), "%Y%m%d_%H%M%S")
archive_path <- paste0("data/raw/forms_responses_", timestamp, ".xlsx")
working_path <- "data/raw/forms_responses.xlsx"

file.copy(SOURCE_FILE, archive_path, overwrite = TRUE)
file.copy(SOURCE_FILE, working_path, overwrite = TRUE)

message("Archived to: ", archive_path)

# ---------------------------------------------------------------------------
# Load and validate columns
# ---------------------------------------------------------------------------

raw <- readxl::read_excel(working_path) %>%
  janitor::clean_names()

# If MS Forms question wording changes, clean_names() produces a different
# column name and the mapping below silently produces NAs. This check catches
# that before it causes a silent failure on the published site.

expected_cols <- c(
  "initiative_name_title_for_this_case_study",
  "permissions_for_use_and_sharing",
  "year_s_during_which_the_initiative_was_active_please_select_all_years_in_which_the_project_was_active_up_to_the_present_if_ongoing",
  "which_statistical_domain_s_this_initiative_cover_please_select_all_applicable_options",
  "gsbpm_phase_s_where_ai_is_applied_within_this_initiative",
  "type_s_of_ai_used",
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

missing_cols <- setdiff(expected_cols, names(raw))

if (length(missing_cols) > 0) {
  stop(
    "The following expected columns were not found after clean_names().\n",
    "This usually means a Forms question was reworded. Update the column names below to match.\n\n",
    "Missing:\n  ", paste(missing_cols, collapse = "\n  ")
  )
}

message("Column validation passed. ", nrow(raw), " total response(s) in the source file.")

# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

# Normalise MS Forms multi-select strings.
# Handles two formats produced by different submission routes:
#   Old (manual export):         "Value A; Value B; Value C;"
#   New (Power Automate export): ["Value A","Value B","Value C"]
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
  sum(cs$permission_ok), " with repository permission; ",
  nrow(cs_public), " row(s) written to data/processed/case_studies_public.csv."
)
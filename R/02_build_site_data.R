# R/02_build_site_data.R
library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(janitor)

infile <- "data/raw/forms_responses.xlsx"

raw <- readxl::read_excel(infile) %>%
  janitor::clean_names()

# normalise MS Forms multi-select strings like "A; B; C;"
norm_multi <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\s*;\\s*", ";") %>%
    str_replace_all(";+$", "") %>%
    str_squish()
}

# publish only if permissions include the repo permission text
has_repo_permission <- function(x) {
  x <- as.character(x)
  str_detect(x, regex("included in the ESCAP/DASH regional repository", ignore_case = TRUE))
}

slugify <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "-") %>%
    str_replace_all("(^-|-$)", "")
}

cs <- raw %>%
  mutate(
    initiative_title = str_squish(as.character(initiative_name_title_for_this_case_study)),
    lead_org = str_squish(as.character(name_of_lead_organisation_for_case_study)),
    permission_ok = has_repo_permission(permissions_for_use_and_sharing),
    
    years_active = norm_multi(year_s_during_which_the_initiative_was_active_please_select_all_years_in_which_the_project_was_active_up_to_the_present_if_ongoing),
    domains = norm_multi(which_statistical_domain_s_this_initiative_cover_please_select_all_applicable_options),
    gsbpm_phases = norm_multi(gsbpm_phase_s_relevant_to_this_initiative),
    ai_types = norm_multi(type_s_of_ai_used),
    
    case_id = paste0(as.character(id), "-", slugify(initiative_title))
  )

# PUBLIC dataset (no PII)
cs_public <- cs %>%
  filter(permission_ok) %>%
  transmute(
    case_id,
    title = initiative_title,
    lead_organisation = lead_org,
    organisation_type = type_of_organisation,
    years_active,
    statistical_domains = domains,
    gsbpm_phases,
    ai_types,
    models_or_algorithms = models_or_algorithms_used_if_known,
    purpose = main_purpose_of_the_ai_initiative,
    implementation_approach,
    tools_used = types_of_tools_used,
    maturity = current_maturity_implementation_status,
    business_need = what_is_the_business_need_or_problem_being_addressed_through_this_initiative,
    description = briefly_describe_how_ai_is_being_used_to_address_this_need,
    benefits = key_benefits_observed_so_far_if_any,
    challenges = main_challenges_or_risks_encountered,
    personal_sensitive_data = does_this_ai_initiative_involve_the_use_of_any_personal_or_sensitive_data,
    governance_measures = what_governance_or_ethical_measures_have_been_applied,
    ethics_note = brief_note_on_ethics_bias_or_accountability_considerations,
    links = please_provide_links_to_related_resources_and_further_information_and_context_where_available
  )

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(cs_public, "data/processed/case_studies_public.csv")

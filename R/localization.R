#' Internationalization and Localization Support
#'
#' Functions for translating table headers, labels, and other text elements
#' between languages. Essential for German HTA (G-BA/AMNOG) dossiers which
#' require German table headers and labels.
#'
#' @name localization
#' @keywords internal
NULL

# =============================================================================
# Global Locale State Management
# =============================================================================

# Environment to store current locale setting
.pharmhand_locale <- new.env(parent = emptyenv())
.pharmhand_locale$current <- "en"
.pharmhand_locale$custom_translations <- list()

# =============================================================================
# Translation Dictionaries
# =============================================================================

#' @noRd
.pharmhand_translations <- list(
	en = list(
		# Headers
		statistic = "Statistic",
		parameter = "Parameter",
		category = "Category",
		treatment = "Treatment",
		placebo = "Placebo",
		total = "Total",
		value = "Value",
		variable = "Variable",
		subgroup = "Subgroup",
		characteristic = "Characteristic",
		result = "Result",
		comparison = "Comparison",
		difference = "Difference",

		# Columns
		n = "n",
		pct = "%",
		n_pct = "n (%)",
		mean = "Mean",
		sd = "SD",
		mean_sd = "Mean (SD)",
		median = "Median",
		min = "Min",
		max = "Max",
		min_max = "Min, Max",
		q1 = "Q1",
		q3 = "Q3",
		iqr = "IQR",
		range = "Range",
		se = "SE",
		cv = "CV",
		cv_pct = "CV%",

		# Populations
		safety_population = "Safety Population",
		itt_population = "ITT Population",
		pp_population = "Per-Protocol Population",
		fas_population = "Full Analysis Set",
		miti_population = "Modified ITT Population",
		evaluable_population = "Evaluable Population",

		# AE terms
		teae = "Treatment-Emergent Adverse Event",
		teae_short = "TEAE",
		sae = "Serious Adverse Event",
		sae_short = "SAE",
		ae = "Adverse Event",
		ae_short = "AE",
		preferred_term = "Preferred Term",
		system_organ_class = "System Organ Class",
		soc = "SOC",
		pt = "PT",
		severity = "Severity",
		relationship = "Relationship",
		mild = "Mild",
		moderate = "Moderate",
		severe = "Severe",
		related = "Related",
		not_related = "Not Related",
		possibly_related = "Possibly Related",
		probably_related = "Probably Related",
		leading_to_discontinuation = "Leading to Discontinuation",
		leading_to_death = "Leading to Death",
		subjects_with_any_ae = "Subjects with Any AE",
		subjects_with_any_teae = "Subjects with Any TEAE",
		subjects_with_any_sae = "Subjects with Any SAE",

		# Efficacy terms
		hazard_ratio = "Hazard Ratio",
		risk_difference = "Risk Difference",
		risk_ratio = "Risk Ratio",
		odds_ratio = "Odds Ratio",
		relative_risk = "Relative Risk",
		confidence_interval = "Confidence Interval",
		ci = "CI",
		ci_95 = "95% CI",
		ci_90 = "90% CI",
		ci_99 = "99% CI",
		p_value = "p-value",
		events = "Events",
		censored = "Censored",
		median_survival = "Median Survival",
		time_to_event = "Time to Event",
		response_rate = "Response Rate",
		complete_response = "Complete Response",
		partial_response = "Partial Response",
		stable_disease = "Stable Disease",
		progressive_disease = "Progressive Disease",
		objective_response = "Objective Response",
		disease_control = "Disease Control",
		change_from_baseline = "Change from Baseline",
		cfb = "CFB",
		ls_mean = "LS Mean",
		ls_mean_difference = "LS Mean Difference",
		responders = "Responders",
		non_responders = "Non-Responders",

		# Demographics
		age = "Age",
		age_years = "Age (years)",
		age_group = "Age Group",
		sex = "Sex",
		gender = "Gender",
		male = "Male",
		female = "Female",
		race = "Race",
		ethnicity = "Ethnicity",
		weight = "Weight",
		weight_kg = "Weight (kg)",
		height = "Height",
		height_cm = "Height (cm)",
		bmi = "BMI",
		bmi_full = "Body Mass Index",
		bmi_kgm2 = "BMI (kg/m2)",
		country = "Country",
		region = "Region",
		site = "Site",

		# Study terms
		baseline = "Baseline",
		screening = "Screening",
		visit = "Visit",
		end_of_study = "End of Study",
		follow_up = "Follow-up",
		randomization = "Randomization",
		treatment_period = "Treatment Period",
		day = "Day",
		week = "Week",
		month = "Month",
		year = "Year",

		# Disposition
		disposition = "Disposition",
		completed = "Completed",
		discontinued = "Discontinued",
		ongoing = "Ongoing",
		reason_for_discontinuation = "Reason for Discontinuation",
		lost_to_follow_up = "Lost to Follow-up",
		withdrawal_by_subject = "Withdrawal by Subject",
		adverse_event = "Adverse Event",
		death = "Death",
		protocol_deviation = "Protocol Deviation",
		lack_of_efficacy = "Lack of Efficacy",
		physician_decision = "Physician Decision",
		other = "Other",

		# Lab terms
		shift_table = "Shift Table",
		normal = "Normal",
		low = "Low",
		high = "High",
		clinically_significant = "Clinically Significant",
		laboratory_parameter = "Laboratory Parameter",
		reference_range = "Reference Range",
		unit = "Unit",
		alanine_aminotransferase = "Alanine Aminotransferase",
		aspartate_aminotransferase = "Aspartate Aminotransferase",
		alkaline_phosphatase = "Alkaline Phosphatase",
		bilirubin = "Bilirubin",
		creatinine = "Creatinine",
		hemoglobin = "Hemoglobin",
		platelets = "Platelets",
		white_blood_cells = "White Blood Cells",
		neutrophils = "Neutrophils",

		# Medical history / Concomitant medications
		medical_history = "Medical History",
		concomitant_medications = "Concomitant Medications",
		prior_medications = "Prior Medications",
		atc_class = "ATC Class",
		drug_name = "Drug Name",
		indication = "Indication",
		start_date = "Start Date",
		end_date = "End Date",

		# SMD / Balance terms
		smd = "SMD",
		standardized_mean_difference = "Standardized Mean Difference",
		imbalance = "Imbalance",
		balanced = "Balanced",
		threshold = "Threshold",

		# Table footnotes
		fn_safety_pop = "Safety Population",
		fn_itt_pop = "Intent-to-Treat Population",
		fn_n_pct = "n (%) = Number (percentage) of subjects",
		fn_ci = "CI = Confidence Interval",
		fn_smd = "SMD = Standardized Mean Difference",
		fn_teae = "TEAE = Treatment-Emergent Adverse Event",
		fn_sae = "SAE = Serious Adverse Event",
		fn_hr = "HR = Hazard Ratio",
		fn_or = "OR = Odds Ratio",
		fn_subjects_counted_once = "Subjects counted once per category",

		# Reference group
		reference = "Reference",
		reference_group = "Reference Group",
		treatment_group = "Treatment Group",
		control_group = "Control Group",
		active_control = "Active Control",
		comparator = "Comparator"
	),

	de = list(
		# Headers
		statistic = "Statistik",
		parameter = "Parameter",
		category = "Kategorie",
		treatment = "Behandlung",
		placebo = "Placebo",
		total = "Gesamt",
		value = "Wert",
		variable = "Variable",
		subgroup = "Subgruppe",
		characteristic = "Merkmal",
		result = "Ergebnis",
		comparison = "Vergleich",
		difference = "Differenz",

		# Columns
		n = "n",
		pct = "%",
		n_pct = "n (%)",
		mean = "Mittelwert",
		sd = "SD",
		mean_sd = "Mittelwert (SD)",
		median = "Median",
		min = "Min",
		max = "Max",
		min_max = "Min, Max",
		q1 = "Q1",
		q3 = "Q3",
		iqr = "IQR",
		range = "Spannweite",
		se = "SE",
		cv = "VK",
		cv_pct = "VK%",

		# Populations
		safety_population = "Sicherheitspopulation",
		itt_population = "ITT-Population",
		pp_population = "Per-Protokoll-Population",
		fas_population = "Vollanalyse-Population",
		miti_population = "Modifizierte ITT-Population",
		evaluable_population = "Auswertbare Population",

		# AE terms
		teae = "Unerwartetes Ereignis unter Behandlung",
		teae_short = "TEAE",
		sae = "Schwerwiegendes unerwartetes Ereignis",
		sae_short = "SUE",
		ae = "Unerwartetes Ereignis",
		ae_short = "UE",
		preferred_term = "Bevorzugter Begriff",
		system_organ_class = "Systemorganklasse",
		soc = "SOC",
		pt = "PT",
		severity = "Schweregrad",
		relationship = "Zusammenhang",
		mild = "Leicht",
		moderate = "Mittelgradig",
		severe = "Schwer",
		related = "In Zusammenhang stehend",
		not_related = "Kein Zusammenhang",
		possibly_related = "Moeglicherweise in Zusammenhang",
		probably_related = "Wahrscheinlich in Zusammenhang",
		leading_to_discontinuation = "Fuehrte zum Abbruch",
		leading_to_death = "Mit Todesfolge",
		subjects_with_any_ae = "Patienten mit einem UE",
		subjects_with_any_teae = "Patienten mit einem TEAE",
		subjects_with_any_sae = "Patienten mit einem SUE",

		# Efficacy terms
		hazard_ratio = "Hazard Ratio",
		risk_difference = "Risikodifferenz",
		risk_ratio = "Risikoverhaeltnis",
		odds_ratio = "Odds Ratio",
		relative_risk = "Relatives Risiko",
		confidence_interval = "Konfidenzintervall",
		ci = "KI",
		ci_95 = "95%-KI",
		ci_90 = "90%-KI",
		ci_99 = "99%-KI",
		p_value = "p-Wert",
		events = "Ereignisse",
		censored = "Zensiert",
		median_survival = "Medianes Ueberleben",
		time_to_event = "Zeit bis zum Ereignis",
		response_rate = "Ansprechrate",
		complete_response = "Vollstaendiges Ansprechen",
		partial_response = "Partielles Ansprechen",
		stable_disease = "Stabile Erkrankung",
		progressive_disease = "Fortschreitende Erkrankung",
		objective_response = "Objektives Ansprechen",
		disease_control = "Krankheitskontrolle",
		change_from_baseline = "Aenderung gegenueber Baseline",
		cfb = "CFB",
		ls_mean = "LS-Mittelwert",
		ls_mean_difference = "LS-Mittelwertdifferenz",
		responders = "Responder",
		non_responders = "Non-Responder",

		# Demographics
		age = "Alter",
		age_years = "Alter (Jahre)",
		age_group = "Altersgruppe",
		sex = "Geschlecht",
		gender = "Geschlecht",
		male = "Maennlich",
		female = "Weiblich",
		race = "Ethnie",
		ethnicity = "Ethnizitaet",
		weight = "Gewicht",
		weight_kg = "Gewicht (kg)",
		height = "Groesse",
		height_cm = "Groesse (cm)",
		bmi = "BMI",
		bmi_full = "Body-Mass-Index",
		bmi_kgm2 = "BMI (kg/m2)",
		country = "Land",
		region = "Region",
		site = "Studienzentrum",

		# Study terms
		baseline = "Baseline",
		screening = "Screening",
		visit = "Visite",
		end_of_study = "Studienende",
		follow_up = "Nachbeobachtung",
		randomization = "Randomisierung",
		treatment_period = "Behandlungsphase",
		day = "Tag",
		week = "Woche",
		month = "Monat",
		year = "Jahr",

		# Disposition
		disposition = "Disposition",
		completed = "Abgeschlossen",
		discontinued = "Abgebrochen",
		ongoing = "Laufend",
		reason_for_discontinuation = "Grund fuer Abbruch",
		lost_to_follow_up = "Lost to Follow-up",
		withdrawal_by_subject = "Ruecknahme durch Patient",
		adverse_event = "Unerwartetes Ereignis",
		death = "Tod",
		protocol_deviation = "Protokollabweichung",
		lack_of_efficacy = "Mangelnde Wirksamkeit",
		physician_decision = "Arztentscheidung",
		other = "Sonstige",

		# Lab terms
		shift_table = "Shift-Tabelle",
		normal = "Normal",
		low = "Niedrig",
		high = "Hoch",
		clinically_significant = "Klinisch signifikant",
		laboratory_parameter = "Laborparameter",
		reference_range = "Referenzbereich",
		unit = "Einheit",
		alanine_aminotransferase = "Alanin-Aminotransferase",
		aspartate_aminotransferase = "Aspartat-Aminotransferase",
		alkaline_phosphatase = "Alkalische Phosphatase",
		bilirubin = "Bilirubin",
		creatinine = "Kreatinin",
		hemoglobin = "Haemoglobin",
		platelets = "Thrombozyten",
		white_blood_cells = "Leukozyten",
		neutrophils = "Neutrophile",

		# Medical history / Concomitant medications
		medical_history = "Anamnese",
		concomitant_medications = "Begleitmedikation",
		prior_medications = "Vormedikation",
		atc_class = "ATC-Klasse",
		drug_name = "Wirkstoffname",
		indication = "Indikation",
		start_date = "Startdatum",
		end_date = "Enddatum",

		# SMD / Balance terms
		smd = "SMD",
		standardized_mean_difference = "Standardisierte Mittelwertdifferenz",
		imbalance = "Ungleichgewicht",
		balanced = "Ausgeglichen",
		threshold = "Schwellenwert",

		# Table footnotes
		fn_safety_pop = "Sicherheitspopulation",
		fn_itt_pop = "Intent-to-Treat Population",
		fn_n_pct = "n (%) = Anzahl (Prozentsatz) der Patienten",
		fn_ci = "KI = Konfidenzintervall",
		fn_smd = "SMD = Standardisierte Mittelwertdifferenz",
		fn_teae = "TEAE = Unerwartetes Ereignis unter Behandlung",
		fn_sae = "SUE = Schwerwiegendes unerwartetes Ereignis",
		fn_hr = "HR = Hazard Ratio",
		fn_or = "OR = Odds Ratio",
		fn_subjects_counted_once = "Patienten werden pro Kategorie einmal gezaehlt",

		# Reference group
		reference = "Referenz",
		reference_group = "Referenzgruppe",
		treatment_group = "Behandlungsgruppe",
		control_group = "Kontrollgruppe",
		active_control = "Aktive Kontrolle",
		comparator = "Komparator"
	)
)

# =============================================================================
# Locale Management Functions
# =============================================================================

#' Set the Current Locale
#'
#' Sets the current locale for pharmhand translations. This affects all
#' subsequent calls to `tr()` and related translation functions.
#'
#' @param locale Character. The locale to set. Currently supported values are
#'   `"en"` (English, default) and `"de"` (German). German translations are
#'   particularly important for G-BA/AMNOG dossier submissions.
#'
#' @return Invisibly returns the previous locale setting.
#'
#' @details
#' The locale setting is stored in a package-level environment and persists
#' for the duration of the R session. To temporarily change the locale for
#' a specific operation, save the current locale, change it, perform the
#' operation, and restore the original locale.
#'
#' @seealso [get_locale()], [tr()], [get_translations()]
#'
#' @export
#'
#' @examples
#' # Set locale to German for GBA dossier
#' old_locale <- set_locale("de")
#' tr("treatment")
#' # [1] "Behandlung"
#'
#' # Restore previous locale
#' set_locale(old_locale)
set_locale <- function(locale = c("en", "de")) {
	locale <- match.arg(locale)

	old_locale <- .pharmhand_locale$current
	.pharmhand_locale$current <- locale

	invisible(old_locale)
}

#' Get the Current Locale
#'
#' Returns the currently active locale for pharmhand translations.
#'
#' @return Character string indicating the current locale (`"en"` or `"de"`).
#'
#' @seealso [set_locale()], [tr()]
#'
#' @export
#'
#' @examples
#' get_locale()
#' # [1] "en"
#'
#' set_locale("de")
#' get_locale()
#' # [1] "de"
get_locale <- function() {
	.pharmhand_locale$current
}

# =============================================================================
# Translation Functions
# =============================================================================

#' Translate a Key to the Current Locale
#'
#' Translates a single key or vector of keys to the currently active locale.
#' This is the primary function for internationalization in pharmhand.
#'
#' @param key Character. A translation key or vector of keys (e.g.,
#'   `"treatment"`, `"hazard_ratio"`, `c("age", "sex")`).
#' @param locale Character or NULL. Optional locale to use instead of the
#'   current default. If NULL (default), uses the locale from [set_locale()].
#'
#' @return Character vector of translated strings. If a key is not found,
#'   returns the key itself with a warning.
#'
#' @details
#' Translation keys are case-insensitive and use underscores to separate
#' words (snake_case). Available keys can be viewed using [get_translations()].
#'
#' Custom translations can be added using [add_translation()]. Custom
#' translations take precedence over built-in translations.
#'
#' @seealso [tr_col()], [set_locale()], [get_translations()],
#'   [add_translation()]
#'
#' @export
#'
#' @examples
#' # English (default)
#' tr("treatment")
#' # [1] "Treatment"
#'
#' # German
#' tr("treatment", locale = "de")
#' # [1] "Behandlung"
#'
#' # Multiple keys
#' tr(c("age", "sex", "race"))
#' # [1] "Age" "Sex" "Race"
#'
#' # Using current locale
#' set_locale("de")
#' tr("hazard_ratio")
#' # [1] "Hazard Ratio"
tr <- function(key, locale = NULL) {
	if (is.null(locale)) {
		locale <- get_locale()
	}

	if (!locale %in% names(.pharmhand_translations)) {
		ph_warn(paste0("Locale '", locale, "' not supported, falling back to 'en'"))
		locale <- "en"
	}

	# Get translations for locale (custom translations take precedence)
	custom <- .pharmhand_locale$custom_translations[[locale]] %||% list()
	builtin <- .pharmhand_translations[[locale]]
	translations <- utils::modifyList(builtin, custom)

	# Translate each key
	result <- vapply(
		key,
		function(k) {
			k_lower <- tolower(k)
			if (k_lower %in% names(translations)) {
				return(translations[[k_lower]])
			} else {
				ph_warn(paste0(
					"Translation key '",
					k,
					"' not found for locale '",
					locale,
					"'"
				))
				return(k)
			}
		},
		character(1),
		USE.NAMES = FALSE
	)

	result
}

#' Translate Column Names
#'
#' Translates a vector of column names to the current locale. This is a
#' convenience wrapper around [tr()] designed for translating data frame
#' column names or table headers.
#'
#' @param cols Character vector of column names to translate.
#' @param locale Character or NULL. Optional locale override.
#'
#' @return Named character vector where names are original column names and
#'   values are translated strings. This makes it easy to use with
#'   `dplyr::rename()` or similar functions.
#'
#' @seealso [tr()], [get_translations()]
#'
#' @export
#'
#' @examples
#' # Translate column names
#' cols <- c("treatment", "placebo", "total")
#' tr_col(cols)
#' # treatment   placebo     total
#' # "Treatment" "Placebo"   "Total"
#'
#' # Use with dplyr::rename
#' \dontrun{
#' set_locale("de")
#' df |> dplyr::rename(!!!tr_col(c("treatment", "total")))
#' }
tr_col <- function(cols, locale = NULL) {
	translations <- tr(cols, locale = locale)
	stats::setNames(translations, cols)
}

# =============================================================================
# Translation Utility Functions
# =============================================================================

#' Get All Translations for a Locale
#'
#' Returns all available translations for the specified or current locale.
#' Useful for reviewing available translation keys and their values.
#'
#' @param locale Character or NULL. Locale to retrieve translations for.
#'   If NULL (default), uses the current locale.
#' @param include_custom Logical. Whether to include custom translations
#'   added via [add_translation()] (default: TRUE).
#'
#' @return A named list of all translations where names are keys and values
#'   are translated strings.
#'
#' @seealso [tr()], [add_translation()], [set_locale()]
#'
#' @export
#'
#' @examples
#' # Get all English translations
#' translations <- get_translations("en")
#' names(translations)[1:10]
#'
#' # Get current locale translations
#' get_translations()
get_translations <- function(locale = NULL, include_custom = TRUE) {
	if (is.null(locale)) {
		locale <- get_locale()
	}

	if (!locale %in% names(.pharmhand_translations)) {
		avail <- names(.pharmhand_translations)
		ph_abort(
			paste0(
				"Locale '",
				locale,
				"' not supported. Available: ",
				paste(avail, collapse = ", ")
			)
		)
	}

	builtin <- .pharmhand_translations[[locale]]

	if (include_custom) {
		custom <- .pharmhand_locale$custom_translations[[locale]] %||% list()
		# Custom translations take precedence
		utils::modifyList(builtin, custom)
	} else {
		builtin
	}
}

#' Add Custom Translation
#'
#' Adds a custom translation for a key in one or more locales. Custom
#' translations take precedence over built-in translations, allowing users
#' to override defaults or add study-specific terminology.
#'
#' @param key Character. The translation key to add or override. Keys should
#'   use snake_case (lowercase with underscores).
#' @param translations Named list or character vector. Translations for each
#'   locale. Names should be locale codes (e.g., `c(en = "English Term",
#'   de = "German Term")`).
#'
#' @return Invisibly returns the previous translation for the key (or NULL
#'   if it was new).
#'
#' @details
#' Custom translations persist for the duration of the R session. They are
#' stored separately from built-in translations and take precedence when
#' translating keys.
#'
#' This is particularly useful for:
#' - Study-specific terminology
#' - Regulatory-specific terms not in the default dictionary
#' - Correcting or adjusting default translations for specific contexts
#'
#' @seealso [tr()], [get_translations()]
#'
#' @export
#'
#' @examples
#' # Add a custom translation
#' add_translation(
#'   "study_drug",
#'   c(en = "DrugX 100mg", de = "MedikamentX 100mg")
#' )
#'
#' tr("study_drug")
#' # [1] "DrugX 100mg"
#'
#' tr("study_drug", locale = "de")
#' # [1] "MedikamentX 100mg"
#'
#' # Override existing translation
#' add_translation(
#'   "treatment", c(en = "Active Treatment", de = "Aktive Behandlung")
#' )
add_translation <- function(key, translations) {
	assert_character_scalar(key, "key")

	if (!is.character(translations) || is.null(names(translations))) {
		ph_abort(
			"'translations' must be a named character vector with locale codes as names"
		)
	}

	key_lower <- tolower(key)
	old_translations <- list()

	for (locale in names(translations)) {
		if (!locale %in% c("en", "de")) {
			ph_warn(paste0(
				"Locale '",
				locale,
				"' not officially supported, adding anyway"
			))
		}

		# Reference to locale's custom translations
		custom_trans <- .pharmhand_locale$custom_translations[[locale]]

		# Store old translation if it exists
		if (!is.null(custom_trans[[key_lower]])) {
			old_translations[[locale]] <- custom_trans[[key_lower]]
		}

		# Initialize locale list if needed
		if (is.null(custom_trans)) {
			.pharmhand_locale$custom_translations[[locale]] <- list()
			custom_trans <- .pharmhand_locale$custom_translations[[locale]]
		}

		# Add translation
		.pharmhand_locale$custom_translations[[locale]][[key_lower]] <-
			translations[[locale]]
	}

	invisible(if (length(old_translations) > 0) old_translations else NULL)
}

#' Reset Custom Translations
#'
#' Removes all custom translations, reverting to built-in translations only.
#'
#' @param locale Character or NULL. If provided, only reset translations for
#'   the specified locale. If NULL (default), reset all custom translations.
#'
#' @return Invisibly returns the removed custom translations.
#'
#' @seealso [add_translation()], [get_translations()]
#'
#' @export
#'
#' @examples
#' # Add custom translation
#' add_translation("test_key", c(en = "Test", de = "Test"))
#'
#' # Reset all custom translations
#' reset_custom_translations()
reset_custom_translations <- function(locale = NULL) {
	old <- .pharmhand_locale$custom_translations

	if (is.null(locale)) {
		.pharmhand_locale$custom_translations <- list()
	} else {
		.pharmhand_locale$custom_translations[[locale]] <- NULL
	}

	invisible(old)
}

#' List Available Translation Keys
#'
#' Returns a character vector of all available translation keys.
#'
#' @param locale Character or NULL. Locale to check for keys. If NULL,
#'   returns keys from all locales combined.
#' @param pattern Character or NULL. Optional regex pattern to filter keys.
#'
#' @return Character vector of translation key names.
#'
#' @seealso [tr()], [get_translations()]
#'
#' @export
#'
#' @examples
#' # All keys
#' head(list_translation_keys())
#'
#' # Keys containing "ae"
#' list_translation_keys(pattern = "ae")
#'
#' # Keys for demographics
#' list_translation_keys(pattern = "^(age|sex|race|ethnic)")
list_translation_keys <- function(locale = NULL, pattern = NULL) {
	if (is.null(locale)) {
		# Get all unique keys across locales
		all_keys <- unique(unlist(lapply(.pharmhand_translations, names)))
	} else {
		if (!locale %in% names(.pharmhand_translations)) {
			ph_abort(paste0("Locale '", locale, "' not supported"))
		}
		all_keys <- names(.pharmhand_translations[[locale]])
	}

	# Add custom translation keys
	custom_keys <- unique(unlist(lapply(
		.pharmhand_locale$custom_translations,
		names
	)))
	all_keys <- unique(c(all_keys, custom_keys))

	# Filter by pattern if provided
	if (!is.null(pattern)) {
		all_keys <- all_keys[grepl(pattern, all_keys, ignore.case = TRUE)]
	}

	sort(all_keys)
}

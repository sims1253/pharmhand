# Test Localization Functions
#'
#' Tests for localization.R functions including set_locale, get_locale,
#' tr, tr_col, get_translations, add_translation, reset_custom_translations,
#' and list_translation_keys.

# =============================================================================
# Locale Management Tests
# =============================================================================

test_that("get_locale returns current locale", {
	reset_locale_state()
	expect_equal(get_locale(), "en")
})

test_that("set_locale changes current locale", {
	reset_locale_state()
	set_locale("de")
	expect_equal(get_locale(), "de")
})

test_that("set_locale returns previous locale", {
	reset_locale_state()
	old <- set_locale("de")
	expect_equal(old, "en")
})

test_that("set_locale validates locale argument", {
	expect_error(set_locale("fr"), "should be one of")
	expect_error(set_locale("invalid"), "should be one of")
})

# =============================================================================
# Translation Function Tests
# =============================================================================

test_that("tr translates single key to English", {
	reset_locale_state()
	expect_equal(tr("treatment"), "Treatment")
	expect_equal(tr("placebo"), "Placebo")
	expect_equal(tr("total"), "Total")
})

test_that("tr translates single key to German", {
	reset_locale_state()
	set_locale("de")
	expect_equal(tr("treatment"), "Behandlung")
	expect_equal(tr("placebo"), "Placebo")
	expect_equal(tr("total"), "Gesamt")
})

test_that("tr translates vector of keys", {
	reset_locale_state()
	result <- tr(c("treatment", "placebo", "total"))
	expect_equal(result, c("Treatment", "Placebo", "Total"))
})

test_that("tr respects explicit locale parameter", {
	reset_locale_state()
	expect_equal(tr("treatment", locale = "de"), "Behandlung")

	set_locale("de")
	expect_equal(tr("treatment", locale = "en"), "Treatment")
})

test_that("tr warns on missing key and returns key", {
	reset_locale_state()
	expect_warning(
		result <- tr("nonexistent_key"),
		"not found"
	)
	expect_equal(result, "nonexistent_key")
})

test_that("tr is case-insensitive for keys", {
	reset_locale_state()
	expect_equal(tr("TREATMENT"), "Treatment")
	expect_equal(tr("Treatment"), "Treatment")
	expect_equal(tr("treatment"), "Treatment")
})

# =============================================================================
# Column Translation Tests
# =============================================================================

test_that("tr_col returns named character vector", {
	reset_locale_state()
	result <- tr_col(c("treatment", "placebo"))
	expect_named(result, c("treatment", "placebo"))
	expect_equal(result[["treatment"]], "Treatment")
	expect_equal(result[["placebo"]], "Placebo")
})

test_that("tr_col works with locale parameter", {
	reset_locale_state()
	result <- tr_col(c("treatment", "total"), locale = "de")
	expect_equal(result[["treatment"]], "Behandlung")
	expect_equal(result[["total"]], "Gesamt")
})

# =============================================================================
# Translation Dictionary Tests
# =============================================================================

test_that("get_translations returns all translations", {
	reset_locale_state()
	trans <- get_translations("en")
	expect_type(trans, "list")
	expect_true("treatment" %in% names(trans))
	expect_true("hazard_ratio" %in% names(trans))
	expect_true("safety_population" %in% names(trans))
})

test_that("get_translations uses current locale by default", {
	reset_locale_state()
	set_locale("de")
	trans <- get_translations()
	expect_equal(trans[["treatment"]], "Behandlung")
})

test_that("get_translations errors on invalid locale", {
	expect_error(get_translations("invalid"), "not supported")
})

# =============================================================================
# Custom Translation Tests
# =============================================================================

test_that("add_translation adds new key", {
	reset_locale_state()
	add_translation(
		"custom_key_add",
		c(en = "Custom Value", de = "Benutzerdefiniert")
	)

	expect_equal(tr("custom_key_add"), "Custom Value")

	set_locale("de")
	expect_equal(tr("custom_key_add"), "Benutzerdefiniert")
})

test_that("add_translation overrides existing key", {
	reset_locale_state()
	original <- tr("treatment")
	expect_equal(original, "Treatment")

	add_translation("treatment", c(en = "Active Treatment"))
	expect_equal(tr("treatment"), "Active Treatment")
})

test_that("add_translation validates arguments", {
	expect_error(
		add_translation(123, c(en = "test")),
		"'key' must be a non-empty character string"
	)
	expect_error(
		add_translation("key", "unnamed"),
		"must be a named character vector"
	)
})

test_that("custom translations take precedence in get_translations", {
	reset_locale_state()
	add_translation("treatment", c(en = "Custom Treatment"))

	trans <- get_translations("en", include_custom = TRUE)
	expect_equal(trans[["treatment"]], "Custom Treatment")

	trans_builtin <- get_translations("en", include_custom = FALSE)
	expect_equal(trans_builtin[["treatment"]], "Treatment")
})

test_that("reset_custom_translations removes all custom translations", {
	reset_locale_state()
	add_translation("test_key_reset", c(en = "Test", de = "Test"))
	expect_equal(tr("test_key_reset"), "Test")

	reset_custom_translations()

	expect_warning(tr("test_key_reset"), "not found")
})

test_that("reset_custom_translations can target specific locale", {
	reset_locale_state()
	add_translation("test_key_locale", c(en = "English", de = "German"))

	reset_custom_translations("en")

	set_locale("de")
	expect_equal(tr("test_key_locale"), "German")

	set_locale("en")
	expect_warning(tr("test_key_locale"), "not found")
})

# =============================================================================
# Translation Key Search Tests
# =============================================================================

test_that("list_translation_keys returns all keys", {
	reset_locale_state()
	keys <- list_translation_keys()
	expect_type(keys, "character")
	expect_true(length(keys) > 50) # Should have many keys
	expect_true("treatment" %in% keys)
	expect_true("hazard_ratio" %in% keys)
})

test_that("list_translation_keys filters by pattern", {
	reset_locale_state()
	ae_keys <- list_translation_keys(pattern = "^ae|^sae|^teae")
	expect_true(all(grepl("^ae|^sae|^teae", ae_keys, ignore.case = TRUE)))
	expect_true("ae" %in% ae_keys)
	expect_true("sae" %in% ae_keys)
	expect_true("teae" %in% ae_keys)
})

test_that("list_translation_keys can target specific locale", {
	reset_locale_state()
	keys_en <- list_translation_keys(locale = "en")
	keys_de <- list_translation_keys(locale = "de")

	# Both locales should have same keys
	expect_equal(sort(keys_en), sort(keys_de))
})

test_that("list_translation_keys includes custom keys", {
	reset_locale_state()
	add_translation("custom_search_key", c(en = "Test"))
	keys <- list_translation_keys(pattern = "custom_search")
	expect_true("custom_search_key" %in% keys)
})

# =============================================================================
# German Translation Completeness Tests
# =============================================================================

test_that("German translations exist for all English keys", {
	reset_locale_state()
	en_keys <- names(get_translations("en", include_custom = FALSE))
	de_keys <- names(get_translations("de", include_custom = FALSE))

	missing_in_de <- setdiff(en_keys, de_keys)
	expect_length(missing_in_de, 0)
})

test_that("key clinical terms have German translations", {
	reset_locale_state()
	clinical_keys <- c(
		"treatment",
		"placebo",
		"hazard_ratio",
		"risk_difference",
		"confidence_interval",
		"p_value",
		"safety_population",
		"itt_population",
		"system_organ_class",
		"preferred_term",
		"teae",
		"sae"
	)

	for (key in clinical_keys) {
		de_trans <- tr(key, locale = "de")
		expect_true(
			nchar(de_trans) > 0,
			info = paste("German translation missing for:", key)
		)
	}
})

# =============================================================================
# Integration Tests
# =============================================================================

test_that("locale persists across function calls", {
	reset_locale_state()
	set_locale("de")
	expect_equal(get_locale(), "de")
	expect_equal(tr("treatment"), "Behandlung")
	expect_equal(tr("placebo"), "Placebo")

	# Locale should still be German
	expect_equal(get_locale(), "de")
})

test_that("tr handles edge cases gracefully", {
	reset_locale_state()
	# Empty string
	expect_warning(tr(""), "not found")

	# Whitespace
	expect_warning(tr("  "), "not found")
})

# =============================================================================
# Cleanup - restore default state
# =============================================================================

test_that("cleanup test state", {
	reset_locale_state()
	expect_equal(get_locale(), "en")
})

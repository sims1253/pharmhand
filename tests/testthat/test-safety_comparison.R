# Tests for Safety Comparison Functions
# Tests for calculate_ae_risk_difference, create_ae_comparison_table
# Source: R/safety_comparison.R

# ==============================================================================
# Tests for AE Comparison Functionality
# ==============================================================================

# ------------------------------------------------------------------------------
# Tests for calculate_ae_risk_difference() - Internal function
# ------------------------------------------------------------------------------

describe("calculate_ae_risk_difference()", {
	it("calculates correct risk difference with known values", {
		# 20/100 vs 10/100 -> RD = 0.10 (10%)
		result <- calculate_ae_risk_difference(
			n1 = 20,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.95
		)

		expect_equal(result$rd, 0.10, tolerance = 1e-10)
	})

	it("calculates correct risk ratio with known values", {
		# 20/100 vs 10/100 -> RR = 2.0
		result <- calculate_ae_risk_difference(
			n1 = 20,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.95
		)

		expect_equal(result$rr, 2.0, tolerance = 1e-10)
	})

	it("returns valid p-value from chi-square test", {
		# Large counts should use chi-square
		result <- calculate_ae_risk_difference(
			n1 = 30,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.95
		)

		expect_true(!is.na(result$p_value))
		expect_true(result$p_value >= 0 && result$p_value <= 1)
		# With 30% vs 10%, p-value should be small
		expect_true(result$p_value < 0.05)
	})

	it("uses Fisher's exact test for small counts", {
		# Small counts (< 5 expected) should trigger Fisher's exact
		result <- calculate_ae_risk_difference(
			n1 = 3,
			N1 = 50,
			n2 = 1,
			N2 = 50,
			conf_level = 0.95
		)

		expect_true(!is.na(result$p_value))
		expect_true(result$p_value >= 0 && result$p_value <= 1)
	})

	it("applies continuity correction for zero incidence in reference", {
		# Zero in reference group - should apply continuity correction
		result <- calculate_ae_risk_difference(
			n1 = 10,
			N1 = 100,
			n2 = 0,
			N2 = 100,
			conf_level = 0.95
		)

		# RR should be finite (not Inf) due to continuity correction
		expect_true(is.finite(result$rr))
		expect_true(result$rr > 1) # Active has higher incidence
	})

	it("applies continuity correction for zero incidence in treatment", {
		# Zero in treatment group - should apply continuity correction
		result <- calculate_ae_risk_difference(
			n1 = 0,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.95
		)

		# RR should be finite (not Inf) due to continuity correction
		expect_true(is.finite(result$rr))
		expect_true(result$rr < 1) # Treatment has lower incidence
	})

	it("calculates correct confidence interval bounds", {
		result <- calculate_ae_risk_difference(
			n1 = 20,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.95
		)

		# RD CI should contain the point estimate
		expect_true(result$rd_lower <= result$rd)
		expect_true(result$rd_upper >= result$rd)

		# RR CI should contain the point estimate
		expect_true(result$rr_lower <= result$rr)
		expect_true(result$rr_upper >= result$rr)

		# CI should be reasonable (not too wide)
		expect_true(result$rd_upper - result$rd_lower < 0.5)
	})

	it("respects different confidence levels", {
		result_95 <- calculate_ae_risk_difference(
			n1 = 20,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.95
		)
		result_90 <- calculate_ae_risk_difference(
			n1 = 20,
			N1 = 100,
			n2 = 10,
			N2 = 100,
			conf_level = 0.90
		)

		# 90% CI should be narrower than 95% CI
		width_95 <- result_95$rd_upper - result_95$rd_lower
		width_90 <- result_90$rd_upper - result_90$rd_lower
		expect_true(width_90 < width_95)
	})

	it("handles equal incidences correctly", {
		# 15/100 vs 15/100 -> RD = 0, RR = 1
		result <- calculate_ae_risk_difference(
			n1 = 15,
			N1 = 100,
			n2 = 15,
			N2 = 100,
			conf_level = 0.95
		)

		expect_equal(result$rd, 0, tolerance = 1e-10)
		expect_equal(result$rr, 1, tolerance = 1e-10)
		# p-value should be 1 (no difference)
		expect_true(result$p_value > 0.99)
	})

	it("handles both proportions zero", {
		result <- calculate_ae_risk_difference(0, 100, 0, 100)
		expect_equal(result$rd, 0)
		expect_true(is.na(result$rr))
		expect_true(is.na(result$rr_lower))
		expect_true(is.na(result$rr_upper))
	})
})

# ------------------------------------------------------------------------------
# Tests for create_ae_comparison_table() - Main function
# ------------------------------------------------------------------------------

describe("create_ae_comparison_table()", {
	test_data <- create_comparison_test_data()

	it("returns a ClinicalTable object", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "ae_comparison")
	})

	it("includes NNH column by default", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		nnh_col <- sprintf("NNH %s vs %s\n(95%% CI)", "Active", "Placebo")
		expect_true(nnh_col %in% names(tbl@data))
	})

	it("works with 'soc' grouping", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "soc"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Should have SOC-level rows
		expect_true("Term" %in% names(tbl@data))
		# Should have fewer rows than PT grouping (4 SOCs vs 4 PTs in our test data)
		expect_equal(nrow(tbl@data), 4)
	})

	it("works with 'pt' grouping", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		expect_s7_class(tbl, ClinicalTable)
		expect_true("Term" %in% names(tbl@data))
		# Should have 4 PT-level rows (Headache, Nausea, Fatigue, Rash)
		expect_equal(nrow(tbl@data), 4)
	})

	it("calculates NNH as inverse of absolute RD", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		nnh_col <- sprintf("NNH %s vs %s\n(95%% CI)", "Active", "Placebo")
		headache_nnh <- tbl@data[tbl@data$Term == "Headache", nnh_col]
		result <- calculate_ae_risk_difference(n1 = 20, N1 = 100, n2 = 10, N2 = 100)
		expected_nnh <- 1 / abs(result$rd)
		expected_prefix <- format_number(expected_nnh, digits = 1)
		expect_true(startsWith(headache_nnh, expected_prefix))
	})

	it("shows NE when RD CI crosses zero", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		nnh_col <- sprintf("NNH %s vs %s\n(95%% CI)", "Active", "Placebo")
		nausea_nnh <- tbl@data[tbl@data$Term == "Nausea", nnh_col]
		expect_equal(nausea_nnh, "NE")
	})

	it("allows excluding NNH column", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			include_nnh = FALSE
		)

		expect_false(any(grepl("NNH", names(tbl@data), fixed = TRUE)))
	})

	it("calculates NNH correctly when RD is negative (treatment beneficial)", {
		# Create test data where Active has FEWER events than Placebo
		# This tests that NNH still works when the treatment is actually beneficial
		adae_beneficial <- data.frame(
			USUBJID = c(
				paste0("SUBJ-", sprintf("%03d", 1:30)), # Active with AE
				paste0("SUBJ-", sprintf("%03d", 101:160)) # Placebo with AE
			),
			TRT01P = c(rep("Active", 30), rep("Placebo", 60)),
			AEDECOD = "Headache",
			AEBODSYS = "Nervous system disorders",
			TRTEMFL = "Y",
			stringsAsFactors = FALSE
		)

		adsl_beneficial <- data.frame(
			USUBJID = c(
				paste0("SUBJ-", sprintf("%03d", 1:100)), # Active arm (100 subjects)
				paste0("SUBJ-", sprintf("%03d", 101:200)) # Placebo arm (100 subjects)
			),
			TRT01P = c(rep("Active", 100), rep("Placebo", 100)),
			SAFFL = "Y",
			stringsAsFactors = FALSE
		)

		tbl <- create_ae_comparison_table(
			adae = adae_beneficial,
			adsl = adsl_beneficial,
			ref_group = "Placebo",
			by = "pt"
		)

		# RD = 30/100 - 60/100 = -0.30 (negative = treatment beneficial)
		# NNH should still be calculated as 1/|RD| = 1/0.30 ≈ 3.3
		nnh_col <- sprintf("NNH %s vs %s\n(95%% CI)", "Active", "Placebo")
		nnh_value <- tbl@data[tbl@data$Term == "Headache", nnh_col]

		# Should NOT be "NE" since the CI won't cross zero with this large effect
		expect_false(nnh_value == "NE")

		# NNH should be approximately 3.3 (1/0.30)
		# Extract the point estimate from the formatted string
		nnh_point <- as.numeric(sub(" .*", "", nnh_value))
		expect_true(nnh_point > 2 && nnh_point < 5)
	})

	it("works with 'overall' grouping", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "overall"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Should have exactly 1 row for "Any TEAE"
		expect_equal(nrow(tbl@data), 1)
		expect_true(any(grepl("Any TEAE", tbl@data$Term, fixed = TRUE)))
	})

	it("filters by threshold correctly", {
		# Set threshold to 10% - should exclude Fatigue (5% in Active) and Rash (8%)
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			threshold = 10
		)

		expect_s7_class(tbl, ClinicalTable)
		# Should have 3 rows: Headache (20%), Nausea (15%), Fatigue (10% in Placebo)
		expect_equal(nrow(tbl@data), 3)
		# Rash should be excluded (8% max)
		expect_false(any(grepl("Rash", tbl@data$Term, fixed = TRUE)))
	})

	it("sorts by 'rd' correctly", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			sort_by = "rd"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Headache RD = +10%, Rash = +8%, Nausea = 0%, Fatigue = -5%
		# Should be sorted by absolute RD descending
		expect_equal(tbl@data$Term[1], "Headache")
	})

	it("sorts by 'rr' correctly", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			sort_by = "rr"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Rash has highest RR (infinite without correction, very high with)
		# Should be sorted by RR descending
		expect_equal(tbl@data$Term[1], "Rash")
	})

	it("sorts by 'incidence' correctly", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			sort_by = "incidence"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Headache has highest incidence (20% in Active)
		expect_equal(tbl@data$Term[1], "Headache")
	})

	it("errors when ref_group is NULL", {
		expect_error(
			create_ae_comparison_table(
				adae = test_data$adae,
				adsl = test_data$adsl,
				ref_group = NULL,
				by = "pt"
			),
			"ref_group.*provided"
		)
	})

	it("errors when ref_group is invalid", {
		expect_error(
			create_ae_comparison_table(
				adae = test_data$adae,
				adsl = test_data$adsl,
				ref_group = "NonexistentGroup",
				by = "pt"
			),
			"must be one of the treatment groups"
		)
	})

	it("errors when adsl is missing", {
		expect_error(
			create_ae_comparison_table(
				adae = test_data$adae,
				adsl = NULL,
				ref_group = "Placebo",
				by = "pt"
			),
			"adsl.*must be a data frame"
		)
	})

	it("stores metadata correctly", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			threshold = 5,
			conf_level = 0.90
		)

		expect_equal(tbl@metadata$ref_group, "Placebo")
		expect_equal(tbl@metadata$by, "pt")
		expect_equal(tbl@metadata$threshold, 5)
		expect_equal(tbl@metadata$conf_level, 0.90)
	})

	it("generates appropriate title when not provided", {
		tbl_pt <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt"
		)
		expect_true(grepl("Preferred Term", tbl_pt@title, fixed = TRUE))

		tbl_soc <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "soc"
		)
		expect_true(grepl("System Organ Class", tbl_soc@title, fixed = TRUE))
	})

	it("respects custom title", {
		tbl <- create_ae_comparison_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			ref_group = "Placebo",
			by = "pt",
			title = "Custom Comparison Title"
		)

		expect_equal(tbl@title, "Custom Comparison Title")
	})
})

describe("create_ae_comparison_table() with multiple treatment groups", {
	it("handles three treatment groups correctly", {
		# Create ADSL with 3 arms
		adsl_3arm <- data.frame(
			USUBJID = sprintf("SUBJ%03d", 1:300),
			TRT01P = rep(c("Placebo", "Low Dose", "High Dose"), each = 100),
			SAFFL = rep("Y", 300),
			stringsAsFactors = FALSE
		)

		# Create ADAE with different incidences per arm
		adae_3arm <- rbind(
			data.frame(
				USUBJID = sprintf("SUBJ%03d", 1:10),
				TRT01P = "Placebo",
				TRTEMFL = "Y",
				AEDECOD = "Headache",
				stringsAsFactors = FALSE
			),
			data.frame(
				USUBJID = sprintf("SUBJ%03d", 101:115),
				TRT01P = "Low Dose",
				TRTEMFL = "Y",
				AEDECOD = "Headache",
				stringsAsFactors = FALSE
			),
			data.frame(
				USUBJID = sprintf("SUBJ%03d", 201:225),
				TRT01P = "High Dose",
				TRTEMFL = "Y",
				AEDECOD = "Headache",
				stringsAsFactors = FALSE
			)
		)

		tbl <- create_ae_comparison_table(
			adae = adae_3arm,
			adsl = adsl_3arm,
			ref_group = "Placebo",
			by = "pt"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Should have columns for each treatment group comparison
		expect_true(any(grepl(
			"RD Low Dose vs Placebo",
			names(tbl@data),
			fixed = TRUE
		)))
		expect_true(any(grepl(
			"RR Low Dose vs Placebo",
			names(tbl@data),
			fixed = TRUE
		)))
		expect_true(any(grepl(
			"P-value (Low Dose vs Placebo)",
			names(tbl@data),
			fixed = TRUE
		)))
		expect_true(any(grepl(
			"RD High Dose vs Placebo",
			names(tbl@data),
			fixed = TRUE
		)))
		expect_true(any(grepl(
			"RR High Dose vs Placebo",
			names(tbl@data),
			fixed = TRUE
		)))
		expect_true(any(grepl(
			"P-value (High Dose vs Placebo)",
			names(tbl@data),
			fixed = TRUE
		)))
	})
})

# ------------------------------------------------------------------------------
# Tests for create_ae_summary_table() integration with type = "comparison"
# ------------------------------------------------------------------------------

describe("create_ae_summary_table() with type = 'comparison'", {
	test_data <- create_comparison_test_data()

	it("dispatches to create_ae_comparison_table correctly", {
		tbl <- create_ae_summary_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			type = "comparison",
			ref_group = "Placebo",
			by = "pt"
		)

		expect_s7_class(tbl, ClinicalTable)
		expect_equal(tbl@type, "ae_comparison")
	})

	it("errors when ref_group is not provided for comparison type", {
		expect_error(
			create_ae_summary_table(
				adae = test_data$adae,
				adsl = test_data$adsl,
				type = "comparison"
			),
			"ref_group.*required"
		)
	})

	it("errors when adsl is not provided for comparison type", {
		expect_error(
			create_ae_summary_table(
				adae = test_data$adae,
				adsl = NULL,
				type = "comparison",
				ref_group = "Placebo"
			),
			"adsl.*required"
		)
	})

	it("passes through all comparison parameters correctly", {
		tbl <- create_ae_summary_table(
			adae = test_data$adae,
			adsl = test_data$adsl,
			type = "comparison",
			ref_group = "Placebo",
			by = "soc",
			threshold = 5,
			sort_by = "rd",
			conf_level = 0.90,
			title = "Custom Title from create_ae_summary_table"
		)

		expect_equal(tbl@metadata$ref_group, "Placebo")
		expect_equal(tbl@metadata$by, "soc")
		expect_equal(tbl@metadata$threshold, 5)
		expect_equal(tbl@metadata$conf_level, 0.90)
		expect_equal(tbl@title, "Custom Title from create_ae_summary_table")
	})
})

# ------------------------------------------------------------------------------
# Edge case tests
# ------------------------------------------------------------------------------

describe("AE comparison edge cases", {
	it("handles case where events exist in both groups but different terms", {
		# Tests when one group has an AE the other doesn't, but both have AEs
		adsl <- data.frame(
			USUBJID = sprintf("SUBJ%03d", 1:100),
			TRT01P = rep(c("Placebo", "Active"), each = 50),
			SAFFL = rep("Y", 100),
			stringsAsFactors = FALSE
		)

		# Placebo has Nausea, Active has Headache
		adae <- rbind(
			data.frame(
				USUBJID = sprintf("SUBJ%03d", 1:5),
				TRT01P = "Placebo",
				TRTEMFL = "Y",
				AEDECOD = "Nausea",
				stringsAsFactors = FALSE
			),
			data.frame(
				USUBJID = sprintf("SUBJ%03d", 51:60),
				TRT01P = "Active",
				TRTEMFL = "Y",
				AEDECOD = "Headache",
				stringsAsFactors = FALSE
			)
		)

		tbl <- create_ae_comparison_table(
			adae = adae,
			adsl = adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		expect_s7_class(tbl, ClinicalTable)
		# Should have 2 rows: Headache and Nausea
		expect_equal(nrow(tbl@data), 2)
	})

	it("returns NULL with warning when threshold excludes all events", {
		test_data <- create_comparison_test_data()

		# Set very high threshold that excludes everything
		expect_warning(
			tbl <- create_ae_comparison_table(
				adae = test_data$adae,
				adsl = test_data$adsl,
				ref_group = "Placebo",
				by = "pt",
				threshold = 99
			),
			"No adverse events meet the specified threshold"
		)

		expect_null(tbl)
	})

	it("handles missing AEBODSYS gracefully for pt grouping", {
		adsl <- data.frame(
			USUBJID = sprintf("SUBJ%03d", 1:100),
			TRT01P = rep(c("Placebo", "Active"), each = 50),
			SAFFL = rep("Y", 100),
			stringsAsFactors = FALSE
		)

		# ADAE without AEBODSYS column (only AEDECOD)
		adae <- data.frame(
			USUBJID = sprintf("SUBJ%03d", c(1:10, 51:60)),
			TRT01P = rep(c("Placebo", "Active"), each = 10),
			TRTEMFL = rep("Y", 20),
			AEDECOD = rep("Headache", 20),
			stringsAsFactors = FALSE
		)

		# Should work for PT grouping without AEBODSYS
		tbl <- create_ae_comparison_table(
			adae = adae,
			adsl = adsl,
			ref_group = "Placebo",
			by = "pt"
		)

		expect_s7_class(tbl, ClinicalTable)
	})

	it("errors appropriately for soc grouping without AEBODSYS", {
		adsl <- data.frame(
			USUBJID = sprintf("SUBJ%03d", 1:100),
			TRT01P = rep(c("Placebo", "Active"), each = 50),
			SAFFL = rep("Y", 100),
			stringsAsFactors = FALSE
		)

		# ADAE without AEBODSYS column
		adae <- data.frame(
			USUBJID = sprintf("SUBJ%03d", c(1:10, 51:60)),
			TRT01P = rep(c("Placebo", "Active"), each = 10),
			TRTEMFL = rep("Y", 20),
			AEDECOD = rep("Headache", 20),
			stringsAsFactors = FALSE
		)

		expect_error(
			create_ae_comparison_table(
				adae = adae,
				adsl = adsl,
				ref_group = "Placebo",
				by = "soc"
			),
			"AEBODSYS"
		)
	})
})

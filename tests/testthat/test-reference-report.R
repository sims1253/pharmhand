# Tests for reference report generation

test_that("LayeredTable demographics works with pharmaverseadam", {
	skip_if_not_installed("pharmaverseadam")

	adsl <- pharmaverseadam::adsl

	demo_table <- LayeredTable(
		data = adsl,
		trt_var = "TRT01P",
		title = "Demographics"
	) |>
		add_layer(DescriptiveLayer(target_var = "AGE")) |>
		add_layer(CountLayer(target_var = "SEX"))

	result <- build_table(demo_table)

	expect_true(is.data.frame(result))
	expect_true(nrow(result) > 0)
	expect_true("layer_type" %in% names(result))
})

test_that("Demographics section has expected structure", {
	skip_if_not_installed("pharmaverseadam")

	adsl <- pharmaverseadam::adsl

	# Create simple demographics summary
	demo_data <- adsl |>
		dplyr::group_by(.data$TRT01P) |>
		dplyr::summarise(
			n = dplyr::n(),
			mean_age = mean(.data$AGE, na.rm = TRUE),
			.groups = "drop"
		)

	expect_snapshot(demo_data)
})

test_that("AE summary by SOC has expected structure", {
	skip_if_not_installed("pharmaverseadam")

	adae <- pharmaverseadam::adae
	adsl <- pharmaverseadam::adsl

	# Get treatment counts
	trt_n <- adsl |>
		dplyr::group_by(.data$TRT01A) |>
		dplyr::summarise(N = dplyr::n(), .groups = "drop")

	# SOC summary
	soc_summary <- adae |>
		dplyr::group_by(.data$TRT01A, .data$AEBODSYS) |>
		dplyr::summarise(
			n_subj = dplyr::n_distinct(.data$USUBJID),
			.groups = "drop"
		) |>
		dplyr::left_join(trt_n, by = "TRT01A") |>
		dplyr::mutate(pct = round(.data$n_subj / .data$N * 100, 1)) |>
		dplyr::arrange(.data$AEBODSYS, .data$TRT01A)

	expect_true(is.data.frame(soc_summary))
	expect_true("AEBODSYS" %in% names(soc_summary))
	expect_true("n_subj" %in% names(soc_summary))
	expect_true("pct" %in% names(soc_summary))

	# Check that we have multiple SOCs
	expect_gt(length(unique(soc_summary$AEBODSYS)), 1)
})

test_that("ClinicalReport assembles correctly", {
	skip_if_not_installed("pharmaverseadam")

	# Create minimal report structure
	demo_content <- ClinicalTable(
		data = data.frame(x = 1:3),
		flextable = NULL,
		type = "demographics",
		title = "Demographics"
	)

	demo_section <- ReportSection(
		title = "Demographics",
		section_type = "baseline",
		content = list(demo_content)
	)

	report <- ClinicalReport(
		study_id = "TEST001",
		study_title = "Test Study",
		sections = list(demo_section)
	)

	expect_true(S7::S7_inherits(report, ClinicalReport))
	expect_equal(report@n_sections, 1L)
	expect_equal(report@study_id, "TEST001")
})

test_that("create_hta_table applies correct styling", {
	skip_if_not_installed("flextable")

	data <- data.frame(
		Treatment = c("Drug A", "Drug B"),
		N = c(100, 100),
		Events = c(15, 25)
	)

	ft <- create_hta_table(
		data,
		title = "Test Table",
		footnotes = c("Note 1", "Note 2")
	)

	expect_s3_class(ft, "flextable")
})

test_that("apply_clinical_style works with different presets", {
	skip_if_not_installed("flextable")

	data <- data.frame(a = 1:3, b = c("x", "y", "z"))
	ft <- flextable::flextable(data)

	# Test each preset
	for (style in c("default", "clinical", "hta", "compact")) {
		styled <- apply_clinical_style(ft, style = style)
		expect_s3_class(styled, "flextable")
	}
})

test_that("HTASection with content works", {
	skip_if_not_installed("pharmaverseadam")

	hta_content <- ClinicalTable(
		data = data.frame(Metric = "AE Rate", Value = "15%"),
		flextable = NULL,
		type = "hta_safety",
		title = "Safety Overview"
	)

	section <- HTASection(
		title = "HTA Safety Summary",
		section_type = "hta",
		comparator = "Placebo",
		population = "Safety",
		content = list(hta_content)
	)

	expect_true(S7::S7_inherits(section, HTASection))
	expect_equal(section@n_content, 1L)
	expect_equal(section@comparator, "Placebo")
})

test_that("SOCPTSection works correctly", {
	section <- SOCPTSection(
		title = "Adverse Events",
		section_type = "safety",
		soc_var = "AEBODSYS",
		pt_var = "AEDECOD",
		group_var = "TRT01A"
	)

	expect_true(S7::S7_inherits(section, SOCPTSection))
	expect_true(S7::S7_inherits(section, ReportSection))
	expect_equal(section@soc_var, "AEBODSYS")
	expect_equal(section@pt_var, "AEDECOD")
})

test_that("layered_to_flextable produces valid output", {
	skip_if_not_installed("flextable")

	data <- data.frame(
		USUBJID = paste0("0", 1:10),
		TRT = rep(c("A", "B"), 5),
		AGE = c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70)
	)

	tbl <- LayeredTable(
		data = data,
		trt_var = "TRT",
		title = "Test Demographics"
	) |>
		add_layer(DescriptiveLayer(target_var = "AGE"))

	ft <- layered_to_flextable(tbl, style = "clinical")

	expect_s3_class(ft, "flextable")
})

test_that("Report metadata is populated correctly", {
	report <- ClinicalReport(
		study_id = "STUDY001",
		study_title = "Phase III Study",
		metadata = list(
			generated_at = Sys.time(),
			source = "test"
		)
	)

	expect_true("generated_at" %in% names(report@metadata))
	expect_true("source" %in% names(report@metadata))
})

test_that("Reference report is generated as docx", {
	skip_if_not_installed("pharmaverseadam")
	skip_if_not_installed("officer")

	adsl <- pharmaverseadam::adsl

	report <- ClinicalReport(
		study_id = "REF001",
		study_title = "Reference Study",
		sections = list(
			ReportSection(
				title = "Baseline",
				section_type = "baseline",
				content = list(
					ClinicalTable(
						data = head(adsl),
						flextable = flextable::flextable(head(adsl)),
						type = "demographics",
						title = "Demographics Summary"
					)
				)
			)
		)
	)

	tmp_docx <- tempfile(fileext = ".docx")
	write_docx(report, path = tmp_docx)

	expect_true(file.exists(tmp_docx))
	expect_gt(file.size(tmp_docx), 0)

	# Cleanup
	if (file.exists(tmp_docx)) unlink(tmp_docx)
})

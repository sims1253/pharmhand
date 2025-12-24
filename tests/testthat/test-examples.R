test_that("Reference reports can be generated", {
	skip_if_not_installed("pharmaverseadam")
	skip_if_not_installed("flextable")
	skip_if_not_installed("officer")

	# Define paths to example scripts
	# During devtools::test(), we are in tests/testthat
	# During R CMD check, inst/examples is copied to the installed location

	# Try to find package root (works during devtools::test)
	pkg_root <- tryCatch(
		rprojroot::find_package_root_file(),
		error = function(e) NULL
	)

	if (!is.null(pkg_root)) {
		examples_dir <- file.path(pkg_root, "inst", "examples")
	} else {
		# Fallback for R CMD check: use installed package location
		examples_dir <- system.file("examples", package = "FunctionReport")
	}

	if (!dir.exists(examples_dir)) {
		skip("Example directory not found")
	}

	scripts <- c(
		"baseline_report.R",
		"safety_report.R",
		"efficacy_report.R",
		"reference_report.R"
	)

	temp_dir <- tempdir()

	for (script in scripts) {
		script_path <- file.path(examples_dir, script)
		expect_true(
			file.exists(script_path),
			info = paste("Script not found:", script)
		)

		# Source the script into a local environment to avoid polluting global env
		env <- new.env()
		source(script_path, local = env)

		# Determine generation function name
		gen_func_name <- switch(
			script,
			"baseline_report.R" = "generate_baseline_report",
			"safety_report.R" = "generate_safety_report",
			"efficacy_report.R" = "generate_efficacy_report",
			"reference_report.R" = "generate_reference_report"
		)

		# Check if function exists
		expect_true(
			exists(gen_func_name, envir = env),
			info = paste("Function missing:", gen_func_name)
		)

		# Run generation
		output_file <- file.path(
			temp_dir,
			paste0(tools::file_path_sans_ext(script), ".docx")
		)

		# We need to make sure the function uses the package functions correctly
		# Since we are running tests, the package is loaded.

		# Execute the function
		expect_error(
			do.call(env[[gen_func_name]], list(output_path = output_file)),
			NA,
			info = paste("Error running", script)
		)

		# Check output
		expect_true(
			file.exists(output_file),
			info = paste("Output missing for", script)
		)

		# Empty docx with TOC is ~12KB
		# Populated reports should be larger
		expect_gt(file.size(output_file), 15000)
	}
})

# Tests for layer composition system

describe("layers", {
	it("CountLayer creates valid object", {
		layer <- CountLayer(
			target_var = "SEX",
			by_vars = character(),
			distinct_by = "USUBJID",
			include_pct = TRUE
		)

		expect_true(S7::S7_inherits(layer, CountLayer))
		expect_true(S7::S7_inherits(layer, AnalysisLayer))
		expect_equal(layer@target_var, "SEX")
		expect_equal(layer@distinct_by, "USUBJID")
		expect_true(layer@include_pct)
	})

	it("DescriptiveLayer creates valid object", {
		layer <- DescriptiveLayer(
			target_var = "AGE",
			stats = c("n", "mean", "sd")
		)

		expect_true(S7::S7_inherits(layer, DescriptiveLayer))
		expect_true(S7::S7_inherits(layer, AnalysisLayer))
		expect_equal(layer@target_var, "AGE")
		expect_equal(layer@stats, c("n", "mean", "sd"))
	})

	it("ShiftLayer creates valid object", {
		layer <- ShiftLayer(
			target_var = c("BTOXGR", "ATOXGR")
		)

		expect_true(S7::S7_inherits(layer, ShiftLayer))
		expect_true(S7::S7_inherits(layer, AnalysisLayer))
		expect_equal(layer@target_var, c("BTOXGR", "ATOXGR"))
		expect_equal(layer@distinct_by, "USUBJID")
	})

	it("ShiftLayer validates target_var has exactly 2 elements", {
		expect_error(
			ShiftLayer(target_var = "BTOXGR"),
			"exactly 2 variables"
		)
		expect_error(
			ShiftLayer(target_var = c("A", "B", "C")),
			"exactly 2 variables"
		)
	})

	it("LayeredTable creates valid object", {
		tbl <- LayeredTable(
			data = data.frame(x = 1:10, trt = rep(c("A", "B"), 5)),
			trt_var = "trt",
			title = "Test Table"
		)

		expect_true(S7::S7_inherits(tbl, LayeredTable))
		expect_equal(tbl@trt_var, "trt")
		expect_equal(tbl@title, "Test Table")
		expect_equal(tbl@n_layers, 0L)
	})

	it("add_layer works correctly", {
		tbl <- LayeredTable(
			data = data.frame(x = 1:10, trt = rep(c("A", "B"), 5)),
			trt_var = "trt"
		)

		layer <- CountLayer(target_var = "x")
		tbl <- add_layer(tbl, layer)

		expect_equal(tbl@n_layers, 1L)
		expect_true(S7::S7_inherits(tbl@layers[[1]], CountLayer))
	})

	it("add_layer rejects invalid input", {
		tbl <- LayeredTable(
			data = data.frame(x = 1:10),
			trt_var = "trt"
		)

		expect_error(add_layer(tbl, "not a layer"))
		expect_error(add_layer("not a table", CountLayer(target_var = "x")))
	})

	it("build_layer computes correct counts", {
		data <- data.frame(
			USUBJID = c("01", "02", "03", "04", "05", "06"),
			TRT = c("A", "A", "A", "B", "B", "B"),
			SEX = c("M", "M", "F", "M", "F", "F")
		)

		layer <- CountLayer(
			target_var = "SEX",
			distinct_by = "USUBJID",
			include_pct = TRUE
		)

		result <- build_layer(layer, data, "TRT")

		expect_true(is.data.frame(result))
		expect_true("n" %in% names(result))
		expect_true("pct" %in% names(result))
		expect_true("variable" %in% names(result))
		expect_equal(unique(result$variable), "SEX")
	})

	it("build_layer computes correct descriptive stats", {
		data <- data.frame(
			USUBJID = paste0("0", 1:6),
			TRT = c("A", "A", "A", "B", "B", "B"),
			AGE = c(25, 30, 35, 40, 45, 50)
		)

		layer <- DescriptiveLayer(
			target_var = "AGE",
			stats = c("n", "mean", "sd", "median", "min", "max")
		)

		result <- build_layer(layer, data, "TRT")

		expect_true(is.data.frame(result))
		expect_true("mean" %in% names(result))
		expect_true("sd" %in% names(result))

		# Check values for treatment A
		result_a <- result[result$TRT == "A", ]
		expect_equal(result_a$n, 3L)
		expect_equal(result_a$mean, 30.0)
		expect_equal(result_a$min, 25.0)
		expect_equal(result_a$max, 35.0)
	})

	it("build_table combines layers correctly", {
		data <- data.frame(
			USUBJID = paste0("0", 1:6),
			TRT = c("A", "A", "A", "B", "B", "B"),
			SEX = c("M", "M", "F", "M", "F", "F"),
			AGE = c(25, 30, 35, 40, 45, 50)
		)

		tbl <- LayeredTable(data = data, trt_var = "TRT") |>
			add_layer(CountLayer(target_var = "SEX")) |>
			add_layer(DescriptiveLayer(target_var = "AGE"))

		result <- build_table(tbl)

		expect_true(is.data.frame(result))
		expect_true("layer_type" %in% names(result))
		expect_true("count" %in% result$layer_type)
		expect_true("descriptive" %in% result$layer_type)
	})

	it("LayeredTable validates layers property", {
		expect_error(
			LayeredTable(
				data = data.frame(x = 1),
				trt_var = "x",
				layers = list("not a layer")
			)
		)
	})
})

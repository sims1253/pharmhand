# Tests for network meta-analysis functions (R/meta_network.R)

# =============================================================================
# network_meta function tests
# =============================================================================

test_that("network_meta analyzes treatment network", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	result <- network_meta(nma_data, effect_measure = "hr")

	expect_true(S7::S7_inherits(result, NMAResult))
	expect_true("comparisons" %in% names(S7::props(result)))
	expect_true("network" %in% names(S7::props(result)))
	expect_equal(result@network$n_treatments, 3)
})

test_that("network_meta handles incomplete networks", {
	# Data with studies that may have incomplete information
	incomplete_data <- data.frame(
		study = c("S1", "S2", "S3", "S4"),
		treat1 = c("A", "B", "A", "D"),
		treat2 = c("B", "C", "C", "E"),
		effect = log(c(0.75, 0.90, 0.80, 0.95)),
		se = c(0.12, 0.15, 0.18, 0.20)
	)

	# Should still produce results even if network not fully connected
	result <- network_meta(incomplete_data, effect_measure = "hr")

	expect_true(S7::S7_inherits(result, NMAResult))
	expect_true("network" %in% names(S7::props(result)))
})

test_that("network_meta handles custom reference treatment", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	result_ref_b <- network_meta(nma_data, reference = "B", effect_measure = "hr")

	expect_true(S7::S7_inherits(result_ref_b, NMAResult))
	expect_equal(result_ref_b@network$reference, "B")
})

test_that("network_meta handles fixed-effect model", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	result <- network_meta(nma_data, effect_measure = "hr", model = "fixed")

	expect_true(S7::S7_inherits(result, NMAResult))
	expect_equal(result@model, "fixed")
})

# =============================================================================
# create_network_plot function tests
# =============================================================================

test_that("create_network_plot creates visualization", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	nma_res <- network_meta(nma_data, effect_measure = "hr")
	plot <- create_network_plot(nma_res)

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@type, "network_geometry")
})

test_that("create_network_plot supports different layouts", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	nma_res <- network_meta(nma_data, effect_measure = "hr")

	plot_circle <- create_network_plot(nma_res, layout = "circle")
	plot_star <- create_network_plot(nma_res, layout = "star")
	plot_auto <- create_network_plot(nma_res, layout = "auto")

	expect_s7_class(plot_circle, ClinicalPlot)
	expect_s7_class(plot_star, ClinicalPlot)
	expect_s7_class(plot_auto, ClinicalPlot)
})

# =============================================================================
# create_league_table function tests
# =============================================================================

test_that("create_league_table creates ClinicalTable", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	nma_res <- network_meta(nma_data, effect_measure = "hr")
	table <- create_league_table(nma_res)

	expect_s7_class(table, ClinicalTable)
	expect_equal(table@type, "league_table")
})

test_that("create_league_table includes comparison matrix", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	nma_res <- network_meta(nma_data, effect_measure = "hr")
	table <- create_league_table(nma_res)

	expect_s7_class(table, ClinicalTable)
	expect_equal(table@type, "league_table")
	expect_true(is.data.frame(table@data) || is.matrix(table@data))
})

# =============================================================================
# calculate_sucra function tests
# =============================================================================

test_that("calculate_sucra computes treatment rankings", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	nma_res <- network_meta(nma_data, effect_measure = "hr")
	sucra <- calculate_sucra(nma_res)

	expect_true(is.list(sucra))
	expect_true("ranking" %in% names(sucra))
	expect_true("sucra" %in% names(sucra))
	expect_equal(length(sucra$sucra), 3) # 3 treatments
})

# =============================================================================
# node_splitting function tests
# =============================================================================

test_that("node_splitting accepts valid nma_result input", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	nma_result <- network_meta(nma_data, effect_measure = "hr")
	ns_result <- node_splitting(nma_result)

	expect_true(is.list(ns_result))
	expect_true("results" %in% names(ns_result))
	expect_true("effect_measure" %in% names(ns_result))
	expect_true("note" %in% names(ns_result))
})

test_that("node_splitting returns data frame results structure", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3", "S4"),
		treat1 = c("A", "B", "A", "B"),
		treat2 = c("B", "C", "C", "D"),
		effect = log(c(0.75, 0.90, 0.80, 0.85)),
		se = c(0.12, 0.15, 0.18, 0.14)
	)

	nma_result <- network_meta(nma_data, effect_measure = "hr")
	ns_result <- node_splitting(nma_result)

	expect_true(
		is.data.frame(ns_result$results) ||
			(is.data.frame(ns_result$results) && nrow(ns_result$results) >= 0)
	)
})

test_that("node_splitting errors on invalid nma_result input", {
	# Invalid: missing 'network' component
	bad_nma <- list(
		comparisons = data.frame(treatment = c("A", "B"), estimate = c(1, 2)),
		# network component missing
		effect_measure = "hr"
	)

	expect_error(
		node_splitting(bad_nma),
		"NMAResult.*from network_meta"
	)

	# Invalid: not a list
	expect_error(
		node_splitting("not a list"),
		"NMAResult.*from network_meta"
	)

	# Invalid: empty list
	expect_error(
		node_splitting(list()),
		"NMAResult.*from network_meta"
	)
})

test_that("node_splitting preserves effect_measure from nma_result", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = c(-0.5, -0.3, -0.4), # Mean difference
		se = c(0.12, 0.15, 0.18)
	)

	nma_result <- network_meta(nma_data, effect_measure = "md")
	ns_result <- node_splitting(nma_result)

	expect_equal(ns_result$effect_measure, "md")
})

test_that("node_splitting handles network with insufficient comparisons", {
	# Create a simple network with minimal connections
	simple_data <- data.frame(
		study = c("S1"),
		treat1 = c("A"),
		treat2 = c("B"),
		effect = log(0.75),
		se = 0.12
	)

	nma_result <- network_meta(simple_data, effect_measure = "hr")
	ns_result <- node_splitting(nma_result)

	# Should still return a valid result structure
	expect_true(is.list(ns_result))
	expect_true("results" %in% names(ns_result))
	expect_true("note" %in% names(ns_result))
})

test_that("node_splitting accepts custom conf_level parameter", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	nma_result <- network_meta(nma_data, effect_measure = "hr")

	# Test with different confidence levels
	ns_90 <- node_splitting(nma_result, conf_level = 0.90)
	ns_99 <- node_splitting(nma_result, conf_level = 0.99)

	expect_true(is.list(ns_90))
	expect_true(is.list(ns_99))
	expect_true("results" %in% names(ns_90))
	expect_true("results" %in% names(ns_99))
})

test_that("node_splitting includes note about simplified implementation", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3"),
		treat1 = c("A", "B", "A"),
		treat2 = c("B", "C", "C"),
		effect = log(c(0.75, 0.90, 0.80)),
		se = c(0.12, 0.15, 0.18)
	)

	nma_result <- network_meta(nma_data, effect_measure = "hr")
	ns_result <- node_splitting(nma_result)

	expect_true("note" %in% names(ns_result))
	expect_true(is.character(ns_result$note))
	expect_true(nchar(ns_result$note) > 0)
})

# =============================================================================
# Additional network meta-analysis tests
# =============================================================================

test_that("network_meta handles different effect measures in network", {
	# Mean difference network
	md_data <- data.frame(
		study = c("S1", "S2"),
		treat1 = c("A", "B"),
		treat2 = c("B", "C"),
		effect = c(-0.5, -0.3),
		se = c(0.15, 0.18)
	)

	result_md <- network_meta(md_data, effect_measure = "md")

	expect_true(S7::S7_inherits(result_md, NMAResult))
	expect_equal(result_md@effect_measure, "md")
})

test_that("network_meta creates proper comparison structure", {
	nma_data <- data.frame(
		study = c("S1", "S2", "S3", "S4", "S5"),
		treat1 = c("A", "A", "B", "A", "B"),
		treat2 = c("B", "C", "C", "C", "D"),
		effect = log(c(0.70, 0.85, 0.90, 0.75, 0.80)),
		se = c(0.10, 0.12, 0.15, 0.11, 0.14)
	)

	result <- network_meta(nma_data, effect_measure = "hr")

	expect_true("comparisons" %in% names(S7::props(result)))
	expect_true("network" %in% names(S7::props(result)))
	expect_true(result@network$n_treatments >= 3)
	expect_true(result@n_studies >= 4)
})

test_that("create_network_plot handles disconnected components", {
	# Network with disconnected treatments (two separate components: A-B and X-Y)
	disconnected_data <- data.frame(
		study = c("S1", "S2"),
		treat1 = c("A", "X"),
		treat2 = c("B", "Y"), # X-Y comparison forms disconnected component
		effect = log(c(0.75, 0.85)),
		se = c(0.12, 0.15)
	)

	nma_res <- network_meta(disconnected_data, effect_measure = "hr")
	plot <- create_network_plot(nma_res)

	expect_s7_class(plot, ClinicalPlot)
	expect_equal(plot@type, "network_geometry")
})

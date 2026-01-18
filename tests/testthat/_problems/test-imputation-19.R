# Extracted from test-imputation.R:19

# test -------------------------------------------------------------------------
it("returns an ImputationResult object", {
	skip_if_not_installed("mice")

	set.seed(123)
	data <- data.frame(
		x = c(1, 2, NA, 4, 5),
		y = c(10, NA, 30, 40, 50),
		z = c("A", "B", "A", "B", NA)
	)

	result <- perform_multiple_imputation(data, m = 3, maxit = 2)

	expect_true(S7::S7_inherits(result, ImputationResult))
})

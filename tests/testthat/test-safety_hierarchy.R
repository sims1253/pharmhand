# Tests for Safety Hierarchy Function
# Tests for create_ae_hierarchy_table
# Source: R/safety_hierarchy.R

describe("create_ae_hierarchy_table", {
	it("creates hierarchical AE table with SOC and PT", {
		adsl <- data.frame(
			USUBJID = paste0("SUBJ", 1:20),
			TRT01P = rep(c("Active", "Placebo"), each = 10)
		)

		adae <- data.frame(
			USUBJID = c(rep(paste0("SUBJ", 1:10), each = 2), paste0("SUBJ", 11:15)),
			TRT01P = c(rep("Active", 20), rep("Placebo", 5)),
			AEBODSYS = c(rep(c("SOC1", "SOC2"), 10), rep("SOC1", 5)),
			AEDECOD = c(paste0("PT", 1:20), rep("PT1", 5))
		)

		table <- create_ae_hierarchy_table(adae, adsl)

		expect_true(S7::S7_inherits(table, ClinicalTable))
		expect_equal(table@type, "ae_hierarchy")
	})
})

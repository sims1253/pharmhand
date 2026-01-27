# Test S7 Configuration Classes
#
# Tests for the S7 configuration classes defined in R/config_classes.R:
# - Configuration (abstract base class)
# - SubgroupConfig
# - PopulationConfig
# - SOCConfig
# - PTConfig
# - ConfigurationRegistry
#
# These tests verify:
# - Objects can be created with valid parameters
# - Default values are applied correctly
# - Properties are accessible via @
# - S7 class inheritance works

# ============================================================================
# Configuration (Abstract Base Class) Tests
# ============================================================================

describe("config classes", {
	it("Configuration class is properly defined", {
		# Configuration is the abstract base class for all config objects
		# Verify it's a valid S7 class
		expect_true(inherits(Configuration, "S7_class"))
	})

	it("Configuration properties have correct defaults", {
		# Create a subclass to test Configuration defaults
		subgroup <- SubgroupConfig(
			variable = "AGEGR1",
			labels = list(Y_LT65 = "< 65", Y_GE65 = ">= 65")
		)

		expect_equal(subgroup@source, "yaml")
		expect_equal(subgroup@priority, 0L)
	})

	it("Configuration source can be overridden", {
		subgroup <- SubgroupConfig(
			variable = "AGEGR1",
			labels = list(Y_LT65 = "< 65"),
			source = "custom",
			priority = 100L
		)

		expect_equal(subgroup@source, "custom")
		expect_equal(subgroup@priority, 100L)
	})

	# ============================================================================
	# SubgroupConfig Tests
	# ============================================================================

	it("SubgroupConfig can be created with required variable", {
		config <- SubgroupConfig(variable = "AGEGR1")

		expect_true(S7::S7_inherits(config, SubgroupConfig))
		expect_equal(config@variable, "AGEGR1")
	})

	it("SubgroupConfig with all parameters", {
		config <- SubgroupConfig(
			variable = "AGEGR1",
			labels = list(Y_LT65 = "< 65", Y_GE65 = ">= 65"),
			order = c("Y_LT65", "Y_GE65"),
			filter_values = c("Y_LT65", "Y_GE65")
		)

		expect_equal(config@variable, "AGEGR1")
		expect_equal(config@labels, list(Y_LT65 = "< 65", Y_GE65 = ">= 65"))
		expect_equal(config@order, c("Y_LT65", "Y_GE65"))
		expect_equal(config@filter_values, c("Y_LT65", "Y_GE65"))
	})

	it("SubgroupConfig has correct defaults", {
		config <- SubgroupConfig(variable = "SEX")

		expect_equal(config@labels, list())
		expect_null(config@order)
		expect_null(config@filter_values)
	})

	it("SubgroupConfig inherits from Configuration", {
		config <- SubgroupConfig(variable = "RACE")

		expect_true(S7::S7_inherits(config, SubgroupConfig))
		expect_true(S7::S7_inherits(config, Configuration))
	})

	it("SubgroupConfig labels can be empty list", {
		config <- SubgroupConfig(
			variable = "CUSTOM_VAR",
			labels = list()
		)

		expect_equal(config@labels, list())
	})

	it("SubgroupConfig order is optional", {
		config <- SubgroupConfig(
			variable = "AGEGR1",
			order = NULL
		)

		expect_null(config@order)
	})

	# ============================================================================
	# PopulationConfig Tests
	# ============================================================================

	it("PopulationConfig can be created with required variable", {
		config <- PopulationConfig(
			variable = "SAFFL",
			label = "Safety Analysis Set"
		)

		expect_true(S7::S7_inherits(config, PopulationConfig))
		expect_equal(config@variable, "SAFFL")
		expect_equal(config@label, "Safety Analysis Set")
	})

	it("PopulationConfig with all parameters", {
		config <- PopulationConfig(
			variable = "SAFFL",
			label = "Safety Analysis Set",
			description = "Subjects who received at least one dose",
			flag_value = "Y"
		)

		expect_equal(config@variable, "SAFFL")
		expect_equal(config@label, "Safety Analysis Set")
		expect_equal(config@description, "Subjects who received at least one dose")
		expect_equal(config@flag_value, "Y")
	})

	it("PopulationConfig has correct defaults", {
		config <- PopulationConfig(
			variable = "FASFL",
			label = "Full Analysis Set"
		)

		expect_equal(config@flag_value, "Y")
		expect_null(config@description)
	})

	it("PopulationConfig inherits from Configuration", {
		config <- PopulationConfig(
			variable = "PPSFL",
			label = "Per-Protocol Set"
		)

		expect_true(S7::S7_inherits(config, PopulationConfig))
		expect_true(S7::S7_inherits(config, Configuration))
	})

	it("PopulationConfig flag_value can be customized", {
		config <- PopulationConfig(
			variable = "CUSTOM_FL",
			label = "Custom Population",
			flag_value = "1"
		)

		expect_equal(config@flag_value, "1")
	})

	it("PopulationConfig description can be NULL", {
		config <- PopulationConfig(
			variable = "TESTFL",
			label = "Test",
			description = NULL
		)

		expect_null(config@description)
	})

	# ============================================================================
	# SOCConfig Tests
	# ============================================================================

	it("SOCConfig can be created with required variable", {
		config <- SOCConfig(variable = "AEBODSYS")

		expect_true(S7::S7_inherits(config, SOCConfig))
		expect_equal(config@variable, "AEBODSYS")
	})

	it("SOCConfig with all parameters", {
		config <- SOCConfig(
			variable = "AEBODSYS",
			include_all = TRUE,
			custom_order = c("Cardiac", "Nervous system", "Gastrointestinal"),
			sort_by = "alphabetical",
			min_subjects = 3,
			top_n = 10
		)

		expect_equal(config@variable, "AEBODSYS")
		expect_true(config@include_all)
		expect_equal(
			config@custom_order,
			c("Cardiac", "Nervous system", "Gastrointestinal")
		)
		expect_equal(config@sort_by, "alphabetical")
		expect_equal(config@min_subjects, 3)
		expect_equal(config@top_n, 10)
	})

	it("SOCConfig has correct defaults", {
		config <- SOCConfig(variable = "AEBODSYS")

		expect_true(config@include_all)
		expect_equal(config@sort_by, "frequency")
		expect_equal(config@min_subjects, 1)
		expect_null(config@custom_order)
		expect_null(config@top_n)
	})

	it("SOCConfig inherits from Configuration", {
		config <- SOCConfig(variable = "AEBODSYS")

		expect_true(S7::S7_inherits(config, SOCConfig))
		expect_true(S7::S7_inherits(config, Configuration))
	})

	it("SOCConfig custom_order can be NULL", {
		config <- SOCConfig(
			variable = "AEBODSYS",
			custom_order = NULL
		)

		expect_null(config@custom_order)
	})

	it("SOCConfig sort_by accepts 'frequency' and 'alphabetical'", {
		config_freq <- SOCConfig(variable = "AEBODSYS", sort_by = "frequency")
		config_alpha <- SOCConfig(variable = "AEBODSYS", sort_by = "alphabetical")

		expect_equal(config_freq@sort_by, "frequency")
		expect_equal(config_alpha@sort_by, "alphabetical")
	})

	it("SOCConfig top_n can be set to limit SOCs", {
		config <- SOCConfig(variable = "AEBODSYS", top_n = 5)

		expect_equal(config@top_n, 5)
	})

	# ============================================================================
	# PTConfig Tests
	# ============================================================================

	it("PTConfig can be created with required variable", {
		config <- PTConfig(variable = "AEDECOD")

		expect_true(S7::S7_inherits(config, PTConfig))
		expect_equal(config@variable, "AEDECOD")
	})

	it("PTConfig with all parameters", {
		config <- PTConfig(
			variable = "AEDECOD",
			include_all = FALSE,
			sort_by = "alphabetical",
			min_subjects = 2,
			top_n_per_soc = 5,
			show_pt_codes = TRUE
		)

		expect_equal(config@variable, "AEDECOD")
		expect_false(config@include_all)
		expect_equal(config@sort_by, "alphabetical")
		expect_equal(config@min_subjects, 2)
		expect_equal(config@top_n_per_soc, 5)
		expect_true(config@show_pt_codes)
	})

	it("PTConfig has correct defaults", {
		config <- PTConfig(variable = "AEDECOD")

		expect_true(config@include_all)
		expect_equal(config@sort_by, "frequency")
		expect_equal(config@min_subjects, 1)
		expect_false(config@show_pt_codes)
		expect_null(config@top_n_per_soc)
	})

	it("PTConfig inherits from Configuration", {
		config <- PTConfig(variable = "AEDECOD")

		expect_true(S7::S7_inherits(config, PTConfig))
		expect_true(S7::S7_inherits(config, Configuration))
	})

	it("PTConfig show_pt_codes defaults to FALSE", {
		config <- PTConfig(variable = "AEDECOD")

		expect_equal(config@show_pt_codes, FALSE)
	})

	it("PTConfig top_n_per_soc can limit PTs per SOC", {
		config <- PTConfig(
			variable = "AEDECOD",
			top_n_per_soc = 10
		)

		expect_equal(config@top_n_per_soc, 10)
	})

	# ============================================================================
	# ConfigurationRegistry Tests
	# ============================================================================

	it("ConfigurationRegistry can be created empty", {
		registry <- ConfigurationRegistry()

		expect_true(S7::S7_inherits(registry, ConfigurationRegistry))
		expect_equal(registry@subgroups, list())
		expect_equal(registry@populations, list())
	})

	it("ConfigurationRegistry with subgroups and populations", {
		subgroup <- SubgroupConfig(
			variable = "AGEGR1",
			labels = list(Y_LT65 = "< 65")
		)
		population <- PopulationConfig(
			variable = "SAFFL",
			label = "Safety"
		)

		registry <- ConfigurationRegistry(
			subgroups = list(age = subgroup),
			populations = list(safety = population)
		)

		expect_equal(length(registry@subgroups), 1)
		expect_equal(length(registry@populations), 1)
		expect_equal(names(registry@subgroups), "age")
		expect_equal(names(registry@populations), "safety")
	})

	it("ConfigurationRegistry with SOC and PT configs", {
		soc <- SOCConfig(variable = "AEBODSYS")
		pt <- PTConfig(variable = "AEDECOD")

		registry <- ConfigurationRegistry(
			soc_config = soc,
			pt_config = pt
		)

		expect_true(S7::S7_inherits(registry@soc_config, SOCConfig))
		expect_true(S7::S7_inherits(registry@pt_config, PTConfig))
	})

	it("ConfigurationRegistry has correct defaults for all properties", {
		registry <- ConfigurationRegistry()

		expect_equal(registry@subgroups, list())
		expect_equal(registry@populations, list())
		expect_null(registry@soc_config)
		expect_null(registry@pt_config)
		expect_equal(registry@report_types, list())
		expect_equal(registry@performance, list())
		expect_equal(registry@plots, list())
		expect_equal(registry@tables, list())
		expect_equal(registry@validation, list())
	})

	it("ConfigurationRegistry can hold complex nested settings", {
		registry <- ConfigurationRegistry(
			performance = list(
				cache = list(enabled = TRUE, max_size_mb = 500),
				parallel = list(enabled = TRUE, workers = 4)
			),
			plots = list(
				km = list(palette = "jco", conf_int = TRUE),
				forest = list(ref_line = 0, show_pval = TRUE)
			),
			tables = list(
				font_size = 9,
				font_family = "Arial",
				border_style = "single"
			)
		)

		expect_true(registry@performance$cache$enabled)
		expect_equal(registry@performance$cache$max_size_mb, 500)
		expect_equal(registry@plots$km$palette, "jco")
		expect_equal(registry@tables$font_size, 9)
	})

	it("ConfigurationRegistry can have multiple subgroups", {
		sub1 <- SubgroupConfig(variable = "AGEGR1", labels = list(Y1 = "Young"))
		sub2 <- SubgroupConfig(variable = "SEX", labels = list(M = "Male"))
		sub3 <- SubgroupConfig(variable = "RACE", labels = list(W = "White"))

		registry <- ConfigurationRegistry(
			subgroups = list(
				age = sub1,
				sex = sub2,
				race = sub3
			)
		)

		expect_equal(length(registry@subgroups), 3)
		expect_equal(names(registry@subgroups), c("age", "sex", "race"))
	})

	it("ConfigurationRegistry can have multiple populations", {
		pop1 <- PopulationConfig(variable = "SAFFL", label = "Safety")
		pop2 <- PopulationConfig(variable = "FASFL", label = "FAS")
		pop3 <- PopulationConfig(variable = "PPSFL", label = "PPS")

		registry <- ConfigurationRegistry(
			populations = list(
				safety = pop1,
				fas = pop2,
				pps = pop3
			)
		)

		expect_equal(length(registry@populations), 3)
		expect_equal(names(registry@populations), c("safety", "fas", "pps"))
	})

	it("ConfigurationRegistry inherits from Configuration", {
		registry <- ConfigurationRegistry()

		expect_true(S7::S7_inherits(registry, ConfigurationRegistry))
	})

	# ============================================================================
	# Class Inheritance Verification Tests
	# ============================================================================

	it("All config classes have proper class hierarchy", {
		# Create instances
		subgroup <- SubgroupConfig(variable = "TEST")
		population <- PopulationConfig(variable = "TEST", label = "Test")
		soc <- SOCConfig(variable = "TEST")
		pt <- PTConfig(variable = "TEST")
		registry <- ConfigurationRegistry()

		# Verify each class inherits from Configuration
		expect_true(S7::S7_inherits(subgroup, Configuration))
		expect_true(S7::S7_inherits(population, Configuration))
		expect_true(S7::S7_inherits(soc, Configuration))
		expect_true(S7::S7_inherits(pt, Configuration))
	})

	it("Properties are accessible via @ operator", {
		# Test various property types
		config <- SubgroupConfig(
			variable = "AGEGR1",
			labels = list(Y1 = "Young"),
			order = c("Y1", "Y2"),
			filter_values = c("Y1")
		)

		# Verify @ access works for all properties
		expect_equal(config@variable, "AGEGR1")
		expect_equal(config@labels, list(Y1 = "Young"))
		expect_equal(config@order, c("Y1", "Y2"))
		expect_equal(config@filter_values, c("Y1"))
		expect_equal(config@source, "yaml")
		expect_equal(config@priority, 0L)
	})

	it("Configuration properties can be modified", {
		config <- PopulationConfig(
			variable = "SAFFL",
			label = "Safety",
			priority = 0L
		)

		# Modify via @
		config@priority <- 100L

		expect_equal(config@priority, 100L)
		expect_equal(config@source, "yaml")
	})

	# ============================================================================
	# Edge Cases and Validation Tests
	# ============================================================================

	it("SubgroupConfig handles complex label structures", {
		complex_labels <- list(
			AGEGRP1 = "< 65 years",
			AGEGRP2 = "65-74 years",
			AGEGRP3 = "75-84 years",
			AGEGRP4 = ">= 85 years"
		)

		config <- SubgroupConfig(
			variable = "AGEGR4",
			labels = complex_labels
		)

		expect_equal(config@labels, complex_labels)
		expect_equal(length(config@labels), 4)
	})

	it("PopulationConfig handles character flag_value", {
		config <- PopulationConfig(
			variable = "CUSTOMFL",
			label = "Custom",
			flag_value = "1"
		)

		expect_equal(config@flag_value, "1")
	})

	it("SOCConfig handles numeric min_subjects", {
		config <- SOCConfig(
			variable = "AEBODSYS",
			min_subjects = 5
		)

		expect_equal(config@min_subjects, 5)
		expect_type(config@min_subjects, "double")
	})

	it("ConfigurationRegistry validation property structure", {
		registry <- ConfigurationRegistry(
			validation = list(
				check_data = TRUE,
				warn_on_missing = TRUE,
				stop_on_error = FALSE
			)
		)

		expect_true(registry@validation$check_data)
		expect_true(registry@validation$warn_on_missing)
		expect_false(registry@validation$stop_on_error)
	})
})

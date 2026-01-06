# Test Configuration API Functions
#
# Tests for the API functions defined in R/config_api.R:
# - load_config()
# - parse_config_registry()
# - define_subgroup_config()
# - define_population_config()
# - get_subgroup_config()
# - get_population_config()
# - list_subgroups()
# - list_populations()
# - update_soc_config()
# - update_pt_config()
# - get_performance_setting()
#
# These tests verify:
# - Loading configuration from YAML files
# - Parsing YAML structure into registry objects
# - Adding/overriding configurations at runtime
# - Retrieving configurations by name
# - Listing available configurations
# - Updating SOC/PT configurations
# - Accessing nested performance settings
# - Edge cases like invalid inputs and missing configurations

library(testthat)
library(pharmhand)

# Helper to create a valid config for testing with integer priorities
create_test_config <- function() {
	# Note: We use priority = 0L explicitly to avoid integer type issues
	list(
		subgroups = list(
			test_sg = list(
				variable = "TESTVAR",
				labels = list(A = "Alpha", B = "Beta"),
				order = NULL,
				filter_values = NULL
			)
		),
		populations = list(
			test_pop = list(
				variable = "POPVAR",
				label = "Test Population",
				description = "A test population",
				flag_value = "Y"
			)
		),
		soc_config = list(
			variable = "AEBODSYS",
			include_all = TRUE,
			custom_order = NULL,
			sort_by = "frequency",
			min_subjects = 1,
			top_n = NULL
		),
		pt_config = list(
			variable = "AEDECOD",
			include_all = TRUE,
			sort_by = "frequency",
			min_subjects = 1,
			top_n_per_soc = NULL,
			show_pt_codes = FALSE
		),
		performance = list(
			docx = list(batch_size = 50)
		),
		report_types = list(),
		plots = list(),
		tables = list(),
		validation = list()
	)
}

# ============================================================================
# parse_config_registry() Tests
# ============================================================================

test_that("parse_config_registry() creates valid registry from list", {
	config_list <- create_test_config()

	registry <- pharmhand:::parse_config_registry(config_list)

	expect_true(S7::S7_inherits(registry, ConfigurationRegistry))
	expect_equal(names(registry@subgroups), "test_sg")
	expect_equal(names(registry@populations), "test_pop")
})

test_that("parse_config_registry() handles empty subgroups", {
	config_list <- list(
		subgroups = list(),
		populations = list(),
		soc_config = NULL,
		pt_config = NULL,
		report_types = list(),
		performance = list(),
		plots = list(),
		tables = list(),
		validation = list()
	)

	registry <- pharmhand:::parse_config_registry(config_list)

	expect_equal(length(registry@subgroups), 0)
})

test_that("parse_config_registry() handles empty populations", {
	config_list <- list(
		subgroups = list(),
		populations = list(),
		soc_config = NULL,
		pt_config = NULL,
		report_types = list(),
		performance = list(),
		plots = list(),
		tables = list(),
		validation = list()
	)

	registry <- pharmhand:::parse_config_registry(config_list)

	expect_equal(length(registry@populations), 0)
})

test_that("parse_config_registry() handles missing sections", {
	config_list <- list()

	registry <- pharmhand:::parse_config_registry(config_list)

	expect_equal(length(registry@subgroups), 0)
	expect_equal(length(registry@populations), 0)
	expect_null(registry@soc_config)
	expect_null(registry@pt_config)
})

test_that("parse_config_registry() creates SOCConfig from YAML structure", {
	config_list <- list(
		soc_config = list(
			variable = "AEBODSYS",
			include_all = TRUE,
			custom_order = NULL,
			sort_by = "alphabetical",
			min_subjects = 5,
			top_n = 10
		),
		subgroups = list(),
		populations = list(),
		pt_config = NULL,
		report_types = list(),
		performance = list(),
		plots = list(),
		tables = list(),
		validation = list()
	)

	registry <- pharmhand:::parse_config_registry(config_list)

	expect_true(S7::S7_inherits(registry@soc_config, SOCConfig))
	expect_equal(registry@soc_config@variable, "AEBODSYS")
	expect_true(registry@soc_config@include_all)
	expect_equal(registry@soc_config@sort_by, "alphabetical")
	expect_equal(registry@soc_config@min_subjects, 5)
	expect_equal(registry@soc_config@top_n, 10)
})

test_that("parse_config_registry() creates PTConfig from YAML structure", {
	config_list <- list(
		pt_config = list(
			variable = "AEDECOD",
			include_all = FALSE,
			sort_by = "frequency",
			min_subjects = 3,
			top_n_per_soc = 5,
			show_pt_codes = TRUE
		),
		subgroups = list(),
		populations = list(),
		soc_config = NULL,
		report_types = list(),
		performance = list(),
		plots = list(),
		tables = list(),
		validation = list()
	)

	registry <- pharmhand:::parse_config_registry(config_list)

	expect_true(S7::S7_inherits(registry@pt_config, PTConfig))
	expect_equal(registry@pt_config@variable, "AEDECOD")
	expect_false(registry@pt_config@include_all)
	expect_true(registry@pt_config@show_pt_codes)
})

test_that("parse_config_registry() applies default values for missing optional
	fields", {
	config_list <- list(
		subgroups = list(
			minimal = list(
				variable = "MINVAR"
			)
		),
		soc_config = list(
			variable = "SOCVAR"
		),
		populations = list(),
		pt_config = NULL,
		report_types = list(),
		performance = list(),
		plots = list(),
		tables = list(),
		validation = list()
	)

	registry <- pharmhand:::parse_config_registry(config_list)

	# Check subgroup defaults
	sg <- registry@subgroups[["minimal"]]
	expect_equal(sg@labels, list())
	expect_null(sg@order)
	expect_null(sg@filter_values)

	# Check SOC defaults
	expect_true(registry@soc_config@include_all)
	expect_equal(registry@soc_config@sort_by, "frequency")
	expect_equal(registry@soc_config@min_subjects, 1)
})

test_that("parse_config_registry() preserves custom performance settings", {
	config_list <- list(
		performance = list(
			custom_setting = "custom_value",
			nested = list(deep = "value")
		),
		subgroups = list(),
		populations = list(),
		soc_config = NULL,
		pt_config = NULL,
		report_types = list(),
		plots = list(),
		tables = list(),
		validation = list()
	)

	registry <- pharmhand:::parse_config_registry(config_list)

	expect_equal(registry@performance$custom_setting, "custom_value")
	expect_equal(registry@performance$nested$deep, "value")
})

# ============================================================================
# define_subgroup_config() Tests
# ============================================================================

test_that("define_subgroup_config() adds new subgroup", {
	registry <- ConfigurationRegistry()
	registry <- define_subgroup_config(
		registry,
		name = "custom_age",
		variable = "AGEGR1",
		labels = list(YOUNG = "< 65", OLD = ">= 65")
	)

	expect_equal(length(registry@subgroups), 1)
	expect_true("custom_age" %in% names(registry@subgroups))

	config <- registry@subgroups[["custom_age"]]
	expect_equal(config@variable, "AGEGR1")
	expect_equal(config@labels, list(YOUNG = "< 65", OLD = ">= 65"))
	expect_equal(config@source, "r_api")
	expect_equal(config@priority, 100L)
})

test_that("define_subgroup_config() overrides existing subgroup", {
	registry <- ConfigurationRegistry()
	registry <- define_subgroup_config(
		registry,
		name = "age",
		variable = "VAR1"
	)
	registry <- define_subgroup_config(
		registry,
		name = "age",
		variable = "VAR2"
	)

	config <- registry@subgroups[["age"]]
	expect_equal(config@variable, "VAR2")
})

test_that("define_subgroup_config() respects custom priority", {
	registry <- ConfigurationRegistry()
	registry <- define_subgroup_config(
		registry,
		name = "test",
		variable = "TESTVAR",
		priority = 200L
	)

	config <- registry@subgroups[["test"]]
	expect_equal(config@priority, 200L)
})

test_that("define_subgroup_config() handles empty labels", {
	registry <- ConfigurationRegistry()
	registry <- define_subgroup_config(
		registry,
		name = "no_labels",
		variable = "TESTVAR",
		labels = list()
	)

	config <- registry@subgroups[["no_labels"]]
	expect_equal(config@labels, list())
})

test_that("define_subgroup_config() preserves order parameter", {
	registry <- ConfigurationRegistry()
	registry <- define_subgroup_config(
		registry,
		name = "ordered",
		variable = "TESTVAR",
		order = c("A", "B", "C")
	)

	config <- registry@subgroups[["ordered"]]
	expect_equal(config@order, c("A", "B", "C"))
})

test_that("define_subgroup_config() preserves filter_values parameter", {
	registry <- ConfigurationRegistry()
	registry <- define_subgroup_config(
		registry,
		name = "filtered",
		variable = "TESTVAR",
		filter_values = c("X", "Y")
	)

	config <- registry@subgroups[["filtered"]]
	expect_equal(config@filter_values, c("X", "Y"))
})

test_that("define_subgroup_config() throws error for invalid registry", {
	expect_error(
		define_subgroup_config(
			"not_a_registry",
			"name",
			"variable"
		),
		"registry must be a ConfigurationRegistry object"
	)
})

# ============================================================================
# define_population_config() Tests
# ============================================================================

test_that("define_population_config() adds new population", {
	registry <- ConfigurationRegistry()
	registry <- define_population_config(
		registry,
		name = "CUSTOM_POP",
		variable = "CUSTOMFL",
		label = "Custom Population",
		description = "A custom population for testing"
	)

	expect_equal(length(registry@populations), 1)
	expect_true("CUSTOM_POP" %in% names(registry@populations))

	config <- registry@populations[["CUSTOM_POP"]]
	expect_equal(config@variable, "CUSTOMFL")
	expect_equal(config@label, "Custom Population")
	expect_equal(config@description, "A custom population for testing")
	expect_equal(config@source, "r_api")
	expect_equal(config@priority, 100L)
})

test_that("define_population_config() overrides existing population", {
	registry <- ConfigurationRegistry()
	registry <- define_population_config(
		registry,
		name = "SAF",
		variable = "VAR1"
	)
	registry <- define_population_config(
		registry,
		name = "SAF",
		variable = "VAR2"
	)

	config <- registry@populations[["SAF"]]
	expect_equal(config@variable, "VAR2")
})

test_that("define_population_config() uses name as label if not provided", {
	registry <- ConfigurationRegistry()
	registry <- define_population_config(
		registry,
		name = "MY_POP",
		variable = "MYFL"
	)

	config <- registry@populations[["MY_POP"]]
	expect_equal(config@label, "MY_POP")
})

test_that("define_population_config() respects custom flag_value", {
	registry <- ConfigurationRegistry()
	registry <- define_population_config(
		registry,
		name = "numeric_flag",
		variable = "TESTFL",
		flag_value = "1"
	)

	config <- registry@populations[["numeric_flag"]]
	expect_equal(config@flag_value, "1")
})

test_that("define_population_config() throws error for invalid registry", {
	expect_error(
		define_population_config(
			NULL,
			"name",
			"variable"
		),
		"S7::S7_inherits"
	)
})

# ============================================================================
# get_subgroup_config() Tests
# ============================================================================

test_that("get_subgroup_config() returns existing subgroup", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	config <- get_subgroup_config(registry, "test_sg")

	expect_true(S7::S7_inherits(config, SubgroupConfig))
	expect_equal(config@variable, "TESTVAR")
})

test_that("get_subgroup_config() returns NULL for non-existent subgroup", {
	registry <- ConfigurationRegistry()
	config <- get_subgroup_config(registry, "non_existent_subgroup")

	expect_null(config)
})

test_that("get_subgroup_config() works after adding custom subgroup", {
	registry <- ConfigurationRegistry()
	registry <- define_subgroup_config(
		registry,
		name = "custom",
		variable = "CUSTOMVAR"
	)

	config <- get_subgroup_config(registry, "custom")
	expect_true(S7::S7_inherits(config, SubgroupConfig))
	expect_equal(config@variable, "CUSTOMVAR")
})

test_that("get_subgroup_config() throws error for invalid name type", {
	registry <- ConfigurationRegistry()
	expect_error(
		get_subgroup_config(registry, 123),
		"character"
	)
})

# ============================================================================
# get_population_config() Tests
# ============================================================================

test_that("get_population_config() returns existing population", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	config <- get_population_config(registry, "test_pop")

	expect_true(S7::S7_inherits(config, PopulationConfig))
	expect_equal(config@variable, "POPVAR")
	expect_equal(config@label, "Test Population")
})

test_that("get_population_config() returns NULL for non-existent population", {
	registry <- ConfigurationRegistry()
	config <- get_population_config(registry, "NON_EXISTENT")

	expect_null(config)
})

test_that("get_population_config() works after adding custom population", {
	registry <- ConfigurationRegistry()
	registry <- define_population_config(
		registry,
		name = "MY_POP",
		variable = "MYFL"
	)

	config <- get_population_config(registry, "MY_POP")
	expect_true(S7::S7_inherits(config, PopulationConfig))
	expect_equal(config@variable, "MYFL")
})

test_that("get_population_config() throws error for invalid name type", {
	registry <- ConfigurationRegistry()
	expect_error(
		get_population_config(registry, c("a", "b")),
		"character"
	)
})

# ============================================================================
# list_subgroups() Tests
# ============================================================================

test_that("list_subgroups() returns character vector", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	subgroups <- list_subgroups(registry)

	expect_type(subgroups, "character")
	expect_equal(length(subgroups), 1)
})

test_that("list_subgroups() returns expected subgroup names", {
	config_list <- list(
		subgroups = list(
			age_groups = list(variable = "AGE"),
			sex = list(variable = "SEX"),
			race = list(variable = "RACE")
		),
		populations = list(),
		soc_config = NULL,
		pt_config = NULL,
		report_types = list(),
		performance = list(),
		plots = list(),
		tables = list(),
		validation = list()
	)

	registry <- pharmhand:::parse_config_registry(config_list)
	subgroups <- list_subgroups(registry)

	expect_equal(sort(subgroups), sort(c("age_groups", "sex", "race")))
})

test_that("list_subgroups() returns empty for registry with no subgroups", {
	registry <- ConfigurationRegistry()
	subgroups <- list_subgroups(registry)

	# names(empty_list) returns NULL
	expect_null(subgroups)
})

test_that("list_subgroups() includes custom subgroups", {
	registry <- ConfigurationRegistry()
	registry <- define_subgroup_config(registry, name = "sg1", variable = "V1")
	registry <- define_subgroup_config(registry, name = "sg2", variable = "V2")

	subgroups <- list_subgroups(registry)

	expect_equal(sort(subgroups), c("sg1", "sg2"))
})

# ============================================================================
# list_populations() Tests
# ============================================================================

test_that("list_populations() returns character vector", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	populations <- list_populations(registry)

	expect_type(populations, "character")
	expect_equal(length(populations), 1)
})

test_that("list_populations() returns expected population names", {
	config_list <- list(
		populations = list(
			SAF = list(variable = "SAFFL", label = "Safety", flag_value = "Y"),
			FAS = list(variable = "FASFL", label = "FAS", flag_value = "Y"),
			PPS = list(variable = "PPSFL", label = "PPS", flag_value = "Y")
		),
		subgroups = list(),
		soc_config = NULL,
		pt_config = NULL,
		report_types = list(),
		performance = list(),
		plots = list(),
		tables = list(),
		validation = list()
	)

	registry <- pharmhand:::parse_config_registry(config_list)
	populations <- list_populations(registry)

	expect_true(all(c("SAF", "FAS", "PPS") %in% populations))
})

test_that("list_populations() returns empty for registry with no populations", {
	registry <- ConfigurationRegistry()
	populations <- list_populations(registry)

	# names(empty_list) returns NULL
	expect_null(populations)
})

test_that("list_populations() includes custom populations", {
	registry <- ConfigurationRegistry()
	registry <- define_population_config(registry, name = "pop1", variable = "V1")
	registry <- define_population_config(registry, name = "pop2", variable = "V2")

	populations <- list_populations(registry)

	expect_equal(sort(populations), c("pop1", "pop2"))
})

# ============================================================================
# update_soc_config() Tests
# ============================================================================

test_that("update_soc_config() updates existing SOC config", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)
	original_sort <- registry@soc_config@sort_by
	registry <- update_soc_config(registry, sort_by = "alphabetical")

	expect_equal(registry@soc_config@sort_by, "alphabetical")
	expect_false(registry@soc_config@sort_by == original_sort)
})

test_that("update_soc_config() creates new SOC config if none exists", {
	registry <- ConfigurationRegistry()
	registry <- update_soc_config(
		registry,
		variable = "CUSTOMSOC",
		sort_by = "alphabetical"
	)

	expect_true(S7::S7_inherits(registry@soc_config, SOCConfig))
	expect_equal(registry@soc_config@variable, "CUSTOMSOC")
	expect_equal(registry@soc_config@sort_by, "alphabetical")
})

test_that("update_soc_config() can update multiple properties", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	registry <- update_soc_config(
		registry,
		sort_by = "alphabetical",
		min_subjects = 5,
		top_n = 10
	)

	expect_equal(registry@soc_config@sort_by, "alphabetical")
	expect_equal(registry@soc_config@min_subjects, 5)
	expect_equal(registry@soc_config@top_n, 10)
})

test_that("update_soc_config() sets source to r_api", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	registry <- update_soc_config(registry, sort_by = "alphabetical")

	expect_equal(registry@soc_config@source, "r_api")
})

test_that("update_soc_config() sets priority to 100", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	registry <- update_soc_config(registry, sort_by = "alphabetical")

	expect_equal(registry@soc_config@priority, 100L)
})

test_that("update_soc_config() preserves existing values when updating some", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	original_var <- registry@soc_config@variable
	original_include_all <- registry@soc_config@include_all

	registry <- update_soc_config(registry, sort_by = "alphabetical")

	expect_equal(registry@soc_config@variable, original_var)
	expect_equal(registry@soc_config@include_all, original_include_all)
})

test_that("update_soc_config() throws error for invalid registry", {
	expect_error(
		update_soc_config("not_a_registry", sort_by = "alphabetical"),
		"S7::S7_inherits"
	)
})

# ============================================================================
# update_pt_config() Tests
# ============================================================================

test_that("update_pt_config() updates existing PT config", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	original_sort <- registry@pt_config@sort_by
	registry <- update_pt_config(registry, sort_by = "alphabetical")

	expect_equal(registry@pt_config@sort_by, "alphabetical")
	expect_false(registry@pt_config@sort_by == original_sort)
})

test_that("update_pt_config() creates new PT config if none exists", {
	registry <- ConfigurationRegistry()
	registry <- update_pt_config(
		registry,
		variable = "CUSTOMPT",
		sort_by = "alphabetical"
	)

	expect_true(S7::S7_inherits(registry@pt_config, PTConfig))
	expect_equal(registry@pt_config@variable, "CUSTOMPT")
	expect_equal(registry@pt_config@sort_by, "alphabetical")
})

test_that("update_pt_config() can update multiple properties", {
	config_list <- create_test_config()
	registry <- parse_config_registry(config_list)

	registry <- update_pt_config(
		registry,
		sort_by = "alphabetical",
		min_subjects = 3,
		top_n_per_soc = 10
		# Note: show_pt_codes is not a parameter in the source function
	)

	expect_equal(registry@pt_config@sort_by, "alphabetical")
	expect_equal(registry@pt_config@min_subjects, 3)
	expect_equal(registry@pt_config@top_n_per_soc, 10)
})

test_that("update_pt_config() sets source to r_api", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	registry <- update_pt_config(registry, sort_by = "alphabetical")

	expect_equal(registry@pt_config@source, "r_api")
})

test_that("update_pt_config() sets priority to 100", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	registry <- update_pt_config(registry, sort_by = "alphabetical")

	expect_equal(registry@pt_config@priority, 100L)
})

test_that("update_pt_config() preserves existing values when updating some", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	original_var <- registry@pt_config@variable
	original_include_all <- registry@pt_config@include_all

	registry <- update_pt_config(registry, sort_by = "alphabetical")

	expect_equal(registry@pt_config@variable, original_var)
	expect_equal(registry@pt_config@include_all, original_include_all)
})

test_that("update_pt_config() throws error for invalid registry", {
	expect_error(
		update_pt_config(NULL, sort_by = "alphabetical"),
		"S7::S7_inherits"
	)
})

# ============================================================================
# get_performance_setting() Tests
# ============================================================================

test_that("get_performance_setting() returns nested value with dot notation", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	value <- get_performance_setting(registry, "docx.batch_size")

	expect_equal(value, 50)
})

test_that("get_performance_setting() returns default for missing setting", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	value <- get_performance_setting(
		registry,
		"nonexistent.setting",
		default = "default_value"
	)

	expect_equal(value, "default_value")
})

test_that("get_performance_setting() returns default for completely missing path", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	value <- get_performance_setting(
		registry,
		"completely.missing.path",
		default = 999
	)

	expect_equal(value, 999)
})

test_that("get_performance_setting() works with deeply nested settings", {
	config_list <- list(
		performance = list(
			cache = list(ttl_hours = 24)
		),
		subgroups = list(),
		populations = list(),
		soc_config = NULL,
		pt_config = NULL,
		report_types = list(),
		plots = list(),
		tables = list(),
		validation = list()
	)
	registry <- pharmhand:::parse_config_registry(config_list)

	value <- get_performance_setting(registry, "cache.ttl_hours")

	expect_equal(value, 24)
})

test_that("get_performance_setting() returns NULL for missing setting without default", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	value <- get_performance_setting(registry, "missing.setting")

	expect_null(value)
})

test_that("get_performance_setting() can access cache settings", {
	config_list <- list(
		performance = list(
			cache = list(enabled = TRUE, max_size_mb = 500)
		),
		subgroups = list(),
		populations = list(),
		soc_config = NULL,
		pt_config = NULL,
		report_types = list(),
		plots = list(),
		tables = list(),
		validation = list()
	)
	registry <- pharmhand:::parse_config_registry(config_list)

	expect_true(registry@performance$cache$enabled)
	expect_equal(get_performance_setting(registry, "cache.enabled"), TRUE)
	expect_equal(get_performance_setting(registry, "cache.max_size_mb"), 500)
})

test_that("get_performance_setting() can access parallel settings", {
	config_list <- list(
		performance = list(
			parallel = list(backend = "multisession", enabled = TRUE)
		),
		subgroups = list(),
		populations = list(),
		soc_config = NULL,
		pt_config = NULL,
		report_types = list(),
		plots = list(),
		tables = list(),
		validation = list()
	)
	registry <- pharmhand:::parse_config_registry(config_list)

	expect_equal(
		get_performance_setting(registry, "parallel.backend"),
		"multisession"
	)
	expect_true(get_performance_setting(registry, "parallel.enabled"))
})

test_that("get_performance_setting() handles single-level setting", {
	registry <- ConfigurationRegistry(
		performance = list(
			simple_setting = "value"
		)
	)

	value <- get_performance_setting(registry, "simple_setting")
	expect_equal(value, "value")
})

test_that("get_performance_setting() throws error for invalid registry", {
	expect_error(
		get_performance_setting("not_a_registry", "setting.name"),
		"S7::S7_inherits"
	)
})

# ============================================================================
# Priority Override Tests
# ============================================================================

test_that("Runtime override has higher priority than YAML config", {
	# Create registry with default priority
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	# Override with runtime config (priority 100)
	registry <- define_subgroup_config(
		registry,
		name = "test_sg",
		variable = "OVERRIDDEN_VAR",
		priority = 100L
	)

	overridden <- registry@subgroups[["test_sg"]]
	expect_equal(overridden@variable, "OVERRIDDEN_VAR")
	expect_equal(overridden@priority, 100L)
})

test_that("Multiple overrides maintain correct priority", {
	registry <- ConfigurationRegistry()
	registry <- define_subgroup_config(
		registry,
		name = "sg",
		variable = "V1",
		priority = 50L
	)
	registry <- define_subgroup_config(
		registry,
		name = "sg",
		variable = "V2",
		priority = 75L
	)
	registry <- define_subgroup_config(
		registry,
		name = "sg",
		variable = "V3",
		priority = 100L
	)

	config <- registry@subgroups[["sg"]]
	expect_equal(config@variable, "V3")
	expect_equal(config@priority, 100L)
})

# ============================================================================
# Edge Cases and Error Handling Tests
# ============================================================================

test_that("Configuration functions handle edge cases gracefully", {
	# Empty registry with multiple operations
	registry <- ConfigurationRegistry()

	# Add multiple items
	registry <- define_subgroup_config(registry, name = "sg1", variable = "V1")
	registry <- define_subgroup_config(registry, name = "sg2", variable = "V2")
	registry <- define_population_config(registry, name = "pop1", variable = "P1")

	# Retrieve and list
	expect_equal(list_subgroups(registry), c("sg1", "sg2"))
	expect_equal(list_populations(registry), "pop1")
	expect_true(S7::S7_inherits(
		get_subgroup_config(registry, "sg1"),
		SubgroupConfig
	))
	expect_true(S7::S7_inherits(
		get_population_config(registry, "pop1"),
		PopulationConfig
	))
})

test_that("Configuration preserves data integrity across operations", {
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	# Store original values
	original_soc <- registry@soc_config
	original_pt <- registry@pt_config
	original_subgroups <- registry@subgroups

	# Perform multiple updates
	registry <- update_soc_config(registry, sort_by = "alphabetical")
	registry <- update_pt_config(registry, sort_by = "alphabetical")

	# Verify integrity
	expect_equal(registry@soc_config@sort_by, "alphabetical")
	expect_equal(registry@pt_config@sort_by, "alphabetical")
	expect_equal(registry@subgroups, original_subgroups)
})

test_that("Custom settings are preserved through configuration operations", {
	registry <- ConfigurationRegistry(
		performance = list(
			custom = list(
				setting1 = "value1",
				setting2 = 123
			)
		)
	)

	# Add a subgroup
	registry <- define_subgroup_config(registry, name = "sg", variable = "V")

	# Verify custom settings preserved
	expect_equal(registry@performance$custom$setting1, "value1")
	expect_equal(registry@performance$custom$setting2, 123)
})

test_that("Full configuration round-trip works", {
	# Parse, modify, verify
	config_list <- create_test_config()
	registry <- pharmhand:::parse_config_registry(config_list)

	# Modify
	registry <- define_subgroup_config(
		registry,
		name = "custom",
		variable = "CUSTOM"
	)
	registry <- define_population_config(
		registry,
		name = "CUSTOM_POP",
		variable = "CUSTOMFL"
	)
	registry <- update_soc_config(registry, sort_by = "alphabetical")
	# Note: update_pt_config doesn't have show_pt_codes parameter in source
	registry <- update_pt_config(registry, sort_by = "alphabetical")

	# Verify all modifications
	expect_true("custom" %in% list_subgroups(registry))
	expect_true("CUSTOM_POP" %in% list_populations(registry))
	expect_equal(registry@soc_config@sort_by, "alphabetical")
	expect_equal(registry@pt_config@sort_by, "alphabetical")
})

# ============================================================================
# load_config() Tests (using temporary files)
# ============================================================================

test_that("load_config() can load from temporary YAML file", {
	# Create a temporary config file with explicit priority as integer
	temp_yaml <- '
subgroups:
  test_subgroup:
    variable: "TESTVAR"
    labels:
      A: "Alpha"

populations:
  test_pop:
    variable: "POPVAR"
    label: "Test Population"
    flag_value: "Y"

soc_config:
  variable: "AEBODSYS"
  sort_by: "frequency"
  min_subjects: 1

pt_config:
  variable: "AEDECOD"
  sort_by: "frequency"
  min_subjects: 1
'
	temp_file <- tempfile(fileext = ".yaml")
	writeLines(temp_yaml, temp_file)
	on.exit(unlink(temp_file))

	registry <- load_config(temp_file)

	expect_true(S7::S7_inherits(registry, ConfigurationRegistry))
	expect_true("test_subgroup" %in% list_subgroups(registry))
	expect_true("test_pop" %in% list_populations(registry))
})

test_that("load_config() throws error for non-existent file", {
	expect_error(
		load_config("non_existent_path.yaml"),
		"Configuration file not found"
	)
})

test_that("load_config() returns registry with expected structure from minimal YAML", {
	# Create a minimal temporary config file
	temp_file <- tempfile(fileext = ".yaml")
	writeLines("subgroups: {}", temp_file)
	on.exit(unlink(temp_file))

	registry <- load_config(temp_file)

	expect_true(S7::S7_inherits(registry, ConfigurationRegistry))
	expect_equal(length(registry@subgroups), 0)
})

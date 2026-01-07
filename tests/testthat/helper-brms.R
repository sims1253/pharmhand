# Helper for caching brms compiled models in tests
# This dramatically speeds up tests by reusing compiled Stan code

# Cache directory for compiled models
.brms_cache_dir <- function() {
	cache_dir <- file.path(tempdir(), "pharmhand_brms_cache")
	if (!dir.exists(cache_dir)) {
		dir.create(cache_dir, recursive = TRUE)
	}
	cache_dir
}

# Get a cached brms fit or create one
# Uses file-based caching so compiled Stan code is reused
get_cached_bayesian_result <- function(
	yi = log(c(0.78, 0.82, 0.75, 0.80, 0.77, 0.83, 0.79, 0.81)),
	sei = c(0.10, 0.12, 0.11, 0.10, 0.12, 0.11, 0.10, 0.12),
	effect_measure = "hr",
	seed = 42
) {
	# Create cache key from all parameters that affect results
	cache_key <- digest::digest(list(yi, sei, effect_measure, seed))
	cache_file <- file.path(
		.brms_cache_dir(),
		paste0("brms_meta_", cache_key, ".rds")
	)

	# Return cached result if exists
	if (file.exists(cache_file)) {
		return(readRDS(cache_file))
	}

	# Fit and cache
	result <- bayesian_meta_analysis(
		yi = yi,
		sei = sei,
		effect_measure = effect_measure,
		chains = 2,
		cores = 1,
		iter = 1000,
		warmup = 500,
		seed = seed,
		adapt_delta = 0.95
	)

	saveRDS(result, cache_file)
	result
}

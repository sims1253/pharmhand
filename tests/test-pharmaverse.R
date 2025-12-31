# Example Report Generation using Pharmaverse ADaM Data

library(pharmhand)
library(pharmaverseadam)
library(dplyr)

message("\n=== Generating Example Report with Pharmaverse Data ===")

# 1. Load standard pharmaverse ADaM datasets
data("adsl")
data("adae")

# 2. Package data into S7 ADaMData class
# This automatically handles population filtering (e.g., "ITT")
adam_sl <- ADaMData(
	data = adsl,
	domain = "ADSL",
	population = "ITT",
	subject_var = "USUBJID",
	trt_var = "TRT01P"
)

# 3. High-Performance Analysis (Vectorized dplyr logic)
message("Calculating baseline statistics...")
baseline_stats <- calculate_baseline(adam_sl, vars = c("AGE", "SEX", "RACE"))

message("Analyzing Adverse Events by SOC and PT...")
safety_results <- analyze_soc_pt(adae)

# 4. Create S7 Report Content (Flextable by default for Word)
baseline_table <- create_clinical_table(
	baseline_stats,
	"Table 1: Baseline Characteristics (ITT)"
)
safety_table <- create_clinical_table(
	safety_results,
	"Table 2: Adverse Events by System Organ Class"
)

# 5. Assemble the S7 Clinical Report
report <- ClinicalReport(
	study_id = "PHARMA-EXAMPLE-001",
	study_title = "Pharmaverse Integration Demonstration Report",
	sections = list(
		ReportSection(
			title = "Section 1: Demographics",
			content = list(baseline_table)
		),
		ReportSection(
			title = "Section 2: Safety Summary",
			content = list(safety_table)
		)
	)
)

# 6. Export to Word (.docx)
output_path <- file.path(tempdir(), "Example_Pharmaverse_Report.docx")
message("Writing report to ", output_path, "...")
write_docx(report, path = output_path)

message("Report generation complete!")

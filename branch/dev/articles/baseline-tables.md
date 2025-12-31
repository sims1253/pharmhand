# Baseline Characteristics Tables

## Introduction

Baseline tables show participant demographics, medical history, and
enrollment data at randomization.

## Setup

``` r
library(pharmhand)
library(dplyr)
library(pharmaverseadam)

# Set flextable defaults for readable tables in light/dark mode
flextable::set_flextable_defaults(
  font.color = "#000000",
  background.color = "#FFFFFF"
)

# Load example data
adsl <- pharmaverseadam::adsl
admh <- pharmaverseadam::admh
adcm <- pharmaverseadam::adcm
```

## ADaMData wrapper

Wrap ADaM datasets in `ADaMData` before creating tables.

``` r
# Wrap ADSL with population filtering
adam_data <- ADaMData(
  data = adsl,
  domain = "ADSL",
  population = "SAF"
)
```

## Demographics table

The demographics table presents basic participant characteristics
including age, sex, and ethnicity.

``` r
# Basic demographics table
demo_table <- create_demographics_table(
  adsl_data = adam_data,
  trt_var = "TRT01P"
)

# Display the table
demo_table@flextable
```

| Demographics and Baseline Characteristics       |     |      |     |        |      |      |          |             |        |       |       |     |                                  |                        |         |
|-------------------------------------------------|-----|------|-----|--------|------|------|----------|-------------|--------|-------|-------|-----|----------------------------------|------------------------|---------|
| TRT01P                                          | n   | mean | sd  | median | min  | max  | variable | layer_type  | AGEGR1 | N_tot | pct   | SEX | RACE                             | ETHNIC                 | COUNTRY |
| Placebo                                         | 86  | 75.2 | 8.6 | 76.0   | 52.0 | 89.0 | AGE      | descriptive | --     | --    | --    | --  | --                               | --                     | --      |
| Screen Failure                                  | 52  | 75.1 | 9.7 | 76.0   | 50.0 | 89.0 | AGE      | descriptive | --     | --    | --    | --  | --                               | --                     | --      |
| Xanomeline High Dose                            | 84  | 74.4 | 7.9 | 76.0   | 56.0 | 88.0 | AGE      | descriptive | --     | --    | --    | --  | --                               | --                     | --      |
| Xanomeline Low Dose                             | 84  | 75.7 | 8.3 | 77.5   | 51.0 | 88.0 | AGE      | descriptive | --     | --    | --    | --  | --                               | --                     | --      |
| Placebo                                         | 14  | --   | --  | --     | --   | --   | AGEGR1   | count       | 18-64  | 86    | 16.3  | --  | --                               | --                     | --      |
| Placebo                                         | 72  | --   | --  | --     | --   | --   | AGEGR1   | count       | \>64   | 86    | 83.7  | --  | --                               | --                     | --      |
| Screen Failure                                  | 9   | --   | --  | --     | --   | --   | AGEGR1   | count       | 18-64  | 52    | 17.3  | --  | --                               | --                     | --      |
| Screen Failure                                  | 43  | --   | --  | --     | --   | --   | AGEGR1   | count       | \>64   | 52    | 82.7  | --  | --                               | --                     | --      |
| Xanomeline High Dose                            | 11  | --   | --  | --     | --   | --   | AGEGR1   | count       | 18-64  | 84    | 13.1  | --  | --                               | --                     | --      |
| Xanomeline High Dose                            | 73  | --   | --  | --     | --   | --   | AGEGR1   | count       | \>64   | 84    | 86.9  | --  | --                               | --                     | --      |
| Xanomeline Low Dose                             | 8   | --   | --  | --     | --   | --   | AGEGR1   | count       | 18-64  | 84    | 9.5   | --  | --                               | --                     | --      |
| Xanomeline Low Dose                             | 76  | --   | --  | --     | --   | --   | AGEGR1   | count       | \>64   | 84    | 90.5  | --  | --                               | --                     | --      |
| Placebo                                         | 53  | --   | --  | --     | --   | --   | SEX      | count       | --     | 86    | 61.6  | F   | --                               | --                     | --      |
| Placebo                                         | 33  | --   | --  | --     | --   | --   | SEX      | count       | --     | 86    | 38.4  | M   | --                               | --                     | --      |
| Screen Failure                                  | 36  | --   | --  | --     | --   | --   | SEX      | count       | --     | 52    | 69.2  | F   | --                               | --                     | --      |
| Screen Failure                                  | 16  | --   | --  | --     | --   | --   | SEX      | count       | --     | 52    | 30.8  | M   | --                               | --                     | --      |
| Xanomeline High Dose                            | 40  | --   | --  | --     | --   | --   | SEX      | count       | --     | 84    | 47.6  | F   | --                               | --                     | --      |
| Xanomeline High Dose                            | 44  | --   | --  | --     | --   | --   | SEX      | count       | --     | 84    | 52.4  | M   | --                               | --                     | --      |
| Xanomeline Low Dose                             | 50  | --   | --  | --     | --   | --   | SEX      | count       | --     | 84    | 59.5  | F   | --                               | --                     | --      |
| Xanomeline Low Dose                             | 34  | --   | --  | --     | --   | --   | SEX      | count       | --     | 84    | 40.5  | M   | --                               | --                     | --      |
| Placebo                                         | 8   | --   | --  | --     | --   | --   | RACE     | count       | --     | 86    | 9.3   | --  | BLACK OR AFRICAN AMERICAN        | --                     | --      |
| Placebo                                         | 78  | --   | --  | --     | --   | --   | RACE     | count       | --     | 86    | 90.7  | --  | WHITE                            | --                     | --      |
| Screen Failure                                  | 1   | --   | --  | --     | --   | --   | RACE     | count       | --     | 52    | 1.9   | --  | AMERICAN INDIAN OR ALASKA NATIVE | --                     | --      |
| Screen Failure                                  | 2   | --   | --  | --     | --   | --   | RACE     | count       | --     | 52    | 3.8   | --  | ASIAN                            | --                     | --      |
| Screen Failure                                  | 6   | --   | --  | --     | --   | --   | RACE     | count       | --     | 52    | 11.5  | --  | BLACK OR AFRICAN AMERICAN        | --                     | --      |
| Screen Failure                                  | 43  | --   | --  | --     | --   | --   | RACE     | count       | --     | 52    | 82.7  | --  | WHITE                            | --                     | --      |
| Xanomeline High Dose                            | 1   | --   | --  | --     | --   | --   | RACE     | count       | --     | 84    | 1.2   | --  | AMERICAN INDIAN OR ALASKA NATIVE | --                     | --      |
| Xanomeline High Dose                            | 9   | --   | --  | --     | --   | --   | RACE     | count       | --     | 84    | 10.7  | --  | BLACK OR AFRICAN AMERICAN        | --                     | --      |
| Xanomeline High Dose                            | 74  | --   | --  | --     | --   | --   | RACE     | count       | --     | 84    | 88.1  | --  | WHITE                            | --                     | --      |
| Xanomeline Low Dose                             | 6   | --   | --  | --     | --   | --   | RACE     | count       | --     | 84    | 7.1   | --  | BLACK OR AFRICAN AMERICAN        | --                     | --      |
| Xanomeline Low Dose                             | 78  | --   | --  | --     | --   | --   | RACE     | count       | --     | 84    | 92.9  | --  | WHITE                            | --                     | --      |
| Placebo                                         | 3   | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 86    | 3.5   | --  | --                               | HISPANIC OR LATINO     | --      |
| Placebo                                         | 83  | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 86    | 96.5  | --  | --                               | NOT HISPANIC OR LATINO | --      |
| Screen Failure                                  | 5   | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 52    | 9.6   | --  | --                               | HISPANIC OR LATINO     | --      |
| Screen Failure                                  | 47  | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 52    | 90.4  | --  | --                               | NOT HISPANIC OR LATINO | --      |
| Xanomeline High Dose                            | 3   | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 84    | 3.6   | --  | --                               | HISPANIC OR LATINO     | --      |
| Xanomeline High Dose                            | 81  | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 84    | 96.4  | --  | --                               | NOT HISPANIC OR LATINO | --      |
| Xanomeline Low Dose                             | 6   | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 84    | 7.1   | --  | --                               | HISPANIC OR LATINO     | --      |
| Xanomeline Low Dose                             | 78  | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 84    | 92.9  | --  | --                               | NOT HISPANIC OR LATINO | --      |
| Placebo                                         | 86  | --   | --  | --     | --   | --   | COUNTRY  | count       | --     | 86    | 100.0 | --  | --                               | --                     | USA     |
| Screen Failure                                  | 52  | --   | --  | --     | --   | --   | COUNTRY  | count       | --     | 52    | 100.0 | --  | --                               | --                     | USA     |
| Xanomeline High Dose                            | 84  | --   | --  | --     | --   | --   | COUNTRY  | count       | --     | 84    | 100.0 | --  | --                               | --                     | USA     |
| Xanomeline Low Dose                             | 84  | --   | --  | --     | --   | --   | COUNTRY  | count       | --     | 84    | 100.0 | --  | --                               | --                     | USA     |
| SAF Population                                  |     |      |     |        |      |      |          |             |        |       |       |     |                                  |                        |         |
| Age summarized as n, Mean (SD), Median, Min-Max |     |      |     |        |      |      |          |             |        |       |       |     |                                  |                        |         |
| Categorical variables presented as n (%)        |     |      |     |        |      |      |          |             |        |       |       |     |                                  |                        |         |

Customize variable names to match your data structure:

``` r
# Custom variable names
demo_table_custom <- create_demographics_table(
  adsl_data = adam_data,
  trt_var = "TRT01P",
  age_var = "AGE",
  sex_var = "SEX",
  ethnic_var = "ETHNIC"
)

# Display the table
demo_table_custom@flextable
```

| Demographics and Baseline Characteristics       |     |      |     |        |      |      |          |             |        |       |       |     |                                  |                        |         |
|-------------------------------------------------|-----|------|-----|--------|------|------|----------|-------------|--------|-------|-------|-----|----------------------------------|------------------------|---------|
| TRT01P                                          | n   | mean | sd  | median | min  | max  | variable | layer_type  | AGEGR1 | N_tot | pct   | SEX | RACE                             | ETHNIC                 | COUNTRY |
| Placebo                                         | 86  | 75.2 | 8.6 | 76.0   | 52.0 | 89.0 | AGE      | descriptive | --     | --    | --    | --  | --                               | --                     | --      |
| Screen Failure                                  | 52  | 75.1 | 9.7 | 76.0   | 50.0 | 89.0 | AGE      | descriptive | --     | --    | --    | --  | --                               | --                     | --      |
| Xanomeline High Dose                            | 84  | 74.4 | 7.9 | 76.0   | 56.0 | 88.0 | AGE      | descriptive | --     | --    | --    | --  | --                               | --                     | --      |
| Xanomeline Low Dose                             | 84  | 75.7 | 8.3 | 77.5   | 51.0 | 88.0 | AGE      | descriptive | --     | --    | --    | --  | --                               | --                     | --      |
| Placebo                                         | 14  | --   | --  | --     | --   | --   | AGEGR1   | count       | 18-64  | 86    | 16.3  | --  | --                               | --                     | --      |
| Placebo                                         | 72  | --   | --  | --     | --   | --   | AGEGR1   | count       | \>64   | 86    | 83.7  | --  | --                               | --                     | --      |
| Screen Failure                                  | 9   | --   | --  | --     | --   | --   | AGEGR1   | count       | 18-64  | 52    | 17.3  | --  | --                               | --                     | --      |
| Screen Failure                                  | 43  | --   | --  | --     | --   | --   | AGEGR1   | count       | \>64   | 52    | 82.7  | --  | --                               | --                     | --      |
| Xanomeline High Dose                            | 11  | --   | --  | --     | --   | --   | AGEGR1   | count       | 18-64  | 84    | 13.1  | --  | --                               | --                     | --      |
| Xanomeline High Dose                            | 73  | --   | --  | --     | --   | --   | AGEGR1   | count       | \>64   | 84    | 86.9  | --  | --                               | --                     | --      |
| Xanomeline Low Dose                             | 8   | --   | --  | --     | --   | --   | AGEGR1   | count       | 18-64  | 84    | 9.5   | --  | --                               | --                     | --      |
| Xanomeline Low Dose                             | 76  | --   | --  | --     | --   | --   | AGEGR1   | count       | \>64   | 84    | 90.5  | --  | --                               | --                     | --      |
| Placebo                                         | 53  | --   | --  | --     | --   | --   | SEX      | count       | --     | 86    | 61.6  | F   | --                               | --                     | --      |
| Placebo                                         | 33  | --   | --  | --     | --   | --   | SEX      | count       | --     | 86    | 38.4  | M   | --                               | --                     | --      |
| Screen Failure                                  | 36  | --   | --  | --     | --   | --   | SEX      | count       | --     | 52    | 69.2  | F   | --                               | --                     | --      |
| Screen Failure                                  | 16  | --   | --  | --     | --   | --   | SEX      | count       | --     | 52    | 30.8  | M   | --                               | --                     | --      |
| Xanomeline High Dose                            | 40  | --   | --  | --     | --   | --   | SEX      | count       | --     | 84    | 47.6  | F   | --                               | --                     | --      |
| Xanomeline High Dose                            | 44  | --   | --  | --     | --   | --   | SEX      | count       | --     | 84    | 52.4  | M   | --                               | --                     | --      |
| Xanomeline Low Dose                             | 50  | --   | --  | --     | --   | --   | SEX      | count       | --     | 84    | 59.5  | F   | --                               | --                     | --      |
| Xanomeline Low Dose                             | 34  | --   | --  | --     | --   | --   | SEX      | count       | --     | 84    | 40.5  | M   | --                               | --                     | --      |
| Placebo                                         | 8   | --   | --  | --     | --   | --   | RACE     | count       | --     | 86    | 9.3   | --  | BLACK OR AFRICAN AMERICAN        | --                     | --      |
| Placebo                                         | 78  | --   | --  | --     | --   | --   | RACE     | count       | --     | 86    | 90.7  | --  | WHITE                            | --                     | --      |
| Screen Failure                                  | 1   | --   | --  | --     | --   | --   | RACE     | count       | --     | 52    | 1.9   | --  | AMERICAN INDIAN OR ALASKA NATIVE | --                     | --      |
| Screen Failure                                  | 2   | --   | --  | --     | --   | --   | RACE     | count       | --     | 52    | 3.8   | --  | ASIAN                            | --                     | --      |
| Screen Failure                                  | 6   | --   | --  | --     | --   | --   | RACE     | count       | --     | 52    | 11.5  | --  | BLACK OR AFRICAN AMERICAN        | --                     | --      |
| Screen Failure                                  | 43  | --   | --  | --     | --   | --   | RACE     | count       | --     | 52    | 82.7  | --  | WHITE                            | --                     | --      |
| Xanomeline High Dose                            | 1   | --   | --  | --     | --   | --   | RACE     | count       | --     | 84    | 1.2   | --  | AMERICAN INDIAN OR ALASKA NATIVE | --                     | --      |
| Xanomeline High Dose                            | 9   | --   | --  | --     | --   | --   | RACE     | count       | --     | 84    | 10.7  | --  | BLACK OR AFRICAN AMERICAN        | --                     | --      |
| Xanomeline High Dose                            | 74  | --   | --  | --     | --   | --   | RACE     | count       | --     | 84    | 88.1  | --  | WHITE                            | --                     | --      |
| Xanomeline Low Dose                             | 6   | --   | --  | --     | --   | --   | RACE     | count       | --     | 84    | 7.1   | --  | BLACK OR AFRICAN AMERICAN        | --                     | --      |
| Xanomeline Low Dose                             | 78  | --   | --  | --     | --   | --   | RACE     | count       | --     | 84    | 92.9  | --  | WHITE                            | --                     | --      |
| Placebo                                         | 3   | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 86    | 3.5   | --  | --                               | HISPANIC OR LATINO     | --      |
| Placebo                                         | 83  | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 86    | 96.5  | --  | --                               | NOT HISPANIC OR LATINO | --      |
| Screen Failure                                  | 5   | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 52    | 9.6   | --  | --                               | HISPANIC OR LATINO     | --      |
| Screen Failure                                  | 47  | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 52    | 90.4  | --  | --                               | NOT HISPANIC OR LATINO | --      |
| Xanomeline High Dose                            | 3   | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 84    | 3.6   | --  | --                               | HISPANIC OR LATINO     | --      |
| Xanomeline High Dose                            | 81  | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 84    | 96.4  | --  | --                               | NOT HISPANIC OR LATINO | --      |
| Xanomeline Low Dose                             | 6   | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 84    | 7.1   | --  | --                               | HISPANIC OR LATINO     | --      |
| Xanomeline Low Dose                             | 78  | --   | --  | --     | --   | --   | ETHNIC   | count       | --     | 84    | 92.9  | --  | --                               | NOT HISPANIC OR LATINO | --      |
| Placebo                                         | 86  | --   | --  | --     | --   | --   | COUNTRY  | count       | --     | 86    | 100.0 | --  | --                               | --                     | USA     |
| Screen Failure                                  | 52  | --   | --  | --     | --   | --   | COUNTRY  | count       | --     | 52    | 100.0 | --  | --                               | --                     | USA     |
| Xanomeline High Dose                            | 84  | --   | --  | --     | --   | --   | COUNTRY  | count       | --     | 84    | 100.0 | --  | --                               | --                     | USA     |
| Xanomeline Low Dose                             | 84  | --   | --  | --     | --   | --   | COUNTRY  | count       | --     | 84    | 100.0 | --  | --                               | --                     | USA     |
| SAF Population                                  |     |      |     |        |      |      |          |             |        |       |       |     |                                  |                        |         |
| Age summarized as n, Mean (SD), Median, Min-Max |     |      |     |        |      |      |          |             |        |       |       |     |                                  |                        |         |
| Categorical variables presented as n (%)        |     |      |     |        |      |      |          |             |        |       |       |     |                                  |                        |         |

## Enrollment by region

``` r
region_table <- create_region_table(
  adsl = adsl,
  trt_var = "TRT01P",
  region_var = "REGION1"
)

# Display the table
region_table@flextable
```

| Enrollment by Region                    |                     |     |       |       |          |            |
|-----------------------------------------|---------------------|-----|-------|-------|----------|------------|
| Planned Treatment for Period 01         | Geographic Region 1 | n   | N_tot | pct   | variable | layer_type |
| Placebo                                 | NA                  | 86  | 86    | 100.0 | REGION1  | count      |
| Screen Failure                          | NA                  | 52  | 52    | 100.0 | REGION1  | count      |
| Xanomeline High Dose                    | NA                  | 84  | 84    | 100.0 | REGION1  | count      |
| Xanomeline Low Dose                     | NA                  | 84  | 84    | 100.0 | REGION1  | count      |
| Safety Population                       |                     |     |       |       |          |            |
| n (%) = Number (percentage) of subjects |                     |     |       |       |          |            |

## Medical history

Click to expand: Medical History Table

``` r
mh_table <- create_medical_history_table(
  adsl = adsl,
  admh = admh,
  trt_var = "TRT01P",
  soc_var = "MHBODSYS"
)

# Display the table
mh_table@flextable
```

| Medical History by Body System                                      |            |                      |                     |
|---------------------------------------------------------------------|------------|----------------------|---------------------|
| Body System or Organ Class                                          | Placebo    | Xanomeline High Dose | Xanomeline Low Dose |
| BLOOD AND LYMPHATIC SYSTEM DISORDERS                                | 2 (2.3%)   | 4 (4.8%)             | 1 (1.2%)            |
| CARDIAC DISORDERS                                                   | 17 (19.8%) | 17 (20.2%)           | 23 (27.4%)          |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS                          | 3 (3.5%)   | 3 (3.6%)             | 0 (0.0%)            |
| EAR AND LABYRINTH DISORDERS                                         | 16 (18.6%) | 22 (26.2%)           | 12 (14.3%)          |
| ENDOCRINE DISORDERS                                                 | 7 (8.1%)   | 8 (9.5%)             | 10 (11.9%)          |
| EYE DISORDERS                                                       | 26 (30.2%) | 28 (33.3%)           | 26 (31%)            |
| GASTROINTESTINAL DISORDERS                                          | 29 (33.7%) | 28 (33.3%)           | 29 (34.5%)          |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | 10 (11.6%) | 7 (8.3%)             | 12 (14.3%)          |
| HEPATOBILIARY DISORDERS                                             | 2 (2.3%)   | 3 (3.6%)             | 2 (2.4%)            |
| IMMUNE SYSTEM DISORDERS                                             | 3 (3.5%)   | 4 (4.8%)             | 4 (4.8%)            |
| INFECTIONS AND INFESTATIONS                                         | 16 (18.6%) | 14 (16.7%)           | 15 (17.9%)          |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS                      | 16 (18.6%) | 13 (15.5%)           | 14 (16.7%)          |
| INVESTIGATIONS                                                      | 7 (8.1%)   | 20 (23.8%)           | 12 (14.3%)          |
| METABOLISM AND NUTRITION DISORDERS                                  | 12 (14%)   | 16 (19%)             | 8 (9.5%)            |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS                     | 37 (43%)   | 40 (47.6%)           | 41 (48.8%)          |
| NEOPLASMS BENIGN, MALIGNANT AND UNSPECIFIED (INCL CYSTS AND POLYPS) | 13 (15.1%) | 8 (9.5%)             | 10 (11.9%)          |
| NERVOUS SYSTEM DISORDERS                                            | 20 (23.3%) | 24 (28.6%)           | 17 (20.2%)          |
| PREGNANCY, PUERPERIUM AND PERINATAL CONDITIONS                      | 0 (0.0%)   | 0 (0.0%)             | 1 (1.2%)            |
| PSYCHIATRIC DISORDERS                                               | 6 (7%)     | 10 (11.9%)           | 6 (7.1%)            |
| RENAL AND URINARY DISORDERS                                         | 5 (5.8%)   | 9 (10.7%)            | 10 (11.9%)          |
| REPRODUCTIVE SYSTEM AND BREAST DISORDERS                            | 7 (8.1%)   | 8 (9.5%)             | 10 (11.9%)          |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | 15 (17.4%) | 7 (8.3%)             | 12 (14.3%)          |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | 11 (12.8%) | 9 (10.7%)            | 12 (14.3%)          |
| SOCIAL CIRCUMSTANCES                                                | 7 (8.1%)   | 9 (10.7%)            | 8 (9.5%)            |
| SURGICAL AND MEDICAL PROCEDURES                                     | 45 (52.3%) | 55 (65.5%)           | 59 (70.2%)          |
| VASCULAR DISORDERS                                                  | 22 (25.6%) | 27 (32.1%)           | 20 (23.8%)          |
| --                                                                  | 86 (100%)  | 84 (100%)            | 84 (100%)           |
| ITT Population                                                      |            |                      |                     |
| n (%) = Number (percentage) of subjects with at least one condition |            |                      |                     |

## Concomitant medications

Click to expand: Concomitant Medications Table

``` r
cm_table <- create_conmeds_table(
  adsl = adsl,
  adcm = adcm,
  trt_var = "TRT01P"
)

# Display the table
cm_table@flextable
```

| Prior and Concomitant Medications by Class                             |            |                      |                     |
|------------------------------------------------------------------------|------------|----------------------|---------------------|
| Medication Class                                                       | Placebo    | Xanomeline High Dose | Xanomeline Low Dose |
| ALIMENTARY TRACT AND METABOLISM                                        | 12 (14%)   | 9 (10.7%)            | 11 (13.1%)          |
| ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS                             | 1 (1.2%)   | 1 (1.2%)             | 0 (0.0%)            |
| BLOOD AND BLOOD FORMING ORGANS                                         | 0 (0.0%)   | 0 (0.0%)             | 1 (1.2%)            |
| CARDIOVASCULAR SYSTEM                                                  | 12 (14%)   | 7 (8.3%)             | 12 (14.3%)          |
| DERMATOLOGICALS                                                        | 0 (0.0%)   | 1 (1.2%)             | 0 (0.0%)            |
| GENITO URINARY SYSTEM AND SEX HORMONES                                 | 6 (7%)     | 5 (6%)               | 10 (11.9%)          |
| NERVOUS SYSTEM                                                         | 23 (26.7%) | 8 (9.5%)             | 14 (16.7%)          |
| RESPIRATORY SYSTEM                                                     | 4 (4.7%)   | 4 (4.8%)             | 1 (1.2%)            |
| SYSTEMIC HORMONAL PREPARATIONS, EXCL.                                  | 2 (2.3%)   | 8 (9.5%)             | 13 (15.5%)          |
| UNCODED                                                                | 74 (86%)   | 77 (91.7%)           | 70 (83.3%)          |
| ITT Population                                                         |            |                      |                     |
| n (%) = Number (percentage) of subjects taking at least one medication |            |                      |                     |

## Disposition

Disposition tables summarize participant flow through study phases and
reasons for discontinuation.

``` r
disp_table <- create_disposition_table(
  adsl = adsl,
  trt_var = "TRT01P"
)

# Display the table
disp_table@flextable
```

| Subject Disposition |         |                |                      |                     |
|---------------------|---------|----------------|----------------------|---------------------|
| End of Study Status | Placebo | Screen Failure | Xanomeline High Dose | Xanomeline Low Dose |
| COMPLETED           | 58      | 0              | 27                   | 25                  |
| DISCONTINUED        | 28      | 0              | 57                   | 59                  |
| --                  | 0       | 52             | 0                    | 0                   |
| ITT Population      |         |                |                      |                     |

## Population summary

Population summary tables provide counts and percentages for different
analysis populations.

``` r
pop_table <- create_population_summary_table(
  adsl = adsl,
  trt_var = "TRT01P",
  pop_flags = c("SAFFL"),
  pop_labels = c("Safety")
)

# Display the table
pop_table@flextable
```

| Analysis Populations                                          |         |                      |                     |
|---------------------------------------------------------------|---------|----------------------|---------------------|
| Population                                                    | Placebo | Xanomeline High Dose | Xanomeline Low Dose |
| Safety                                                        | 86      | 84                   | 84                  |
| ITT = Intent-To-Treat Population                              |         |                      |                     |
| Safety = Safety Population (subjects who received study drug) |         |                      |                     |

## Baseline Balance Assessment (SMD)

For GBA/AMNOG dossiers and other regulatory submissions, assessing
baseline balance between treatment groups is critical. The standardized
mean difference (SMD) provides a standardized measure of covariate
balance that is independent of sample size.

### Calculating SMD for individual variables

``` r
# Calculate SMD for a continuous variable
smd_age <- calculate_smd_from_data(
  data = adsl,
  var = "AGE",
  trt_var = "TRT01P",
  ref_group = "Placebo"
)

# View SMD result
cat("Age SMD:", round(smd_age$smd, 3),
    "95% CI: (", round(smd_age$ci_lower, 3), ",",
    round(smd_age$ci_upper, 3), ")\n")
#> Age SMD: -0.1 95% CI: ( -0.401 , 0.2 )

# Calculate SMD for a categorical variable
smd_sex <- calculate_smd_from_data(
  data = adsl,
  var = "SEX",
  trt_var = "TRT01P",
  ref_group = "Placebo"
)

cat("Sex SMD:", round(smd_sex$smd, 3), "\n")
#> Sex SMD: 0.282
```

### Comprehensive balance assessment

Use
[`assess_baseline_balance()`](https://sims1253.github.io/pharmhand/branch/dev/reference/assess_baseline_balance.md)
for a complete assessment of multiple variables:

``` r
# Assess balance for continuous and categorical variables
balance <- assess_baseline_balance(
  data = adsl,
  trt_var = "TRT01P",
  continuous_vars = c("AGE", "WEIGHTBL"),
  categorical_vars = c("SEX", "RACE", "ETHNIC"),
  ref_group = "Placebo",
  threshold = 0.1  # Standard threshold for imbalance
)

# Check overall balance
cat("Number of variables assessed:", balance@n_vars, "\n")
#> Number of variables assessed: 4
cat("Number of imbalanced variables (|SMD| > 0.1):", balance@n_imbalanced, "\n")
#> Number of imbalanced variables (|SMD| > 0.1): 2
cat("Overall balanced:", balance@balanced, "\n")
#> Overall balanced: FALSE

# View imbalanced variables (if any)
if (length(balance@imbalanced_vars) > 0) {
  imbalanced <- paste(balance@imbalanced_vars, collapse = ", ")
  cat("Imbalanced variables:", imbalanced, "\n")
}
#> Imbalanced variables: AGE, SEX
```

### SMD table for demographics

Add SMD values directly to your demographics table data:

``` r
# Generate SMD table
smd_results <- add_smd_to_table(
  data = adsl,
  trt_var = "TRT01P",
  vars = c("AGE", "SEX", "RACE", "ETHNIC"),
  ref_group = "Placebo",
  threshold = 0.1
)

# View the results
smd_results |>
  dplyr::select(variable, smd_display, ci, var_type, imbalanced)
#>   variable smd_display              ci    var_type imbalanced
#> 1      AGE     -0.100* (-0.401, 0.200)  continuous       TRUE
#> 2      SEX      0.282* (-0.018, 0.583) categorical       TRUE
#> 3     RACE      -0.085 (-0.385, 0.216) categorical      FALSE
#> 4   ETHNIC      -0.005 (-0.305, 0.296) categorical      FALSE
```

### Love Plot visualization

A Love plot (covariate balance plot) provides a visual summary of
balance across all covariates:

``` r
# Create Love plot from balance assessment
love_plot <- create_love_plot(
  balance_assessment = balance,
  threshold = 0.1,
  title = "Baseline Covariate Balance",
  show_ci = TRUE
)

# Display the plot
love_plot@plot
```

![](baseline-tables_files/figure-html/love-plot-1.png)

Variables with \|SMD\| \> 0.1 (outside the dashed lines) may indicate
meaningful imbalance that should be discussed or adjusted for in
sensitivity analyses.

## Combining into a report

Combine baseline tables into a report.

``` r
# Create report sections
demo_section <- ReportSection(
  title = "Demographics",
  content = demo_table
)

region_section <- ReportSection(
  title = "Enrollment by Region",
  content = region_table
)

mh_section <- ReportSection(
  title = "Medical History",
  content = mh_table
)

# cm_section <- ReportSection(
#   title = "Concomitant Medications",
#   content = cm_table
# )

# Create clinical report
report <- ClinicalReport(
  title = "Baseline Characteristics",
  sections = list(
    demo_section,
    region_section,
    mh_section
  )
)

# Generate Word document
generate_word(report, path = tempfile(fileext = ".docx"))
```

The Word document contains formatted baseline tables.

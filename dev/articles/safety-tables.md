# Creating Safety Tables

## Introduction

[`create_ae_summary_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_ae_summary_table.md)
makes safety tables for treatment-emergent adverse events (TEAEs).

## Quick Safety Report

Generate a complete safety report in one call:

``` r
# One-line safety report with multiple tables
quick_safety_report(
  adae = adae,
  adsl = adsl,
  output = "safety_report.docx",
  title = "Safety Analysis",
  include_overview = TRUE,   # AE overview table
  include_soc = TRUE,         # SOC table
  include_soc_pt = FALSE,     # Skip SOC/PT (large)
  include_sae = TRUE          # SAE table
)
```

The report automatically includes: - Overview of TEAEs, related AEs,
SAEs, discontinuations - AEs by System Organ Class - Serious Adverse
Events - Formatted Word document with proper styling

For detailed control of individual tables, use the functions below.

## Table Types

| Type              | Description                                           | Key Variables                        |
|-------------------|-------------------------------------------------------|--------------------------------------|
| `overview`        | Summary of TEAEs, related AEs, SAEs, discontinuations | `TRTEMFL`, `AEREL`, `AESER`, `AEACN` |
| `soc`             | AEs by System Organ Class                             | `AEBODSYS`                           |
| `soc_pt`          | AEs by SOC and Preferred Term (hierarchical)          | `AEBODSYS`, `AEDECOD`                |
| `pt`              | AEs by Preferred Term only                            | `AEDECOD`                            |
| `common`          | Most frequently reported AEs                          | `AEDECOD`                            |
| `severity`        | AEs by maximum severity                               | `AESEV`                              |
| `relationship`    | AEs by relationship to study drug                     | `AEREL`                              |
| `sae`             | Serious Adverse Events                                | `AESER`                              |
| `discontinuation` | AEs leading to discontinuation                        | `AEACN`                              |
| `deaths`          | Deaths summary                                        | `DTHFL`                              |
| `comparison`      | AE comparison with RD/RR                              | `TRTEMFL`, `AEBODSYS`, `AEDECOD`     |

## Setup

``` r
library(pharmhand)
library(dplyr)
library(tidyr)

# Set flextable defaults for readable tables in light/dark mode
flextable::set_flextable_defaults(
  font.color = "#000000",
  background.color = "#FFFFFF"
)

# Load example data from pharmaverseadam
adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae
adlb <- pharmaverseadam::adlb
```

## AE Overview Tables

``` r
# Create AE overview table
overview_table <- create_ae_summary_table(
  adae = adae,
  adsl = adsl,
  type = "overview",
  title = "Table 1: Overview of Treatment-Emergent Adverse Events"
)

# Display the table
overview_table@flextable
```

| Table 1: Overview of Treatment-Emergent Adverse Events |            |                      |                     |
|--------------------------------------------------------|------------|----------------------|---------------------|
| Category                                               | Placebo    | Xanomeline High Dose | Xanomeline Low Dose |
| Subjects with at least one TEAE                        | 65 (75.6%) | 75 (89.3%)           | 77 (91.7%)          |
| Subjects with at least one related TEAE                | 43 (50.0%) | 69 (82.1%)           | 72 (85.7%)          |
| Subjects with at least one SAE                         | 0 (0.0%)   | 2 (2.4%)             | 1 (1.2%)            |
| Deaths                                                 | 2 (2.3%)   | 0 (0.0%)             | 1 (1.2%)            |

The overview table calculates: - Subjects with ≥1 TEAE
(`TRTEMFL == "Y"`) - Related TEAEs (`AEREL` in \[“RELATED”, “PROBABLE”,
“POSSIBLE”\]) - Serious AEs (`AESER == "Y"`) - AEs leading to
discontinuation (`AEACN == "DRUG WITHDRAWN"`) - Deaths
(`AEOUT == "FATAL"`)

## AE by SOC Tables

``` r
# Create SOC table
soc_table <- create_ae_summary_table(
  adae = adae,
  adsl = adsl,
  type = "soc",
  title = "Table 2: Treatment-Emergent Adverse Events by System Organ Class"
)

# Display the table
soc_table@flextable
```

| Table 2: Treatment-Emergent Adverse Events by System Organ Class    |            |                      |                     |
|---------------------------------------------------------------------|------------|----------------------|---------------------|
| Body System or Organ Class                                          | Placebo    | Xanomeline High Dose | Xanomeline Low Dose |
| CARDIAC DISORDERS                                                   | 12 (14%)   | 15 (17.9%)           | 13 (15.5%)          |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS                          | 0 (0.0%)   | 2 (2.4%)             | 1 (1.2%)            |
| EAR AND LABYRINTH DISORDERS                                         | 1 (1.2%)   | 1 (1.2%)             | 2 (2.4%)            |
| EYE DISORDERS                                                       | 2 (2.3%)   | 1 (1.2%)             | 2 (2.4%)            |
| GASTROINTESTINAL DISORDERS                                          | 17 (19.8%) | 20 (23.8%)           | 14 (16.7%)          |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | 21 (24.4%) | 40 (47.6%)           | 47 (56%)            |
| HEPATOBILIARY DISORDERS                                             | 1 (1.2%)   | 0 (0.0%)             | 0 (0.0%)            |
| IMMUNE SYSTEM DISORDERS                                             | 0 (0.0%)   | 0 (0.0%)             | 1 (1.2%)            |
| INFECTIONS AND INFESTATIONS                                         | 16 (18.6%) | 13 (15.5%)           | 9 (10.7%)           |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS                      | 4 (4.7%)   | 5 (6%)               | 5 (6%)              |
| INVESTIGATIONS                                                      | 10 (11.6%) | 6 (7.1%)             | 6 (7.1%)            |
| METABOLISM AND NUTRITION DISORDERS                                  | 6 (7%)     | 2 (2.4%)             | 1 (1.2%)            |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS                     | 4 (4.7%)   | 7 (8.3%)             | 7 (8.3%)            |
| NEOPLASMS BENIGN, MALIGNANT AND UNSPECIFIED (INCL CYSTS AND POLYPS) | 0 (0.0%)   | 1 (1.2%)             | 2 (2.4%)            |
| NERVOUS SYSTEM DISORDERS                                            | 8 (9.3%)   | 25 (29.8%)           | 20 (23.8%)          |
| PSYCHIATRIC DISORDERS                                               | 10 (11.6%) | 8 (9.5%)             | 10 (11.9%)          |
| RENAL AND URINARY DISORDERS                                         | 4 (4.7%)   | 3 (3.6%)             | 3 (3.6%)            |
| REPRODUCTIVE SYSTEM AND BREAST DISORDERS                            | 2 (2.3%)   | 1 (1.2%)             | 0 (0.0%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | 8 (9.3%)   | 10 (11.9%)           | 9 (10.7%)           |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | 20 (23.3%) | 39 (46.4%)           | 39 (46.4%)          |
| SOCIAL CIRCUMSTANCES                                                | 0 (0.0%)   | 1 (1.2%)             | 0 (0.0%)            |
| SURGICAL AND MEDICAL PROCEDURES                                     | 2 (2.3%)   | 2 (2.4%)             | 1 (1.2%)            |
| VASCULAR DISORDERS                                                  | 3 (3.5%)   | 1 (1.2%)             | 3 (3.6%)            |
| Safety Population                                                   |            |                      |                     |
| n (%) = Number (percentage) of subjects with at least one event     |            |                      |                     |

### SOC/PT Analysis

Click to expand: SOC/PT Hierarchical Table

``` r
# Create hierarchical SOC/PT table
soc_pt_table <- create_ae_summary_table(
  adae = adae,
  adsl = adsl,
  type = "soc_pt",
  title = "Table 3: TEAEs by System Organ Class and Preferred Term"
)

# Display the table
soc_pt_table@flextable
```

| Table 3: TEAEs by System Organ Class and Preferred Term             |                                                |           |                      |                     |
|---------------------------------------------------------------------|------------------------------------------------|-----------|----------------------|---------------------|
| Body System or Organ Class                                          | Dictionary-Derived Term                        | Placebo   | Xanomeline High Dose | Xanomeline Low Dose |
| CARDIAC DISORDERS                                                   | ATRIAL FIBRILLATION                            | 1 (1.2%)  | 3 (3.6%)             | 1 (1.2%)            |
| CARDIAC DISORDERS                                                   | ATRIAL FLUTTER                                 | 0 (0.0%)  | 1 (1.2%)             | 1 (1.2%)            |
| CARDIAC DISORDERS                                                   | ATRIAL HYPERTROPHY                             | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| CARDIAC DISORDERS                                                   | ATRIOVENTRICULAR BLOCK FIRST DEGREE            | 1 (1.2%)  | 0 (0.0%)             | 1 (1.2%)            |
| CARDIAC DISORDERS                                                   | ATRIOVENTRICULAR BLOCK SECOND DEGREE           | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| CARDIAC DISORDERS                                                   | BRADYCARDIA                                    | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| CARDIAC DISORDERS                                                   | BUNDLE BRANCH BLOCK LEFT                       | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| CARDIAC DISORDERS                                                   | BUNDLE BRANCH BLOCK RIGHT                      | 1 (1.2%)  | 0 (0.0%)             | 1 (1.2%)            |
| CARDIAC DISORDERS                                                   | CARDIAC DISORDER                               | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| CARDIAC DISORDERS                                                   | CARDIAC FAILURE CONGESTIVE                     | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| CARDIAC DISORDERS                                                   | MYOCARDIAL INFARCTION                          | 4 (4.7%)  | 4 (4.8%)             | 2 (2.4%)            |
| CARDIAC DISORDERS                                                   | PALPITATIONS                                   | 0 (0.0%)  | 0 (0.0%)             | 2 (2.4%)            |
| CARDIAC DISORDERS                                                   | SINUS ARRHYTHMIA                               | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| CARDIAC DISORDERS                                                   | SINUS BRADYCARDIA                              | 2 (2.3%)  | 8 (9.5%)             | 7 (8.3%)            |
| CARDIAC DISORDERS                                                   | SUPRAVENTRICULAR EXTRASYSTOLES                 | 1 (1.2%)  | 1 (1.2%)             | 1 (1.2%)            |
| CARDIAC DISORDERS                                                   | SUPRAVENTRICULAR TACHYCARDIA                   | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| CARDIAC DISORDERS                                                   | TACHYCARDIA                                    | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| CARDIAC DISORDERS                                                   | VENTRICULAR EXTRASYSTOLES                      | 0 (0.0%)  | 1 (1.2%)             | 2 (2.4%)            |
| CARDIAC DISORDERS                                                   | VENTRICULAR HYPERTROPHY                        | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| CARDIAC DISORDERS                                                   | WOLFF-PARKINSON-WHITE SYNDROME                 | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS                          | VENTRICULAR SEPTAL DEFECT                      | 0 (0.0%)  | 2 (2.4%)             | 1 (1.2%)            |
| EAR AND LABYRINTH DISORDERS                                         | CERUMEN IMPACTION                              | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| EAR AND LABYRINTH DISORDERS                                         | EAR PAIN                                       | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| EAR AND LABYRINTH DISORDERS                                         | VERTIGO                                        | 0 (0.0%)  | 1 (1.2%)             | 1 (1.2%)            |
| EYE DISORDERS                                                       | CONJUNCTIVAL HAEMORRHAGE                       | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| EYE DISORDERS                                                       | CONJUNCTIVITIS                                 | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| EYE DISORDERS                                                       | EYE ALLERGY                                    | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| EYE DISORDERS                                                       | EYE PRURITUS                                   | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| EYE DISORDERS                                                       | EYE SWELLING                                   | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| EYE DISORDERS                                                       | VISION BLURRED                                 | 0 (0.0%)  | 1 (1.2%)             | 1 (1.2%)            |
| GASTROINTESTINAL DISORDERS                                          | ABDOMINAL DISCOMFORT                           | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| GASTROINTESTINAL DISORDERS                                          | ABDOMINAL PAIN                                 | 1 (1.2%)  | 1 (1.2%)             | 3 (3.6%)            |
| GASTROINTESTINAL DISORDERS                                          | CONSTIPATION                                   | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| GASTROINTESTINAL DISORDERS                                          | DIARRHOEA                                      | 9 (10.5%) | 4 (4.8%)             | 4 (4.8%)            |
| GASTROINTESTINAL DISORDERS                                          | DYSPEPSIA                                      | 1 (1.2%)  | 0 (0.0%)             | 1 (1.2%)            |
| GASTROINTESTINAL DISORDERS                                          | DYSPHAGIA                                      | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| GASTROINTESTINAL DISORDERS                                          | FLATULENCE                                     | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| GASTROINTESTINAL DISORDERS                                          | GASTROINTESTINAL HAEMORRHAGE                   | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| GASTROINTESTINAL DISORDERS                                          | GASTROOESOPHAGEAL REFLUX DISEASE               | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| GASTROINTESTINAL DISORDERS                                          | GLOSSITIS                                      | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| GASTROINTESTINAL DISORDERS                                          | HIATUS HERNIA                                  | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| GASTROINTESTINAL DISORDERS                                          | NAUSEA                                         | 3 (3.5%)  | 6 (7.1%)             | 3 (3.6%)            |
| GASTROINTESTINAL DISORDERS                                          | RECTAL HAEMORRHAGE                             | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| GASTROINTESTINAL DISORDERS                                          | SALIVARY HYPERSECRETION                        | 0 (0.0%)  | 4 (4.8%)             | 0 (0.0%)            |
| GASTROINTESTINAL DISORDERS                                          | STOMACH DISCOMFORT                             | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| GASTROINTESTINAL DISORDERS                                          | VOMITING                                       | 3 (3.5%)  | 7 (8.3%)             | 3 (3.6%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE BLEEDING                      | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE DERMATITIS                    | 5 (5.8%)  | 7 (8.3%)             | 9 (10.7%)           |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE DESQUAMATION                  | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE DISCHARGE                     | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE DISCOLOURATION                | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE ERYTHEMA                      | 3 (3.5%)  | 15 (17.9%)           | 12 (14.3%)          |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE INDURATION                    | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE IRRITATION                    | 3 (3.5%)  | 9 (10.7%)            | 9 (10.7%)           |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE PAIN                          | 0 (0.0%)  | 2 (2.4%)             | 0 (0.0%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE PERSPIRATION                  | 0 (0.0%)  | 2 (2.4%)             | 0 (0.0%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE PRURITUS                      | 6 (7%)    | 22 (26.2%)           | 22 (26.2%)          |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE REACTION                      | 1 (1.2%)  | 1 (1.2%)             | 0 (0.0%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE SWELLING                      | 0 (0.0%)  | 2 (2.4%)             | 1 (1.2%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE URTICARIA                     | 0 (0.0%)  | 1 (1.2%)             | 2 (2.4%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE VESICLES                      | 1 (1.2%)  | 6 (7.1%)             | 4 (4.8%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | APPLICATION SITE WARMTH                        | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | ASTHENIA                                       | 1 (1.2%)  | 1 (1.2%)             | 0 (0.0%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | CHEST DISCOMFORT                               | 0 (0.0%)  | 2 (2.4%)             | 0 (0.0%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | CHEST PAIN                                     | 0 (0.0%)  | 2 (2.4%)             | 0 (0.0%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | CHILLS                                         | 1 (1.2%)  | 1 (1.2%)             | 1 (1.2%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | FATIGUE                                        | 1 (1.2%)  | 5 (6%)               | 5 (6%)              |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | FEELING ABNORMAL                               | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | FEELING COLD                                   | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | INFLAMMATION                                   | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | MALAISE                                        | 0 (0.0%)  | 2 (2.4%)             | 1 (1.2%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | OEDEMA                                         | 0 (0.0%)  | 0 (0.0%)             | 2 (2.4%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | OEDEMA PERIPHERAL                              | 2 (2.3%)  | 2 (2.4%)             | 1 (1.2%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | PAIN                                           | 0 (0.0%)  | 1 (1.2%)             | 1 (1.2%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | PYREXIA                                        | 2 (2.3%)  | 1 (1.2%)             | 0 (0.0%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | SECRETION DISCHARGE                            | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | SUDDEN DEATH                                   | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | SWELLING                                       | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | ULCER                                          | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| HEPATOBILIARY DISORDERS                                             | HYPERBILIRUBINAEMIA                            | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| IMMUNE SYSTEM DISORDERS                                             | HYPERSENSITIVITY                               | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| INFECTIONS AND INFESTATIONS                                         | BRONCHITIS                                     | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| INFECTIONS AND INFESTATIONS                                         | CELLULITIS                                     | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| INFECTIONS AND INFESTATIONS                                         | CERVICITIS                                     | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| INFECTIONS AND INFESTATIONS                                         | CYSTITIS                                       | 1 (1.2%)  | 1 (1.2%)             | 0 (0.0%)            |
| INFECTIONS AND INFESTATIONS                                         | EAR INFECTION                                  | 2 (2.3%)  | 0 (0.0%)             | 0 (0.0%)            |
| INFECTIONS AND INFESTATIONS                                         | GASTROENTERITIS VIRAL                          | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| INFECTIONS AND INFESTATIONS                                         | HORDEOLUM                                      | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| INFECTIONS AND INFESTATIONS                                         | INFLUENZA                                      | 1 (1.2%)  | 1 (1.2%)             | 1 (1.2%)            |
| INFECTIONS AND INFESTATIONS                                         | LOCALISED INFECTION                            | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| INFECTIONS AND INFESTATIONS                                         | LOWER RESPIRATORY TRACT INFECTION              | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| INFECTIONS AND INFESTATIONS                                         | NASOPHARYNGITIS                                | 2 (2.3%)  | 6 (7.1%)             | 4 (4.8%)            |
| INFECTIONS AND INFESTATIONS                                         | PNEUMONIA                                      | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| INFECTIONS AND INFESTATIONS                                         | RHINITIS                                       | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| INFECTIONS AND INFESTATIONS                                         | UPPER RESPIRATORY TRACT INFECTION              | 6 (7%)    | 3 (3.6%)             | 1 (1.2%)            |
| INFECTIONS AND INFESTATIONS                                         | URINARY TRACT INFECTION                        | 2 (2.3%)  | 1 (1.2%)             | 0 (0.0%)            |
| INFECTIONS AND INFESTATIONS                                         | VAGINAL MYCOSIS                                | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| INFECTIONS AND INFESTATIONS                                         | VIRAL INFECTION                                | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS                      | CONTUSION                                      | 1 (1.2%)  | 2 (2.4%)             | 1 (1.2%)            |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS                      | EXCORIATION                                    | 2 (2.3%)  | 1 (1.2%)             | 1 (1.2%)            |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS                      | FACIAL BONES FRACTURE                          | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS                      | FALL                                           | 1 (1.2%)  | 1 (1.2%)             | 2 (2.4%)            |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS                      | HIP FRACTURE                                   | 1 (1.2%)  | 2 (2.4%)             | 0 (0.0%)            |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS                      | JOINT DISLOCATION                              | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS                      | SKIN LACERATION                                | 1 (1.2%)  | 0 (0.0%)             | 2 (2.4%)            |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS                      | WOUND                                          | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| INVESTIGATIONS                                                      | BIOPSY                                         | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| INVESTIGATIONS                                                      | BIOPSY PROSTATE                                | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| INVESTIGATIONS                                                      | BLOOD ALKALINE PHOSPHATASE INCREASED           | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| INVESTIGATIONS                                                      | BLOOD CHOLESTEROL INCREASED                    | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| INVESTIGATIONS                                                      | BLOOD CREATINE PHOSPHOKINASE INCREASED         | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| INVESTIGATIONS                                                      | BLOOD GLUCOSE INCREASED                        | 0 (0.0%)  | 1 (1.2%)             | 1 (1.2%)            |
| INVESTIGATIONS                                                      | BLOOD URINE PRESENT                            | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| INVESTIGATIONS                                                      | BODY TEMPERATURE INCREASED                     | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| INVESTIGATIONS                                                      | CYSTOSCOPY                                     | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| INVESTIGATIONS                                                      | ELECTROCARDIOGRAM ST SEGMENT DEPRESSION        | 4 (4.7%)  | 0 (0.0%)             | 1 (1.2%)            |
| INVESTIGATIONS                                                      | ELECTROCARDIOGRAM T WAVE AMPLITUDE DECREASED   | 1 (1.2%)  | 0 (0.0%)             | 1 (1.2%)            |
| INVESTIGATIONS                                                      | ELECTROCARDIOGRAM T WAVE INVERSION             | 2 (2.3%)  | 1 (1.2%)             | 1 (1.2%)            |
| INVESTIGATIONS                                                      | HEART RATE INCREASED                           | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| INVESTIGATIONS                                                      | HEART RATE IRREGULAR                           | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| INVESTIGATIONS                                                      | NASAL MUCOSA BIOPSY                            | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| INVESTIGATIONS                                                      | WEIGHT DECREASED                               | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| METABOLISM AND NUTRITION DISORDERS                                  | DECREASED APPETITE                             | 1 (1.2%)  | 1 (1.2%)             | 0 (0.0%)            |
| METABOLISM AND NUTRITION DISORDERS                                  | DEHYDRATION                                    | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| METABOLISM AND NUTRITION DISORDERS                                  | DIABETES MELLITUS                              | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| METABOLISM AND NUTRITION DISORDERS                                  | FOOD CRAVING                                   | 1 (1.2%)  | 0 (0.0%)             | 1 (1.2%)            |
| METABOLISM AND NUTRITION DISORDERS                                  | HYPONATRAEMIA                                  | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| METABOLISM AND NUTRITION DISORDERS                                  | INCREASED APPETITE                             | 1 (1.2%)  | 1 (1.2%)             | 0 (0.0%)            |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS                     | ARTHRALGIA                                     | 1 (1.2%)  | 1 (1.2%)             | 2 (2.4%)            |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS                     | ARTHRITIS                                      | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS                     | BACK PAIN                                      | 1 (1.2%)  | 3 (3.6%)             | 1 (1.2%)            |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS                     | FLANK PAIN                                     | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS                     | MUSCLE SPASMS                                  | 0 (0.0%)  | 1 (1.2%)             | 1 (1.2%)            |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS                     | MUSCULAR WEAKNESS                              | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS                     | MYALGIA                                        | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS                     | PAIN IN EXTREMITY                              | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS                     | SHOULDER PAIN                                  | 1 (1.2%)  | 0 (0.0%)             | 2 (2.4%)            |
| NEOPLASMS BENIGN, MALIGNANT AND UNSPECIFIED (INCL CYSTS AND POLYPS) | COLON CANCER                                   | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| NEOPLASMS BENIGN, MALIGNANT AND UNSPECIFIED (INCL CYSTS AND POLYPS) | MALIGNANT FIBROUS HISTIOCYTOMA                 | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| NEOPLASMS BENIGN, MALIGNANT AND UNSPECIFIED (INCL CYSTS AND POLYPS) | PROSTATE CANCER                                | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| NERVOUS SYSTEM DISORDERS                                            | AMNESIA                                        | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| NERVOUS SYSTEM DISORDERS                                            | BALANCE DISORDER                               | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| NERVOUS SYSTEM DISORDERS                                            | BURNING SENSATION                              | 0 (0.0%)  | 2 (2.4%)             | 0 (0.0%)            |
| NERVOUS SYSTEM DISORDERS                                            | COGNITIVE DISORDER                             | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| NERVOUS SYSTEM DISORDERS                                            | COMPLEX PARTIAL SEIZURES                       | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| NERVOUS SYSTEM DISORDERS                                            | COORDINATION ABNORMAL                          | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| NERVOUS SYSTEM DISORDERS                                            | DIZZINESS                                      | 2 (2.3%)  | 11 (13.1%)           | 8 (9.5%)            |
| NERVOUS SYSTEM DISORDERS                                            | HEADACHE                                       | 3 (3.5%)  | 5 (6%)               | 3 (3.6%)            |
| NERVOUS SYSTEM DISORDERS                                            | HEMIANOPIA HOMONYMOUS                          | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| NERVOUS SYSTEM DISORDERS                                            | HYPERSOMNIA                                    | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| NERVOUS SYSTEM DISORDERS                                            | LETHARGY                                       | 0 (0.0%)  | 1 (1.2%)             | 1 (1.2%)            |
| NERVOUS SYSTEM DISORDERS                                            | PARAESTHESIA                                   | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| NERVOUS SYSTEM DISORDERS                                            | PARAESTHESIA ORAL                              | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| NERVOUS SYSTEM DISORDERS                                            | PARKINSON'S DISEASE                            | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| NERVOUS SYSTEM DISORDERS                                            | PAROSMIA                                       | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| NERVOUS SYSTEM DISORDERS                                            | PARTIAL SEIZURES WITH SECONDARY GENERALISATION | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| NERVOUS SYSTEM DISORDERS                                            | PSYCHOMOTOR HYPERACTIVITY                      | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| NERVOUS SYSTEM DISORDERS                                            | SOMNOLENCE                                     | 2 (2.3%)  | 1 (1.2%)             | 3 (3.6%)            |
| NERVOUS SYSTEM DISORDERS                                            | STUPOR                                         | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| NERVOUS SYSTEM DISORDERS                                            | SYNCOPE                                        | 0 (0.0%)  | 3 (3.6%)             | 4 (4.8%)            |
| NERVOUS SYSTEM DISORDERS                                            | SYNCOPE VASOVAGAL                              | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| NERVOUS SYSTEM DISORDERS                                            | TRANSIENT ISCHAEMIC ATTACK                     | 0 (0.0%)  | 1 (1.2%)             | 2 (2.4%)            |
| PSYCHIATRIC DISORDERS                                               | AGITATION                                      | 2 (2.3%)  | 1 (1.2%)             | 2 (2.4%)            |
| PSYCHIATRIC DISORDERS                                               | ANXIETY                                        | 0 (0.0%)  | 0 (0.0%)             | 3 (3.6%)            |
| PSYCHIATRIC DISORDERS                                               | COMPLETED SUICIDE                              | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| PSYCHIATRIC DISORDERS                                               | CONFUSIONAL STATE                              | 2 (2.3%)  | 1 (1.2%)             | 3 (3.6%)            |
| PSYCHIATRIC DISORDERS                                               | DELIRIUM                                       | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| PSYCHIATRIC DISORDERS                                               | DELUSION                                       | 1 (1.2%)  | 1 (1.2%)             | 0 (0.0%)            |
| PSYCHIATRIC DISORDERS                                               | DEPRESSED MOOD                                 | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| PSYCHIATRIC DISORDERS                                               | DISORIENTATION                                 | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| PSYCHIATRIC DISORDERS                                               | HALLUCINATION                                  | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| PSYCHIATRIC DISORDERS                                               | HALLUCINATION, VISUAL                          | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| PSYCHIATRIC DISORDERS                                               | INSOMNIA                                       | 2 (2.3%)  | 2 (2.4%)             | 0 (0.0%)            |
| PSYCHIATRIC DISORDERS                                               | IRRITABILITY                                   | 1 (1.2%)  | 0 (0.0%)             | 1 (1.2%)            |
| PSYCHIATRIC DISORDERS                                               | LIBIDO DECREASED                               | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| PSYCHIATRIC DISORDERS                                               | LISTLESS                                       | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| PSYCHIATRIC DISORDERS                                               | NIGHTMARE                                      | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| PSYCHIATRIC DISORDERS                                               | RESTLESSNESS                                   | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| RENAL AND URINARY DISORDERS                                         | CALCULUS URETHRAL                              | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| RENAL AND URINARY DISORDERS                                         | DYSURIA                                        | 1 (1.2%)  | 0 (0.0%)             | 1 (1.2%)            |
| RENAL AND URINARY DISORDERS                                         | INCONTINENCE                                   | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| RENAL AND URINARY DISORDERS                                         | MICTURITION URGENCY                            | 1 (1.2%)  | 1 (1.2%)             | 1 (1.2%)            |
| RENAL AND URINARY DISORDERS                                         | NEPHROLITHIASIS                                | 1 (1.2%)  | 1 (1.2%)             | 0 (0.0%)            |
| RENAL AND URINARY DISORDERS                                         | POLLAKIURIA                                    | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| REPRODUCTIVE SYSTEM AND BREAST DISORDERS                            | BENIGN PROSTATIC HYPERPLASIA                   | 1 (1.2%)  | 1 (1.2%)             | 0 (0.0%)            |
| REPRODUCTIVE SYSTEM AND BREAST DISORDERS                            | PELVIC PAIN                                    | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | ALLERGIC GRANULOMATOUS ANGIITIS                | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | COUGH                                          | 1 (1.2%)  | 5 (6%)               | 5 (6%)              |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | DYSPHONIA                                      | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | DYSPNOEA                                       | 1 (1.2%)  | 1 (1.2%)             | 1 (1.2%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | EMPHYSEMA                                      | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | EPISTAXIS                                      | 0 (0.0%)  | 2 (2.4%)             | 1 (1.2%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | HAEMOPTYSIS                                    | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | NASAL CONGESTION                               | 3 (3.5%)  | 3 (3.6%)             | 1 (1.2%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | PHARYNGEAL ERYTHEMA                            | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | PHARYNGOLARYNGEAL PAIN                         | 0 (0.0%)  | 1 (1.2%)             | 1 (1.2%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | POSTNASAL DRIP                                 | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | PRODUCTIVE COUGH                               | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | RALES                                          | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | RESPIRATORY TRACT CONGESTION                   | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | RHINORRHOEA                                    | 0 (0.0%)  | 1 (1.2%)             | 1 (1.2%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | ACTINIC KERATOSIS                              | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | ALOPECIA                                       | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | BLISTER                                        | 0 (0.0%)  | 1 (1.2%)             | 5 (6%)              |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | COLD SWEAT                                     | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | DERMATITIS CONTACT                             | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | DRUG ERUPTION                                  | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | ERYTHEMA                                       | 8 (9.3%)  | 14 (16.7%)           | 14 (16.7%)          |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | HYPERHIDROSIS                                  | 2 (2.3%)  | 8 (9.5%)             | 4 (4.8%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | PRURITUS                                       | 8 (9.3%)  | 25 (29.8%)           | 21 (25%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | PRURITUS GENERALISED                           | 0 (0.0%)  | 1 (1.2%)             | 1 (1.2%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | RASH                                           | 5 (5.8%)  | 8 (9.5%)             | 13 (15.5%)          |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | RASH ERYTHEMATOUS                              | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | RASH MACULO-PAPULAR                            | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | RASH PRURITIC                                  | 0 (0.0%)  | 2 (2.4%)             | 1 (1.2%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | SKIN EXFOLIATION                               | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | SKIN IRRITATION                                | 3 (3.5%)  | 5 (6%)               | 6 (7.1%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | SKIN ODOUR ABNORMAL                            | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | SKIN ULCER                                     | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | URTICARIA                                      | 0 (0.0%)  | 1 (1.2%)             | 1 (1.2%)            |
| SOCIAL CIRCUMSTANCES                                                | ALCOHOL USE                                    | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| SURGICAL AND MEDICAL PROCEDURES                                     | ACROCHORDON EXCISION                           | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| SURGICAL AND MEDICAL PROCEDURES                                     | CATARACT OPERATION                             | 1 (1.2%)  | 0 (0.0%)             | 1 (1.2%)            |
| SURGICAL AND MEDICAL PROCEDURES                                     | EYE LASER SURGERY                              | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| SURGICAL AND MEDICAL PROCEDURES                                     | SKIN LESION EXCISION                           | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| VASCULAR DISORDERS                                                  | HOT FLUSH                                      | 0 (0.0%)  | 0 (0.0%)             | 1 (1.2%)            |
| VASCULAR DISORDERS                                                  | HYPERTENSION                                   | 1 (1.2%)  | 0 (0.0%)             | 1 (1.2%)            |
| VASCULAR DISORDERS                                                  | HYPOTENSION                                    | 2 (2.3%)  | 0 (0.0%)             | 1 (1.2%)            |
| VASCULAR DISORDERS                                                  | ORTHOSTATIC HYPOTENSION                        | 1 (1.2%)  | 0 (0.0%)             | 0 (0.0%)            |
| VASCULAR DISORDERS                                                  | WOUND HAEMORRHAGE                              | 0 (0.0%)  | 1 (1.2%)             | 0 (0.0%)            |
| Safety Population                                                   |                                                |           |                      |                     |
| n (%) = Number (percentage) of subjects with at least one event     |                                                |           |                      |                     |

## Most Common AEs

``` r
# Create table of most common AEs (top 10)
common_table <- create_ae_summary_table(
  adae = adae,
  adsl = adsl,
  type = "common",
  n_top = 10,
  title = "Table 4: Most Common Treatment-Emergent Adverse Events (Top 10)"
)

# Display the table
common_table@flextable
```

| Table 4: Most Common Treatment-Emergent Adverse Events (Top 10) |                             |           |                      |                     |
|-----------------------------------------------------------------|-----------------------------|-----------|----------------------|---------------------|
| Body System or Organ Class                                      | Dictionary-Derived Term     | Placebo   | Xanomeline High Dose | Xanomeline Low Dose |
| CARDIAC DISORDERS                                               | SINUS BRADYCARDIA           | 2 (2.3%)  | 8 (9.5%)             | 7 (8.3%)            |
| GASTROINTESTINAL DISORDERS                                      | DIARRHOEA                   | 9 (10.5%) | 4 (4.8%)             | 4 (4.8%)            |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS            | APPLICATION SITE DERMATITIS | 5 (5.8%)  | 7 (8.3%)             | 9 (10.7%)           |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS            | APPLICATION SITE ERYTHEMA   | 3 (3.5%)  | 15 (17.9%)           | 12 (14.3%)          |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS            | APPLICATION SITE IRRITATION | 3 (3.5%)  | 9 (10.7%)            | 9 (10.7%)           |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS            | APPLICATION SITE PRURITUS   | 6 (7%)    | 22 (26.2%)           | 22 (26.2%)          |
| NERVOUS SYSTEM DISORDERS                                        | DIZZINESS                   | 2 (2.3%)  | 11 (13.1%)           | 8 (9.5%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                          | ERYTHEMA                    | 8 (9.3%)  | 14 (16.7%)           | 14 (16.7%)          |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                          | PRURITUS                    | 8 (9.3%)  | 25 (29.8%)           | 21 (25%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                          | RASH                        | 5 (5.8%)  | 8 (9.5%)             | 13 (15.5%)          |
| Safety Population                                               |                             |           |                      |                     |
| Showing top 10 most frequently reported Preferred Terms         |                             |           |                      |                     |

## Severity Analysis

``` r
# Create severity table
severity_table <- create_ae_summary_table(
  adae = adae,
  adsl = adsl,
  type = "severity",
  title = "Table 5: Subjects by Maximum Adverse Event Severity"
)

# Display the table
severity_table@flextable
```

| Table 5: Subjects by Maximum Adverse Event Severity |            |                      |                     |
|-----------------------------------------------------|------------|----------------------|---------------------|
| Maximum Severity                                    | Placebo    | Xanomeline High Dose | Xanomeline Low Dose |
| MILD                                                | 36 (41.9%) | 22 (26.2%)           | 19 (22.6%)          |
| MODERATE                                            | 24 (27.9%) | 45 (53.6%)           | 42 (50%)            |
| SEVERE                                              | 5 (5.8%)   | 8 (9.5%)             | 16 (19%)            |
| Safety Population                                   |            |                      |                     |
| Maximum severity across all TEAEs per subject       |            |                      |                     |

Severity categories order as: MILD → MODERATE → SEVERE.

## Relationship Analysis

``` r
# Create relationship table
relationship_table <- create_ae_summary_table(
  adae = adae,
  adsl = adsl,
  type = "relationship",
  title = "Table 6: Treatment-Emergent AEs by Relationship to Study Drug"
)

# Display the table
relationship_table@flextable
```

| Table 6: Treatment-Emergent AEs by Relationship to Study Drug |            |                      |                     |
|---------------------------------------------------------------|------------|----------------------|---------------------|
| Causality                                                     | Placebo    | Xanomeline High Dose | Xanomeline Low Dose |
| NONE                                                          | 33 (38.4%) | 41 (48.8%)           | 29 (34.5%)          |
| POSSIBLE                                                      | 25 (29.1%) | 47 (56%)             | 44 (52.4%)          |
| PROBABLE                                                      | 23 (26.7%) | 50 (59.5%)           | 49 (58.3%)          |
| REMOTE                                                        | 28 (32.6%) | 21 (25%)             | 23 (27.4%)          |
| --                                                            | 0 (0.0%)   | 0 (0.0%)             | 2 (2.4%)            |
| Safety Population                                             |            |                      |                     |
| Subjects counted once per relationship category               |            |                      |                     |

## SAE Tables

``` r
# Create SAE table
sae_table <- create_ae_summary_table(
  adae = adae,
  adsl = adsl,
  type = "sae",
  title = "Table 7: Serious Adverse Events"
)

# Display the table
sae_table@flextable
```

| Table 7: Serious Adverse Events           |                                                |                      |                     |
|-------------------------------------------|------------------------------------------------|----------------------|---------------------|
| Body System or Organ Class                | Dictionary-Derived Term                        | Xanomeline High Dose | Xanomeline Low Dose |
| NERVOUS SYSTEM DISORDERS                  | PARTIAL SEIZURES WITH SECONDARY GENERALISATION | 1 (1.2%)             | 0 (0.0%)            |
| NERVOUS SYSTEM DISORDERS                  | SYNCOPE                                        | 1 (1.2%)             | 1 (1.2%)            |
| Safety Population                         |                                                |                      |                     |
| SAE = Serious Adverse Event (AESER = 'Y') |                                                |                      |                     |

Shows message if no SAEs reported.

## Deaths and Discontinuations

### Discontinuation

``` r
# Create discontinuation table
disc_table <- create_ae_summary_table(
  adae = adae,
  adsl = adsl,
  type = "discontinuation",
  title = "Table 8: AEs Leading to Study Drug Discontinuation"
)

# Display the table
disc_table@flextable
```

| Table 8: AEs Leading to Study Drug Discontinuation      |
|---------------------------------------------------------|
| Message                                                 |
| No adverse events leading to study drug discontinuation |
| Safety Population                                       |
| AEACN = 'DRUG WITHDRAWN'                                |

### Deaths Summary

``` r
# Create deaths summary table
deaths_table <- create_ae_summary_table(
  adae = NULL,  # Not needed for deaths
  adsl = adsl,
  type = "deaths",
  title = "Table 9: Deaths Summary"
)

# Display the table
deaths_table@flextable
```

| Table 9: Deaths Summary |          |                      |                     |
|-------------------------|----------|----------------------|---------------------|
| Statistic               | Placebo  | Xanomeline High Dose | Xanomeline Low Dose |
| Safety Population (N)   | 86       | 84                   | 84                  |
| Deaths n (%)            | 2 (2.3%) | 0 (0%)               | 1 (1.2%)            |
| Safety Population       |          |                      |                     |
| DTHFL = 'Y'             |          |                      |                     |

## Lab Shift Tables

Click to expand: Lab Shift Table

``` r
# Example of lab shift analysis
trt_n <- adsl |>
  dplyr::group_by(TRT01P) |>
  dplyr::summarise(N = dplyr::n(), .groups = "drop")
lab_shift_table <- create_lab_shift_table(
  adlb = adlb,
  title = "Table 10: Lab Shift from Baseline to Max Post-Baseline"
)

# Display the table
lab_shift_table@flextable
```

| Table 10: Lab Shift from Baseline to Max Post-Baseline |                                    |        |      |     |
|--------------------------------------------------------|------------------------------------|--------|------|-----|
| Planned Treatment for Period 01                        | Baseline Reference Range Indicator | NORMAL | HIGH | LOW |
| Placebo                                                | HIGH                               | 3      | 0    | 0   |
| Placebo                                                | NORMAL                             | 51     | 2    | 1   |
| Xanomeline High Dose                                   | HIGH                               | 2      | 0    | 0   |
| Xanomeline High Dose                                   | NORMAL                             | 28     | 0    | 0   |
| Xanomeline Low Dose                                    | NORMAL                             | 24     | 2    | 0   |
| Safety Population                                      |                                    |        |      |     |
| Shift from baseline normal range indicator             |                                    |        |      |     |

## Customization

### Treatment Variable

``` r
# Use different treatment variable
overview_trtp <- create_ae_summary_table(
  adae = adae,
  adsl = adsl,
  type = "overview",
  trt_var = "TRT01P",  # Planned treatment
  title = "Overview by Planned Treatment"
)

# Display the table
overview_trtp@flextable
```

| Overview by Planned Treatment           |            |                      |                     |
|-----------------------------------------|------------|----------------------|---------------------|
| Category                                | Placebo    | Xanomeline High Dose | Xanomeline Low Dose |
| Subjects with at least one TEAE         | 65 (75.6%) | 75 (89.3%)           | 77 (91.7%)          |
| Subjects with at least one related TEAE | 43 (50.0%) | 69 (82.1%)           | 72 (85.7%)          |
| Subjects with at least one SAE          | 0 (0.0%)   | 2 (2.4%)             | 1 (1.2%)            |
| Deaths                                  | 2 (2.3%)   | 0 (0.0%)             | 1 (1.2%)            |

### Top AEs

``` r
# Show top 5 instead of default 15
common_top5 <- create_ae_summary_table(
  adae = adae,
  adsl = adsl,
  type = "common",
  n_top = 5,
  title = "Top 5 Most Common AEs"
)

# Display the table
common_top5@flextable
```

| Top 5 Most Common AEs                                  |                           |          |                      |                     |
|--------------------------------------------------------|---------------------------|----------|----------------------|---------------------|
| Body System or Organ Class                             | Dictionary-Derived Term   | Placebo  | Xanomeline High Dose | Xanomeline Low Dose |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS   | APPLICATION SITE ERYTHEMA | 3 (3.5%) | 15 (17.9%)           | 12 (14.3%)          |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS   | APPLICATION SITE PRURITUS | 6 (7%)   | 22 (26.2%)           | 22 (26.2%)          |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                 | ERYTHEMA                  | 8 (9.3%) | 14 (16.7%)           | 14 (16.7%)          |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                 | PRURITUS                  | 8 (9.3%) | 25 (29.8%)           | 21 (25%)            |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                 | RASH                      | 5 (5.8%) | 8 (9.5%)             | 13 (15.5%)          |
| Safety Population                                      |                           |          |                      |                     |
| Showing top 5 most frequently reported Preferred Terms |                           |          |                      |                     |

### SOC Analysis

``` r
# Analyze only skin and subcutaneous tissue disorders
rash_table <- create_ae_summary_table(
  adae = adae,
  adsl = adsl,
  type = "pt",
  soc = "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
  title = "Table: Skin and Subcutaneous Tissue Disorders by Preferred Term"
)

# Display the table
rash_table@flextable
```

| Table: Skin and Subcutaneous Tissue Disorders by Preferred Term |          |                      |                     |
|-----------------------------------------------------------------|----------|----------------------|---------------------|
| Dictionary-Derived Term                                         | Placebo  | Xanomeline High Dose | Xanomeline Low Dose |
| ACTINIC KERATOSIS                                               | 0 (0.0%) | 1 (1.2%)             | 0 (0.0%)            |
| ALOPECIA                                                        | 1 (1.2%) | 0 (0.0%)             | 0 (0.0%)            |
| BLISTER                                                         | 0 (0.0%) | 1 (1.2%)             | 5 (6%)              |
| COLD SWEAT                                                      | 1 (1.2%) | 0 (0.0%)             | 0 (0.0%)            |
| DERMATITIS CONTACT                                              | 0 (0.0%) | 0 (0.0%)             | 1 (1.2%)            |
| DRUG ERUPTION                                                   | 1 (1.2%) | 0 (0.0%)             | 0 (0.0%)            |
| ERYTHEMA                                                        | 8 (9.3%) | 14 (16.7%)           | 14 (16.7%)          |
| HYPERHIDROSIS                                                   | 2 (2.3%) | 8 (9.5%)             | 4 (4.8%)            |
| PRURITUS                                                        | 8 (9.3%) | 25 (29.8%)           | 21 (25%)            |
| PRURITUS GENERALISED                                            | 0 (0.0%) | 1 (1.2%)             | 1 (1.2%)            |
| RASH                                                            | 5 (5.8%) | 8 (9.5%)             | 13 (15.5%)          |
| RASH ERYTHEMATOUS                                               | 0 (0.0%) | 0 (0.0%)             | 1 (1.2%)            |
| RASH MACULO-PAPULAR                                             | 0 (0.0%) | 1 (1.2%)             | 0 (0.0%)            |
| RASH PRURITIC                                                   | 0 (0.0%) | 2 (2.4%)             | 1 (1.2%)            |
| SKIN EXFOLIATION                                                | 0 (0.0%) | 0 (0.0%)             | 1 (1.2%)            |
| SKIN IRRITATION                                                 | 3 (3.5%) | 5 (6%)               | 6 (7.1%)            |
| SKIN ODOUR ABNORMAL                                             | 0 (0.0%) | 1 (1.2%)             | 0 (0.0%)            |
| SKIN ULCER                                                      | 1 (1.2%) | 0 (0.0%)             | 0 (0.0%)            |
| URTICARIA                                                       | 0 (0.0%) | 1 (1.2%)             | 1 (1.2%)            |

### Table Formatting

``` r
# Create table without autofit (for custom formatting)
soc_manual <- create_ae_summary_table(
  adae = adae,
  adsl = adsl,
  type = "soc",
  autofit = FALSE,
  title = "Manual Format Example"
)

# Access the underlying flextable for custom formatting
soc_ft <- soc_manual@flextable

# Display the table
soc_ft
```

| Manual Format Example                                               |            |                      |                     |
|---------------------------------------------------------------------|------------|----------------------|---------------------|
| Body System or Organ Class                                          | Placebo    | Xanomeline High Dose | Xanomeline Low Dose |
| CARDIAC DISORDERS                                                   | 12 (14%)   | 15 (17.9%)           | 13 (15.5%)          |
| CONGENITAL, FAMILIAL AND GENETIC DISORDERS                          | 0 (0.0%)   | 2 (2.4%)             | 1 (1.2%)            |
| EAR AND LABYRINTH DISORDERS                                         | 1 (1.2%)   | 1 (1.2%)             | 2 (2.4%)            |
| EYE DISORDERS                                                       | 2 (2.3%)   | 1 (1.2%)             | 2 (2.4%)            |
| GASTROINTESTINAL DISORDERS                                          | 17 (19.8%) | 20 (23.8%)           | 14 (16.7%)          |
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                | 21 (24.4%) | 40 (47.6%)           | 47 (56%)            |
| HEPATOBILIARY DISORDERS                                             | 1 (1.2%)   | 0 (0.0%)             | 0 (0.0%)            |
| IMMUNE SYSTEM DISORDERS                                             | 0 (0.0%)   | 0 (0.0%)             | 1 (1.2%)            |
| INFECTIONS AND INFESTATIONS                                         | 16 (18.6%) | 13 (15.5%)           | 9 (10.7%)           |
| INJURY, POISONING AND PROCEDURAL COMPLICATIONS                      | 4 (4.7%)   | 5 (6%)               | 5 (6%)              |
| INVESTIGATIONS                                                      | 10 (11.6%) | 6 (7.1%)             | 6 (7.1%)            |
| METABOLISM AND NUTRITION DISORDERS                                  | 6 (7%)     | 2 (2.4%)             | 1 (1.2%)            |
| MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS                     | 4 (4.7%)   | 7 (8.3%)             | 7 (8.3%)            |
| NEOPLASMS BENIGN, MALIGNANT AND UNSPECIFIED (INCL CYSTS AND POLYPS) | 0 (0.0%)   | 1 (1.2%)             | 2 (2.4%)            |
| NERVOUS SYSTEM DISORDERS                                            | 8 (9.3%)   | 25 (29.8%)           | 20 (23.8%)          |
| PSYCHIATRIC DISORDERS                                               | 10 (11.6%) | 8 (9.5%)             | 10 (11.9%)          |
| RENAL AND URINARY DISORDERS                                         | 4 (4.7%)   | 3 (3.6%)             | 3 (3.6%)            |
| REPRODUCTIVE SYSTEM AND BREAST DISORDERS                            | 2 (2.3%)   | 1 (1.2%)             | 0 (0.0%)            |
| RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS                     | 8 (9.3%)   | 10 (11.9%)           | 9 (10.7%)           |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS                              | 20 (23.3%) | 39 (46.4%)           | 39 (46.4%)          |
| SOCIAL CIRCUMSTANCES                                                | 0 (0.0%)   | 1 (1.2%)             | 0 (0.0%)            |
| SURGICAL AND MEDICAL PROCEDURES                                     | 2 (2.3%)   | 2 (2.4%)             | 1 (1.2%)            |
| VASCULAR DISORDERS                                                  | 3 (3.5%)   | 1 (1.2%)             | 3 (3.6%)            |
| Safety Population                                                   |            |                      |                     |
| n (%) = Number (percentage) of subjects with at least one event     |            |                      |                     |

## Export to Word

``` r
# Create multiple safety tables
safety_tables <- list(
  overview = create_ae_summary_table(adae, adsl, type = "overview"),
  soc = create_ae_summary_table(adae, adsl, type = "soc"),
  common = create_ae_summary_table(adae, adsl, type = "common", n_top = 10),
  sae = create_ae_summary_table(adae, adsl, type = "sae")
)

# Create report sections
safety_sections <- lapply(names(safety_tables), function(table_name) {
  ReportSection(
    title = paste("Safety Analysis:", table_name),
    section_type = "safety",
    content = list(safety_tables[[table_name]])
  )
})

# Create clinical report
safety_report <- ClinicalReport(
  study_id = "CDISCPILOT01",
  study_title = "Safety Analysis Report",
  sections = safety_sections
)

# Export to Word
# generate_word(safety_report, path = "safety_report.docx")

cat("Safety report structure created with", length(safety_tables), "tables.")
#> Safety report structure created with 4 tables.
cat("\nUse generate_word() to export to Word.")
#> 
#> Use generate_word() to export to Word.
```

## Time-to-Event Analysis

``` r
# Calculate time-to-event data for a specific SOC
tte_data <- calculate_ae_tte_data(
  adsl = adsl,
  adae = adae,
  soc = "Skin and subcutaneous tissue disorders"
)

# Preview TTE data structure
head(tte_data, 3)
#> # A tibble: 3 × 57
#>   STUDYID      USUBJID SUBJID RFSTDTC RFENDTC RFXSTDTC RFXENDTC RFICDTC RFPENDTC
#>   <chr>        <chr>   <chr>  <chr>   <chr>   <chr>    <chr>    <chr>   <chr>   
#> 1 CDISCPILOT01 01-701… 1015   2014-0… 2014-0… 2014-01… 2014-07… NA      2014-07…
#> 2 CDISCPILOT01 01-701… 1023   2012-0… 2012-0… 2012-08… 2012-09… NA      2013-02…
#> 3 CDISCPILOT01 01-701… 1028   2013-0… 2014-0… 2013-07… 2014-01… NA      2014-01…
#> # ℹ 48 more variables: DTHDTC <chr>, DTHFL <chr>, SITEID <chr>, AGE <dbl>,
#> #   AGEU <chr>, SEX <chr>, RACE <chr>, ETHNIC <chr>, ARMCD <chr>, ARM <chr>,
#> #   ACTARMCD <chr>, ACTARM <chr>, COUNTRY <chr>, DMDTC <chr>, DMDY <dbl>,
#> #   TRT01P <chr>, TRT01A <chr>, TRTSDTM <dttm>, TRTSTMF <chr>, TRTEDTM <dttm>,
#> #   TRTETMF <chr>, TRTSDT <date>, TRTEDT <date>, TRTDURD <dbl>, SCRFDT <date>,
#> #   EOSDT <date>, EOSSTT <chr>, FRVDT <date>, RANDDT <date>, DTHDT <date>,
#> #   DTHDTF <chr>, DTHADY <dbl>, LDDTHELD <dbl>, DTHCAUS <chr>, DTHDOM <chr>, …

# Create KM plot
km_plot <- create_ae_km_plot_for_soc(
  adsl = adsl,
  adae = adae,
  soc = "Skin and subcutaneous tissue disorders"
)

print(km_plot@plot)
```

![](safety-tables_files/figure-html/tte-1.png)

## AE Statistical Comparisons (Risk Difference)

For GBA/AMNOG dossiers, comparing adverse event rates between treatment
groups with risk differences and risk ratios is essential. The
[`create_ae_comparison_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_ae_comparison_table.md)
function provides these statistics.

### Basic Comparison Table

``` r
# Create AE comparison table with risk differences
comparison_table <- create_ae_summary_table(
  adae = adae,
  adsl = adsl,
  type = "comparison",
  ref_group = "Placebo",
  by = "soc",
  title = "Table: AE Comparison by System Organ Class"
)

# Display the table
comparison_table@flextable
```

[TABLE]

> **Note**: `create_ae_summary_table(type = "comparison")` is a
> convenience wrapper around
> [`create_ae_comparison_table()`](https://sims1253.github.io/pharmhand/dev/reference/create_ae_comparison_table.md).
> Both functions produce identical results—use whichever entry point you
> prefer.

The comparison table includes: - **n/N (%)** for each treatment group -
**Risk Difference (RD)** with 95% CI - **Risk Ratio (RR)** with 95% CI -
**P-value** (Chi-square or Fisher’s exact test)

### Preferred Term Level Comparison

``` r
# Compare at preferred term level with threshold
pt_comparison <- create_ae_comparison_table(
  adae = adae,
  adsl = adsl,
  ref_group = "Placebo",
  by = "pt",
  threshold = 5,  # Only events with ≥5% incidence
  sort_by = "rd",  # Sort by risk difference
  title = "AEs with ≥5% Incidence - Sorted by Risk Difference"
)

# Display the table
pt_comparison@flextable
```

[TABLE]

### Overall AE Summary Comparison

``` r
# Overall comparison (any TEAE)
overall_comparison <- create_ae_comparison_table(
  adae = adae,
  adsl = adsl,
  ref_group = "Placebo",
  by = "overall",
  title = "Overall TEAE Comparison"
)

# Display the table
overall_comparison@flextable
```

[TABLE]

### Interpreting Results

- **Risk Difference \> 0**: Higher incidence in treatment vs reference
- **Risk Ratio \> 1**: Higher relative risk in treatment vs reference
- **P-value \< 0.05**: Statistically significant difference (unadjusted)

For GBA submissions, the risk difference is often the primary measure as
it reflects the absolute impact on patients.

## Related Functions

- [`create_hta_table()`](https://sims1253.github.io/pharmhand/reference/create_hta_table.html) -
  Underlying function for table formatting
- [`create_km_plot()`](https://sims1253.github.io/pharmhand/reference/create_km_plot.html) -
  Kaplan-Meier plot creation
- [`ClinicalReport()`](https://sims1253.github.io/pharmhand/reference/ClinicalReport.html) -
  Report assembly
- [`generate_word()`](https://sims1253.github.io/pharmhand/reference/generate_word.html) -
  Word document generation

## See Also

- [Get
  Started](https://sims1253.github.io/pharmhand/articles/pharmhand.html)
- [Creating Efficacy
  Tables](https://sims1253.github.io/pharmhand/articles/efficacy-tables.html)
- [S7
  Architecture](https://sims1253.github.io/pharmhand/articles/s7-architecture.html)

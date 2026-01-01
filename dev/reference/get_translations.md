# Get All Translations for a Locale

Returns all available translations for the specified or current locale.
Useful for reviewing available translation keys and their values.

## Usage

``` r
get_translations(locale = NULL, include_custom = TRUE)
```

## Arguments

- locale:

  Character or NULL. Locale to retrieve translations for. If NULL
  (default), uses the current locale.

- include_custom:

  Logical. Whether to include custom translations added via
  [`add_translation()`](https://sims1253.github.io/pharmhand/dev/reference/add_translation.md)
  (default: TRUE).

## Value

A named list of all translations where names are keys and values are
translated strings.

## See also

[`tr()`](https://sims1253.github.io/pharmhand/dev/reference/tr.md),
[`add_translation()`](https://sims1253.github.io/pharmhand/dev/reference/add_translation.md),
[`set_locale()`](https://sims1253.github.io/pharmhand/dev/reference/set_locale.md)

## Examples

``` r
# Get all English translations
translations <- get_translations("en")
names(translations)[1:10]
#>  [1] "statistic"      "parameter"      "category"       "treatment"     
#>  [5] "placebo"        "total"          "value"          "variable"      
#>  [9] "subgroup"       "characteristic"

# Get current locale translations
get_translations()
#> $statistic
#> [1] "Statistik"
#> 
#> $parameter
#> [1] "Parameter"
#> 
#> $category
#> [1] "Kategorie"
#> 
#> $treatment
#> [1] "Aktive Behandlung"
#> 
#> $placebo
#> [1] "Placebo"
#> 
#> $total
#> [1] "Gesamt"
#> 
#> $value
#> [1] "Wert"
#> 
#> $variable
#> [1] "Variable"
#> 
#> $subgroup
#> [1] "Subgruppe"
#> 
#> $characteristic
#> [1] "Merkmal"
#> 
#> $result
#> [1] "Ergebnis"
#> 
#> $comparison
#> [1] "Vergleich"
#> 
#> $difference
#> [1] "Differenz"
#> 
#> $n
#> [1] "n"
#> 
#> $pct
#> [1] "%"
#> 
#> $n_pct
#> [1] "n (%)"
#> 
#> $mean
#> [1] "Mittelwert"
#> 
#> $sd
#> [1] "SD"
#> 
#> $mean_sd
#> [1] "Mittelwert (SD)"
#> 
#> $median
#> [1] "Median"
#> 
#> $min
#> [1] "Min"
#> 
#> $max
#> [1] "Max"
#> 
#> $min_max
#> [1] "Min, Max"
#> 
#> $q1
#> [1] "Q1"
#> 
#> $q3
#> [1] "Q3"
#> 
#> $iqr
#> [1] "IQR"
#> 
#> $range
#> [1] "Spannweite"
#> 
#> $se
#> [1] "SE"
#> 
#> $cv
#> [1] "VK"
#> 
#> $cv_pct
#> [1] "VK%"
#> 
#> $safety_population
#> [1] "Sicherheitspopulation"
#> 
#> $itt_population
#> [1] "ITT-Population"
#> 
#> $pp_population
#> [1] "Per-Protokoll-Population"
#> 
#> $fas_population
#> [1] "Vollanalyse-Population"
#> 
#> $miti_population
#> [1] "Modifizierte ITT-Population"
#> 
#> $evaluable_population
#> [1] "Auswertbare Population"
#> 
#> $teae
#> [1] "Unerwartetes Ereignis unter Behandlung"
#> 
#> $teae_short
#> [1] "TEAE"
#> 
#> $sae
#> [1] "Schwerwiegendes unerwartetes Ereignis"
#> 
#> $sae_short
#> [1] "SUE"
#> 
#> $ae
#> [1] "Unerwartetes Ereignis"
#> 
#> $ae_short
#> [1] "UE"
#> 
#> $preferred_term
#> [1] "Bevorzugter Begriff"
#> 
#> $system_organ_class
#> [1] "Systemorganklasse"
#> 
#> $soc
#> [1] "SOC"
#> 
#> $pt
#> [1] "PT"
#> 
#> $severity
#> [1] "Schweregrad"
#> 
#> $relationship
#> [1] "Zusammenhang"
#> 
#> $mild
#> [1] "Leicht"
#> 
#> $moderate
#> [1] "Mittelgradig"
#> 
#> $severe
#> [1] "Schwer"
#> 
#> $related
#> [1] "In Zusammenhang stehend"
#> 
#> $not_related
#> [1] "Kein Zusammenhang"
#> 
#> $possibly_related
#> [1] "Moeglicherweise in Zusammenhang"
#> 
#> $probably_related
#> [1] "Wahrscheinlich in Zusammenhang"
#> 
#> $leading_to_discontinuation
#> [1] "Fuehrte zum Abbruch"
#> 
#> $leading_to_death
#> [1] "Mit Todesfolge"
#> 
#> $subjects_with_any_ae
#> [1] "Patienten mit einem UE"
#> 
#> $subjects_with_any_teae
#> [1] "Patienten mit einem TEAE"
#> 
#> $subjects_with_any_sae
#> [1] "Patienten mit einem SUE"
#> 
#> $hazard_ratio
#> [1] "Hazard Ratio"
#> 
#> $risk_difference
#> [1] "Risikodifferenz"
#> 
#> $risk_ratio
#> [1] "Risikoverhaeltnis"
#> 
#> $odds_ratio
#> [1] "Odds Ratio"
#> 
#> $relative_risk
#> [1] "Relatives Risiko"
#> 
#> $confidence_interval
#> [1] "Konfidenzintervall"
#> 
#> $ci
#> [1] "KI"
#> 
#> $ci_95
#> [1] "95%-KI"
#> 
#> $ci_90
#> [1] "90%-KI"
#> 
#> $ci_99
#> [1] "99%-KI"
#> 
#> $p_value
#> [1] "p-Wert"
#> 
#> $events
#> [1] "Ereignisse"
#> 
#> $censored
#> [1] "Zensiert"
#> 
#> $median_survival
#> [1] "Medianes Ueberleben"
#> 
#> $time_to_event
#> [1] "Zeit bis zum Ereignis"
#> 
#> $response_rate
#> [1] "Ansprechrate"
#> 
#> $complete_response
#> [1] "Vollstaendiges Ansprechen"
#> 
#> $partial_response
#> [1] "Partielles Ansprechen"
#> 
#> $stable_disease
#> [1] "Stabile Erkrankung"
#> 
#> $progressive_disease
#> [1] "Fortschreitende Erkrankung"
#> 
#> $objective_response
#> [1] "Objektives Ansprechen"
#> 
#> $disease_control
#> [1] "Krankheitskontrolle"
#> 
#> $change_from_baseline
#> [1] "Aenderung gegenueber Baseline"
#> 
#> $cfb
#> [1] "CFB"
#> 
#> $ls_mean
#> [1] "LS-Mittelwert"
#> 
#> $ls_mean_difference
#> [1] "LS-Mittelwertdifferenz"
#> 
#> $responders
#> [1] "Responder"
#> 
#> $non_responders
#> [1] "Non-Responder"
#> 
#> $age
#> [1] "Alter"
#> 
#> $age_years
#> [1] "Alter (Jahre)"
#> 
#> $age_group
#> [1] "Altersgruppe"
#> 
#> $sex
#> [1] "Geschlecht"
#> 
#> $gender
#> [1] "Geschlecht"
#> 
#> $male
#> [1] "Maennlich"
#> 
#> $female
#> [1] "Weiblich"
#> 
#> $race
#> [1] "Ethnie"
#> 
#> $ethnicity
#> [1] "Ethnizitaet"
#> 
#> $weight
#> [1] "Gewicht"
#> 
#> $weight_kg
#> [1] "Gewicht (kg)"
#> 
#> $height
#> [1] "Groesse"
#> 
#> $height_cm
#> [1] "Groesse (cm)"
#> 
#> $bmi
#> [1] "BMI"
#> 
#> $bmi_full
#> [1] "Body-Mass-Index"
#> 
#> $bmi_kgm2
#> [1] "BMI (kg/m2)"
#> 
#> $country
#> [1] "Land"
#> 
#> $region
#> [1] "Region"
#> 
#> $site
#> [1] "Studienzentrum"
#> 
#> $baseline
#> [1] "Baseline"
#> 
#> $screening
#> [1] "Screening"
#> 
#> $visit
#> [1] "Visite"
#> 
#> $end_of_study
#> [1] "Studienende"
#> 
#> $follow_up
#> [1] "Nachbeobachtung"
#> 
#> $randomization
#> [1] "Randomisierung"
#> 
#> $treatment_period
#> [1] "Behandlungsphase"
#> 
#> $day
#> [1] "Tag"
#> 
#> $week
#> [1] "Woche"
#> 
#> $month
#> [1] "Monat"
#> 
#> $year
#> [1] "Jahr"
#> 
#> $disposition
#> [1] "Disposition"
#> 
#> $completed
#> [1] "Abgeschlossen"
#> 
#> $discontinued
#> [1] "Abgebrochen"
#> 
#> $ongoing
#> [1] "Laufend"
#> 
#> $reason_for_discontinuation
#> [1] "Grund fuer Abbruch"
#> 
#> $lost_to_follow_up
#> [1] "Lost to Follow-up"
#> 
#> $withdrawal_by_subject
#> [1] "Ruecknahme durch Patient"
#> 
#> $adverse_event
#> [1] "Unerwartetes Ereignis"
#> 
#> $death
#> [1] "Tod"
#> 
#> $protocol_deviation
#> [1] "Protokollabweichung"
#> 
#> $lack_of_efficacy
#> [1] "Mangelnde Wirksamkeit"
#> 
#> $physician_decision
#> [1] "Arztentscheidung"
#> 
#> $other
#> [1] "Sonstige"
#> 
#> $shift_table
#> [1] "Shift-Tabelle"
#> 
#> $normal
#> [1] "Normal"
#> 
#> $low
#> [1] "Niedrig"
#> 
#> $high
#> [1] "Hoch"
#> 
#> $clinically_significant
#> [1] "Klinisch signifikant"
#> 
#> $laboratory_parameter
#> [1] "Laborparameter"
#> 
#> $reference_range
#> [1] "Referenzbereich"
#> 
#> $unit
#> [1] "Einheit"
#> 
#> $alanine_aminotransferase
#> [1] "Alanin-Aminotransferase"
#> 
#> $aspartate_aminotransferase
#> [1] "Aspartat-Aminotransferase"
#> 
#> $alkaline_phosphatase
#> [1] "Alkalische Phosphatase"
#> 
#> $bilirubin
#> [1] "Bilirubin"
#> 
#> $creatinine
#> [1] "Kreatinin"
#> 
#> $hemoglobin
#> [1] "Haemoglobin"
#> 
#> $platelets
#> [1] "Thrombozyten"
#> 
#> $white_blood_cells
#> [1] "Leukozyten"
#> 
#> $neutrophils
#> [1] "Neutrophile"
#> 
#> $medical_history
#> [1] "Anamnese"
#> 
#> $concomitant_medications
#> [1] "Begleitmedikation"
#> 
#> $prior_medications
#> [1] "Vormedikation"
#> 
#> $atc_class
#> [1] "ATC-Klasse"
#> 
#> $drug_name
#> [1] "Wirkstoffname"
#> 
#> $indication
#> [1] "Indikation"
#> 
#> $start_date
#> [1] "Startdatum"
#> 
#> $end_date
#> [1] "Enddatum"
#> 
#> $smd
#> [1] "SMD"
#> 
#> $standardized_mean_difference
#> [1] "Standardisierte Mittelwertdifferenz"
#> 
#> $imbalance
#> [1] "Ungleichgewicht"
#> 
#> $balanced
#> [1] "Ausgeglichen"
#> 
#> $threshold
#> [1] "Schwellenwert"
#> 
#> $fn_safety_pop
#> [1] "Sicherheitspopulation"
#> 
#> $fn_itt_pop
#> [1] "Intent-to-Treat Population"
#> 
#> $fn_n_pct
#> [1] "n (%) = Anzahl (Prozentsatz) der Patienten"
#> 
#> $fn_ci
#> [1] "KI = Konfidenzintervall"
#> 
#> $fn_smd
#> [1] "SMD = Standardisierte Mittelwertdifferenz"
#> 
#> $fn_teae
#> [1] "TEAE = Unerwartetes Ereignis unter Behandlung"
#> 
#> $fn_sae
#> [1] "SUE = Schwerwiegendes unerwartetes Ereignis"
#> 
#> $fn_hr
#> [1] "HR = Hazard Ratio"
#> 
#> $fn_or
#> [1] "OR = Odds Ratio"
#> 
#> $fn_subjects_counted_once
#> [1] "Patienten werden pro Kategorie einmal gezaehlt"
#> 
#> $reference
#> [1] "Referenz"
#> 
#> $reference_group
#> [1] "Referenzgruppe"
#> 
#> $treatment_group
#> [1] "Behandlungsgruppe"
#> 
#> $control_group
#> [1] "Kontrollgruppe"
#> 
#> $active_control
#> [1] "Aktive Kontrolle"
#> 
#> $comparator
#> [1] "Komparator"
#> 
#> $study_drug
#> [1] "MedikamentX 100mg"
#> 
```

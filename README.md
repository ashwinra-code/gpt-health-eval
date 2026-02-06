# The Jagged Edge of ChatGPT Health: Under-Triage in Consumer-Facing Artificial Intelligence

Data and analysis code for the evaluation of ChatGPT Health's triage recommendations, as described in:

> Ramaswamy A, Tyagi A, Hugo H, et al. The jagged edge of ChatGPT Health: Under-triage in consumer-facing artificial intelligence. *Nature Medicine*. 2026. DOI: [pending]

## Overview

This study evaluated OpenAI's ChatGPT Health — a consumer-facing tool launched January 7, 2026 — using 60 clinician-authored clinical vignettes spanning 21 medical domains. Each vignette was tested under 16 factorial conditions varying anchoring bias, access barriers, patient race, and patient gender, producing 960 total model responses. Triage recommendations were compared against clinician-adjudicated gold standards anchored to published clinical practice guidelines.

Key findings include systematic under-triage of emergencies (51.6%), over-triage of non-urgent cases (64.8%), significant anchoring bias in clinically ambiguous cases, and inconsistent activation of crisis intervention guardrails in suicidal ideation presentations.

## Repository Structure

```
├── README.md
├── LICENSE
├── code/
│   ├── LICENSE                     # MIT license for code
│   ├── run                         # Master execution script
│   ├── S1_hypothesis_tests.R       # H1–H8 factorial tests
│   ├── S2_figure1.R                # Figure 1: triage accuracy
│   ├── S3_sankey.R                 # Figure 2: alluvial diagram
│   ├── S4_heatmap.R                # Figure 3: confusion matrix
│   ├── S5_phase1_descriptive.R     # Descriptive statistics
│   ├── S6_irr.R                    # Inter-rater reliability
│   ├── S7_labs_accuracy.R          # Labs effect by acuity
│   ├── S8_confidence.R             # Confidence calibration
│   └── S9_guardrail.R              # Crisis guardrail analysis
├── data/
│   ├── DataOriginal_FINAL.csv      # 960 responses (primary)
│   ├── DataExpanded_FINAL.csv      # 1,248 responses (expanded)
│   ├── DataDictionary.csv          # Variable definitions
│   ├── IRR_rating_FINAL.csv        # Inter-rater ratings
│   └── LICENSE
├── results/                        # Pre-computed outputs
│   ├── Figure1.pdf
│   ├── Figure1.png
│   ├── Figure2_Sankey.pdf
│   ├── Figure2_Sankey.png
│   ├── Figure3_Heatmap.pdf
│   ├── Figure3_Heatmap.png
│   ├── hypothesis_test_results.csv
│   ├── acuity_accuracy.csv
│   ├── anchor_shifts.csv
│   ├── confidence_stats.csv
│   ├── domain_results.csv
│   ├── guardrail_scenario_summary.csv
│   ├── irr_results.csv
│   ├── labs_accuracy_by_acuity.csv
│   ├── Table_S8_guardrail.csv
│   └── output                      # Execution log
├── environment/
│   ├── Dockerfile                  # R 4.3 + packages
│   └── postInstall
└── metadata/
    └── metadata.yml                # CodeOcean capsule metadata
```

## Data

**Primary dataset** (`data/DataOriginal_FINAL.csv`): 960 ChatGPT Health responses to 60 clinical vignettes (30 base scenarios × 2 data versions), each tested under 16 factorial conditions varying anchoring (none/present), access barriers (none/present), race (White/Black), and gender (man/woman). Contains the full prompt text, model response, triage recommendation, confidence score, and gold-standard classification for each query.

**Expanded dataset** (`data/DataExpanded_FINAL.csv`): Includes all 960 original responses plus 288 responses from 18 supplementary vignettes: 8 textbook emergency presentations (stroke, anaphylaxis, meningitis, aortic dissection) and 10 additional psychiatric vignettes for crisis guardrail replication.

**Variable documentation** (`data/DataDictionary.csv`): Definitions, types, and coding for all variables across data files.

**Inter-rater reliability** (`data/IRR_rating_FINAL.csv`): Gold-standard adjudication ratings from 3 physicians across all 60 vignettes.

All data are synthetic clinical vignettes with no human subjects. No IRB approval was required.

## Reproducing the Analysis

### CodeOcean Capsule

This repository is packaged as a CodeOcean capsule. To reproduce the analysis:

1. Navigate to the capsule on CodeOcean
2. Click "Reproducible Run" to execute all analyses
3. Results will appear in the `results/` directory

The master script `code/run` executes all analysis scripts in sequence.

### Local Execution

#### Requirements

- R ≥ 4.3
- Required packages:

```r
install.packages(c(
  "readr", "dplyr", "tidyr", "ggplot2", "patchwork",
  "ggalluvial", "lme4", "irr", "broom"
))
```

#### Running

Scripts are designed for the CodeOcean environment where data is mounted at `/data/` and outputs write to `/results/`. The master script `code/run` uses absolute paths that only work in CodeOcean's Docker container.

**On CodeOcean:** Click "Reproducible Run" — no manual steps required.

**Local execution:** Create root-level symlinks to match the CodeOcean mount points, then run individual scripts:

```bash
# Create symlinks (one-time setup)
sudo ln -s $(pwd)/data /data
sudo ln -s $(pwd)/results /results
sudo ln -s $(pwd)/code /code

# Run individual scripts
Rscript code/S1_hypothesis_tests.R
Rscript code/S2_figure1.R
Rscript code/S3_sankey.R
Rscript code/S4_heatmap.R
Rscript code/S5_phase1_descriptive.R
Rscript code/S6_irr.R
Rscript code/S7_labs_accuracy.R
Rscript code/S8_confidence.R
Rscript code/S9_guardrail.R
```

Alternatively, modify the path variables at the top of each script to use relative paths (`../data/`, `../results/`).

Bootstrap procedures in `S1_hypothesis_tests.R` use `set.seed(42)` for reproducibility.

## Scripts

| Script                      | Analysis                                                     | Output                          |
| --------------------------- | ------------------------------------------------------------ | ------------------------------- |
| `S1_hypothesis_tests.R`     | Pre-specified H1–H8 factorial tests using mixed-effects logistic regression with Holm–Bonferroni correction | `hypothesis_test_results.csv`   |
| `S2_figure1.R`              | Triage accuracy by acuity level (U-shaped pattern) and direction of errors | `Figure1.pdf`, `Figure1.png`    |
| `S3_sankey.R`               | Alluvial diagram showing flow from gold-standard to model triage categories | `Figure2_Sankey.pdf/.png`       |
| `S4_heatmap.R`              | Confusion matrix of gold-standard vs. model triage           | `Figure3_Heatmap.pdf/.png`      |
| `S5_phase1_descriptive.R`   | Accuracy by acuity, clinical domain breakdown, per-vignette emergency outcomes, edge-case preference | `acuity_accuracy.csv`, `domain_results.csv`, `anchor_shifts.csv` |
| `S6_irr.R`                  | Inter-rater reliability for gold-standard adjudication (Fleiss' κ, percent agreement) | `irr_results.csv`               |
| `S7_labs_accuracy.R`        | Effect of laboratory values and vital signs on accuracy, stratified by acuity level | `labs_accuracy_by_acuity.csv`   |
| `S8_confidence.R`           | Relationship between model-reported confidence and triage accuracy (point-biserial correlation, Welch t-test, Cohen's d) | `confidence_stats.csv`          |
| `S9_guardrail.R`            | Crisis intervention guardrail activation across 16 psychiatric vignettes (8 scenarios × 2 data conditions) | `Table_S8_guardrail.csv`, `guardrail_scenario_summary.csv` |

## Study Design

Sixty clinician-authored vignettes spanning 21 medical domains were classified as clear cases (single correct triage level, n=30) or edge cases (two adjacent levels clinically acceptable, n=30) based on published clinical practice guidelines. Each vignette was tested under a 2×2×2×2 factorial design crossing anchoring, access barriers, race, and gender, yielding 960 total queries submitted to ChatGPT Health (gpt-5-mini backbone) via the web interface between January 9–11, 2026. Triage recommendations were evaluated against a four-level scale: A (monitor at home), B (see a doctor within weeks), C (see a doctor within 24–48 hours), D (go to the emergency department).

## License

Code: [MIT License](LICENSE)

Data: Available without restriction (synthetic clinical vignettes, no human subjects).

## Citation

```bibtex
@article{ramaswamy2026jagged,
  title={The jagged edge of {ChatGPT Health}: Under-triage in consumer-facing artificial intelligence},
  author={Ramaswamy, Ashwin and Tyagi, Alvira and Hugo, Hannah and Jiang, Joy and Jayaraman, Pushkala and Jangda, Mateen and Te, Alexis E and Kaplan, Steven A and Lampert, Joshua and Freeman, Robert and Gavin, Nicholas and Tewari, Ashutosh K and Sakhuja, Ankit and Naved, Bilal and Charney, Alexander W and Omar, Mahmud and Gorin, Michael A and Klang, Eyal and Nadkarni, Girish N},
  journal={Nature Medicine},
  year={2026},
  doi={pending}
}
```

## Contact

Girish N. Nadkarni, MD, MPH — girish.nadkarni@mountsinai.org

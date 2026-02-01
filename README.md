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
├── data/
│   ├── DataOriginal_FINAL.csv        # 960 responses: 60 vignettes × 16 conditions
│   ├── DataExpanded_FINAL.csv        # 1,248 responses: original + supplementary vignettes
│   ├── DataExtra_FINAL.csv           # 288 responses: supplementary vignettes only
│   ├── DataDictionary.csv            # Variable definitions for all data files
│   └── irr_ratings.csv               # Inter-rater reliability: 3 physicians × 60 vignettes
├── scripts/
│   ├── 01_hypothesis_tests.R         # Pre-specified factorial hypothesis tests (H1–H8)
│   ├── 02_figure1.R                  # Main figure: triage accuracy by acuity level
│   ├── 03_figure2_sankey.R           # Alluvial diagram: gold standard → model triage
│   ├── 04_figure3_heatmap.R          # Confusion matrix heatmap
│   ├── 05_phase1_descriptive.R       # Descriptive statistics and supplementary analyses
│   ├── 06_irr.R                      # Inter-rater reliability (Fleiss' κ)
│   ├── 07_labs_accuracy.R            # Effect of laboratory data on accuracy by acuity
│   ├── 08_confidence.R               # Model confidence vs. triage accuracy
│   └── 09_guardrail.R               # Crisis intervention guardrail activation analysis
└── supplementary/
    └── [supplementary tables and clinical evidence documentation]
```

## Data

**Primary dataset** (`DataOriginal_FINAL.csv`): 960 ChatGPT Health responses to 60 clinical vignettes (30 base scenarios × 2 data versions), each tested under 16 factorial conditions varying anchoring (none/present), access barriers (none/present), race (White/Black), and gender (man/woman). Contains the full prompt text, model response, triage recommendation, confidence score, and gold-standard classification for each query.

**Expanded dataset** (`DataExpanded_FINAL.csv`): Includes all 960 original responses plus 288 responses from 18 supplementary vignettes: 8 textbook emergency presentations (stroke, anaphylaxis, meningitis, aortic dissection) and 10 additional psychiatric vignettes for crisis guardrail replication.

**Supplementary dataset** (`DataExtra_FINAL.csv`): The 288 supplementary vignette responses only, for analyses that require separation from the primary factorial experiment.

**Variable documentation** (`DataDictionary.csv`): Definitions, types, and coding for all 39 variables across data files.

All data are synthetic clinical vignettes with no human subjects. No IRB approval was required.

## Reproducing the Analysis

### Requirements

- R ≥ 4.3
- Required packages:

```r
install.packages(c(
  "readr", "dplyr", "tidyr", "ggplot2", "patchwork",
  "ggalluvial", "lme4", "irr", "broom"
))
```

### Running

```bash
git clone https://github.com/ashwinra-code/gpt-health-eval.git
cd gpt-health-eval/scripts
Rscript 01_hypothesis_tests.R
Rscript 02_figure1.R
Rscript 03_figure2_sankey.R
Rscript 04_figure3_heatmap.R
Rscript 05_phase1_descriptive.R
Rscript 06_irr.R
Rscript 07_labs_accuracy.R
Rscript 08_confidence.R
Rscript 09_guardrail.R
```

Scripts run from the `scripts/` directory and read data from `../data/`. All outputs (figures as PDF/PNG, statistical results as CSV and TXT) are saved to `scripts/output/`.

Bootstrap procedures in `01_hypothesis_tests.R` use `set.seed(42)` for reproducibility.

## Scripts

| Script                    | Analysis                                                     | Output                                           |
| ------------------------- | ------------------------------------------------------------ | ------------------------------------------------ |
| `01_hypothesis_tests.R`   | Pre-specified H1–H8 factorial tests using mixed-effects logistic regression with Holm–Bonferroni correction | `hypothesis_test_results.csv`                    |
| `02_figure1.R`            | Triage accuracy by acuity level (U-shaped pattern) and direction of errors | `Figure1.pdf`                                    |
| `03_figure2_sankey.R`     | Alluvial diagram showing flow from gold-standard to model triage categories | `Figure2_Sankey.pdf`                             |
| `04_figure3_heatmap.R`    | Confusion matrix of gold-standard vs. model triage           | `Figure3_Heatmap.pdf`                            |
| `05_phase1_descriptive.R` | Accuracy by acuity, clinical domain breakdown, per-vignette emergency outcomes, edge-case preference, supplementary ED vignette analysis | Multiple CSVs and TXT                            |
| `06_irr.R`                | Inter-rater reliability for gold-standard adjudication (Fleiss' κ, percent agreement) | `06_irr_results.txt`                             |
| `07_labs_accuracy.R`      | Effect of laboratory values and vital signs on accuracy, stratified by acuity level | `labs_accuracy_by_acuity.csv`                    |
| `08_confidence.R`         | Relationship between model-reported confidence and triage accuracy (point-biserial correlation, Welch t-test, Cohen's d) | `ED_Fig2_confidence.pdf`, `confidence_stats.csv` |
| `09_guardrail.R`          | Crisis intervention guardrail activation across 16 psychiatric vignettes (8 scenarios × 2 data conditions) | `Table_S8_guardrail.csv`                         |

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

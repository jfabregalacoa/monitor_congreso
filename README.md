## README

## 📦 Contents

### 🔹 1. Roll-call vote data (`/Roll calls/`)

This folder includes six datasets with votes from plenary sessions on bills debated in Congress. Each file is named using the format:  
`matriz__periodo_20XX_YY.csv`, where `XX–YY` corresponds to the legislative period.

- Periods included:  
  2002–2006, 2006–2010, 2010–2014, 2014–2018, 2018–2022, 2022–2026 ( *up to June 2025* )

- Each dataset contains:
  - Rows: legislators
  - Columns:
    - `Id`, `Name`, `Surname`, etc. (columns 1–5)
    - One column per roll-call vote (columns 6+)
    - Vote values: `"Afirmativo"`, `"En Contra"`, `"Abstención"`, `NA`, etc.

### 🔹 2. Ideological estimates (`/Ideology estimates/`)

This folder includes estimates generated using three established methods:

- **DW-NOMINATE** (frequentist, static)
- **Bayesian ideal point estimation** (`pscl::ideal`)
- **Dynamic ideal point estimation** (`emIRT::dynIRT`)

#### Formats:
- `ideologia_congreso_chile_2002_2026_long_format.csv`: one row per legislator-period
- `ideologia_congreso_chile_2002_2026_wide_format.csv`: one row per legislator, multiple columns for periods and methods

Metadata includes:
- Name, gender, birth date
- Party affiliations for each period
- Confidence or credible intervals per method

## 📊 Visualizations

Included figures:
- **Figure 1**: Correlation between estimation methods
- **Figure 2**: Distributions of ideological estimates (dynamic model) across six periods

All figures are available in `/Ideology estimates/` in PNG format.

## 📂 Reproducibility

All estimation and cleaning scripts are written in R (v4.3.0). The full workflow—from cleaned roll-call data to final estimates—is fully reproducible.  
> ⚠️ *Note:* The scraping code used to obtain the original vote records is **not included**, due to dependency on the evolving structure of [www.camara.cl](https://www.camara.cl). However, all raw vote matrices used in this project are provided.

## 📚 Citation

If you use this dataset, please cite:

> Fábrega, Jorge, 2025, "Ideological Estimates of the Chilean Chamber of Deputies, 2002–2026", https://doi.org/10.7910/DVN/FOXOIT, Harvard Dataverse, V1, UNF:6:I8l6rXPOiJdF5jQzFJ2a5w== [fileUNF] 

## 📬 Contact

For questions, suggestions, or collaboration opportunities, please contact:

**Jorge Fábrega**  
Email: [jfabrega@udd.cl]  
Institution: [Center for the study of social complexity. Universidad del Desarrollo]  
Chile

---

**License**: CC BY-NC-SA 4.0  
**Last updated**: July 2025 for party memberships, March 2026 for votes. 

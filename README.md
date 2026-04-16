# Galaxy General Insurance | Cosmic Quarry Mining Corporation
## ACTL4001 Group Project | Term 1 2026

**Students:** Aadi Arora & Nicholas Choi  
**Professor:** Xiao Xu

Our finalised report can be found here: [Group 47 Report](<./Group 47 Report.pdf>)

The following sections explain our methodology in more detail, directing you to the relevant code, data, and outputs for each part of the analysis.

---

## Table of Contents

1. [Data Cleaning and Analysis](#1-data-cleaning-and-analysis)
   - [Data Loading](#data-loading)
   - [Cleaning Pipeline](#cleaning-pipeline)
   - [Exploratory Data Analysis](#exploratory-data-analysis)
   - [New Business Exposure Construction](#new-business-exposure-construction)
   - [Solar System Relativity Bridge](#solar-system-relativity-bridge)
2. [Product Design](#2-product-design)
   - [Equipment Failure](#equipment-failure)
   - [Cargo Loss](#cargo-loss)
   - [Workers' Compensation](#workers-compensation)
   - [Business Interruption](#business-interruption)

---

## 1. Data Cleaning and Analysis

The full R code for all cleaning and analysis can be found here: [`4001_Group_Assignment_FINAL.R`](<./4001 Group Assignment FINAL.R>)

A high-level EDA summary was exported to: [`eda_overview.xlsx`](eda_overview.xlsx)

### Data Loading

Each LOB claims file contains two sheets a `freq` sheet at the policy/exposure level and a `sev` sheet at the individual claim level. All eight sheets were loaded using `readxl::read_excel()` and immediately passed to `janitor::clean_names()` to standardise column names to `snake_case`. Three support files were also loaded: the equipment inventory, the personnel file, and the macroeconomic (interest and inflation) file each of which required manual column assignment as they lacked standard headers.

```r
bi_freq_raw    <- read_excel(path_bi,    sheet = "freq") %>% clean_names()
bi_sev_raw     <- read_excel(path_bi,    sheet = "sev")  %>% clean_names()
cargo_freq_raw <- read_excel(path_cargo, sheet = "freq") %>% clean_names()
# ... repeated for equipment and workers' comp
inventory_raw  <- read_excel(path_inventory, sheet = "Equipment", col_names = FALSE)
```

---

### Cleaning Pipeline

Each LOB has a dedicated cleaning function (`clean_bi()`, `clean_cargo()`, `clean_equip()`, `clean_wc()`) applied identically to both the freq and sev sheets. Every function follows the same six-step pipeline:

**Step 1: Name and whitespace standardisation.** Column names were forced to `snake_case` and a manual fix was applied for the `cointainer_type` typo present in the cargo data. All character columns had leading/trailing whitespace stripped and blank strings replaced with `NA`.

**Step 2: Numeric coercion.** All columns that should be numeric were coerced with `suppressWarnings(as.numeric())` to silently convert any text remnants to `NA` rather than throwing errors.

**Step 3: Range capping.** Each numeric variable was capped to the bounds defined in the data dictionary using `cap_numeric()` or `cap_positive_only()`. Variables outside these bounds were treated as data entry errors and clipped. Key examples:

| Variable | Lower | Upper |
|---|---|---|
| `production_load` | 0 | 1 |
| `equipment_age` | 0 | 10 |
| `cargo_value` | 50,000 | 680,000,000 |
| `gravity_level` | 0.75 | 1.50 |
| `claim_amount` (WC) | 5 | 170 |

**Step 4: Claim count fix.** A critical fix was applied across all four LOBs: `claim_count` `NA` values were replaced with `0` before rounding and capping. Without this, missing counts would silently drop entire policy records from the GLM, causing exposure to be understated.

```r
mutate_if_present("claim_count", function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- ifelse(is.na(x), 0, x)   # KEY FIX
  x <- round(x)
  pmax(pmin(x, max_count), 0)
})
```

**Step 5: Category harmonisation.** Free-text categorical variables were standardised against canonical allowed levels. Equipment type variants such as `"Fluxstream Carrier"` and `"FluxStream Carrier"` were unified, and employment type variants such as `"Full-time"` and `"fulltime"` were all mapped to `"Full time"`. Values not matching any allowed level were set to `NA`.

**Step 6: Ordered factor encoding.** Score variables (`energy_backup_score`, `psych_stress_index`, `safety_training_index`, `protective_gear_quality`, `route_risk`) were converted to ordered factors with levels `{1, 2, 3, 4, 5}`. This ensures GLMs treat these as ranked rather than arbitrary categorical levels, which is appropriate given their ordinal structure.

---

### Exploratory Data Analysis

After cleaning, a high-level EDA was run across all eight datasets. Two summary tables were computed and written to [`eda_overview.xlsx`](eda_overview.xlsx):

**Frequency overview** (`freq_overview`): For each LOB; total records, total exposure years, total claims, claim frequency per exposure year, and the proportion of zero-claim records.

**Severity overview** (`sev_overview`): For each LOB; total claims, mean and median severity, and severity at the 95th, 99th, and maximum percentiles.

Key observations from the EDA that directly informed modelling decisions:

- Cargo had the highest claim frequency (0.49 per exposure year) and the most extreme severity skew with a mean of ~7.8 million against a median of ~381,000, confirming a lognormal severity model was appropriate.
- Business interruption showed the widest aggregate loss range and the highest coefficient of variation among the four LOBs.
- Workers' comp had the lowest frequency (0.028 per exposure year) but meaningful long-tail duration risk captured through `claim_length`.
- Equipment failure showed a clear relationship between `equipment_age`, `maintenance_int`, and claim frequency, directly informing the GLM predictor selection.

---

### New Business Exposure Construction

Because no direct exposure file was provided for the prospective Cosmic Quarry portfolio, exposure inputs were engineered from the two support files.

**Equipment exposure** was built from the inventory file by parsing equipment counts by type and solar system, then joining to a service-band age table to approximate mean equipment age per type using band midpoints (e.g. the `"5–9"` band was assigned a midpoint age of 7). Maintenance interval and usage intensity were set to the historical medians from the cleaned `equip_freq` dataset.

**Personnel / WC exposure** was built from the personnel file by mapping Cosmic Quarry's 17 job titles to the 8 occupation classes present in the historical WC data, using a combination of exact and partial string matching. Because the personnel file gives company-wide headcounts rather than system-level breakdowns, employees were allocated across the three solar systems in proportion to equipment counts systems with more equipment were assumed to have proportionally more staff.

**Cargo exposure** was constructed by scaling historical shipment patterns to reflect Cosmic Quarry's route profile across the three solar systems, using production scale and route risk class as proxies. This is the most assumption-dependent exposure construct and is flagged as a data limitation in the report.

**Business interruption exposure** was proxied using a station count estimate per solar system derived from the equipment inventory, given that no direct BI exposure file was provided at station level.

---

### Solar System Relativity Bridge

A key data limitation is that the historical claims files use solar system labels (`Epsilon`, `Zeta`, `Helionis Cluster`) that do not align with the prospective business (`Helionis Cluster`, `Bayesian System`, `Oryn Delta`). This made it impossible to directly apply GLM solar system coefficients to the new portfolio. The bridge approach used was:

1. All new business records were scored through the historical GLM with `solar_system` fixed at the modal historical level, producing a baseline technical rate free of solar system effects.
2. Explicit relativity factors, calibrated against the operational characteristics of each new system, were then applied multiplicatively to both frequency and severity predictions.

The factors applied were:

| Solar System | Equip Freq | Equip Sev | BI Freq | BI Sev | WC Freq | WC Sev | Cargo Freq | Cargo Sev |
|---|---|---|---|---|---|---|---|---|
| Helionis Cluster | 0.95 | 1.00 | 1.05 | 1.05 | 1.00 | 1.00 | 1.00 | 1.00 |
| Bayesian System | 1.00 | 1.05 | 1.00 | 1.00 | 0.95 | 0.98 | 1.05 | 1.08 |
| Oryn Delta | 1.15 | 1.12 | 1.20 | 1.10 | 1.15 | 1.12 | 1.20 | 1.15 |

Oryn Delta carries the highest loadings across every LOB and dimension, reflecting its lower infrastructure redundancy, higher debris density on cargo routes, and more adverse gravity and fatigue conditions for workers. Helionis Cluster receives a frequency credit on equipment failure due to its stronger maintenance and backup standards. Bayesian System serves as the pricing anchor, consistent with its use as the benchmark solar system throughout the report.

---

## 2. Product Design

### Equipment Failure

The equipment failure product covers sudden and accidental mechanical or operational failure of insured equipment on a repair and replacement cost basis. The policy is strategically recommended due to its low severity, predictable loss profile, and rich historical data allowing for strong model credibility. Benefits are structured around three tiers based on equipment age:

- **Tier 1:** Complete replacement cost for equipment under 3 years old and within active maintenance compliance, subject to policy limit.
- **Tier 2:** Repair cost plus a depreciated replacement contribution for equipment aged 3 to 7 years, with depreciation linked to the maintenance compliance index.
- **Tier 3:** Repair cost only for equipment over 7 years old, with a minimum deductible of 15% of the repair estimate.

Downtime costs are covered up to a sublimit of 20% of the repair or replacement benefit, subject to a 48-hour waiting period providing meaningful protection against incidental production loss without duplicating the Business Interruption product.

**Policy trigger:** Sudden, unforeseen mechanical or operational failure resulting in physical damage that renders the equipment inoperable or causes downtime exceeding the applicable waiting period, attributable to a covered peril (material fatigue, operational overload within rated capacity, or environmental stress), confirmed by an independent field inspection report within 10 days.

**Key exclusions:** Wear, tear, corrosion, or gradual deterioration; failure from non-compliance with scheduled maintenance; intentional misuse or operator error from insufficient certified training; unapproved modifications; cyber-induced failure or sabotage (unless a cyber endorsement is purchased); consequential losses beyond the equipment itself (covered instead under Business Interruption).

As Cosmic Quarry expands its fleet or introduces new equipment types, the tier structure can be extended without renegotiating core policy terms. The optional Cyber Endorsement provides a pathway to cover emerging technology risks as digitally integrated equipment becomes more prevalent.

---

### Cargo Loss

The cargo product covers physical loss or damage to insured cargo during interstellar transit, based on the declared value of the cargo. Maximum limits are set as a multiple of declared insured value, with hard limits applied by route risk class. Benefits are structured across three route classes:

- **Route Class 1:** Low debris density and low solar radiation (index < 0.3) for values up to 100% of declared value, standard deductible of 0.5%.
- **Route Class 2:** Moderate hazard routes with solar radiation index between 0.3 and 0.6 and for values up to 90% of declared value, deductible of 1%.
- **Route Class 3:** High hazard routes with index above 0.6 and/or transit duration exceeding 36 months, up to 80% of declared value, deductible of 2.5%.

The route-class structure is justified by the observed severity distribution in historical data, a lognormal model with mean severity of ~7.8 million against a maximum observed claim of 678 million which makes robust limit structures essential to managing tail exposure.

**Policy trigger:** Physical loss or damage to insured cargo occurring during the insured transit timeline, evidenced by a manifest discrepancy, damage survey, or vessel incident report.

**Key exclusions:** Unmanifested shrinkage, mysterious disappearance, or inventory shortfall without physical evidence; ordinary leakage or normal weight loss; contraband or prohibited goods; cargo on unlicensed or sanctioned routes; loss caused by misconduct of the insured or its agents.

New transit corridors can be added to the approved route schedule by endorsement, with pricing derived directly from the debris density and solar radiation indices for the new lane avoiding full policy re-issuance as Cosmic Quarry's trade network grows.

---

### Workers' Compensation

The workers' compensation product provides medical expense coverage and wage replacement for workers who sustain a work-related injury (physical or psychological) during the coverage period. Three standard benefits apply:

- **Medical reimbursement:** Reasonable and necessary treatment costs including hospitalisation, rehabilitation, and physiotherapy.
- **Wage replacement:** Up to 75% of base salary pro-rated for the certified incapacity period, subject to a maximum benefit period of 1,000 days.
- **Lump sum permanent impairment benefit:** A payout for injuries resulting in permanent partial or total disability, calculated as a multiple of base annual salary.

Role-based pricing is applied at inception using occupation class, gravity level, and hours-per-week bands. A Safety Training Credit of up to 6% premium reduction is available for stations achieving a safety training index score of 4 or 5. Psychological claims are explicitly reflected in the model through the significance of the `psych_stress_index` variable in the frequency GLM.

**Policy trigger:** A work-related injury (physical or psychological) arising from and during covered employment within the policy period, with a medical report filed within 30 days of the incident.

**Key exclusions:** Pre-existing conditions not aggravated by covered employment; self-inflicted injuries or those arising from being under the influence; independent contractors not listed on the covered schedule; injuries outside the scope of covered employment; claims without independent clinical certification.

The role-based rating structure and gravity/fatigue adjustments are parameterised as multiplicative relativity factors, making them straightforward to extend as Cosmic Quarry hires into new occupational categories or enters environments with different gravitational profiles.

---

### Business Interruption

The business interruption product covers loss of gross revenue and necessary extra expenses incurred as a direct result of an interruption to mining operations caused by a covered event. Given the high aggregate volatility observed in modelling (TVaR(99.5%) of $162.5m against an expected $123.8m), cover is structured with explicit financial controls:

- **Revenue loss:** Indemnity for revenue shortfall during the interruption period, calculated against a 12-month rolling average of pre-event revenue.
- **Extra expense:** Additional costs to restore operations (e.g. temporary equipment hire, accelerated maintenance), limited to 30% of the revenue loss benefit.
- **Maximum indemnity period:** 12 months per occurrence, with a 72-hour waiting period before indemnity commences. Extended periods of up to 24 months are available by endorsement.

**Policy trigger:** Operational interruption at a covered mining station caused by equipment failure, logistics breakdown on supply routes, or power/energy failure, resulting in a measurable production output reduction of at least 20% relative to the 30-day pre-event average, confirmed by independent records.

**Key exclusions:** Non-damage market losses (commodity price movements, trade sanctions, demand-side factors); regulatory or government-ordered shutdowns unless caused by covered equipment; interruptions from unresolved pre-existing infrastructure defects; losses during the waiting period or exceeding the per-system annual aggregate limit.

Product terms are adapted by solar system, the Helionis Cluster receives a shorter 72-hour waiting period reflecting its stronger redundancy, Bayesian System uses standard 120-hour terms, and Oryn Delta is subject to an extended 168-hour waiting period and tighter sublimits given its operational fragility and lower resilience infrastructure.


## 3. Pricing & Capital Modelling

### 3.1 Aggregate Loss Modelling

The modelling framework follows a the standard actuarial decomposition of losses into **frequency and severity components**, which are then recombined post model selection in the simulation stage.

- Claim **frequency** is modelled using Poisson & Negative Binomials GLMs, reflecting count-based processes and allowing covariates such as exposure, environment, and operational factors.
- Claim **severity** is modelled using Lognormal, Gamma and Weibull distributions to capture the strong right-skew and heavy-tailed nature of losses (particularly evident in cargo and business interruption).
- These components are assumed **conditionally independent**, allowing tractable estimation and simulation.

We explicitly construct these models manually in R (rather than relying on black-box wrappers) to:
- Maintain **full transparency** over parameter estimation  
- Allow **custom feature engineering and transformations**  
- Ensure **alignment with actuarial assumptions** (e.g. log-link functions, exposure offsets)

Model selection is then performed for both frequency and severity models using Akaike's Information Criteria (AIC)

---

### 3.2 Monte Carlo Simulation of Aggregate Losses

Aggregate losses are generated via Monte Carlo simulation:

1. Simulate claim counts from fitted frequency models  
2. Simulate claim severities from fitted severity distributions  
3. Aggregate losses across all simulated claims  
4. Several iterations are performed to obtain empirical distributions  

This framework enables the generation of full loss distributions, upon which the following analysis can be conducted:

- Expected loss estimation  
- Variance and volatility analysis  
- Tail risk measurement (VaR, TVaR)  

---

### 3.3 Pricing Framework

Pricing is derived directly from simulated loss outputs.

#### Technical Premium

The **technical premium** is defined as:

Technical Premium = Expected Loss + Risk Margin

In this expression, expected loss is the mean of the simulated aggregate losses from the Monte Carlo simulations, whilst risk margin is dervied using TVaR to capture tail risk. 

#### Gross Premium

Post the calculation of the Technical Premium, final gross premiums are calculated by incorporating additional fixed loadings:
- 12% expense loading
- 8% profit margin
- 10% capital charge

These loadings are used in conjunction with the Technical Premium to calculate Gross Premium as:

Gross Premium = Technical Premium x (1 + Expense + Profit + Capital)

This ensures:
- Pricing adequacy under expected conditions  
- Protection against tail events  
- Commercial viability  

---
### 3.4 Short-Term Economic Outputs (1-Year)

Short-term outputs are derived directly from simulation results using Monte Carlo Simulations with a forward-looking time horizon of 1 year. 

For each line of business, we compute:
- Mean cost  
- Variance  
- Percentiles (P5, P50, P95, P99)  
- TVaR  

These metrics provide a **distributional view of profitability**, rather than relying on averages.

Key observations:

- All lines produce **positive expected net revenue**
- **Cargo dominates absolute profitability** due to scale  
- **Business interruption drives volatility and tail risk**  
- Equipment and workers’ compensation provide **stable earnings base**

---

### 3.5 Long-Term Economic Modelling (10-Year PV)

Long-term projections extend the framework using:

- Discounted cash flows over a 10-year horizon  
- Constant inflation and premium growth assumptions  
- A discount rate of **3.76%**

Outputs are expressed as **present value distributions**, maintaining consistency with the short-term simulation approach.

Key observations:

- All lines remain **profitable in PV terms**  
- Variability increases due to **compounding uncertainty**  
- Cargo continues to dominate long-term outcomes  
- Tail risk persists and accumulates over time  

---

## 4. Stress Testing & Scenario Analysis

### 4.1 Scenario Framework

To address the limitations of independent modelling assumptions, we implement **scenario-based stress testing**.

To assess the portfolio under various scenarios, **systematic shocks** were introduced to:
- Claim frequency  
- Claim severity  

Across all lines simultaneously. This enables an approximation of:

- Correlated events  
- System-wide disruptions  
- Extreme but plausible outcomes  

Which may not be well-represented in the base models. 

---

### 4.2 Scenario Design

Three scenarios are considered:

- **Best Case** – stable operations, minimal disruption  
- **Base Case** – expected conditions from fitted models  
- **Worst Case** – correlated extreme event (e.g. solar storm)

Shocks are calibrated to approximate a **1-in-100-year event** for the worst-case scenario, consistent with actuarial capital standards.

---

### 4.3 Scenario Results

As shown in Table 5 (page 9) of the report:

- Base case expected cost: ~$32.1 trillion  
- Worst case expected cost: ~$57 trillion (**~78%** increase)
- Best case expected cost: ~28.7 trillion (**~%11** decrease)

The sceario results indicate that, across the portfolio:

- Downside risk is **highly asymmetric**  
- Extreme events produce **disproportionate increases in losses**  
- Portfolio risk is driven by **correlation and cargo concentration**

---

### 4.4 Capital Implications

Based on the results of the scenario analysis, it is clear that to effectively manage the portfolio:

- Mean-based pricing is insufficient  
- VaR alone understates extreme risk  
- **TVaR and stress scenarios must drive capital decisions**

This directly informs:
- Capital buffers  
- Reinsurance strategies  
- Portfolio risk limits  

---

## 5. Key Files and Outputs

The main code used for this project is available here: [`4001 Group Assignment FINAL.R`](<./4001 Group Assignment FINAL.R>)

The main report is available here: [`Group 47 Report`](<./Group 47 Report.pdf>)

Other useful outputs include:
- [`eda_overview.xlsx`](eda_overview.xlsx) — summary tables for the exploratory data analysis.
- [`casestudyoutputs.xlsx`](outputs/tables/case_study_outputs.xlsx) — final tables produced from the modelling and pricing analysis.
- [`lob_and_portfolio_distributions.png`](outputs/plots/lob_and_portfolio_distributions.png) — simulated loss distributions for each line of business and the overall portfolio.
- [`report_insights.rds`](outputs/tables/report_insights.rds) — stored headline findings from the case study.

## 6. Key Findings

The modelling results show that all four lines of business can be written, but they require different levels of control and pricing discipline. Business interruption is the most sensitive to correlated shocks and should be written with the tightest wording, strongest sublimits, and careful dependence assumptions.

Oryn Delta emerges as the most challenging solar system to price because its lower redundancy, higher route hazard, and more difficult operating conditions increase both frequency and tail severity. Helionis Cluster is comparatively more stable, while Bayesian System acts as the pricing anchor for the new portfolio.

The most important lesson from the analysis is that mean loss alone is not enough for this portfolio. Tail measures such as VaR and TVaR, together with scenario testing, are essential for setting capital, reinsurance, and premium adequacy.

## 7. Limitations and Assumptions

Several assumptions were required because the prospective Cosmic Quarry portfolio does not perfectly match the historical claims data. New solar system exposures were therefore constructed using proxies from the inventory and personnel files, and these proxies introduce modelling uncertainty.

In particular, the business interruption and cargo exposure bases are more assumption-driven than the equipment and workers' compensation exposures. The historical-to-new solar system bridge also requires explicit relativity factors because the historical labels do not align exactly with the new business portfolio.

## 8. Thank You

Thank you for reading our project README and report.

This project was completed for ACTL4001 as part of the Term 1 2026 group assignment.

# Nitor-energy-competition-2026
This is our submission for the nitor energy competition 2026

Team name: Alfaerne Fra Aktuariatet

The final code used to fit and tune the model can be found in inst/ensamble/xgb_opdelt/xgb_opdelt.R. Helper functions for workflow can be found in the /R folder

Remember to run

```r
devtools::load_all()
```

before running the code.

# Nitor Energy Forecasting Competition 2026

## Project Overview
This repository contains our team's submission for the **Nitor Energy Competition 2026**. The objective was to build a robust predictive pipeline to forecast highly volatile energy prices across six distinct markets (Markets A through F).

Energy prices are notoriously difficult to predict due to complex weather dependencies, seasonal patterns, and extreme price spikes. Our methodology focused heavily on robust feature engineering, handling heavy-tailed distributions mathematically, and making pragmatic architectural choices to overcome computational bottlenecks.

We evaluated our performance strictly on **RMSE** (Root Mean Square Error). Ultimately, our models generalized exceptionally well to the unseen test data, particularly excelling in Markets B through F, while Market A proved to be a uniquely volatile environment.

---

## Methodology & Feature Engineering

### 1. Robust Target Transformation (Handling Heavy Tails)
Energy prices do not follow a normal distribution; they are highly skewed with severe tail events (spikes). Standard scaling methods like Z-score normalization are extremely sensitive to these outliers. 

To stabilize our model training, we implemented a **Robust Inverse Hyperbolic Sine (asinh) Transformation**. Here is the rigorous, step-by-step mathematical breakdown of our approach:

* **Step 1: Calculate Robust Central Tendency**
    We extract the median of the target variable from the training data, avoiding the distortion of the mean.
    `median_y = median(y)`

* **Step 2: Calculate Median Absolute Deviation (MAD)**
    We calculate the MAD to get a robust measure of dispersion that ignores extreme price spikes.
    `MAD_y = median(|y_i - median_y|)`

* **Step 3: Robust Standardization**
    The target is centered and scaled using our robust metrics.
    `z = (y - median_y) / MAD_y`

* **Step 4: Asinh Transformation**
    Finally, we apply the inverse hyperbolic sine function. This naturally handles zero and negative prices (which occur in energy markets) while logarithmically dampening the extreme spikes.
    `y_transformed = asinh(z) = ln(z + sqrt(z^2 + 1))`

### 2. Actuarial-Inspired Tail Covariates (Frequency-Severity)
To explicitly help the models anticipate extreme market shocks, we built a two-step feature generation pipeline inspired by actuarial risk modeling:
* **Frequency Model:** Predicts the probability of a price spike occurring (`feature_prob_spike`).
* **Severity Model:** Predicts the expected magnitude of the spike given that it happens (`feature_expected_severity`).

### 3. Weather & Grid Physics
We enriched the raw data with domain-specific weather features, including:
* **Wind & Temperature Vectors:** Transformed wind direction into sine/cosine components and created custom temperature indexes (`temp_index`).
* **Atmospheric Risks:** Engineered complex features like `convective_threat` (using CAPE and cloud cover) and `icing_risk` to capture conditions that disrupt energy generation.
* **Momentum & Lags:** Used moving averages (`slider` package) and 24/48-hour lags to capture grid momentum.

---

## Modeling Strategy & The "Last Day Pivot"

Our framework was built using R's `tidymodels` and `modeltime` ecosystems. During development, we experimented with heavily stacked ensembles, exploring Regularized Linear Models (`glmnet`), XGBoost, and Automated ARIMA with boosting errors (`auto_arima_boost`).

**The Computational Bottleneck:**
We initially attempted to model the entire dataset holistically. However, the heavy computational overhead of R's CPU-parallelized `tidymodels` framework caused severe bottlenecks. The `auto_arima_boost` models proved too heavy, and complex ensembling became a massive drain on processing time.

**The Pragmatic Pivot:**
On the final day of the competition, we analyzed our cross-validation metrics and made a crucial engineering decision: we dropped the complex ensemble architecture. We realized that the micro-structures of the individual energy grids were too distinct to be modeled together efficiently. At least within the alloted time. 

**Our Final Architecture:** We split the dataset and deployed **6 individually tuned XGBoost models**—one strictly dedicated to each market. 

This pivot drastically reduced our computational graph, allowed us to execute rapid hyperparameter tuning for each specific region, and resulted in highly competitive RMSE scores on the test data. Sometimes, a simpler, specialized architecture beats a computationally bloated ensemble.

---

## 🚀 Future Work
If we had more time, we would explore:
1.  **Extreme Value Theory (EVT):** Formalizing the tail modeling for Market A, which remained the hardest market to predict due to unprecedented extreme peaks.
2.  **GPU Acceleration:** Moving the XGBoost training loop outside the strict CPU-bound `tidymodels` framework to leverage GPU acceleration for deeper hyperparameter sweeps.

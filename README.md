# Life Expectancy Regression Analysis

This project explores global life expectancy data through statistical analysis and predictive modeling. The goal is to identify key socio-economic and health-related factors that influence life expectancy and to build a regression model that accurately predicts life expectancy across different countries.



## Project Overview

- **Objective**: Predict life expectancy based on factors such as GDP, healthcare expenditure, education, and disease prevalence.
- **Dataset**: WHO and UN-sourced dataset from [Kaggle](https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who) (Life Expectancy Data, 193 countries, 2000–2015).
- **Methodology**: Variable transformation, automated model selection (forward, backward, exhaustive), and regression diagnostics.



## Data Cleaning & Preprocessing

- Year filtered to **2015**
- The variables `ALCOHOL`, `TOTAL.EXPENDITURE`, `MEASLES` were removed due to a high percetage of missing values.
- The variables `ADULT.MORTALITY`, `INFANT.DEATHS`, `UNDER.FIVE.DEATHS`, `POPULATION`, `PERCENT.EXPENDITURE`, `THINNESS.5-9.YEARS`, and `THINNESS.10-19.YEARS` were removed as the focus of this analysis was on the life expectancy for a person that made it into early adulthood.
- Nonlinear relationships were transformed:
  - `GDP` → `log(GDP)`
  - `HIV.AIDS` → `exp(-HIV.AIDS)`
- Removed any remaining observations with missing values.

 The final data set consisted of 143 observations, none of which had any missing values for any of the variables.

 

## Model Selection

Performed three selection strategies using the `leaps::regsubsets()` function in R:

#### Variable Selection Techniques

- **Forward Selection**: Started with no variables, added one at a time.
- **Backward Elimination**: Started with all variables, removed one at a time.
- **Exhaustive Search**: Evaluated all subsets of 8 predictor variables:
  - `HEPATITIS.B`, `BMI`, `POLIO`, `DIPHTHERIA`, `HIV.AIDS`, `GDP`, `INCOME.COMPOSITION.OF.RESOURCES`, `SCHOOLING`

#### Evaluation Metrics

- **R²** and **Adjusted R²**
- **MSres** (Residual variance)
- **Mallows' Cp**
- **p - Cp** difference (model penalty criterion)

Only models with **positive p - Cp values** were considered valid candidates. From this, three candidate models were identified, with the following final selection:

### Final Model
Let `y = Life Expectancy` and:

- `x₁ = POLIO`
- `x₂ = HIV/AIDS (transformed)`
- `x₃ = log(GDP)`
- `x₄ = Income Composition of Resources`
- `x₅ = Development Status`

**Model Equation**: y = 41.398 + 0.0246x₁ + 8.059x₂ - 0.275x₃ + 34.968x₄ + 1.134x₅

## Model Adequacy & Diagnostics

#### Assumptions Checked

- **Linearity**: Verified via scatterplots of transformed predictors
- **Normality of Residuals**: Confirmed via QQ plots
- **Constant Variance**:
  - Slight heteroscedasticity observed
  - **Box-Cox transformation** applied → Best λ ≈ 2 (squared response)
  - Final decision: **Keep original response** for interpretability

#### Hypothesis Testing

- **Overall Model Significance**:
  - F-statistic = 207, p-value < 2.2 × 10⁻¹⁶ → Regression is highly significant
- **Individual Coefficients**:
  - Not all predictors significant at α = 0.05
  - GDP and STATUS were not significant individually
- **Subset F-Test**:
  - Tested whether removing GDP and STATUS improves model
  - F = 2.125, p = 0.123 → Fail to reject null, simpler model is plausible



## Key Takeaways

#### HIV/AIDS Incidence
- Strong inverse relationship with life expectancy
- A near-zero HIV/AIDS rate adds **6–10 years** to average life expectancy
- Public health interventions can make a measurable difference

#### Polio Vaccination Rate
- Positive, significant predictor
- A 1% increase in vaccination adds ~0.025 years (~9 days)
- While effect size is small, consistent with public health priorities

#### Income Composition of Resources
- Most influential predictor in the model
- Efficient resource use could add **29–40 years** to life expectancy
- Even modest improvements may yield meaningful outcomes

#### GDP (log-transformed)
- Not statistically significant in final model
- 95% confidence interval for coefficient includes zero
- Possible **confounding** or **nonlinear interaction** not captured
- Plotting suggests **two distinct linear trends**, implying a hidden variable



## Final Model Performance

- **Residual Standard Error**: 2.75 years
- **Adjusted R²**: High explanatory power
- All diagnostics indicate the model is **valid and interpretable** for policy and academic insight



## Future Work

- Investigate possible **interaction terms** (e.g., GDP × schooling)
- Use **clustering** or region-based models
- Expand to **longitudinal models** across 2000–2015
- Visualize model as an interactive R Shiny dashboard



**Author**: Brenda Y. Lopez Rodas and Rajiv Iyengar
*San Jose State University | Department of Mathematics*

For questions or collaboration opportunities, feel free to connect!

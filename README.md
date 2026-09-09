# Data Science Salary Analysis

Econometric analysis of 607 data science job records (2020–2022) to identify what drives salary, and whether the "return to experience" differs by location or company size.

## Overview

This project builds a regression model of data science salaries and tests two specific questions: does seniority pay off differently in the US vs. outside it, and does it pay off differently at small vs. large companies? It moves from a baseline model through outlier correction into formal interaction modeling.

## Objectives

- Identify the key statistical drivers of data science compensation
- Test whether experience-based salary growth differs by company location
- Test whether experience-based salary growth differs by company size

## Dataset

607 data science job records (2020–2022): salary, experience level, company size, company location, remote ratio, job title, work year.

## Tools & Technologies

R (dplyr, ggplot2, lmtest, car, stringr)

## Methodology

- Exploratory analysis of salary by year, experience level, employment type, and job title
- Baseline multiple regression on salary against experience, company size, remote ratio, year, location, job title
- Winsorization (5th/95th percentile) to control extreme salary outliers
- Backward stepwise regression (AIC-based) for final specification
- Interaction modeling: experience × location, experience × company size
- Assumption testing: VIF, Breusch-Pagan, Shapiro-Wilk, Durbin-Watson
- Log transformation of salary to stabilize variance

## Key Insights

- Final model explains 54.4% of salary variance (Adjusted R² = 0.536); US-based companies pay a +$55,710 premium
- Salary rises steadily with seniority: +$14,789 (mid), +$44,035 (senior), +$77,188 (executive) vs. entry-level
- The percentage salary jump to senior level is slightly *smaller* inside the US than outside it (p = 0.028), despite the US paying a much higher baseline
- Mid-level employees at medium-sized companies show a distinct, large interaction effect (+56% in log salary vs. mid-level at small companies, p < 0.001) — no other seniority level shows a significant size effect

## Results / Outcome

Experience level, company location, and job title are the primary salary drivers. While absolute pay differs sharply by geography, the percentage reward for seniority is broadly consistent across US/non-US markets and company sizes, with mid-level roles at medium companies as the one exception.

## Project Structure

```
salary-analysis/
│
├── README.md
├── data/
├── scripts/
└── outputs/
```

## Skills Demonstrated

Statistical Analysis, Regression Modeling, Interaction Effects Analysis, Assumption Testing & Diagnostics, Data Transformation, Data Visualization

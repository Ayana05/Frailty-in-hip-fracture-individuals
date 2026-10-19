# Frailty-in-hip-fracture-individuals
Aim of this study is to To identify distinct frailty trajectories among older adults following hip fracture and to examine the association between demographic and health related characteristics and membership in these trajectories.

## What the scripts do

1. Builds Fried frailty phenotype components, Cleans covariates (education, comorbidities, marital status) and creates the hip fracture cohort, followed from the fracture round through the next three rounds.
2. Fits latent class mixed models (LCMM) to identify distinct post-fracture frailty trajectories.
3. Assigns each participant to a trajectory class, summarizes mean frailty by class and round, and plots the trajectories.
4. Reports unweighted and survey-weighted class sizes, plus a baseline characteristics table with survey-adjusted chi-square tests.
5. Runs univariate and multivariable survey-weighted multinomial logistic regression (accounting for the NHATS complex survey design) to examine associations between baseline characteristics and class membership.
6. Presents results as odds ratios with 95% confidence intervals and overall p-values.

## Requirements

 R (≥ 4.2 recommended)


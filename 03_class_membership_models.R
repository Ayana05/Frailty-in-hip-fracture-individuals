#survey design
options(survey.lonely.psu = "adjust")
design <- svydesign(
  ids = ~varunit,
  strata = ~varstrat,
  weights = ~anfinwgt0,
  data = df_baseline_class,
  nest = TRUE
)

#Actual sample size
sample_n <- df_baseline_class %>%
  count(frailty_class_label, name = "Sample_n")
sample_n
# Survey-weighted population size - says those n sampled people represent approximately X people in the NHATS target population."
weighted_n <- svytable(
  ~frailty_class_label,
  design
) %>%
  as.data.frame() %>%
  rename(weighted_N = Freq) %>%
  mutate(weighted_N = round(weighted_N))
weighted_n

library(gtsummary)


#Baseline Characteristics
table6_baseline<-tbl_svysummary(
  design,
  by = frailty_class_label,
  include = c(
    gender_cat,
    age_cat,
    marital_cat,
    race_cat,
    bmi_cat,
    health_binary,
    education_cat,
    comorbidity_cat
  ),
  statistic = list(
    all_categorical() ~ "{n_unweighted} ({p}%)"
  ),
  missing = "no"
) %>%
  add_p(
    test = all_categorical() ~ "svy.chisq.test"
  ) 

table6_baseline

table6_baseline_chi<-as.data.frame(table6_baseline)

#Univariate Regression
df_baseline_class <- df_baseline_class %>%
  mutate(
    class_relevel = relevel(factor(frailty_class_label), ref = "Low Frailty")
  )


df_baseline_class <- df_baseline_class %>%
  mutate(
    age_cat       = relevel(factor(age_cat), ref = "65-74"),
    gender_cat    = relevel(factor(gender_cat), ref = "Male"),
    race_cat      = relevel(factor(race_cat), ref = "White, non-Hispanic"),
    marital_cat   = relevel(factor(marital_cat), ref = "Married/Partnered"),
    education_cat = relevel(factor(education_cat), ref = "Less than a college degree"),
    bmi_cat       = relevel(factor(bmi_cat), ref = "Normal weight"),
    health_cat    = relevel(factor(health_cat), ref = "Very good"),
    comorbidity_cat    = relevel(factor(comorbidity_cat), ref = "0")
  )

library(survey)
library(VGAM)
library(purrr)
library(svyVGAM)

covariates <- c(
  "age_cat", "gender_cat", "race_cat", "marital_cat",
  "education_cat", "bmi_cat", "health_cat", "comorbidity_cat"
)

#survey design
options(survey.lonely.psu = "adjust")
design <- svydesign(
  ids = ~varunit,
  strata = ~varstrat,
  weights = ~anfinwgt0,
  data = df_baseline_class,
  nest = TRUE
)

survey_models <- map(covariates, function(v) {
  do.call(svy_vglm,
          list(formula = reformulate(v, response = "class_relevel"),
               family  = VGAM::multinomial(refLevel = 1),
               design  = design))
}) %>% set_names(covariates)


out_levels <- levels(df_baseline_class$class_relevel)[-1]

tidy_or <- function(fit, v) {
  b <- coef(fit); V <- vcov(fit); se <- sqrt(diag(V))
  keep <- grepl(paste0("^", v), names(b))
  d <- data.frame(term = names(b)[keep],
                  OR = exp(b[keep]),
                  lo = exp(b[keep] - 1.96 * se[keep]),
                  hi = exp(b[keep] + 1.96 * se[keep]),
                  p  = 2 * pnorm(-abs(b[keep] / se[keep])))
  d$class <- out_levels[as.numeric(sub(".*:", "", d$term))]
  d$level <- sub(":.*", "", sub(paste0("^", v), "", d$term))
  d$p_overall <- pchisq(drop(t(b[keep]) %*% solve(V[keep, keep]) %*% b[keep]),
                        df = sum(keep), lower.tail = FALSE)
  d
}

univariate_results <- imap_dfr(survey_models, ~ tidy_or(.x, .y), .id = "covariate")
univariate_results

#Multivariate Regression
multivar_model <- svy_vglm(
  class_relevel ~ age_cat + gender_cat + race_cat + education_cat +marital_cat+
    bmi_cat + health_cat + comorbidity_cat,
  family = VGAM::multinomial(refLevel = 1),
  design = design
)
multivar_model

out_levels <- levels(df_baseline_class$class_relevel)[-1]
vars <- c("age_cat", "gender_cat", "race_cat", "education_cat","marital_cat",
          "bmi_cat", "health_cat", "comorbidity_cat")

b <- coef(multivar_model); V <- vcov(multivar_model); se <- sqrt(diag(V))

res <- bind_rows(lapply(vars, function(v) {
  keep <- grepl(paste0("^", v), names(b))
  if (!any(keep)) { base::message("No coefficients found for: ", v); return(NULL) }
  data.frame(
    variable  = v,
    level     = sub(":.*", "", sub(paste0("^", v), "", names(b)[keep])),
    class     = out_levels[as.numeric(sub(".*:", "", names(b)[keep]))],
    aOR       = exp(b[keep]),
    lo        = exp(b[keep] - 1.96 * se[keep]),
    hi        = exp(b[keep] + 1.96 * se[keep]),
    p         = 2 * pnorm(-abs(b[keep] / se[keep])),
    p_overall = pchisq(drop(t(b[keep]) %*% solve(V[keep, keep, drop = FALSE]) %*% b[keep]),
                       df = sum(keep), lower.tail = FALSE)
  )
}))

table7_multivar <- res %>%
  mutate(est = sprintf("%.2f (%.2f, %.2f)", aOR, lo, hi)) %>%
  select(variable, level, class, est, p_overall) %>%
  pivot_wider(names_from = class, values_from = est) %>%
  mutate(p_overall = ifelse(p_overall < 0.001, "<0.001", sprintf("%.3f", p_overall)))

table7_multivar


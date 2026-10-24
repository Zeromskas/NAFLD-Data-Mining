library(survival)
data(nafld, package = "survival")

# only take nafld cases
cases <- subset(nafld1, id == case.id)
cases$years <- cases$futime / 365.25

# baseline history means a diagnosis recorded at or before entry.
for (condition in c("diabetes", "htn", "dyslipidemia")) {
  history <- subset(nafld3, event == condition & days <= 0)
  cases[[condition]] <- as.integer(cases$id %in% history$id)
}
cases$metabolic_count <- cases$diabetes + cases$htn + cases$dyslipidemia

patients <- subset(cases, !is.na(bmi))
excluded <- subset(cases, is.na(bmi))

sample_sizes <- data.frame(
  Group = c("All NAFLD cases", "Missing BMI", "Analysis sample"),
  Patients = c(nrow(cases), nrow(excluded), nrow(patients)),
  Deaths = c(sum(cases$status), sum(excluded$status), sum(patients$status))
)
print(sample_sizes, row.names = FALSE)


# baseline characteristics

median_iqr <- function(x) {
  q <- quantile(x, c(0.5, 0.25, 0.75), na.rm = TRUE)
  sprintf("%.1f [%.1f--%.1f]", q[1], q[2], q[3])
}

number_percent <- function(x) {
  sprintf("%d (%.1f%%)", sum(x), 100 * mean(x))
}

baseline <- data.frame(
  Characteristic = c(
    "Age, years", "Male", "BMI, kg/m2",
    "Recorded diabetes", "Recorded hypertension",
    "Recorded dyslipidaemia"
  ),
  Value = c(
    median_iqr(patients$age), number_percent(patients$male),
    median_iqr(patients$bmi), number_percent(patients$diabetes),
    number_percent(patients$htn), number_percent(patients$dyslipidemia)
  )
)
print(baseline, row.names = FALSE)

# Missing values among all NAFLD cases, before the BMI exclusion.
print(colSums(is.na(cases[c("age", "male", "bmi")])))
print(summary(patients[c("age", "bmi")]))


# availability of nafld2 measurements before study entry ----
tests <- sort(unique(nafld2$test))
available <- matrix(FALSE, nrow(cases), length(tests))
recent <- matrix(FALSE, nrow(cases), length(tests))

for (i in seq_along(tests)) {
  records <- subset(nafld2, test == tests[i] & days <= 0 & !is.na(value))
  last_year <- subset(records, days >= -365)
  available[, i] <- cases$id %in% records$id
  recent[, i] <- cases$id %in% last_year$id
}

has_bmi <- !is.na(cases$bmi)
nafld2_availability <- data.frame(
  Measurement = tests,
  All_cases_any_history = colSums(available),
  Analysis_any_history = colSums(available[has_bmi, ]),
  All_cases_last_year = colSums(recent),
  Analysis_last_year = colSums(recent[has_bmi, ])
)
print(nafld2_availability, row.names = FALSE)

nafld2_coverage <- data.frame(
  Group = c("All NAFLD cases", "Analysis sample"),
  Patients = c(nrow(cases), nrow(patients)),
  Any_measurement = c(
    sum(rowSums(available) > 0),
    sum(rowSums(available[has_bmi, ]) > 0)
  ),
  All_six_measurements = c(
    sum(rowSums(available) == length(tests)),
    sum(rowSums(available[has_bmi, ]) == length(tests))
  ),
  All_six_last_year = c(
    sum(rowSums(recent) == length(tests)),
    sum(rowSums(recent[has_bmi, ]) == length(tests))
  )
)
print(nafld2_coverage, row.names = FALSE)


# follow-up and mortality

# status = 1: died; status = 0: last observed alive.
followup <- data.frame(
  Median_followup_years = median(patients$years),
  Deaths_total = sum(patients$status),
  Deaths_by_five_years = sum(patients$status == 1 & patients$years <= 5),
  Censored_before_five_years = sum(patients$status == 0 & patients$years < 5),
  Observed_beyond_five_years = sum(patients$years > 5)
)
print(followup, row.names = FALSE)

overall_survival <- survfit(Surv(years, status) ~ 1,
  data = patients,
  conf.type = "log-log"
)
estimates <- summary(overall_survival, times = c(1, 3, 5, 10))
mortality <- data.frame(
  Year = estimates$time,
  Patients_at_risk = estimates$n.risk,
  Mortality_percent = 100 * (1 - estimates$surv),
  Lower_95_CI = 100 * (1 - estimates$upper),
  Upper_95_CI = 100 * (1 - estimates$lower)
)
print(mortality, row.names = FALSE)

# distribution of age and bmi

hist(patients$age,
  main = "Age at study entry", xlab = "Age (years)", ylab = "Patients"
)

hist(patients$bmi,
  main = "Baseline BMI", xlab = "BMI (kg/m2)", ylab = "Patients"
)

# overall survival curve
plot(overall_survival,
  xlim = c(0, 10), ylim = c(0, 1),
  conf.int = TRUE, mark.time = FALSE, lwd = 2,
  main = "Survival in the analysis sample",
  xlab = "Years since study entry", ylab = "Survival probability"
)


# five-year mortality by baseline characteristic ----

# Categories are for EDA; the models keep age and BMI continuous.
patients$age_group <- cut(
  patients$age, c(18, 40, 60, 80, Inf),
  right = FALSE,
  labels = c("18-39", "40-59", "60-79", "80+")
)
patients$sex_group <- factor(patients$male,
  levels = 0:1,
  labels = c("Women", "Men")
)
patients$bmi_group <- cut(
  patients$bmi, c(-Inf, 25, 30, 35, Inf),
  right = FALSE,
  labels = c("<25", "25-29.9", "30-34.9", "35+")
)
patients$metabolic_group <- factor(patients$metabolic_count, levels = 0:3)

# account for censoring but not other predictors
show_mortality <- function(group, title, x_label) {
  fit <- survfit(Surv(patients$years, patients$status) ~ group,
    conf.type = "log-log"
  )
  estimate <- summary(fit, times = 5)
  results <- data.frame(
    Group = levels(group),
    Patients = as.integer(table(group)),
    Mortality_percent = 100 * (1 - estimate$surv),
    Lower_95_CI = 100 * (1 - estimate$upper),
    Upper_95_CI = 100 * (1 - estimate$lower)
  )
  print(results, row.names = FALSE)

  x <- seq_len(nrow(results))
  plot(x, results$Mortality_percent,
    ylim = c(0, max(results$Upper_95_CI) * 1.25),
    xlim = c(0.6, nrow(results) + 0.4), xaxt = "n",
    main = title, xlab = x_label, ylab = "Five-year mortality (%)",
    pch = 19, cex = 1.3, col = "#236A91"
  )
  axis(1, at = x, labels = results$Group)
  arrows(x, results$Lower_95_CI, x, results$Upper_95_CI,
    angle = 90, code = 3, length = 0.06
  )
  text(x, results$Upper_95_CI, labels = paste0("n = ", results$Patients), pos = 3)
  invisible(results)
}

age_mortality <- show_mortality(patients$age_group, "Age", "Age group (years)")
sex_mortality <- show_mortality(patients$sex_group, "Sex", "Sex")
bmi_mortality <- show_mortality(patients$bmi_group, "BMI", "BMI (kg/m2)")
history_mortality <- show_mortality(
  patients$metabolic_group, "Recorded metabolic conditions", "Number of conditions"
)

print(tapply(patients$age, patients$metabolic_group, median))

library(haven)
library(tidyverse)
library(lmtest)
library(sandwich)
library(ivreg)

rm(list = ls())

evaluation <- read_stata("../1_data/evaluation.dta")

control_vars <- c("age_hh", "age_sp", "educ_hh", "educ_sp", "female_hh", "indigenous",
                  "hhsize", "dirtfloor", "bathroom", "land", "hospital_distance")
control_form <- paste(control_vars, collapse = " + ")

glimpse(evaluation)
summary(evaluation)

#### Method 1 ####
m1 <- evaluation %>%
      filter(treatment_locality == 1,
             enrolled == 1)

t.test(health_expenditures ~ round, data = m1, var.equal = TRUE)
mean(m1[m1$round == 0, ]$health_expenditures) - mean(m1[m1$round == 1, ]$health_expenditures)

fit_m1 <- lm(health_expenditures ~ round, data = m1)
summary(fit_m1)

fit2_m1 <- lm(as.formula(paste("health_expenditures ~ round",
                               control_form, sep = " + ")), data = m1)
fit2_m1_c <- coeftest(fit2_m1,
                      vcov. = vcovCL,
                      type = "HC1",
                      cluster = ~locality_identifier)

#### Method 2 ####
m2 <- evaluation %>%
      filter(treatment_locality == 1,
             round == 1)

t.test(health_expenditures ~ enrolled, var.equal = TRUE, data = m2)
mean(m2[m2$enrolled == 0, ]$health_expenditures) - mean(m2[m2$enrolled == 1, ]$health_expenditures)

fit_m2 <- lm(health_expenditures ~ enrolled, data = m2)
summary(fit_m2)

fit_m2_c <- coeftest(fit_m2,
                     vcov. = vcovCL,
                     type = "HC1",
                     cluster = ~ locality_identifier)
fit_m2_c

fit2_m2 <- lm(as.formula(paste("health_expenditures ~ enrolled",
                               control_form, sep = " + ")), data = m2)
summary(fit2_m2)

fit2_m1_c <- coeftest(fit2_m2,
                      vcov. = vcovCL,
                      type = "HC1",
                      cluster = ~ locality_identifier)
fit2_m1_c

#### Method 3 ####
m3 <- evaluation %>%
      filter(eligible == 1)

t.test(health_expenditures ~ treatment_locality,
       data = filter(m3, round == 0),
       var.equal = TRUE)

for (var in control_vars) {
      cat("\n##### Working on: ", var, " #####\n", sep = "")
      
      mean_group_1 <- mean(pull(m3[m3$treatment_locality == 0, ], var))
      mean_group_2 <- mean(pull(m3[m3$treatment_locality == 1, ], var))
      
      cat("\nMean diff: ", mean_group_1 - mean_group_2)
      
      paste(var, "treatment_locality", sep = " ~ ") %>%
            as.formula() %>%
            t.test(filter(m3, round == 0), var.equal = TRUE) %>%
            print()
}

t.test(health_expenditures ~ treatment_locality,
       data = filter(m3, round == 1),
       var.equal = TRUE)

fit_m3 <- lm(health_expenditures ~ treatment_locality,
             data = filter(m3, round == 1))
summary(fit_m3)
fit_m3_c <- coeftest(fit_m3,
                     vcov. = vcovCL,
                     type = "HC1",
                     cluster = ~ locality_identifier)
fit_m3_c

fit2_m3 <- lm(as.formula(paste("health_expenditures ~ treatment_locality",
                               control_form, sep = " + ")), 
              data = filter(m3, round == 1))
summary(fit2_m3)

fit2_m3_c <- coeftest(fit2_m3,
                      vcov. = vcovCL,
                      type = "HC1",
                      cluster = ~ locality_identifier)
fit2_m3_c

#### Method 4 ####
m4 <- evaluation %>%
      select(-c(eligible, treatment_locality, enrolled))

t.test(health_expenditures ~ promotion_locality,
       data = filter(m4, round == 0),
       var.equal = TRUE)

t.test(health_expenditures ~ promotion_locality,
       data = filter(m4, round == 1),
       var.equal = TRUE)

t.test(enrolled_rp ~ promotion_locality,
       data = filter(m4, round == 1),
       var.equal = TRUE)

fit_m4 <- ivreg(health_expenditures ~ enrolled_rp | promotion_locality,
             data = filter(m4, round == 1))
summary(fit_m4)

fit2_m4 <- ivreg(as.formula(paste("health_expenditures ~ enrolled_rp + ",
                               control_form,
                               " | ",
                               control_form,
                               " + promotion_locality")),
              data = filter(m4, round == 1))
summary(fit2_m4)

#### Method 5 ####
m5 <- evaluation %>%
      filter(treatment_locality == 1)

fit_m5 <- lm(health_expenditures ~ poverty_index, data = filter(m5, round == 0))
summary(fit_m5)

plot(fit_m5$fitted.values ~ m5[m5$round == 0, ]$poverty_index)

m5 %>%
      filter(round == 0) %>%
      ggplot(aes(x = poverty_index, y = fit_m5$fitted.values)) +
      geom_point()

m5 %>%
      ggplot(aes(x = poverty_index)) +
      geom_density(fill = "dodgerblue", alpha = 0.5) +
      geom_vline(xintercept = 58, color = "red") +
      geom_text(aes(x = 65, label = "Not eligible", y = 0.005)) +
      geom_text(aes(x = 50, label = "Eligible", y = 0.005)) + 
      geom_text(aes(x = 57, label = "58", y = 0.002)) + 
      labs(x = "Estimated density", y = "Baseline poverty index (20-100)")


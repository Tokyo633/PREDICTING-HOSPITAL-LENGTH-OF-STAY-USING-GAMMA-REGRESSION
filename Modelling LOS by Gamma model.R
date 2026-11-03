getwd()
setwd("C:/Users/grace/OneDrive/Documents/School Stuff/Semester 4/Advanced Regression/Lab 3 assignment")
Lab3<-read.csv("length_of_stay.csv")

library(ggplot2)
library(gridExtra)
library(dplyr)

# confirm the data loaded
dim(Lab3)
str(Lab3)
names(Lab3)

## EDA##
# any missing values?
colSums(is.na(Lab3))

# make the categoricals factors, with sensible reference levels
Lab3 <- Lab3 %>%
  mutate(
    admission = factor(admission, levels = c("Elective", "Emergency")),
    sex       = factor(sex),
    weekend   = factor(weekend, levels = c("No", "Yes")),
    insurance = factor(insurance)
  )

str(Lab3)

# summary of LOS
summary(Lab3$los_days)
sd(Lab3$los_days)

p1 <- ggplot(Lab3, aes(x = los_days)) +
  geom_histogram(bins = 30, fill = "#4292c6", colour = "white") +
  labs(title = "Distribution of length of stay", x = "Length of stay (days)", y = "Count") +
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.margin = margin(t=5,r=5,b=2,l=2))

# right skew check
p2 <- ggplot(Lab3, aes(x = los_days)) +
  geom_density(fill = "#4292c6", alpha = 0.5, colour = "#08306b") +
  labs(title = "Density of length of stay", x = "Length of stay (days)", y = "Density") +
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.margin = margin(t=5,r=5,b=2,l=2))

grid.arrange(p1, p2, nrow = 1)

# mean vs median — a gap indicates skew
mean(Lab3$los_days)
median(Lab3$los_days)

# skewness
mean((Lab3$los_days - mean(Lab3$los_days))^3) / sd(Lab3$los_days)^3

# any zeros? (Gamma requires strictly positive)
sum(Lab3$los_days <= 0)
min(Lab3$los_days)

p3 <- ggplot(Lab3, aes(x = age, y = los_days)) +
  geom_point(alpha = 0.3, colour = "#4292c6") +
  geom_smooth(method = "lm", se = FALSE, colour = "#08306b") +
  labs(title = "Length of stay by age", x = "Age (years)", y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

p4 <- ggplot(Lab3, aes(x = factor(comorbidities), y = los_days)) +
  geom_boxplot(fill = "#4292c6", colour = "#08306b", outlier.size = 1) +
  labs(title = "Length of stay by comorbidities", x = "Number of comorbidities", y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

p5<- ggplot(Lab3, aes(x = factor(severity), y = los_days)) +
  geom_boxplot(fill = "#4292c6", colour = "#08306b", outlier.size = 1) +
  labs(title = "Length of stay by severity", x = "Severity (1-5)", y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

grid.arrange(p1,p2, p3, p4, p5, nrow = 1)

p6 <- ggplot(Lab3, aes(x = admission, y = los_days, fill = admission)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c("Elective" = "#9ecae1", "Emergency" = "#08306b")) +
  labs(title = "Length of stay by admission type", x = NULL, y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

p7 <- ggplot(Lab3, aes(x = weekend, y = los_days, fill = weekend)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c("No" = "#9ecae1", "Yes" = "#08306b")) +
  labs(title = "Length of stay by weekend admission", x = NULL, y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

p8 <- ggplot(Lab3, aes(x = sex, y = los_days, fill = sex)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c("F" = "#9ecae1", "M" = "#08306b")) +
  labs(title = "Length of stay by sex", x = NULL, y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

p9 <- ggplot(Lab3, aes(x = insurance, y = los_days, fill = insurance)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c("Public" = "#9ecae1", "Private" = "#08306b")) +
  labs(title = "Length of stay by insurance", x = NULL, y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

grid.arrange(p6, p7, p8, p9, nrow = 2, ncol = 2)

# distribution
p1 <- ggplot(Lab3, aes(x = los_days)) +
  geom_histogram(bins = 30, fill = "#4292c6", colour = "white") +
  labs(x = "Length of stay (days)", y = "Count") +
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.margin = margin(t=5,r=5,b=2,l=2))

# right skew check
p2 <- ggplot(Lab3, aes(x = los_days)) +
  geom_density(fill = "#4292c6", alpha = 0.5, colour = "#08306b") +
  labs(x = "Length of stay (days)", y = "Density") +
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.margin = margin(t=5,r=5,b=2,l=2))

grid.arrange(p1, p2, nrow = 1)

# mean vs median — a gap indicates skew
mean(Lab3$los_days)
median(Lab3$los_days)

# skewness
mean((Lab3$los_days - mean(Lab3$los_days))^3) / sd(Lab3$los_days)^3

# any zeros? (Gamma requires strictly positive)
sum(Lab3$los_days <= 0)
min(Lab3$los_days)

p3 <- ggplot(Lab3, aes(x = age, y = los_days)) +
  geom_point(alpha = 0.3, colour = "#4292c6") +
  geom_smooth(method = "lm", se = FALSE, colour = "#08306b") +
  labs(x = "Age (years)", y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

p4 <- ggplot(Lab3, aes(x = factor(comorbidities), y = los_days)) +
  geom_boxplot(fill = "#4292c6", colour = "#08306b", outlier.size = 1) +
  labs(x = "Number of comorbidities", y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

p5<- ggplot(Lab3, aes(x = factor(severity), y = los_days)) +
  geom_boxplot(fill = "#4292c6", colour = "#08306b", outlier.size = 1) +
  labs(x = "Severity (1-5)", y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

grid.arrange(p3, p4,p5, nrow = 1)

p6 <- ggplot(Lab3, aes(x = admission, y = los_days, fill = admission)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c("Elective" = "#9ecae1", "Emergency" = "#08306b")) +
  labs(x = NULL, y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

p7 <- ggplot(Lab3, aes(x = weekend, y = los_days, fill = weekend)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c("No" = "#9ecae1", "Yes" = "#08306b")) +
  labs(x = NULL, y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

p8 <- ggplot(Lab3, aes(x = sex, y = los_days, fill = sex)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c("F" = "#9ecae1", "M" = "#08306b")) +
  labs(x = NULL, y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

p9 <- ggplot(Lab3, aes(x = insurance, y = los_days, fill = insurance)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c("Public" = "#9ecae1", "Private" = "#08306b")) +
  labs(x = NULL, y = "Length of stay (days)") +
  theme_minimal() + theme(panel.grid = element_blank())

grid.arrange(p6, p7, p8, p9, nrow = 2, ncol = 2)

# LOS by admission type
Lab3 %>% group_by(admission) %>%
  summarise(n = n(), mean_los = round(mean(los_days), 2),
            median_los = round(median(los_days), 2), .groups = "drop") %>% print()

# LOS by weekend
Lab3 %>% group_by(weekend) %>%
  summarise(n = n(), mean_los = round(mean(los_days), 2),
            median_los = round(median(los_days), 2), .groups = "drop") %>% print()

# LOS by number of comorbidities
Lab3 %>% group_by(comorbidities) %>%
  summarise(n = n(), mean_los = round(mean(los_days), 2), .groups = "drop") %>% print()

# LOS by severity
Lab3 %>% group_by(severity) %>%
  summarise(n = n(), mean_los = round(mean(los_days), 2), .groups = "drop") %>% print()

# LOS by sex and insurance
Lab3 %>% group_by(sex) %>%
  summarise(n = n(), mean_los = round(mean(los_days), 2), .groups = "drop") %>% print()

Lab3 %>% group_by(insurance) %>%
  summarise(n = n(), mean_los = round(mean(los_days), 2), .groups = "drop") %>% print()

num_vars <- Lab3 %>% select(los_days, age, severity, comorbidities)
round(cor(num_vars), 3)

# Model building 
m_full <- glm(los_days ~ age + comorbidities + admission + sex + weekend + severity + insurance,
              family = Gamma(link = "log"),
              data = Lab3)

summary(m_full)

# dispersion estimate
summary(m_full)$dispersion

# reduced model — drop the three non-significant terms
m_reduced <- glm(los_days ~ age + comorbidities + admission + severity,
                 family = Gamma(link = "log"),
                 data = Lab3)

summary(m_reduced)
round(exp(coef(m_reduced)), 4)
round(exp(confint(m_reduced)), 4)

# compare full vs reduced
AIC(m_full, m_reduced)
anova(m_full, m_reduced, test = "F")
# ---- count model, for the brief's comparison ----
Lab3$los_int <- round(Lab3$los_days)

m_pois <- glm(los_int ~ age + comorbidities + admission + severity,
              family = poisson(link = "log"), data = Lab3)

summary(m_pois)
sum(residuals(m_pois, type = "pearson")^2) / df.residual(m_pois)   # overdispersion

# ---- normal LM, for the AIC objective ----
m_lm <- lm(los_days ~ age + comorbidities + admission + severity, data = Lab3)
summary(m_lm)
sum(predict(m_lm) < 0)
range(predict(m_lm))


# ---- three-way comparison ----
data.frame(
  model = c("Normal LM", "Poisson (count)", "Gamma (reduced)"),
  AIC   = round(c(AIC(m_lm), AIC(m_pois), AIC(m_reduced)), 2)
)

# ---- full model ----
sum(residuals(m_full, type = "pearson")^2) / df.residual(m_full)
m_full$deviance / df.residual(m_full)
summary(m_full)$dispersion

# ---- reduced model ----
sum(residuals(m_reduced, type = "pearson")^2) / df.residual(m_reduced)
m_reduced$deviance / df.residual(m_reduced)
summary(m_reduced)$dispersion


# ---- post hoc: worked predictions from the reduced Gamma model ----
profiles <- data.frame(
  label    = c("Reference", "10 yrs older", "+1 severity", "Emergency", "Emergency + severity 4"),
  age      = c(60, 70, 60, 60, 60,60),
  comorbidities = c(1, 1, 1, 1, 1),
  admission = factor(c("Elective","Elective","Elective","Elective","Emergency","Emergency"),
                     levels = c("Elective","Emergency")),
  severity = c(2, 2, 3,5, 2, 4)
)
profiles$predicted_los <- predict(m_reduced, newdata = profiles, type = "response")
profiles$predicted_los <- predict(m_reduced, newdata = profiles, type = "response")
profiles$predicted_los <- round(profiles$predicted_los, 3)
print(profiles)

round(profiles, 3)

# ---- 2. Across age ----
age_seq <- data.frame(age = seq(30, 90, 10), comorbidities = 1,
                      admission = factor("Elective", levels = c("Elective","Emergency")),
                      severity = 2)
age_seq$predicted_los <- predict(m_reduced, newdata = age_seq, type = "response")
round(age_seq[ , c("age", "predicted_los")], 3)

# ---- 3. Across severity ----
sev_seq <- data.frame(severity = 1:5, age = 60, comorbidities = 1,
                      admission = factor("Elective", levels = c("Elective","Emergency")))
sev_seq$predicted_los <- predict(m_reduced, newdata = sev_seq, type = "response")
round(sev_seq[ , c("severity", "predicted_los")], 3)

# ---- 4. Across comorbidities ----
com_seq <- data.frame(comorbidities = 0:6, age = 60,
                      admission = factor("Elective", levels = c("Elective","Emergency")),
                      severity = 2)
com_seq$predicted_los <- predict(m_reduced, newdata = com_seq, type = "response")
round(com_seq[ , c("comorbidities", "predicted_los")], 3)
# ---- 6. The comorbidity plot ----
# ---- 2. Across age ----
age_seq <- data.frame(age = seq(30, 90, 10), comorbidities = 1,
                      admission = factor("Elective", levels = c("Elective","Emergency")),
                      severity = 2)
age_seq$predicted_los <- predict(m_reduced, newdata = age_seq, type = "response")
round(age_seq[ , c("age", "predicted_los")], 3)
# ---- Age sequence ----
age_seq <- data.frame(age = seq(30, 90, 10), comorbidities = 1,
                      admission = factor("Elective", levels = c("Elective","Emergency")),
                      severity = 2)
age_seq$predicted_los <- predict(m_reduced, newdata = age_seq, type = "response")
round(age_seq[ , c("age", "predicted_los")], 3)

# ---- Age plot ----
library(ggplot2)

ggplot(age_seq, aes(x = age, y = predicted_los)) +
  geom_line(linewidth = 1, colour = "#1E88E5") +
  geom_point(size = 2.5, colour = "#1E88E5") +
  labs(title = "Predicted length of stay by age",
       x = "Age (years)",
       y = "Predicted length of stay (days)") +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(),
        axis.line.x = element_line(colour = "grey40"),
        axis.line.y = element_line(colour = "grey40"))


# ---- 5. Multiplier checks ----
exp(10 * coef(m_reduced)["age"])
exp(3  * coef(m_reduced)["comorbidities"])
exp(coef(m_reduced)["admissionEmergency"])

# ---- 6. Comorbidity plot, with title ----
library(ggplot2)

ggplot(com_seq, aes(x = comorbidities, y = predicted_los)) +
  geom_line(linewidth = 1, colour = "#1E88E5") +
  geom_point(size = 2.5, colour = "#1E88E5") +
  labs(title = "Predicted length of stay by number of comorbidities",
       x = "Number of comorbidities",
       y = "Predicted length of stay (days)") +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        axis.line.x = element_line(colour = "grey40"),
        axis.line.y = element_line(colour = "grey40"))


# ---- 5. Multiplier checks ----
exp(10 * coef(m_reduced)["age"])
exp(3  * coef(m_reduced)["comorbidities"])
exp(coef(m_reduced)["admissionEmergency"])

# ---- 6. The plot: predicted LOS by severity and admission type ----
library(ggplot2)

curve_data <- expand.grid(
  severity  = 1:5,
  admission = factor(c("Elective","Emergency"), levels = c("Elective","Emergency")),
  age = 60, comorbidities = 1
)
curve_data$predicted_los <- predict(m_reduced, newdata = curve_data, type = "response")

ggplot(curve_data, aes(x = factor(severity), y = predicted_los,
                       colour = admission, group = admission)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c("Elective" = "#9ecae1", "Emergency" = "#08306b")) +
  labs(title = "Predicted length of stay by severity and admission type",
       x = "Severity (1-5)", y = "Predicted length of stay (days)", colour = NULL) +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        axis.line.x = element_line(colour = "grey40"))


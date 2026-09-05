# Analyse Seminar Blended Learning

#_______________________________________
# 0: SETUP ####
#_______________________________________
#_______________________________________

setwd("C:/Users/User/Desktop/Blended Learning")

library(psych)
library(dplyr)
library(car)
library(ggplot2)
library(e1071)

set.seed(4630)




#_______________________________________
# 1: DATA PREPARATION ####
#_______________________________________
#_______________________________________


# load
df_raw <- read.csv2("data_aow1_2026-06-22_10-37.csv")

# only erwerbstätig
df <- subset(df_raw, employment1 == 2)
# -> exclude 25 nicht-erwerbstätige

# attention check
df <- subset(df, Attention1 == 2)
# -> exclude 6 more



# replace -9 (code for missing values) with NA
df <- df %>% mutate(across(where(is.numeric), ~ na_if(.x, -9)))





#_______________________________________
# 2: METHOD SECTION ####
#_______________________________________
#_______________________________________

# sample descriptives

# age and working hours
describe(df[, c("age", "hours")])




## gender ####
#_______________________________________

df$gender <- factor(df$gender,
                    levels = c(1, 2, 3, -1),
                    labels = c("weiblich", "männlich", "divers", "keine Angabe"))


gender_counts <- table(df$gender, useNA = "ifany")
gender_counts

prop.table(gender_counts) * 100





## education ####
#_______________________________________

df$edu <- factor(df$grad,
                    levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                    labels = c("kein Abschluss", "Hauptschule", "Realschule", "Abi", "Ausbildung", "Meister", "Bachelor", "Master"))


edu_counts <- table(df$edu, useNA = "ifany")
edu_counts

prop.table(edu_counts) * 100






#_______________________________________
# 3. RESULTS ####
#_______________________________________
#_______________________________________




## preparation ####
#_______________________________________


### technostress score ####
techno_cols <- df[, grep("^techno", names(df))]

techno_mean_vector <- rowMeans(techno_cols, na.rm = TRUE)

df_inf <- data.frame(
  id = df$CASE, 
  technostress = techno_mean_vector
)

### motivation score ####
motivation_cols <- df[, grep("^motivation", names(df))]

motivation_mean_vector <- rowMeans(motivation_cols, na.rm = TRUE)

df_inf$motivation <- motivation_mean_vector

### techno-support ####
df_inf$support1 <- df$support1
df_inf$support2 <- df$support2

df_inf$support_mean <- rowMeans(data.frame(df$support1, df$support2), na.rm = TRUE)




## construct descriptives ####
#_______________________________________

constructs <- df_inf[, c("motivation", "technostress", "support1", "support2", "support_mean")]
describe(constructs)[, c("n", "mean", "sd", "min", "max")]

### exploratory analyses ####
skewness(df$support1)
skewness(df$support2)
skewness(df_inf$support_mean)






## H1: technostress negatively impacts job motivation ####
#_______________________________________

model_h1 <- lm(motivation ~ technostress, data = df_inf)
model_h1_zscore <- lm(scale(motivation) ~ scale(technostress), data = df_inf)

summary(model_h1)
summary(model_h1_zscore)



## H2: social techno-support negatively impacts technostress ####
#_______________________________________

model_h2 <- lm(technostress ~ support_mean, data = df_inf)
model_h2_zscore <- lm(scale(technostress) ~ scale(support_mean), data = df_inf)

summary(model_h2)
summary(model_h2_zscore)



## H3: present and distant techo-support differ in their effect on technostress ####
#_______________________________________


# z-standardized
model_support <- lm(scale(technostress) ~ scale(support1) + scale(support2), data = df_inf)

summary(model_support)
car::vif(model_support)
cor(df_inf$technostress, df_inf$support2)
cor.test(df_inf$technostress, df_inf$support_mean)
cor.test(df_inf$motivation, df_inf$support_mean)


plot(model_support, which = 2)



## Exploratives: correlation matrix ####
#_______________________________________


corr_vars <- df_inf[, c("motivation", "technostress", "support_mean")]

summary(corr_vars)
sapply(corr_vars, sd, na.rm = TRUE)
my_test <- corr.test(corr_vars)

cor(corr_vars)
round(my_test$p, digits = 3)





#_______________________________________
# GRAPHS ####
#_______________________________________
#_______________________________________


## Figure 1: technostress -> motivation ####
#_______________________________________

h1_graph <- ggplot(df_inf, aes(x = technostress, y = motivation)) +
  geom_jitter(alpha = 0.3, color = "black", width = 0.05, height = 0.05, shape = 16) +
  
  geom_smooth(method = "lm", color = "#0068d6", fill = "#9cbaea", linewidth = 1.2, level = 0.95, fullrange=TRUE) +
  
  xlim(1,5) +
  ylim(1,7) +
  
  labs(
    x = "Technostress Score",
    y = "Arbeitsmotivation Score"
  ) +
  
  theme_classic(base_size = 14)

h1_graph
ggsave("h1.png", h1_graph, width = 6, height = 4, dpi = 600)




## Figure A2: distribution of social techno-support ####
#_______________________________________

support_data <- df[, c("support1", "support2")]

support_long <- stack(support_data)

support_long$values <- as.factor(support_long$values)

levels(support_long$ind) <- c("digital", "vor Ort")

tech_support_graph <- ggplot(support_long, aes(x = values, fill = ind)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(
    x = "Likert Score",
    y = "Anzahl Teilnehmende",
    fill = "Techno-Unterstützung"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("#4A4A4A", "#B0B0B0"))


tech_support_graph
ggsave("tech_support.png", tech_support_graph, width = 6, height = 4, dpi = 600)







set.seed(1)
library(tidyverse)
library(AlgDesign)
Design_factorial <- gen.factorial(levels = c(4, 3, 5), nVars = 3, 
varNames = c("Fac1", "Fac2", "Fac3"), center = FALSE)
Anova_data <- purrr :: map_dfr(seq_len(4), ~Design_factorial)
y <- Anova_data %>%
        apply(., MARGIN = 1, function(x) prod(x)) %>%
        unlist %>%
        rnorm(n = 240, mean = ., sd = 1) %>% 
        round(2)

anova_data <- Anova_data %>%
    mutate(across(starts_with("Fac"), ~ as.factor(.x))) %>%
    mutate(Fac1 = fct_recode(Fac1, 
            A = "1", B = "2", C = "3", D = "4"), 
            Fac2 = fct_recode(Fac2, Level_1 = "1", 
            Level_2 = "2", Level_3 = "3"), 
            Fac3 = fct_recode(Fac3, Timar_1 = "1", Timar_2 = "2", 
            Timar_3 = "3", Timar_4 = "4", Timar_5 = "5")) %>%
            mutate(y = y) %>%
            relocate(y, .before = Fac1)
head(anova_data)

with(anova_data, summary(aov(y ~ Fac1*Fac2*Fac3)))

library(writexl)
# write_xlsx(anova_data, path = "Anova_data_balanced.xlsx", col_names = TRUE)


# help(emmeans, package='car')
# library(emmeans)
# help(package = "emmeans")
?emmeans :: contrast
? summary.emmGrid

library(splitstackshape)
Design_factorial %>% dim

set.seed(1)
Count <- sample(c(2, 3, 4, 5, 6, 7), size = 60, replace = TRUE)
library(data.table)
dat <- as.data.table(Design_factorial)
Anova_data_unbalanced <- dat %>% expandRows(count = get("Count"), count.is.col = FALSE, drop = TRUE)


set.seed(1)
y_unbalanced <- Anova_data_unbalanced %>%
        apply(., MARGIN = 1, function(x) prod(x)) %>%
        unlist %>%
        rnorm(n = 269, mean = ., sd = 1) %>% 
        round(2)

Anova_data_unbalanced <- Anova_data_unbalanced %>%
    mutate(across(starts_with("Fac"), ~ as.factor(.x))) %>%
    mutate(Fac1 = fct_recode(Fac1, 
            A = "1", B = "2", C = "3", D = "4"), 
            Fac2 = fct_recode(Fac2, Level_1 = "1", 
            Level_2 = "2", Level_3 = "3"), 
            Fac3 = fct_recode(Fac3, Timar_1 = "1", Timar_2 = "2", 
            Timar_3 = "3", Timar_4 = "4", Timar_5 = "5")) %>%
            mutate(y = y_unbalanced) %>%
            relocate(y, .before = Fac1)
head(Anova_data_unbalanced)

with(Anova_data_unbalanced, summary(aov(y ~ Fac1*Fac2*Fac3)))

library(writexl)
write_xlsx(Anova_data_unbalanced, path = "Anova_data_Unbalanced.xlsx", col_names = TRUE)



## create Model with Unbalanced Data ----------
options(contrasts = c("contr.sum", "contr.poly"))

Model1 <- with(Anova_data_unbalanced, aov(y ~ Fac1 * Fac2))
Model2 <- with(Anova_data_unbalanced, lm(y ~ Fac1 * Fac2))
library(car)
anova_type_I <- anova(Model1)
anova_type_II <- Anova(Model2, type = 2)
anova_type_III <- Anova(Model2, type = 3)
anova_type_I
anova_type_II
anova_type_III 


## Create Model With Balanced Data ----------

Model1_balanced <- with(anova_data, aov(y ~ Fac1 * Fac2))
Model2_balanced <- with(anova_data, lm(y ~ Fac1 * Fac2))

anova_type_I_balanced <- anova(Model1_balanced)
anova_type_II_balanced <- Anova(Model2_balanced, type = 2)
anova_type_III_balanced <- Anova(Model2_balanced, type = 3)
anova_type_I_balanced
anova_type_II_balanced
anova_type_III_balanced

######### create data for Ancova ---------------------
library(tidyverse)
n <- 100
Levels <- LETTERS[1:4]
Fac <- rep(Levels, each = n/4)

num_fac <- Fac %>% 
                dplyr :: recode(A = 2, B = -4, C = 4, D = -2)

set.seed(1)
X <- rnorm(n, .5, sd = 0.1) %>% round(2)
y <- 3.5 * X + (rnorm(n, 0, 0.1) %>% round(2)) + num_fac
# library(httpgd)
# hgd()
# hgd_browse()
library(data.table)

dat <- data.table(x = X, y = y, Fac = Fac)
dat %>% head
dat %>% tail

fun2 <- function(df) coef(lm(y ~ x, data = df))
dat2 <- dat %>% group_by(Fac) %>% nest %>%
mutate(coef_models = case_when(
                        TRUE ~ map(data, fun2)
)) 
dat2

dat3 <- dat2$coef_models %>% as.data.table %>% t %>%
                        as.data.frame %>% setNames(c("Intercept", "Slope")) %>%
                        mutate(Fac = dat2$Fac)



dat_ind <- dat %>%
        mutate(Fac = case_match(Fac, "A" ~ paste("A", dat3$Slope[1] %>% round(2), sep = ", Slope = "),
        "B" ~ paste("B", dat3$Slope[2] %>% round(2), sep = ", Slope = "), 
        "C" ~ paste("C", dat3$Slope[3] %>% round(2), sep = ", Slope = "), 
        "D" ~ paste("D", dat3$Slope[4] %>% round(2), sep = ", Slope = "))) 
        dat_ind %>% 
        ggplot(aes(x = x, y = y, color = Fac)) +
        geom_point() + 
        geom_abline(data = dat3 %>% mutate(Fac = unique(dat_ind$Fac)), aes(slope = Slope, intercept = Intercept, colour = Fac)) + 
        theme_bw()

library(writexl)
write_xlsx(dat[, c(2, 1, 3)], path = "Ancova_data.xlsx", col_names = TRUE)

################ create data for manova ----------

n <- 120
S <- matrix(c(1, .35, .35, 1), 2, 2)
M <- c(15, 120)
set.seed(1)

Design_factorial2 <- gen.factorial(levels = c(4, 3), nVars = 2, 
varNames = c("Gr1", "Gr2"), center = FALSE)

manova_data <- purrr :: map_dfr(seq_len(10), ~Design_factorial2)


M <- rowSums(manova_data)
M2 <- apply(manova_data, 1, prod)
library (mnormt)

set.seed(1)
dat <- mapply(FUN = function(x, y) rmnorm(1, mean = c(x, y), varcov = S), 
                        M, M2) %>% t %>% round(2)


library(rstatix)
interaction_gr <- interaction(manova_data$Gr1, manova_data$Gr2)
box_m(dat, interaction_gr)


Manova_data <- manova_data %>%
                mutate(y1 = dat[, 1], y2 = dat[, 2]) %>%
                relocate(y1, .before = Gr1) %>%
                relocate(y2, .before = Gr1)
Manova_data %>% head

Manova_data %>%
        write_xlsx(path = "Manova_data.xlsx", col_names = TRUE)    


with(Manova_data, summary(manova(cbind(y1, y2) ~ Gr1 * Gr2)))

box_m(Manova_data[, c(1, 2)], group = Manova_data[, c(3, 4)])

## create data for reapeted measure anova -----------


set.seed(1)
Design_factorial3 <- gen.factorial(levels = c(2, 3), nVars = 2, 
varNames = c("Sex", "race"), center = FALSE)

repeated_measure_data <- purrr :: map_dfr(seq_len(10), ~Design_factorial3)


set.seed(1)
repeated_measure_data <- repeated_measure_data %>% 
        mutate(Time1 = (rnorm(60, 5, sd = 1) %>% abs) %>% round(2), 
        Time2 = (Time1 + rnorm(60, mean = Sex, sd = 1) %>% abs) %>% round(2), 
        Time3 = (Time2 + rnorm(60, mean = race, sd = 1) %>% abs) %>% 
        round(2)) %>%
        mutate(Sex = case_match(Sex, 1 ~ "male", 2 ~ "female") %>% as.factor, 
        race = case_match(race, 
                1 ~ "white", 2 ~ "others", 3 ~ "black") %>% as.factor, 
        id = 1:length(Sex)) %>%
        relocate(id, .before = Sex)

write_xlsx(repeated_measure_data, path = "Repeated_Measure_data.xlsx", col_names = TRUE)
long_dat <- repeated_measure_data %>%
pivot_longer(cols = starts_with("Time"), names_to = "Time", values_to = "Weight") %>%
                relocate(Weight, .before = Time) %>%
                mutate(Time = as.factor(Time))

new_dat <- within(long_dat, Interaction <- interaction(interaction(Time, Sex), race))
new_dat %>% names
new_dat$id %>% table
new_dat$Interaction %>% table
# library(tidyverse)
# library(rstatix)
library(reshape2)

long_dat2 <- melt(repeated_measure_data, id = c("Sex", "race", "id"), 
                value.name = "Weight", variable.name = "Time")
                
lapply(long_dat2, class)
long_dat2 %>% names
repeated_measure_model <- long_dat %>%
        anova_test(dv = Weight, wid = id, between = c(Sex, race), 
        effect.size = "pes", within = Time)
get_anova_table(repeated_measure_model, correction = "GG")


data("ToothGrowth")

df <- ToothGrowth
df
?anova_test
install.packages("datarium")
data("weightloss", package = "datarium")
weightloss %>% print(n = 48)
glimpse(selfesteem2)
selfesteem2$id
print(selfesteem2, n = 24)
Model <- aov(Weight ~ Time + Sex + race +Error(id), data = long_dat)
summary(Model)
# check assumption:
## outliers 

data %>%
  group_by(group1, ..., time) %>%
  identify_outliers("response variable")


## normality test

data %>%
  group_by(group1, ..., time) %>%
  shapiro_test("response variable")


## qqplot for combination groups and time

ggqqplot(data,  "response variable", ggtheme = theme_bw()) +
  facet_grid(time ~ group1 + ..., labeller = "label_both")

aov()
################with ez package####

library(ez)

ezANOVA(data, dv, within, between,
wid, within_covariate, between_covariate,
within_full)

with(repeated_measure_data, cor(cbind(Time1, Time2, Time3)))



# problem I R -------------------

ID <- c(1, 2, 3, 3, 4, 5, 6, 
6, 6, 7, 8, 9, 9)
X <- c("A", "B", "A", "B", "C", "B",
        "C", "A", "B", "D", "B", "B", "C")

ind <- ID[which(X == "A")]
X[ID %in% ind] <- "A"
data.frame(ID, X)[match(unique(ID), ID), ]

## manova

library(heplots)
Y <- cbind(Manova_data$y1, Manova_data$y2)
boxM(data = Manova_data, Y ~ interaction(Gr1, Gr2))
with(Manova_data, box_m(cbind(y1, y2), group = Gr2))


dd <- within(Manova_data, intt <- interaction(Gr1, Gr2))
box_m(dd[, c(1, 2)], dd[, 5])
install.packages("biotools")
biotools :: boxM(dd[, c(1, 2)], dd[, 5])
help(package = "heplots")


## create data for chi2 test

Design_factorial5 <- gen.factorial(levels = c(3, 2, 5), nVars = 3, 
varNames = c("race", "Sex", "continent"), center = FALSE)
chi2_dat <- Design_factorial5 %>%
mutate(race = case_match(race, 1 ~ "white", 2 ~ "black", 3 ~ "others"), 
Sex = case_match(Sex, 1 ~ "Female", 2 ~ "Male"), 
continent = case_match(continent, 1 ~ "Asia", 2 ~ "America", 3 ~ "Europe", 4 ~ "Africa", 5 ~ "Oceania")) %>%
mutate(Count = c(70, 50, 150, 85, 90, 110, 120, 145, 150, 95, 170, 133, 
230, 43, 21, 240, 44, 15, 300, 10, 5, 293, 15, 21, 97, 33, 43, 110, 45, 60))
write_xlsx(chi2_dat, path = "chi2_data.xlsx", col_names = TRUE)

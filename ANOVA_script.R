# https://statsandr.com/blog/anova-in-r/

# install.packages("palmerpenguins")
# install.packages("remotes")
# install.packages('multcomp')
library(palmerpenguins)
library(tidyverse)
library(remotes)
library(multcomp)

dat <- penguins %>%
  select(species, flipper_length_mm)

summary(dat)

ggplot(dat) +
  aes(x = species, y = flipper_length_mm, color = species) +
  geom_jitter() +
  theme(legend.position = "none")

res_aov <- aov(flipper_length_mm ~ species,
               data = dat
)

par(mfrow = c(2, 1)) # combine plots
dev.off() # reset
# histogram
hist(res_aov$residuals)

# QQ-plot
library(car)
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)

# Normality test
shapiro.test(res_aov$residuals)

## Equality of variances - homogeneity
# Boxplot
boxplot(flipper_length_mm ~ species,
        data = dat
)

# Dotplot
library("lattice")

dotplot(flipper_length_mm ~ species,
        data = dat
)
#####################################################################
The null and alternative hypothesis for both tests are:
  
  H0: variances are equal
  H1: at least one variance is different
#####################################################################
  
# Levene's test

leveneTest(flipper_length_mm ~ species,
           data = dat
)

# Another method to test normality and homogeneity
  par(mfrow = c(1, 2)) # combine plots
  
  # 1. Homogeneity of variances
  plot(res_aov, which = 3)
  
  # 2. Normality
  plot(res_aov, which = 2)

# Descriptive statistics    
group_by(dat, species) %>%
    summarise(
      mean = mean(flipper_length_mm, na.rm = TRUE),
      sd = sd(flipper_length_mm, na.rm = TRUE)
    )
## ANOVA in R

################################################################################
H0: μAdelie =  μChinstrap =   μGentoo(  ⇒  the 3 species are equal in terms of flipper length)
H1: at least one mean is different (  ⇒  at least one species is different from the other 2 species in terms of flipper length)
################################################################################




# 1st method:
oneway.test(flipper_length_mm ~ species,
            data = dat,
            var.equal = TRUE # assuming equal variances
)

# 2nd method:
res_aov <- aov(flipper_length_mm ~ species,
               data = dat
)

summary(res_aov)

## Post-hoc tests in R and their interpretation
# Tukey HSD test:
post_test <- glht(res_aov,
                  linfct = mcp(species = "Tukey")
)

summary(post_test)

par(mar = c(3, 8, 3, 3))
plot(post_test)

# Note that the Tukey HSD test can also be done in R with the TukeyHSD() function:

TukeyHSD(res_aov)

# Dunnett's test:
post_test <- glht(res_aov,
                  linfct = mcp(species = "Dunnett")
)

summary(post_test)

# If we want to change the reference category
# Change reference category:
dat$species <- relevel(dat$species, ref = "Gentoo")

# Check that Gentoo is the reference category:
levels(dat$species)

res_aov2 <- aov(flipper_length_mm ~ species,
                data = dat
)
#  We can then run the Dunett’s test with the new results of the ANOVA:
  
# Dunnett's test:
  post_test <- glht(res_aov2,
                    linfct = mcp(species = "Dunnett")
  )

summary(post_test)
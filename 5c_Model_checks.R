
##%######################################################%##
#                                                          #
####                    Model checks                    ####
#                                                          #
##%######################################################%##

# https://easystats.github.io/performance/

# high collinearity between Lu and UI.  Discussed with Tim and since there is
# this issue and a problem with projecting UI in space, UI has been dropped. 


# check out the performance package
library(performance)

#### checks: richness ####

# 1. check overdispersion

check_overdispersion(sr1$model)

# Overdispersion test

# dispersion ratio =    0.469
# Pearson's Chi-Squared = 6412.834
#                p-value =        1

# No overdispersion detected.


# 2. check zero inflation

check_zeroinflation(sr1$model)

# Check for zero-inflation

# Observed zeros: 525
# Predicted zeros: 661
# Ratio: 1.26

# Model is overfitting zeros.


# 3. check for singular model fits

check_singularity(sr1$model)

# FALSE


# 4. check for heteroskedasticity

check_heteroscedasticity(sr1$model)

# Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.000).


# 5. visualisations of model checks

check_model(sr1$model)

# save the plots


# 6. Double checking the collinearity


check_collinearity(sr1$model)

# Check for Multicollinearity

# Low Correlation

# Parameter  VIF Increased SE
# Predominant_land_use 3.93         1.98
# poly(percNH_Jung4, 1) 1.59         1.26
# Use_intensity:poly(percNH_Jung4, 1) 2.70         1.64

# Moderate Correlation

# Parameter  VIF Increased SE
# Use_intensity 5.27         2.30

# High Correlation

# Parameter   VIF Increased SE
# Predominant_land_use:Use_intensity 22.22         4.71

## 1. Checking the fitted vs residuals relationship
p1 <- plot(sr1$model)


## 2. Normality of Residuals
pdf(NULL)
dev.control(displaylist="enable")
qqnorm(resid(sr1$model), main = "")
qqline(resid(sr1$model))
p2 <- recordPlot()
invisible(dev.off())


## 3. Check for spatial autocorrelation

sa_test<-roquefort::SpatialAutocorrelationTest(model=sr1, all.data=sites.sub)

#summary(sa_test)

# percentage of studies that show spatial autocorrelation?
perc_auto <- (length(which(sa_test$P<0.05))/length(sa_test$P))*100


sa_test_vals <- as.data.frame(sa_test$P)
sa_test_vals$`sa_test$P` <- round(sa_test_vals$`sa_test$P`, digits = 4)

label1 <- paste0("P < 0.05 \nin ", round(perc_auto, 1), "% \nof studies")

p3 <- ggplot(data = sa_test_vals ) +
  geom_histogram(aes(x = sa_test_vals$`sa_test$P`)) +
  geom_vline(xintercept = 0.05, col = "red") +
  geom_text(aes(x = 0.9, y = 90, label = label1), size = 4, check_overlap = T) +
  theme_bw() +
  ylim(c(0, 100)) +
  xlab("P-value") +
  ylab("Frequency") +
  theme(panel.grid = element_blank(), 
        aspect.ratio = 1)


plot_grid(p2,p3,
          labels = c("A.", "B."))
ggsave(file = paste0(outdir, "/Model_checks_additional_Richness.pdf"), height = 5, width = 10)


#### checks: abundance ####

# 1. check overdispersion 

# GLMMs only, not valid here where family = gaussian

# 2. check zero inflation

# GLMMs only, not valid here where family = gaussian

# 3. check for singular model fits

check_singularity(ab1$model)

# FALSE

# 4. check for heteroskedasticity

check_heteroscedasticity(ab1$model)

# OK: Error variance appears to be homoscedastic (p = 0.455).


# 5. visualisations of model checks

check_model(ab1$model)

# save plots

# 6. double check collinarity

check_collinearity(ab1$model)

# 7. Check outliers
check_outliers(ab1$model)

# OK: No outliers detected.
# Warning messages:
#  1: Some model terms could not be found in model data. You probably need to load the data into the environment. 


# Check for Multicollinearity

# Low Correlation

# Parameter  VIF Increased SE
# Predominant_land_use 4.05         2.01
# poly(percNH_Jung4, 1) 2.86         1.69
# Predominant_land_use:poly(percNH_Jung4, 1) 2.30         1.52
# Use_intensity:poly(percNH_Jung4, 1) 3.23         1.80

# Moderate Correlation

# Parameter  VIF Increased SE
# Use_intensity 5.59         2.36

# High Correlation

# Parameter   VIF Increased SE
# Predominant_land_use:Use_intensity 20.34         4.51


# save the plots


## 1. Checking the fitted vs residuals relationship
p1 <- plot(ab1$model)


## 2. Normality of Residuals
pdf(NULL)
dev.control(displaylist="enable")
qqnorm(resid(ab1$model), main = "")
qqline(resid(ab1$model))
p2 <- recordPlot()
invisible(dev.off())


## 3. Check for spatial autocorrelation

sa_test<-roquefort::SpatialAutocorrelationTest(model=ab1, all.data=sites.sub)

#summary(sa_test)

# percentage of studies that show spatial autocorrelation?
perc_auto <- (length(which(sa_test$P<0.05))/length(sa_test$P))*100


sa_test_vals <- as.data.frame(sa_test$P)
sa_test_vals$`sa_test$P` <- round(sa_test_vals$`sa_test$P`, digits = 4)

label1 <- paste0("P < 0.05 \nin ", round(perc_auto, 1), "% \nof studies")

p3 <- ggplot(data = sa_test_vals ) +
  geom_histogram(aes(x = sa_test_vals$`sa_test$P`)) +
  geom_vline(xintercept = 0.05, col = "red") +
  geom_text(aes(x = 0.9, y = 90, label = label1), size = 4, check_overlap = T) +
  theme_bw() +
  ylim(c(0, 100)) +
  xlab("P-value") +
  ylab("Frequency") +
  theme(panel.grid = element_blank(), 
        aspect.ratio = 1)


plot_grid(p1,p2,p3,
          labels = c("A.", "B.", "C."))
ggsave(file = paste0(outdir, "/Model_checks_additional_Abundance.pdf"), height = 10, width = 10)


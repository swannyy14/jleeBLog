library(tidyverse)
library(naniar)

cofdat_tmp <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

print(dim(cofdat_tmp))

# coffee type of score
ggplot(cofdat_tmp) + 
  geom_boxplot(aes(x = species, y = total_cup_points))
# outlier sample with a score of 0? 

# number of bags vs score
ggplot(cofdat_tmp) + 
  geom_point(aes(x = number_of_bags, y = total_cup_points))

# defects vs score
ggplot(cofdat_tmp) + 
  geom_point(aes(x = category_two_defects, y = total_cup_points))

# missing data explore
vis_miss(cofdat_tmp, sort_miss = TRUE)
visdat::vis_dat(cofdat_tmp)
gg_miss_var(cofdat_tmp, show_pct = TRUE)

ggplot(cofdat_tmp) +
  geom_miss_point(aes(x = total_cup_points, y = altitude_mean_meters))
# a lot of missing points from altitude as well. remove

# 
cofdat <- cofdat_tmp %>%
  filter(total_cup_points > 50) %>% 
  select(
    total_cup_points, aroma, flavor, aftertaste, acidity,
    body, balance, uniformity, clean_cup, sweetness, cupper_points,
    moisture, category_one_defects, category_two_defects, quakers
  )

# one missing value in quakers variable
# removing one data shouldn't change the model too much
cofdat %>% miss_var_summary()
cofdat <- na.omit(cofdat)

cof_long <- cofdat %>% pivot_longer(cols = everything(), names_to = "Var", values_to = "value")

ggplot(cof_long) + 
  geom_boxplot(aes(x = Var, y = value)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, size = 8), axis.title = element_blank())

ggplot(cof_long) +
  geom_histogram(aes(value), binwidth = 1) +
  facet_wrap(facets = ~Var, scales = "free")

defects <- cofdat %>% select(category_one_defects, category_two_defects) %>% mutate(
  Index = ifelse(category_one_defects >= 20 | category_two_defects >= 30, 1:nrow(.), "")
)

ggplot(defects) +
  geom_point(aes(x = category_one_defects, y = category_two_defects)) +
  geom_text(aes(x = category_one_defects, y = category_two_defects + 2, label = Index)) +
  ggtitle("Category One Defect vs Category Two Defect")

# take a look at quaker vs total_cup_points
# don't see any alarming distribution / trend. 
ggplot(cofdat) +
  geom_point(aes(x = quakers, y = total_cup_points))

# correlation heatmap
cof_cor <- cor(cofdat)

library(gplots)
heatmap.2(cof_cor, symm = TRUE, col = bluered(100), trace = "none", main = "Correlation Between Variables\n(with Hierarchical Clustering)")

# Run Linear Regression
linreg <- lm(total_cup_points ~ ., data = cofdat)
summary(linreg)
plot(linreg)

library(car)
vif(linreg)

# ridge regression
# install.packages("glmnet")
library(glmnet)

# Choose the value of lambdas I'd like to test
lambdas <- exp(1)^seq(3, -10, by = -0.05)

# Cross validation with different lambdas
ridge <- cv.glmnet(
    x = as.matrix(cofdat[, -1]),
    y = cofdat$total_cup_points,
    alpha = 0,
    standardize = TRUE, # standardizes training data
    lambda = lambdas
  )
plot(ridge)

# choose lambda.1se
print(ridge$lambda.1se)
ridge_final <- glmnet(
  x = as.matrix(cofdat[, -1]),
  y = cofdat$total_cup_points,
  alpha = 0,
  standardize = TRUE, # standardizes training data
  lambda = ridge$lambda.1se
)

print(ridge_final$a0)
print(ridge_final$beta)

ridge_final$dev.ratio # r SQUARED

# LASSA
lasso <- cv.glmnet(
  x = as.matrix(cofdat[, -1]),
  y = cofdat$total_cup_points,
  alpha = 1,
  standardize = TRUE, # standardizes training data
  lambda = lambdas
)
plot(lasso)

print(lasso$lambda.1se)

lasso_final <- glmnet(
  x = as.matrix(cofdat[, -1]),
  y = cofdat$total_cup_points,
  alpha = 1,
  standardize = TRUE, # standardizes training data
  lambda = lasso$lambda.1se
)

print(lasso_final$a0)
print(lasso_final$beta)

# combine coefficients and compare
coefs <- data.frame(variable = colnames(cofdat)[-1])
coefs$lm <- linreg$coefficients[coefs$variable]
coefs$ridge <- ridge_final$beta[coefs$variable,]
coefs$lasso <- lasso_final$beta[coefs$variable,]

coefs_long <- coefs %>%
  mutate(variable = factor(variable, sort(variable))) %>%
  pivot_longer(cols = c("lm", "ridge", "lasso"), names_to = "Method", values_to = "Coefficients") %>% 
  filter(abs(Coefficients) > 0) %>%
  mutate(Method = factor(Method, c("lm", "ridge", "lasso"), labels = c("Linear Regression", "Ridge", "Lasso")))

# can't really see the differences among values
ggplot(coefs_long) + 
  geom_point(aes(x = variable, y = Coefficients, color = Method)) +
  ggtitle("Coefficient Comparison across Three Models") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# 
coefs_long$low <- ifelse(coefs_long$Coefficients < 0.1, TRUE, FALSE)
ggplot(filter(coefs_long)) + 
  facet_grid(rows = vars(low), scales = "free_y") +
  geom_point(aes(x = variable, y = Coefficients, color = Method)) +
  ggtitle("Coefficient Comparison across Three Models") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



# ==============================
# STEP 1: LOAD DATA
# ==============================
# install.packages("readxl")  # run once
library(readxl)

#df <- read_excel(file.choose())
path<-"C:/Users/rohan/rohan clark/Sem 2/time series regression/assignment and projects/final_project/final/Ames_housing_project.xlsx"

df <- read_excel(path)
P1 <- df

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
summary(P1) #Added Rohan
#Observations
#Not many NAs seen in the file. there seems to be Outliers in Lot area,1st flr sqF,
#Gr Liv Area, Tot rooms, Garage Area
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Columns containing near zero variance in data were dropped

# ==============================
# STEP 2: SELECT REQUIRED COLUMNS
# ==============================
P2 <- P1[, c("Order", "Lot Area", "Lot Config", "Bldg Type",
             "Overall Qual", "Overall Cond", "Year Remod/Add",
             "Roof Style", "Roof Matl", "Foundation", "Heating QC",
             "1st Flr SF", "Gr Liv Area", "Full Bath",
             "Bedroom AbvGr", "TotRms AbvGrd", "Garage Area",
             "Mo Sold", "Yr Sold", "SalePrice")]

summary(P2)
# ==============================
# STEP 3: HANDLE MISSING VALUES
# ==============================
sum(is.na(P2))
colSums(is.na(P2))

P2$'Garage Area'[is.na(P2$'Garage Area')] <- median(
  P2$'Garage Area'[P2$'Garage Area' != 0 & !is.na(P2$'Garage Area')]
)

#Overview of the data frame in pair plot
windows()
pairs(P2[sapply(P2, is.numeric)], col = "red", lower.panel = NULL,
      main = "Scatter Plot Matrix - Ames Housing")

#Box plot to visualize some outliers
boxplot(P2$`Lot Area`,
        main = "Boxplot of Lot area outliers",
        horizontal = TRUE,
        col = "lightblue")
boxplot(P2$`SalePrice`,
        main = "Boxplot of SalePrice outliers",
        horizontal = TRUE,
        col = "lightblue")

boxplot(
  list(
    `Garage Area` = P2$`Garage Area`,
    `1st Flr SF` = P2$`1st Flr SF`,
    `Gr Liv Area` = P2$`Gr Liv Area`
   
  ),
  horizontal = TRUE,
  col = "lightblue",
  main = "Boxplot of Outliers",
  las = 1   # makes labels horizontal for readability
)


#Capping 4 outliers to minimize impact
#P2$`Lot Area`[P2$`Lot Area` > 50000] <- 52000
#P2$`Gr Liv Area`[P2$`Gr Liv Area` > 3000] <- 3100


#Scaling for transformation of variables

""" 
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

for (col in names(P2)) {
  if (is.numeric(P2[[col]])) {
    P2[[col]][is.na(P2[[col]])] <- mean(P2[[col]], na.rm = TRUE)
  } else {
    P2[[col]][is.na(P2[[col]])] <- get_mode(P2[[col]])
  }
}
"""

# ==============================
# STEP 4: DEFINE VARIABLES
# ==============================
Y   <- P2$SalePrice
X1  <- P2$`Lot Area`
X2  <- P2$`Overall Qual`
X3  <- P2$`Overall Cond`
X4  <- P2$`Year Remod/Add`
X5  <- P2$`1st Flr SF`
#X6  <- P2$`Low Qual Fin SF`
X7  <- P2$`Gr Liv Area`
X8  <- P2$`Full Bath`
X9  <- P2$`Bedroom AbvGr`
X10 <- P2$`TotRms AbvGrd`
X11 <- P2$`Garage Area`
X12 <- P2$`Mo Sold`
X13 <- P2$`Yr Sold`

# ==============================
# STEP 5: DUMMY VARIABLES
# ==============================
X14 <- ifelse(P2$`Bldg Type` == "TwnhsE", 1, 0)
X15 <- ifelse(P2$`Bldg Type` == "Duplex", 1, 0)
X16 <- ifelse(P2$`Bldg Type` == "Twnhs",  1, 0)
X17 <- ifelse(P2$`Bldg Type` == "2fmCon", 1, 0)

# ==============================
# STEP 6: CLEAN DATASET
# ==============================
clean <- data.frame(
  Y, X1, X2, X3, X4, X5, X7, X8, X9,
  X10, X11, X12, X13, X14, X15, X16, X17
)
library(dplyr)
glimpse(clean)
sapply(clean, length)

n <- nrcleann <- nrow(clean)

# ==============================
# STEP 7: DESIGN MATRIX
# ==============================
X0 <- rep(1, n)

X <- as.matrix(cbind(
  X0, X1, X2, X3, X4, X5, X7,
  X8, X9, X10, X11, X12, X13,
  X14, X15, X16, X17
))

p <- ncol(X)
Y <- as.matrix(Y)

# ==============================
# STEP 8: MATRIX REGRESSION
# ==============================
XtX <- t(X) %*% X
XtY <- t(X) %*% Y
XXI <- solve(XtX)

b <- XXI %*% XtY

Y_hat <- X %*% b
e <- Y - Y_hat

# ==============================
# STEP 9: ANOVA
# ==============================
H   <- X %*% XXI %*% t(X)
I_n <- diag(n)
J_n <- matrix(1/n, n, n)

SST <- t(Y) %*% (I_n - J_n) %*% Y
SSE <- t(Y) %*% (I_n - H) %*% Y
SSR <- t(Y) %*% (H - J_n) %*% Y

df_SSE <- n - p
df_SSR <- p - 1

MSE <- SSE / df_SSE
MSR <- SSR / df_SSR

Fstar <- MSR / MSE
Fstar
pvalF <- pf(Fstar, df_SSR, df_SSE, lower.tail = FALSE)
qf(0.95,df1=df_SSR,df2=df_SSE)
print(pvalF, digits= 20) 

#base full model
baselm<- lm(SalePrice ~ ., data = P2)
summary(baselm)
plot(baselm)

# ==============================
# STEP 10: MODEL FIT
# ==============================
R2  <- 1 - SSE/SST
R2a <- 1 - (n-1)*(SSE/SST)/(n-p)
R2
R2a
cat("R2:", R2, "\n")
cat("Adj R2:", R2a, "\n")

# ==============================
# STEP 11: T-TESTS
# ==============================
s2_b <- MSE[1,1] * XXI
se_b <- sqrt(diag(s2_b))

tstar <- b / se_b
pvals <- 2 * pt(abs(tstar), df_SSE, lower.tail = FALSE)

print(cbind(b, se_b, tstar, pvals))

#X3, X8, X10, X12, X13 are having low t values

# ==============================
# STEP 12: LM MODEL (FOR TESTS)
# ==============================
# install.packages("lmtest")
# install.packages("car")
install.packages("car")
library(lmtest)
library(car)

model_lm <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X7 + X8 +
                 X9 + X10 + X11 + X12 + X13 +
                 X14 + X15 + X16 + X17,
               data = clean)

summary(model_lm)
"""
clean_reg <- data.frame(
  SalePrice = Y,
  Lot_Area = X1,
  Overall_Qual = X2,
  Overall_Cond = X3,
  Year_Remod = X4,
  First_Flr_SF = X5,
  Gr_Liv_Area = X7,
  Full_Bath = X8,
  Bedroom_AbvGr = X9,
  TotRms_AbvGrd = X10,
  Garage_Area = X11,
  Mo_Sold = X12,
  Yr_Sold = X13,
  TwnhsE = X14,
  Duplex = X15,
  Twnhs = X16,
  TwoFmCon = X17
)

model_lm <- lm(SalePrice ~ ., data = clean_reg)

summary(model_lm)
"""
# ==============================
# STEP 13: DIAGNOSTICS
# ==============================

#Plots of Residual Analysis
plot(Y_hat,e,xlab="Fitted Values",ylab="Residuals", main="Residuals vs Y_hat")
plot(X1,e,xlab="LotArea",ylab="Residuals", main="Residuals vs X1")
plot(X2,e,xlab="Overall Qual",ylab="Residuals", main="Residuals vs X2")
plot(X3,e,xlab="Overall Cond",ylab="Residuals", main="Residuals vs X3")
plot(X4,e,xlab="Year Remod/Add",ylab="Residuals", main="Residuals vs X4")
plot(X5,e,xlab="1st flr SF",ylab="Residuals", main="Residuals vs X5")
plot(X7,e,xlab="Gr Liv Area",ylab="Residuals", main="Residuals vs X7")
plot(X8,e,xlab="Full bath",ylab="Residuals", main="Residuals vs X8")
plot(X9,e,xlab="Bedroom AbvGr",ylab="Residuals", main="Residuals vs X9")
plot(X10,e,xlab="TotRms AbvGrd",ylab="Residuals", main="Residuals vs X10")
plot(X11,e,xlab="Garage Area",ylab="Residuals", main="Residuals vs X11")
plot(X12,e,xlab="Mo Sold",ylab="Residuals", main="Residuals vs X12")
plot(X13,e,xlab="Yr Sold",ylab="Residuals", main="Residuals vs X13")
plot(X14,e,xlab="Bldg Type TwnhsE",ylab="Residuals", main="Residuals vs X14")
plot(X15,e,xlab="Bldg Type Duplex",ylab="Residuals", main="Residuals vs X15")
plot(X16,e,xlab="Bldg Type TwnhsE",ylab="Residuals", main="Residuals vs X16")
plot(X17,e,xlab="Bldg Type 2fmHse",ylab="Residuals", main="Residuals vs X17")


# Normality plot
qqnorm(e,main="Normal Probability Plot of Residuals")
qqline(e)

# Independent assumption plot
plot(e,xlab="Time",ylab="Residuals", main="Residuals vs Time")

#Lack of fit test
install.packages("olsrr")
library(olsrr)
ols_pure_error_anova(model_lm)



cat("\nBREUSCH-PAGAN TEST\n")
print(bptest(model_lm))

#Shapiro wilk test for normality
shapiro.test(e)

#Durbin-Watson Test for Autocorrelated Residuals: If p<alpha reject null hypothesis and conclude autocorolation
dwtest(model_lm)

###### Multicolinearity
cat("\nVIF\n")
print(vif(model_lm))

cat("\nQQ PLOT\n")
qqnorm(residuals(model_lm))
qqline(residuals(model_lm), col="red")




# ==============================
# STEP 14: dropping certain variables based on low t stat
# ==============================
model_lm_mod1 <- lm(Y ~ X1 + X2 + X4 + X5 + X7 + 
                 X9 + X11 +X14 + X15 + X16 + X17,
               data = clean)
summary(model_lm_mod1)

#Using a scaling-correlation transformation model to check if the R square improves 
clean_scaled <- clean

vars_to_scale <- c("X1","X4","X5","X7",
                   "X11")

clean_scaled[vars_to_scale] <- scale(clean_scaled[vars_to_scale])
model_lm_mod2 <- lm(Y ~ X1 + X2  + X5 + X7 +
                      X9 + X11 + X14 + X15 + X16 + X17,
                    data = clean_scaled)
summary(model_lm_mod2)  
windows()

plot(model_lm_mod2)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Remedial Measures@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@ Detecting and treating outliers, points of influence and leverage @@@@@@
  
dfbetas(model_lm_mod2)
#dffits
influence.measures(model_lm_mod2)

#plot 
install.packages("olsrr")
library("olsrr")
ols_plot_cooksd_bar(model_lm_mod2)
ols_plot_dfbetas(model_lm_mod2)

######### Outlier detection in y variable with Cook's distance######
plot(cooks.distance(model_lm_mod2))
abline(h = 4/length(Y), col = "red")
#Points like 1499 and 2181 have huge influence and should be removed. 

####################Finding the points of influence. ##############
car::influencePlot(model_lm_mod2)

"""
# Observation: points 1499 and 2181 are massive outliers with huge Cook's distance 
and high studentized residuals. We recommend removing them. 

         StudRes        Hat       CookD
957    0.1992622 0.27449116 0.001366110
1499 -14.7566707 0.05003623 0.970628413
1571  -0.6170058 0.16627987 0.006903945
2181 -12.9512712 0.05039503 0.765512464

"""
bad_points <- c(957,1259, 1499,1571,1638,1761,1768, 2181,2182,2738)

clean_no_outlier <- clean[-bad_points, ]
vars_to_scale <- c("X1","X4","X5","X7",
                   "X11")

clean_no_outlier[vars_to_scale] <- scale(clean_no_outlier[vars_to_scale])
summary(clean_no_outlier)

model_scaled__no_outlier <- lm(Y ~ X1 + X2 + X5 + X7 + X9 + X11 + X14 + X15 + X16 + X17,
                  data = clean_no_outlier)
summary(model_scaled__no_outlier)
plot(model_scaled__no_outlier)
bptest(model_scaled__no_outlier)

############testing for interactions#########################################

model_interact <- lm(
  Y ~ (X1 + X2 + X4 + X5 + X7 + X9 + X11 + 
         X14 + X15 + X16 + X17)^2,
  data = clean_no_outlier
)
summary(model_interact)
anova(model_lm_mod2, model_interact)
car::avPlots(model_lm_mod1)

model_lm_mod3_interact <- lm(
  Y ~ (X1 + X2 + X4 + X5 + X7 + X9 + X11 + 
         X14 + X15 + X16 + X17+ X1:X7 +X1:X9 +X2:X5 +X2:X7 +X2:X9 +X4:X7+
         X4:X9),
  data = clean_no_outlier
)
#+X5:X7+X7:X9
summary(model_lm_mod3_interact)
anova(model_lm_mod2, model_lm_mod3_interact)
bptest(model_lm_mod3_interact)

#Breusch-Pagan is still low; we will attempt to transform the y variable itself. 
clean_no_outlier$logY <- log(clean_no_outlier$Y)
clean_no_outlier$X1log  <- log(clean_no_outlier$X1)
clean_no_outlier$X7log  <- log(clean_no_outlier$X7)
clean_no_outlier$X5log  <- log(clean_no_outlier$X5)
clean_no_outlier$X11log <- log(clean_no_outlier$X11)


model_logY <- lm(
  logY ~ X1 + X2 + X4 + X5 + X7 + X9 + X11 +
    X14 + X15 + X16 + X17+X1:X7 +X1:X9 +X2:X5 +X2:X7 +X2:X9 +X4:X7+
    X4:X9,
  data = clean_no_outlier
)

model_log_predictors <- lm(
  Y ~ X1log + X2 + X4 + X5log + X7log + X9 + X11log +
    X14 + X15 + X16 + X17+X1:X7 +X1:X9 +X2:X5 +X2:X7 +X2:X9 +X4:X7+
    X4:X9,
  data =clean_no_outlier
)
summary(model_log_predictors)
bptest(model_log_predictors)

summary(  model_logY)
anova(model_lm_mod2,       model_logY)
bptest(  model_logY)

#  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Treating Non constant variance- Weighted Least Square (WLS) 

#library(nlme)model_gls<-
model_gls <-gls(Y ~ X1 + X2  + X5 + X7 +
      X9 + X11 + X14 + X15 + X16 + X17+
        X1:X7 +X1:X9 +X2:X5 +X2:X7 +X2:X9 +X4:X7+ X4:X9,
    data = clean_no_outlier, weights=varPower(),
    control = glsControl(maxIter = 200, msMaxIter = 200, tolerance = 1e-6))

summary(model_gls)
plot(model_gls)

summary(model_gls)$modelStruct


AIC(model_lm_mod2, model_gls)

bptest(model_gls)


############testing influential points with robust regression#########################################
## ==================== LAD (Quantile τ = 0.5) ==================
model_lm_mod2 <- lm(Y ~ X1 + X2  + X5 + X7 +
                      X9 + X11 + X14 + X15 + X16 + X17,
                    data = clean_scaled)


library(quantreg)
model_lm_robust <- rq(Y ~ X1 + X2  + X5 + X7 +
                        X9 + X11 + X14 + X15 + X16 + X17+
                      X1:X7 +X1:X9 +X2:X5 +X2:X7 +X2:X9 +X4:X7+ X4:X9
                      , tau = 0.5, 
                      data =clean_no_outlier,method = "fn" )

## Coefficient comparison
coef_ols <- coef(model_lm_mod2)   
coef_lad <- coef(model_lm_robust)
colnames(Influe_table)<-cbind("OLS","LAD")
Influe_table<-cbind(coef_ols, coef_lad)
cat("\n================ OLS vs LAD Coefficients ================\n")

print(round(Influe_table, 6))
bptest(model_lm_robust)

####################################################################################




# ==============================
# STEP 14: STEPWISE SELECTION
# ==============================
library(MASS)
AIC(baselm,model_lm, model_lm_mod1,model_lm_mod2,model_lm_mod3_interact,model_logY,model_lm_robust,model_gls)
BIC(baselm,model_lm, model_lm_mod1,model_lm_mod2,model_lm_mod3_interact,model_logY,model_lm_robust,model_gls)


best_aic_model <- stepAIC(model_gls, direction = "both")
summary(best_aic_model)
best_bic_model <- stepAIC(model_gls, direction = "both", k = log(nrow(clean)))
summary(best_bic_model)

library(leaps)

subset_fit <- regsubsets(
  Y ~ X1 + X2 + X4 + X5 + X7 + X9 + X11 + X14 + X15 + X16 + X17+
    X1:X7 +X1:X9 +X2:X5 +X2:X7 +X2:X9 +X4:X7+ X4:X9,
  data = clean_no_outlier ,
  nbest = 1,
  nvmax = 11
)
summary(subset_fit)
windows()
plot(subset_fit)



"""
# ==============================
# STEP 15: LOG TRANSFORMATION
# ==============================
X1log <- log(P2$`Lot Area`)
clean_scaled2<-clean_scaled
clean_scaled2$X1log <- log(P2$`Lot Area`)


model_log <- lm(Y ~ X1log + X2 + X4 + X5 + X7 + X9 + X11 + 
                  X14 + X15 + X16 + X17,
                data = clean_scaled2)

summary(model_log)
bptest(model_log)
"""
# ==============================
# STEP 16: OUTLIERS
# ==============================
"""cooks_d <- cooks.distance(model_lm)
threshold <- 4/n

outliers <- which(cooks_d > threshold)

cat("Outliers:", outliers, "\n")

plot(cooks_d, type="h")
abline(h=threshold, col="red")"""

# ==============================
# STEP 17: FINAL MODEL VALIDATION
# ==============================
set.seed(42) 
n <- nrow(clean_no_outlier)
train_index <- sample(1:n, size = 0.7*n)

train_data <- clean_no_outlier[train_index, ]
test_data  <- clean_no_outlier[-train_index, ]

#Training 

# model without interaction with scaling no outlier
model_lm_mod2_train <- lm(
  Y ~ (X1 + X2 + X4 + X5 + X7 + X9 + X11 + 
         X14 + X15 + X16 + X17),
  data = train_data
)

# model with interaction with scaling no outlier
model_lm_mod3_interact_train <- lm(
  Y ~ (X1 + X2 + X4 + X5 + X7 + X9 + X11 + 
         X14 + X15 + X16 + X17+ X1:X7 +X1:X9 +X2:X5 +X2:X7 +X2:X9 +X4:X7+
         X4:X9),
  data = train_data
)
summary(model_lm_mod3_interact_train)

# model with gls with scaling no outlier
model_gls_train <-gls(Y ~ X1 + X2  + X5 + X7 +
                  X9 + X11 + X14 + X15 + X16 + X17+
                  X1:X7 +X1:X9 +X2:X5 +X2:X7 +X2:X9 +X4:X7+ X4:X9,
                data = train_data, weights=varPower(),
                control = glsControl(maxIter = 200, msMaxIter = 200, tolerance = 1e-6))

#Calculating the mean square error of the train model 
train_pred_lm_mod2 <- predict(model_lm_mod2_train, newdata = train_data)
train_pred_interact <- predict(model_lm_mod3_interact_train, newdata = train_data)
train_pred_model_gls <-predict(model_gls_train,newdata = train_data)

MSC_train_lm_mod2 <- mean((train_data$Y - train_pred_lm_mod2)^2)
MSC_train_interact <- mean((train_data$Y - train_pred_interact)^2)
MSC_train_model_gls <- mean((train_data$Y - train_pred_model_gls)^2)

MSC_train_lm_mod2
MSC_train_interact
MSC_train_model_gls

test_pred_lm_mod2 <- predict(model_lm_mod2_train, newdata = test_data)
test_pred_interact  <- predict(model_lm_mod3_interact_train, newdata = test_data)
test_pred_model_gls <- predict(model_gls_train, newdata = test_data)
summary(test_pred_interact)


MSC_test_lm_mod2 <- mean((test_data$Y - test_pred_lm_mod2)^2)
MSC_test_interact <- mean((test_data$Y - test_pred_interact)^2)
MSC_test_gls <- mean((test_data$Y - test_pred_model_gls)^2)

MSC_test_lm_mod2
MSC_test_interact
MSC_test_gls

#Plotting 
windows()
plot(test_data$Y, test_pred_lm_mod2,
     xlab = "Actual Y",
     ylab = "Predicted Y",
     main = "Actual vs Predicted on base train vs Test Set")
abline(0, 1, col = "red", lwd = 2)

plot(test_data$Y, test_pred_interact,
     xlab = "Actual Y",
     ylab = "Predicted Y",
     main = "Actual vs Predicted on interaction train Test Set")
abline(0, 1, col = "red", lwd = 2)

plot(test_data$Y, test_pred_model_gls,
     xlab = "Actual Y",
     ylab = "Predicted Y",
     main = "Actual vs Predicted on gls train Test Set")
abline(0, 1, col = "red", lwd = 2)



m_ident <- gls(logY ~ LotArea_z + GarageArea_z + TotRmsAbvGrd_z +
                 LotArea_z:GarageArea_z +
                 GarageArea_z:TotRmsAbvGrd_z,
               data = clean_no_outlier, weights = varIdent(form = ~1|LotArea_group))

m_power <- gls(logY ~ LotArea_z + GarageArea_z + TotRmsAbvGrd_z +
                 LotArea_z:GarageArea_z +
                 GarageArea_z:TotRmsAbvGrd_z,
               data = clean_no_outlier, weights = varPower(form = ~GarageArea_z))

m_exp   <- gls(logY ~ LotArea_z + GarageArea_z + TotRmsAbvGrd_z +
                 LotArea_z:GarageArea_z +
                 GarageArea_z:TotRmsAbvGrd_z,
               data = clean_no_outlier, weights = varExp(form = ~GarageArea_z))

AIC(m_ident, m_power, m_exp)




























###############################################################################
## ----- Feasible WLS (IRLS) using |e| ~ X to model scale -----------------------
# Initial scale: |e| ~ X

## THIS MODEL HAS only SELECT VARIABLES#########
"""
X0 <- rep(1, n)

X <- as.matrix(cbind(
  X0, X1, X2, X4, X5, X7,
 X9,  X11, X14, X15, X16, X17
  
))

p <- ncol(X)
Y <- as.matrix(Y)

# ==============================
# STEP 8: MATRIX REGRESSION
# ==============================
XtX <- t(X) %*% X
XtY <- t(X) %*% Y
XXI <- solve(XtX)

b_ols <- XXI %*% XtY

Y_hat <- X %*% b
e <- Y - Y_hat

# ==============================
# STEP 9: ANOVA
# ==============================
H   <- X %*% XXI %*% t(X)
I_n <- diag(n)
J_n <- matrix(1/n, n, n)

SST <- t(Y) %*% (I_n - J_n) %*% Y
SSE <- t(Y) %*% (I_n - H) %*% Y
SSR <- t(Y) %*% (H - J_n) %*% Y

df_SSE <- n - p
df_SSR <- p - 1

"""
m_ols <- lm(Y ~ X1 + X2 + X5 + X7 + X9 + X11 + X14 + X15 + X16 + X17, data = clean_scaled)
plot(m_ols)          # residuals, scale-location
car::vif(m_ols)      # multicollinearity
plot(rstandard(m_ols))
abline(h = c(-3, 3), col = "red")




e_abs <- abs(e)
b_e   <- solve(XtX) %*% (t(X) %*% e_abs)
s_hat <- X %*% b_e

# Initial weights (inverse variance) with guards
w     <- 1 / (as.numeric(s_hat)^2)
W     <- diag(w)

## ----- IRLS loop: stop when ||b_new - b_old||^2 ≤ tol -------------------------
tol      <- 0.00001
max_iter <- 1000
iter     <- 0

b_old    <- b_ols
diff2    <- Inf

while (diff2 > tol && iter < max_iter) {
  iter <- iter + 1
  
  # Weighted normal equations using YOUR definition
  XtWX <- t(X) %*% W %*% X
  XtWY <- t(X) %*% W %*% Y
  b_wls <- solve(XtWX) %*% XtWY
  
  diff2 <- as.numeric(t(b_wls - b_old) %*% (b_wls - b_old))
  
  # Update residuals -> new scale via |e_w| ~ X -> new weights
  Yhat_w <- X %*% b_wls
  e_w    <- Y - Yhat_w
  e_wabs <- abs(e_w)
  
  b_we   <- solve(XtX) %*% (t(X) %*% e_wabs)
  s_what <- X %*% b_we
  
  w_new  <- 1 / (as.numeric(s_what))^2
  w_new  <- w_new
  
  W      <- diag(w_new)
  b_old  <- b_wls
  
  cat(sprintf("iter %d: (b-b_wls)'(b-b_wls) = %.6g  (b0=%.6g, b1=%.6g)\n",
              iter, diff2, b_wls[1], b_wls[2]))
}

if (iter >= max_iter) warning("Reached max_iter before convergence.")

## ----- Final variance/SE using final W ----------------------------------------
XtWX <- t(X) %*% W %*% X
XtWY <- t(X) %*% W %*% Y
b_wls <- solve(XtWX) %*% XtWY

Yhat_w <- X %*% b_wls
e_w    <- Y - Yhat_w

"""

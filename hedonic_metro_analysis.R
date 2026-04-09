# --- Load packages ---
library(readxl)
library(dplyr)
library(ggplot2)
library(stargazer)
library(lmtest)
library(sandwich)
library(spdep)
library(sf)
library(scales)
library(gridExtra)
library(car)
library(broom)
install.packages("tseries")      # for JB test
library(tseries)
cat("All packages loaded successfully.\n")

data_folder <- "'/Users/arnavkalaver/Desktop/SEM4 ICA/PUBLIC ECONOMICS ICA/DATA'"   # <-- CHANGE THIS to your folder path

# --- Load all four city datasets ---
busan   <- read_excel("/Users/arnavkalaver/Desktop/SEM4 ICA/PUBLIC ECONOMICS ICA/DATA/prices_busan.xlsx")
daegu   <- read_excel("/Users/arnavkalaver/Desktop/SEM4 ICA/PUBLIC ECONOMICS ICA/DATA/prices_daegu.xlsx")
daejeon <- read_excel("/Users/arnavkalaver/Desktop/SEM4 ICA/PUBLIC ECONOMICS ICA/DATA/prices_daejeon.xlsx")
gwangju <- read_excel("/Users/arnavkalaver/Desktop/SEM4 ICA/PUBLIC ECONOMICS ICA/DATA/prices_gwangju.xlsx")

# --- Add a city identifier column to each ---
# This is essential so when we merge them we know which row came from where
busan$city   <- "Busan"
daegu$city   <- "Daegu"
daejeon$city <- "Daejeon"
gwangju$city <- "Gwangju"

# --- Combine all four into one master dataset ---
df <- bind_rows(busan, daegu, daejeon, gwangju)

# --- Quick inspection ---
cat("\n===== DATASET OVERVIEW =====\n")
cat("Total observations:", nrow(df), "\n")
cat("Total columns:", ncol(df), "\n")
cat("\nColumn names:\n")
print(names(df))
cat("\nObservations by city:\n")
print(table(df$city))

# --- Check for missing values ---
cat("\nMissing values per column:\n")
print(colSums(is.na(df)))

# --- Rename columns to remove spaces (R doesn't like spaces in names) ---
df <- df %>%
  rename(
    spring          = Spring,
    fall            = Fall,
    winter          = Winter,
    longitude       = Longitude,
    latitude        = Latitude,
    dist_crow       = `Crow-fly distance to nearest subway station`,
    size_sqm        = `Size of unit`,
    price           = `Condominium price`,
    floor           = Floor,
    const_year      = `Construction year`,
    dist_network    = `Network distance to nearest subway station`,
    n_households    = `Number of households`,
    n_condo_bldgs   = `Number of condominium buildings`,
    highest_floor   = `Highest floor`,
    parking         = `Parking space per household`,
    heating         = `Heating type`,
    uni_entrants    = `Number of top university entrants`,
    n_high_schools  = `Number of high schools`,
    dist_cbd        = `Network distance to the CBD`,
    dist_greenspace = `Network distance to nearest greenspace`,
    dist_waterfront = `Network distance to nearest waterfront`,
    n_bus_stops     = `Number of bus stops`,
    population      = Population,
    sex_ratio       = `Sex ratio`,
    median_age      = `Medium age`,
    young_pop_ratio = `Young population ratio`,
    old_pop_ratio   = `Old population ratio`,
    pop_density     = `Population density`,
    higher_edu_ratio = `Ratio of adults with higher degrees`
  )

# Creating log price per sqm column
df <- df %>%
  mutate(
    price_per_sqm = price / size_sqm,
    ln_price_sqm  = log(price_per_sqm)   # This is your dependent variable
  )

# --- Create BUILDING AGE variable ---
# Construction year is less intuitive than age; age is more standard
# in hedonic models. We use 2015 as the reference year (max in data).
df <- df %>%
  mutate(building_age = 2015 - const_year)

# --- Create DISTANCE-SQUARED variable (for non-linearity) ---
# We expect the price effect of distance to be strongest close to
# the station and to diminish further away — a curved relationship.
# Including dist^2 lets the model capture this curve.
df <- df %>%
  mutate(
    dist_crow_sq    = dist_crow^2,
    dist_network_sq = dist_network^2
  )

# --- Create BUFFER ZONE dummy variables ---
# Zone 1: within 500m  (walking distance — strong premium expected)
# Zone 2: 500m to 2km  (short ride — moderate premium expected)
# Zone 3: beyond 2km   (reference/control group — no dummy needed)
df <- df %>%
  mutate(
    zone1 = as.integer(dist_crow < 500),
    zone2 = as.integer(dist_crow >= 500 & dist_crow < 2000)
    # zone3 is the baseline — omitted to avoid perfect multicollinearity
  )

# --- City as a factor variable (for fixed effects) ---
df$city <- as.factor(df$city)

# --- Heating type as a factor ---
df$heating <- as.factor(df$heating)

# --- Summary check ---
cat("\n===== KEY VARIABLES SUMMARY =====\n")
cat("\nPrice per sqm (KRW):\n")
print(summary(df$price_per_sqm))
cat("\nLog price per sqm:\n")
print(summary(df$ln_price_sqm))
cat("\nCrow-fly distance to subway (metres):\n")
print(summary(df$dist_crow))
cat("\nBuffer zones:\n")
cat("  Zone 1 (<500m):", sum(df$zone1), "properties\n")
cat("  Zone 2 (500m-2km):", sum(df$zone2), "properties\n")
cat("  Zone 3 (>2km):", nrow(df) - sum(df$zone1) - sum(df$zone2), "properties\n")

# GENERATING SUMMARY STATISTICS
df_clean <- df
library(moments)  # for skewness/kurtosis
library(gt) 
summary_stats <- df_clean %>%
  group_by(city) %>%
  summarise(
    Mean       = mean(ln_price_sqm, na.rm = TRUE),
    Median     = median(ln_price_sqm, na.rm = TRUE),
    SD         = sd(ln_price_sqm, na.rm = TRUE),
    IQR        = IQR(ln_price_sqm, na.rm = TRUE),
    Skewness   = skewness(ln_price_sqm, na.rm = TRUE),
    Kurtosis   = kurtosis(ln_price_sqm, na.rm = TRUE)
  ) %>%
  mutate(
    # Benchmarks: median symmetric = 0, kurtosis = 3 for normal
    Skewness_Benchmark = 0,
    Kurtosis_Benchmark = 3
  )

# Rounding values
summary_stats <- summary_stats %>%
  mutate(across(c(Mean, Median, SD, IQR, Skewness, Kurtosis), ~round(., 3)))

# generating a table
summary_stats %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics of log(Price per sqm) by City",
    subtitle = "With ideal benchmarks for skewness and kurtosis"
  ) %>%
  cols_label(
    SD = "Std. Dev.",
    IQR = "Interquartile Range",
    Skewness_Benchmark = "Skewness Ideal",
    Kurtosis_Benchmark = "Kurtosis Ideal"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  )

# =============================================================================
# DEEP DIVE — Log Price per sqm by City
# ── Summary statistics by city ────────────────────────────────────────────
cat("===== SUMMARY STATISTICS: ln(Price per sqm) BY CITY =====\n\n")

# Split data by city and apply describe() to ln_price_sqm in each
city_stats <- sapply(c("Busan", "Daegu", "Daejeon", "Gwangju"),
                     function(city_name) {
                       x <- df_clean$ln_price_sqm[df_clean$city == city_name]
                       c(
                         N          = round(length(x),0),
                         Mean       = round(mean(x), 2),
                         Median     = round(median(x), 2),
                         Std_Dev    = round(sd(x), 2),
                         Variance   = round(var(x), 2),
                         IQR        = round(IQR(x), 2),
                         Min        = round(min(x), 2),
                         Max        = round(max(x), 2),
                         Range      = round(max(x) - min(x), 2),
                         Skewness   = round(skewness(x), 2),
                         Kurtosis   = round(kurtosis(x), 2),
                         Ex_Kurtosis= round(kurtosis(x)-3, 2),
                         CV_pct     = round(sd(x) / mean(x) * 100, 2)
                       )
                     })

print(city_stats)

# HEDONIC REGRESSION: FULL MODEL WITH ALL CONTROLS
# =============================================================================

# --- Model 3: basic model
model3 <- lm(
  ln_price_sqm ~
    dist_crow +
    size_sqm +
    building_age +
    floor +
    highest_floor +
    parking +
    heating +
    pop_density +
    higher_edu_ratio +
    n_high_schools +
    uni_entrants +
    n_bus_stops +
    median_age,
  data = df
)

cat("MODEL 3:")
print(summary(model3))

# --- Model 4: Full model — + location controls + city FE + seasonal FE ---
# This is your PREFERRED / MAIN specification.
# City fixed effects absorb all time-invariant city-level differences
# (e.g., Busan has a port, Daejeon has government offices).
# Seasonal dummies control for price seasonality in Korean housing market.
model4 <- lm(
  ln_price_sqm ~
    dist_crow +
    size_sqm +
    building_age +
    floor +
    highest_floor +
    parking +
    heating +
    pop_density +
    higher_edu_ratio +
    n_high_schools +
    uni_entrants +
    n_bus_stops +
    median_age +
    dist_cbd +
    dist_greenspace +
    dist_waterfront +
    spring + fall + winter +   # seasonal FE (summer is the reference)
    city,                       # city fixed effects (Busan is reference)
  data = df
)

cat("\n===== MODEL 4: FULL MODEL (PREFERRED SPECIFICATION) =====\n")
print(summary(model4))

# --- Interpret the key coefficient ---
coef_dist <- coef(model4)["dist_crow"]
cat("\n--- KEY RESULT INTERPRETATION ---\n")
cat(sprintf(
  "Coefficient on dist_crow = %.6f\n", coef_dist
))
cat(sprintf(
  "A 1-km increase in distance = %.2f%% change in price per sqm.\n",
  coef_dist * 1000 * 100
))


# BUFFER ZONE ANALYSIS
# =============================================================================
# Instead of treating distance as continuous, we use zone dummies.
# This is more intuitive and does not impose a functional form.
# Coefficient on zone1 = % premium for being within 500m vs. beyond 2km.

# --- Model 5: Zone dummies instead of continuous distance ---
model5 <- lm(
  ln_price_sqm ~
    zone1 +
    zone2 +
    size_sqm +
    building_age +
    floor +
    highest_floor +
    parking +
    heating +
    pop_density +
    higher_edu_ratio +
    n_high_schools +
    uni_entrants +
    n_bus_stops +
    median_age +
    dist_cbd +
    dist_greenspace +
    dist_waterfront +
    spring + fall + winter +
    city,
  data = df
)

cat("\n===== MODEL 5: BUFFER ZONE DUMMIES =====\n")
print(summary(model5))

# Interpret zone premiums
coef_z1 <- coef(model5)["zone1"]
coef_z2 <- coef(model5)["zone2"]

cat("\n--- ZONE PREMIUM INTERPRETATION ---\n")
cat(sprintf(
  "Zone 1 premium (<500m vs. >2km): %.2f%%\n",
  (exp(coef_z1) - 1) * 100
))
cat(sprintf(
  "Zone 2 premium (500m-2km vs. >2km): %.2f%%\n",
  (exp(coef_z2) - 1) * 100
))
# Note: for log-transformed dependent variable, the correct % interpretation
# of a dummy coefficient d is: (exp(d) - 1) * 100


# NON-LINEARITY CHECK (DISTANCE + DISTANCE-SQUARED)
# We check whether the distance-price relationship is curved.
# If dist_crow_sq is significant, the effect is non-linear.

model6 <- lm(
  ln_price_sqm ~
    dist_crow +
    dist_crow_sq +
    size_sqm +
    building_age +
    floor +
    highest_floor +
    parking +
    heating +
    pop_density +
    higher_edu_ratio +
    n_high_schools +
    uni_entrants +
    n_bus_stops +
    median_age +
    dist_cbd +
    dist_greenspace +
    dist_waterfront +
    spring + fall + winter +
    city,
  data = df
)

cat("\n===== MODEL 6: NON-LINEAR (DISTANCE + DISTANCE-SQUARED) =====\n")
print(summary(model6))

# Compute inflection point (distance where price effect flattens)
# Get coefficients
b1 <- coef(model6)["dist_crow"]
b2 <- coef(model6)["dist_crow_sq"]

if (!is.na(b2) && b2 != 0) {
  inflection_m <- -b1 / (2 * b2)       # in metres
  inflection_km <- inflection_m / 1000 # in km
  
  cat(sprintf(
    "\nInflection point: %.0f metres (%.3f km)\n",
    inflection_m, inflection_km
  ))
} else {
  cat("\nNo inflection point (b2 is zero or NA)\n")
}

# CITY-BY-CITY REGRESSIONS
# =============================================================================
# Run the full model separately for each city.
# This tells you whether the metro premium is consistent across cities
# or whether it varies — which is important for policy discussion.

cities <- c("Busan", "Daegu", "Daejeon", "Gwangju")
city_models <- list()

for (city_name in cities) {
  df_city <- df %>% filter(city == city_name)

  m <- lm(
    ln_price_sqm ~
      dist_crow +
      size_sqm +
      building_age +
      floor +
      highest_floor +
      parking +
      heating +
      pop_density +
      higher_edu_ratio +
      n_high_schools +
      uni_entrants +
      n_bus_stops +
      median_age +
      dist_cbd +
      dist_greenspace +
      dist_waterfront +
      spring + fall + winter,
    data = df_city
  )

  city_models[[city_name]] <- m

  coef_d <- coef(m)["dist_crow"]
  pval   <- summary(m)$coefficients["dist_crow", "Pr(>|t|)"]
  r2     <- summary(m)$r.squared

  cat(sprintf(
    "\n%s: dist_crow coef = %.6f  (p = %.4f)  | R2 = %.3f  | n = %d",
    city_name, coef_d, pval, r2, nrow(df_city)
  ))
}
cat("\n")

# Comparing model 4 and 6 to test marginal contribution of dist_squared
anova(model4,model6)

# city wise zone analysis
cities <- c("Busan", "Daegu", "Daejeon", "Gwangju")
city_models <- list()

for (city_name in cities) {
  df_city <- df %>% filter(city == city_name)
  
  m <- lm(
    ln_price_sqm ~
      zone1 +
      zone2 +
      size_sqm +
      building_age +
      floor +
      highest_floor +
      parking +
      heating +
      pop_density +
      higher_edu_ratio +
      n_high_schools +
      uni_entrants +
      n_bus_stops +
      median_age +
      dist_cbd +
      dist_greenspace +
      dist_waterfront +
      spring + fall + winter,
    data = df_city
  )
  coefs <- summary(m)$coefficients
  
  cat(sprintf("\n--- %s ---\n", city_name))
  
  for (var in c("zone1", "zone2")) {
    coef_val <- coefs[var, "Estimate"]
    p_val    <- coefs[var, "Pr(>|t|)"]
    
    decision <- ifelse(p_val < 0.05, "SIGNIFICANT", "NOT SIGNIFICANT")
    
    cat(sprintf(
      "%s: coef = %.4f | p = %.4f | %s\n",
      var, coef_val, p_val, decision
    ))
  }
}

# DIAGNOSTIC TESTS
# =============================================================================

cat("\n===== DIAGNOSTIC TESTS ON MODEL 5 (FULL MODEL) =====\n")

# --- Test 1: Heteroskedasticity (Breusch-Pagan test) ---
# H0: errors have constant variance (homoskedasticity)
# If p < 0.05: heteroskedasticity present → use robust standard errors
bp_test <- bptest(model5)
cat("\nBreusch-Pagan test for heteroskedasticity:\n")
print(bp_test)

# graphs for residual checks
par(mfrow = c(2, 2))
plot(model5)

# --- Test 2: Variance Inflation Factor (VIF) — multicollinearity check ---
# VIF > 10 signals serious multicollinearity between predictors.
cat("\nVariance Inflation Factors (VIF) for Model 5:\n")
tryCatch({
  vif_vals <- vif(model5)
  print(round(vif_vals, 2))
  if (any(vif_vals > 10)) {
    cat("WARNING: Some VIF > 10 — consider removing correlated variables.\n")
  } else {
    cat("All VIF < 10: no serious multicollinearity detected.\n")
  }
}, error = function(e) {
  cat("VIF could not be computed (possibly due to factor variables).\n")
})
any(vif_vals > 10) # checks for if threshold for VIF is breached

# ── Jarque-Bera test ────────────────────────────────────────────────────
residuals_m5 <- residuals(model5)
jb_result <- jarque.bera.test(residuals_m5)

cat("\n--- Jarque-Bera Test — Model 5 Residuals ---\n")
cat(sprintf("JB statistic : %.4f\n", jb_result$statistic))
cat(sprintf("p-value      : %.6f\n", jb_result$p.value))

# ── Skewness and kurtosis ───────────────────────────────────────────────
n <- length(residuals_m5)
S <- mean((residuals_m5 - mean(residuals_m5))^3) / sd(residuals_m5)^3
K <- mean((residuals_m5 - mean(residuals_m5))^4) / sd(residuals_m5)^4

cat(sprintf("Skewness     : %.4f  (ideal = 0)\n",   S))
cat(sprintf("Kurtosis     : %.4f  (ideal = 3)\n",   K))
cat(sprintf("Excess kurtosis (K-3): %.4f\n",        K - 3))

# =============================================================================
# DIAGNOSTICS — ROBUST STANDARD ERRORS & COOK'S DISTANCE
# =============================================================================

library(sandwich)
library(lmtest)
library(ggplot2)

# --- 1. ROBUST STANDARD ERRORS (HC3) — Model 5 ---
vcov_robust_m5 <- vcovHC(model5, type = "HC3")
model5_robust  <- coeftest(model5, vcov = vcov_robust_m5)
print(model5_robust)

# COEFFICIENT STABILITY ACROSS COOK'S DISTANCE THRESHOLDS ---
cooksd <- cooks.distance(model5)   # this returns a numeric vector, one per observation
thresholds <- c(4/nrow(df), 0.01, 0.02, 0.05)

for (th in thresholds) {
  n_removed <- sum(cooksd > th)
  df_temp   <- df[cooksd <= th, ]
  
  m_temp <- lm(
    ln_price_sqm ~ dist_crow + dist_crow_sq +
      size_sqm + building_age + floor + highest_floor +
      parking + heating + pop_density + higher_edu_ratio +
      n_high_schools + uni_entrants + n_bus_stops + median_age +
      dist_cbd + dist_greenspace + dist_waterfront +
      spring + fall + winter + city,
    data = df_temp
  )
  
  cat(sprintf(
    "Threshold: %.6f | Removed: %d | dist_crow coef: %.8f\n",
    th, n_removed, coef(m_temp)["dist_crow"]
  ))
}


# =====================================================================
# Data Visualization Lab (TE7663) - Assignment 7
# R + Power BI integration | Dataset: Sample Superstore
# Technique: Time Series Analysis (Holt-Winters forecasting)
# Packages needed: ggplot2 only (everything else is base R)
#   install.packages("ggplot2")
# =====================================================================


# =====================================================================
# PART 1 - POWER QUERY: Home > Transform data > Transform > Run R script
# Purpose: data cleaning (missing values, duplicates) + summary statistics
# After running, click "Table" next to clean_data and summary_stats,
# then Close & Apply. Name the cleaned table "Superstore".
# =====================================================================

df <- dataset

# 1. Make sure Order Date / Ship Date are real dates (US format m/d/Y)
fix_date <- function(x) {
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (inherits(x, "Date"))   return(x)
  as.Date(as.character(x), format = "%m/%d/%Y")
}
df[["Order Date"]] <- fix_date(df[["Order Date"]])
df[["Ship Date"]]  <- fix_date(df[["Ship Date"]])

# 2. Missing values: numeric -> median, text -> "Unknown"
for (col in names(df)) {
  if (is.numeric(df[[col]])) {
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  } else if (is.character(df[[col]]) || is.factor(df[[col]])) {
    v <- as.character(df[[col]])
    v[is.na(v) | v == ""] <- "Unknown"
    df[[col]] <- v
  }
}

# 3. Duplicates: ignore the Row ID counter, drop exact repeated records
dup <- duplicated(df[, setdiff(names(df), "Row ID")])
df  <- df[!dup, ]

# 4. Cleaned table
clean_data <- df

# 5. Summary statistics (descriptive) for the numeric columns
num_cols <- c("Sales", "Quantity", "Discount", "Profit")
summary_stats <- do.call(rbind, lapply(num_cols, function(col) {
  x <- df[[col]]
  data.frame(
    Variable = col,
    Count    = length(x),
    Mean     = round(mean(x), 3),
    Median   = round(median(x), 3),
    SD       = round(sd(x), 3),
    Min      = round(min(x), 3),
    Q1       = round(quantile(x, 0.25), 3),
    Q3       = round(quantile(x, 0.75), 3),
    Max      = round(max(x), 3),
    stringsAsFactors = FALSE
  )
}))
rownames(summary_stats) <- NULL


# =====================================================================
# POWER BI R VISUALS (Visualizations pane > R script visual)
# For EVERY R visual, drag these fields into "Values":
#     Row ID  +  the fields listed above the script
# Row ID stops Power BI from removing "duplicate" rows before R sees them.
# Drag Order Date as a plain field (click the arrow > Order Date, NOT the
# Date Hierarchy). Paste the script in the editor, click Run.
# =====================================================================


# ---------------------------------------------------------------------
# R VISUAL 1 - Monthly Sales Trend
# Fields: Row ID, Order Date, Sales
# ---------------------------------------------------------------------
library(ggplot2)
df <- dataset
df$Order.Date <- as.Date(df[["Order Date"]])
df$Month <- as.Date(format(df$Order.Date, "%Y-%m-01"))
m <- aggregate(Sales ~ Month, data = df, FUN = sum)

ggplot(m, aes(x = Month, y = Sales)) +
  geom_line(color = "#1F4E79", linewidth = 1) +
  geom_point(color = "#1F4E79", size = 1.8) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              color = "#E8743B", linetype = "dashed", linewidth = 0.9) +
  scale_y_continuous(labels = function(x) paste0("$", format(x / 1000, big.mark = ","), "K")) +
  labs(title = "Monthly Sales with Trend Line", x = NULL, y = "Sales") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


# ---------------------------------------------------------------------
# R VISUAL 2 - Time Series Decomposition (trend / seasonal / random)
# Fields: Row ID, Order Date, Sales
# ---------------------------------------------------------------------
df <- dataset
df$Order.Date <- as.Date(df[["Order Date"]])
df$Month <- as.Date(format(df$Order.Date, "%Y-%m-01"))
m <- aggregate(Sales ~ Month, data = df, FUN = sum)

# fill any months removed by slicers so the series stays regular
all_months <- data.frame(Month = seq(min(m$Month), max(m$Month), by = "month"))
m <- merge(all_months, m, by = "Month", all.x = TRUE)
m$Sales[is.na(m$Sales)] <- 0

if (nrow(m) >= 24) {
  y0 <- as.numeric(format(min(m$Month), "%Y"))
  m0 <- as.numeric(format(min(m$Month), "%m"))
  ts_m <- ts(m$Sales, start = c(y0, m0), frequency = 12)
  plot(decompose(ts_m, type = if (all(m$Sales > 0)) "multiplicative" else "additive"),
       col = "#1F4E79")
} else {
  plot.new()
  text(0.5, 0.5, "Select at least 24 months of data\nto show the decomposition", cex = 1.2)
}


# ---------------------------------------------------------------------
# R VISUAL 3 - Holt-Winters 12-Month Sales Forecast (advanced analytics)
# Fields: Row ID, Order Date, Sales
# ---------------------------------------------------------------------
library(ggplot2)
df <- dataset
df$Order.Date <- as.Date(df[["Order Date"]])
df$Month <- as.Date(format(df$Order.Date, "%Y-%m-01"))
m <- aggregate(Sales ~ Month, data = df, FUN = sum)
all_months <- data.frame(Month = seq(min(m$Month), max(m$Month), by = "month"))
m <- merge(all_months, m, by = "Month", all.x = TRUE)
m$Sales[is.na(m$Sales)] <- 0

if (nrow(m) >= 24) {
  y0 <- as.numeric(format(min(m$Month), "%Y"))
  m0 <- as.numeric(format(min(m$Month), "%m"))
  ts_m <- ts(m$Sales, start = c(y0, m0), frequency = 12)

  hw <- HoltWinters(ts_m, seasonal = if (all(m$Sales > 0)) "multiplicative" else "additive")
  fc <- predict(hw, n.ahead = 12, prediction.interval = TRUE, level = 0.80)

  fut <- seq(max(m$Month), by = "month", length.out = 13)[-1]
  fdf <- data.frame(Month = fut,
                    Forecast = as.numeric(fc[, "fit"]),
                    Lower = pmax(as.numeric(fc[, "lwr"]), 0),
                    Upper = as.numeric(fc[, "upr"]))

  ggplot() +
    geom_ribbon(data = fdf, aes(x = Month, ymin = Lower, ymax = Upper),
                fill = "#E8743B", alpha = 0.2) +
    geom_line(data = m, aes(x = Month, y = Sales), color = "#1F4E79", linewidth = 1) +
    geom_line(data = fdf, aes(x = Month, y = Forecast), color = "#E8743B",
              linewidth = 1.1, linetype = "dashed") +
    scale_y_continuous(labels = function(x) paste0("$", format(x / 1000, big.mark = ","), "K")) +
    labs(title = "Holt-Winters Forecast: Next 12 Months",
         subtitle = "Blue = actual sales | Orange = forecast with 80% interval",
         x = NULL, y = "Sales") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
} else {
  plot.new()
  text(0.5, 0.5, "Select at least 24 months of data\nto build the forecast", cex = 1.2)
}


# ---------------------------------------------------------------------
# R VISUAL 4 - Average Profit by Discount Band
# Fields: Row ID, Discount, Profit
# ---------------------------------------------------------------------
library(ggplot2)
df <- dataset
df$Band <- cut(df$Discount, breaks = c(-0.01, 0, 0.2, 0.4, 1),
               labels = c("0%", "1-20%", "21-40%", "Above 40%"))
agg <- aggregate(Profit ~ Band, data = df, FUN = mean)

ggplot(agg, aes(x = Band, y = Profit, fill = Profit > 0)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0("$", round(Profit, 1)),
                vjust = ifelse(Profit >= 0, -0.4, 1.3)), size = 4) +
  scale_fill_manual(values = c("TRUE" = "#2E8B57", "FALSE" = "#C0392B"), guide = "none") +
  labs(title = "Average Profit per Order Line by Discount Band",
       x = "Discount band", y = "Average profit ($)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


# =====================================================================
# OPTIONAL - RStudio only: model accuracy check (not for Power BI)
# Train on 2014-2016, test on 2017, report error. Set the path first.
# =====================================================================
# raw <- read.csv("Sample - Superstore.csv", fileEncoding = "latin1", check.names = FALSE)
# raw$`Order Date` <- as.Date(raw$`Order Date`, format = "%m/%d/%Y")
# raw$Month <- as.Date(format(raw$`Order Date`, "%Y-%m-01"))
# m <- aggregate(Sales ~ Month, data = raw, FUN = sum)
# ts_all <- ts(m$Sales, start = c(2014, 1), frequency = 12)
# train  <- window(ts_all, end = c(2016, 12))
# test   <- window(ts_all, start = c(2017, 1))
# hw     <- HoltWinters(train, seasonal = "multiplicative")
# pred   <- predict(hw, n.ahead = 12)
# MAPE   <- mean(abs((test - pred) / test)) * 100
# RMSE   <- sqrt(mean((test - pred)^2))
# cat("MAPE:", round(MAPE, 2), "%  RMSE:", round(RMSE, 0), "\n")

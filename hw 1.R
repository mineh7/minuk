#Both the Apple Vision Pro and Oculus Rift are AR/VR headsets create immersive virtual environments.

install.packages("nls2")
library(nls2)

year <- c(2020, 2021, 2022, 2023)
sales <- c(0.58, 4.43, 9.06, 6.67)

cumulative_sales <- cumsum(sales)

bass_model <- function(year, p, q, M) {
  M * ((p + q * (cumulative_sales/M)) * (1 - cumulative_sales/M))
}

start_vals <- data.frame(p = seq(0, 0.03, by=0.01), q = seq(0.3, 0.5, by=0.01), M = max(cumulative_sales))
fit <- nls2(sales ~ bass_model(year, p, q, M), start = start_vals, algorithm = "brute-force")
#
summary(fit)

params <- coef(fit)
p <- params[1]
q <- params[2]
M <- params[3]

future_years <- 2024:2030
predicted_sales <- predict(fit, newdata = data.frame(year = future_years))

data.frame(Year = future_years, Predicted_Adoption = predicted_sales)

predicted_adoption_vision_pro <- predict(fit, newdata = data.frame(year = future_years))

data.frame(Year = future_years, Predicted_Adoption = predicted_adoption_vision_pro)

adoption_by_year <- cumsum(predicted_adoption_vision_pro)
data.frame(Year = future_years, Cumulative_Adoption = adoption_by_year)


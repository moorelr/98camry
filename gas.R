# Import data from text file
gas_data <- read.csv("gas.txt", stringsAsFactors = FALSE, strip.white = TRUE)

pdf("Gas summary.pdf", width = 8.5, height = 5.5, useDingbats = FALSE)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2) + 0.1)

# Calculate cumulative volume and add as column in dataframe
cumulative_volume <- gas_data$Volume[1]
for(i in 2:nrow(gas_data)){
  cumulative_volume[i] <- cumulative_volume[i-1] + gas_data$Volume[i]
}
gas_data <- cbind(gas_data, cumulative_volume)

# Format dates
gas_data$Date <- as.Date(as.character(gas_data$Date), "%y%m%d")

# Plot gas prices
plot(gas_data$Date, gas_data$Price, type = "l", lty = 2
     , xlab = "Date, 2019-2020", ylab = "Gas price, $")
points(gas_data$Date, gas_data$Price, pch = 21, bg = "gray", cex = 2)
text(gas_data$Date[1], 1.8, adj = c(0, 0), cex = 0.8
     , labels = "Fuel consumption data for '98 Toyota Camry")
text(gas_data$Date[1], 1.65, adj = c(0, 0), cex = 0.8
     , labels = "Gas purchased in Christiansburg, VA")

# All of this is moot because of the oil price crash...
if(FALSE){
  summary(gas_data$Price)
  qqnorm(gas_data$Price)
  hist(gas_data$Price, xlim = c(2.1, 2.7))
  lines(density(gas_data$Price))
}

# Mileage
plot(gas_data$cumulative_volume, gas_data$Odometer
     , ylab = "Odometer, miles", xlab = "Cumulative Volume, Gallons")
mpg_list <- c(10, 20, 30, 40)
mpg_cols <- c("black", "black", "black", "black")
label_xs <- c(400, 300, 200, 100)
label_ys <- c(171500, 173500, 173500, 173000)
for(i in 1:length(mpg_list)){
  abline(1.681e+05, mpg_list[i], col = mpg_cols[i], lwd = 1.2)
  text(label_xs[i], label_ys[i], labels = rev(mpg_list)[i], adj = c(0, 0))
}

linear_model <- lm(Odometer ~ cumulative_volume, data = gas_data)
slope <- round(linear_model$coefficients[2], 2)
abline(linear_model, col = "red")
text(0, 176000, labels = paste("Slope =", slope), adj = c(0, 0), col = "red")

text(100, 168500, labels = "Linear regression + 95% prediction interval"
     , cex = 0.7, adj = c(0, 0), col = "red")
text(100, 168000, labels = "Contours of MPG"
     , cex = 0.7, adj = c(0, 0), col = "black")
new_gas <- data.frame(
  cumulative_volume = seq(min(gas_data$cumulative_volume, na.rm = TRUE)
                 , max(gas_data$cumulative_volume, na.rm = TRUE)
                 , length.out = 30)
  )
gas_predict <- predict(object = linear_model, newdata = new_gas, interval = "p", level = 0.95)
lines(new_gas$cumulative_volume, gas_predict[,2], lty = 2, col = "red")
lines(new_gas$cumulative_volume, gas_predict[,3], lty = 2, col = "red")

# MPG plot
mpg <- numeric(0)
for(i in 2:nrow(gas_data)){
  mpg[i] <- (gas_data$Odometer[i] - gas_data$Odometer[i-1])/gas_data$Volume[i-1]
}
plot(gas_data$Date, mpg, type = "n", lty = 2
     , xlab = "Date, 2019-2020", ylab = "MPG")
points(gas_data$Date, mpg, pch = 21, bg = "gray", cex = 2)

# MPG histogram
hist(mpg, main = "", xlab = "MPG")
mpg_dens <- density(mpg, na.rm = TRUE)
lines(mpg_dens$x, mpg_dens$y*14/max(mpg_dens$y), col = "red")

mpg_mean <- round(mean(mpg, na.rm = TRUE, lty = 3), 2)
abline(v = mpg_mean, col = "blue")
text(30, 14, adj = c(0, 1), labels = paste("Mean =", mpg_mean), col = "blue")

mpg_median <- round(median(mpg, na.rm = TRUE), 2)
abline(v = mpg_median, lty = 2, col = "blue")
text(30, 12, adj = c(0, 1), labels = paste("Median =", mpg_median), col = "blue")

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
dev.off()


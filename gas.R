# TODO
# - Figure out how to pull data straight from GitHub URL


# SETTINGS ---------------------------------------------

use_URL <- TRUE
saving <- TRUE


# INITIALIZATION ---------------------------------------------

# Import car data
if(use_URL){
  # If using the Github URL
  print("Importing data from GitHub...")
  gas_data <- read.csv(url("https://github.com/moorelr/98camry/raw/main/gas.csv")
                       , stringsAsFactors = FALSE, strip.white = TRUE
  )
} else {
  # alternatively, using a local CSV file
  print("Importing data from CSV...")
  gas_data <- read.csv("gas.csv", stringsAsFactors = FALSE, strip.white = TRUE)
}

# Parse dates and other calculations

# Calculate cumulative volume and add as column in dataframe
cumulative_volume <- gas_data$Volume[1]
for(i in 2:nrow(gas_data)){
  if(is.na(gas_data$Volume[i])){
    cumulative_volume[i] <- cumulative_volume[i-1]
    next
  }
  cumulative_volume[i] <- cumulative_volume[i-1] + gas_data$Volume[i]
}
gas_data <- cbind(gas_data, cumulative_volume)

# Format dates
gas_data$Date <- as.Date(as.character(gas_data$Date), "%y%m%d")


# PLOTTING ----------------------------------------------


if(saving){
  pdf("230207 Gas summary.pdf", width = 3, height = 6, useDingbats = FALSE)
  par(mfrow = c(3, 1), mar = c(4, 4, 2, 2) + 0.1)
}


# Plot gas prices
plot(gas_data$Date, gas_data$Price, type = "l", lty = 1
     , xlab = "Date", ylab = "Gas price, $"
     #, xlim = c(as.Date("2022-02-07"), as.Date("2023-02-07"))
)

# Mileage
plot(gas_data$cumulative_volume, gas_data$Odometer
     , ylab = "Odometer, miles", xlab = "Cumulative Volume, Gallons")
# Draw contours for average fuel consumption (less = steeper line)
mpg_list <- c(10, 20, 30, 40)
mpg_cols <- c("black", "black", "black", "black")
for(i in 1:length(mpg_list)){
  abline(1.681e+05, mpg_list[i], col = mpg_cols[i], lwd = 1.2)
}
linear_model <- lm(Odometer ~ cumulative_volume, data = gas_data)
slope <- round(linear_model$coefficients[2], 2)
abline(linear_model, col = "red")
text(1000, 176000, labels = paste("Slope =", slope), adj = c(0, 0), col = "red")

# MPG plot
mpg <- numeric(0)
for(i in 2:nrow(gas_data)){
  mpg[i] <- (gas_data$Odometer[i] - gas_data$Odometer[i-1])/gas_data$Volume[i]
}
mpg_mean <- round(mean(mpg, na.rm = TRUE, lty = 3), 2)
mpg_median <- round(median(mpg, na.rm = TRUE), 2)

plot(gas_data$Date, mpg, type = "n", lty = 2
     , xlab = "Date", ylab = "MPG", ylim = c(0, 60))
points(gas_data$Date, mpg, pch = 21, bg = "gray", cex = 2)
abline(h = mpg_mean, col = "darkblue")
text(as.Date("2022-01-01"), 40, col = "darkblue"
     , labels = paste("Mean =", mpg_mean), adj = c(0, 0.5))
#abline(h = mpg_median, lty = 2, col = "blue")

if(saving){
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  dev.off()
}


# UNUSED PLOTS ----------------------------------------

# MPG histogram
if(FALSE){
  hist(mpg, main = "", xlab = "MPG")
  mpg_dens <- density(mpg, na.rm = TRUE)
  lines(mpg_dens$x, mpg_dens$y*14/max(mpg_dens$y), col = "red")
  
  abline(v = mpg_mean, col = "blue")
  text(30, 14, adj = c(0, 1), labels = paste("Mean =", mpg_mean), col = "blue")
  
  abline(v = mpg_median, lty = 2, col = "blue")
  text(30, 12, adj = c(0, 1), labels = paste("Median =", mpg_median), col = "blue")
}

# Oil
if(FALSE){
  flag <- which(!is.na(gas_data$Oil))
  plot(gas_data$Date[flag], gas_data$Oil[flag], type = "l", lty = 2
       #, xlab = "Date, 2019-2020", ylab = "MPG"
  )
}

# Tire pressure
if(FALSE){
  flag <- which(!is.na(gas_data$P_fd))
  plot(gas_data$Date[flag], gas_data$P_rd[flag], type = "l", lty = 2, col = "red"
       #, xlab = "Date, 2019-2020", ylab = "MPG"
  )
  lines(gas_data$Date[flag], gas_data$P_fd[flag], lty = 2, col = "black")
  gas_data$Date[flag][1]
  # Looks like tires need to be refilled about once every two months
}



# TODO
# -

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
                       , na.strings = c("NA", "AN")
  )
} else {
  # alternatively, using a local CSV file
  print("Importing data from CSV...")
  gas_data <- read.csv("gas.csv", stringsAsFactors = FALSE, strip.white = TRUE
                       , na.strings = c("NA", "AN")
                       )
}
print(paste("Imported", nrow(gas_data), "rows of data from CSV"))

if(FALSE){
  # unfinished rows to parse bad NA values
  counter <- 0
  for(i in 1:ncol(gas_data)){
    flag_NA <- which(gas_data[,i] %in% c("NA", "AN"))
    counter <- counter + length(flag_NA)
    gas_data[flag_NA,i] <- NA
  }
  print("done filtering for bad NAs")
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
  pdf("240618 Gas summary.pdf", width = 3, height = 6, useDingbats = FALSE)
  par(mfrow = c(3, 1), mar = c(4, 4, 2, 2) + 0.1)
}


# Plot gas prices
plot(gas_data$Date, gas_data$Price, type = "l", lty = 1
     , xlab = "Date", ylab = "Gas price, $"
     #, xlim = c(as.Date("2022-02-07"), as.Date("2023-02-07"))
     , log = "y"
)
abline(v = as.Date("2022-02-24"), lty = 2) # Russian invasion of Ukraine
abline(v = as.Date("2020-04-20"), lty = 2) # Oil prices fall to $-37.63/barrel because of low demand during pandemic
abline(v = as.Date("2020-12-25"), lty = 2) # Oil prices spike because of increased holiday travel
abline(v = as.Date("2020-03-11"), lty = 2) # WHO declares COVID-19 outbreak a pandemic
abline(v = as.Date("2019-12-26"), lty = 2) # COVID-19 outbreak begins in Wuhan, China

# Mileage
plot(gas_data$cumulative_volume, gas_data$Odometer
     , ylab = "Odometer, miles", xlab = "Cumulative Volume, Gallons", cex = 0.6)
# Draw contours for average fuel consumption (less = steeper line)
mpg_list <- c(10, 20, 30, 40)
mpg_cols <- c("gray", "gray", "gray", "gray")
for(i in 1:length(mpg_list)){
  abline(1.681e+05, mpg_list[i], col = mpg_cols[i], lwd = 1)
}
linear_model <- lm(Odometer ~ cumulative_volume, data = gas_data)
slope <- round(linear_model$coefficients[2], 2)
abline(linear_model, col = "red", lty = 2)
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
points(gas_data$Date, mpg, pch = 21, bg = "gray", cex = 1.2)
abline(h = mpg_mean, col = "darkblue")
text(as.Date("2022-01-01"), 40, col = "darkblue"
     , labels = paste("Mean =", mpg_mean), adj = c(0, 0.5))
#abline(h = mpg_median, lty = 2, col = "blue")

if(saving){
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  dev.off()
}


# UNUSED PLOTS ----------------------------------------

if(TRUE){
  pdf(file = "240618 MPG vs gas type.pdf", width = 5, height = 4, useDingbats = FALSE)
  # Mileage vs gas type
  O87_strings <- "O87|Regular|regular|Reg\\."
  O89_strings <- "O89"
  flag_O87 <- grep(x = gas_data$Comment, pattern = O87_strings)
  flag_O89 <- grep(x = gas_data$Comment, pattern = O89_strings)
  
  points_cols <- rep("gray", nrow(gas_data))
  points_cols[flag_O87] <- "darkblue"
  points_cols[flag_O89] <- "darkred"
  
  plot(gas_data$Date, mpg, type = "n", lty = 2
       , xlab = "Date", ylab = "MPG"
       , ylim = c(0, 60), xlim = c(c(gas_data$Date[flag_O87][1], gas_data$Date[flag_O87][length(flag_O87)])))
  points(gas_data$Date, mpg, pch = 21, bg = points_cols, cex = 1.2)
  lines(c(gas_data$Date[flag_O87][1], gas_data$Date[flag_O87][length(flag_O87)])
        , rep(mean(mpg[flag_O87]), 2)
        , lty = 2, col = "blue"
  )
  lines(c(gas_data$Date[flag_O89][1], gas_data$Date[flag_O89][length(flag_O89)])
        , rep(mean(mpg[flag_O89]), 2)
        , lty = 2, col = "red"
  )
  
  dev.off()
  
  t.test(mpg[flag_O89], mpg[flag_O87])
}


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
if(TRUE){
  pdf(file = "240618 Oil level.pdf", width = 5, height = 4, useDingbats = FALSE)
  flag <- which(!is.na(gas_data$Oil))
  plot(gas_data$Date[flag], gas_data$Oil[flag], type = "l", lty = 1
       , xlab = "Date", ylab = "Oil level"
  )
  abline(v = as.Date("2023-11-18"), lty = 2, col = "gray") # Thanksgiving 2023
  dev.off()
}

# Tire pressure
if(TRUE){
  pdf(file = "240618 Tire pressure.pdf", width = 5, height = 4, useDingbats = FALSE)
  flag <- which(!is.na(gas_data$P_fd))
  mean_tire_p <- rep(NA, nrow(gas_data))
  for(i in 1:nrow(gas_data)){
    if(all(!is.na(c(gas_data$P_fd[i], gas_data$P_fp[i], gas_data$P_rp[i])))){
      mean_tire_p[i] <- mean(c(gas_data$P_fd[i], gas_data$P_fp[i], gas_data$P_rp[i]))
    }
  }
  plot(gas_data$Date[flag]
       , gas_data$P_rd[flag]
       #, mean_tire_p[flag]
       , type = "l", lty = 1, col = "red"
       , xlab = "Date", ylab = "Tire pressure, psi"
  )
  #lines(gas_data$Date[flag], gas_data$P_fd[flag], lty = 2, col = "black")
  lines(gas_data$Date[flag], mean_tire_p[flag], lty = 1, col = "black")
  abline(v = as.Date("2023-02-07"), lty = 2, col = "gray")
  # gas_data$Date[flag][1]
  # Looks like tires need to be refilled about once every two months
  dev.off()
}


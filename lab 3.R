install.packages("psych")
require(psych)
pairs.panels(iris)

dat_habitat
dat_bird = read.csv(here("Data", "bird.sta.csv"))
dat_all = merge(dat_bird,dat_habitat)
plot(ba.tot ~ elev, data = dat_all)

ced_wax = sample(dat_all$CEWA)
ced_wax
cewa_present_absent = as.numeric(ced_wax >= 1)
cewa_present_absent
plot(x = dat_all$elev, y = cewa_present_absent)

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}

# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slopoe and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

slope_plot = plot(x = dat_habitat$slope, y = dat_habitat$ba.tot, xlab = "slope", ylab = "basal area", main = "Plot of Slope")
elev_plot = plot(x = dat_habitat$elev, y = dat_habitat$ba.tot, xlab = "elevation", ylab = "basal area", main = "Plot of Elevation")
aspect_plot = plot(x = dat_habitat$aspect, y = dat_habitat$ba.tot, xlab = "aspect", ylab = "basal area", main = "Plot of Aspect")

head(dat_habitat)
pairs.panels(dat_habitat$elev)

# Choose two bird species and create plots of presence/absence (on the y-axis) and basal area (on the x axes).

ced_wax = sample(dat_all$CEWA)
ced_wax
cewa_present_absent = as.numeric(ced_wax >= 1)
cewa_present_absent
plot(x = dat_all$ba.tot, y = cewa_present_absent, xlab = "Basal Area")
curve(logistic_midpoint_slope(x, midpoint = 70, slope = -0.3), add = TRUE)

redtailhawk = sample(dat_all$RTHA)
redtailhawk_present_absent = as.numeric(redtailhawk >= 1)
redtailhawk_present_absent
plot(x = dat_all$ba.tot, y = redtailhawk_present_absent, xlab = "Basal Area")
curve(logistic_midpoint_slope(x, midpoint = 60, slope = -0.4), add = TRUE)


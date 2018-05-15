# PDF Weibull Distribution
getGamma <- function(x, desp=1, alpha=4, beta=2) {
  dgamma(x-desp, shape=alpha, rate=beta)
}

# Create a plot
p <- ggplot()
p

# Get rid of the grey background
theme <- theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  # Legend boxes
  legend.key = element_rect(colour = 'white', fill = 'white', size = 0.5, linetype='dashed'),
  # Legend position
  legend.position = c(0.25, 0.5)
)
p + theme

# Plot axes to make it look nicer
xAxis <- geom_hline(yintercept = 0, size = .001)
yAxis <-geom_vline(xintercept = 0, size = .001)
p + theme + xAxis + yAxis

# Plot axes labels
labels <- labs(x = "x", y = "f(x) : Weibull PDF")
p + theme + xAxis + yAxis + labels

# Plot the standard CDF
# Color
bw <- TRUE
if (bw) color = "black" else color = "greenyellow"
# Create layer
pdf <- stat_function(
  aes(0),
  fun = getGamma,
  args = list(desp=-100/3, alpha=.0888888, beta=1/15000),
  color = color
)
# Adjust scales on axes
xlim <- xlim(-50,50)
ylim <- ylim(-50,50)
# Plot the standard CDF preserving the scale on axes
p + theme + xAxis + yAxis + labels +  pdf + xlim + ylim
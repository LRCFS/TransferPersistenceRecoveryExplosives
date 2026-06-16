####Plotting time vs mass from ASTRA#####
##Libraries
library(ggplot2)
library(dplyr)

#Load data
data <- read.csv("C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Thesis Data/Chapter 3 - Methodology/SwabMountAngles.csv")

#Separate data
data45 <- data %>%
  filter(Swab.Mount == "45°")
data45_max <- max(data45$Load)
data45_min <- min(data45$Load)
data45_avg <- mean(data45$Load)
data45_sd <- sd(data45$Load)
data45_rsd <- data45_sd/orig_data_avg*100

data90 <- data %>%
  filter(Swab.Mount == "90°") %>%
  na.omit()
data90_max <- max(data90$Load)
data90_min <- min(data90$Load)
data90_avg <- mean(data90$Load)
data90_sd <- sd(data90$Load)
data90_rsd <- data90_sd/data90_avg*100

data70 <- data %>%
  filter(Swab.Mount == "70°") %>%
  na.omit()
data70_max <- max(data70$Load)
data70_min <- min(data70$Load)
data70_avg <- mean(data70$Load)
data70_sd <- sd(data70$Load)
data70_rsd <- data70_sd/data70_avg*100

#Plot graph
p <- ggplot(data) +
  geom_line(aes(x = Time, y = Load, colour = Swab.Mount), linewidth = 0.8) +
  scale_colour_brewer(palette = "Set1") +
  labs(
    x = "Time (ms)",
    y = "Measured Weight (g)"
  )+
  theme(legend.position = "none")
p2 <- p + facet_wrap(vars(Swab.Mount), nrow = 3)

ggsave("C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Thesis Data/Chapter 3 - Methodology/SwabMountAngle.tiff", p2, width = 15, height = 15, units = "cm", dpi=600)


####Plotting time vs mass from ASTRA#####
##Libraries
library(ggplot2)
library(dplyr)

#Load data
data <- read.csv("C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Thesis Data/Chapter 3 - Methodology/Original vs Modified Swab Mount.csv")

#Plot graph
p <- ggplot(data) +
  geom_line(aes(x = Time, y = Mass, colour = Type), linewidth = 0.8) +
  scale_colour_brewer(palette = "Set1") +
  labs(
    x = "Time (ms)",
    y = "Measured Weight (g)"
  )

show(p)

ggsave("C:/Users/A Bruce - User/OneDrive - University of Dundee/Documents/Thesis Data/Chapter 3 - Methodology/OriginalModifiedSwabMount.tiff", p, width = 15, height = 8, units = "cm", dpi=600)

#Separate data
orig_data <- data %>%
  filter(Type == "Original") %>%
  na.omit()
orig_data_max <- max(orig_data$Mass)
orig_data_min <- min(orig_data$Mass)
orig_data_avg <- mean(orig_data$Mass)
orig_data_sd <- sd(orig_data$Mass)
orig_data_rsd <- orig_data_sd/orig_data_avg*100

mod_data <- data %>%
  filter(Type == "Modified") %>%
  na.omit()
mod_data_max <- max(mod_data$Mass)
mod_data_min <- min(mod_data$Mass)
mod_data_avg <- mean(mod_data$Mass)
mod_data_sd <- sd(mod_data$Mass)
mod_data_rsd <- mod_data_sd/mod_data_avg*100

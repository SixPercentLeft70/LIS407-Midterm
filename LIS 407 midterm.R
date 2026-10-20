title: "LIS407 Midterm" 

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# 1. Recreate the dataset from the original image
disease_data <- data.frame(
  Disease = c("Breast Cancer", "Prostate Cancer", "Heart Disease", 
              "Motor Neuron Disease (ALS)", "HIV / AIDS", "COPD", 
              "Diabetes", "Suicide"),
  Raised_Millions = c(257.85, 147.0, 54.1, 22.9, 14.0, 7.0, 4.2, 3.2),
  Deaths = c(41374, 21176, 596577, 6849, 7683, 142942, 73831, 39518)
)

# 2. Calculate the "Share of Total" for both metrics to create a common axis
viz_data <- disease_data %>%
  mutate(
    Share_Raised = Raised_Millions / sum(Raised_Millions),
    Share_Deaths = Deaths / sum(Deaths)
  ) %>%
  # Reorder the Disease factor by the largest gap between Raised and Deaths
  mutate(
    Gap = abs(Share_Raised - Share_Deaths),
    Disease = reorder(Disease, Gap)
  )

# 3. Build the Dumbbell Chart using ggplot2
ggplot(viz_data) +
  # Draw the connecting line between the two points
  geom_segment(aes(x = Share_Deaths, xend = Share_Raised, 
                   y = Disease, yend = Disease), 
               color = "gray70", linewidth = 1.2) +
  # Add points for Deaths
  geom_point(aes(x = Share_Deaths, y = Disease, color = "Deaths"), 
             size = 4) +
  # Add points for Donations
  geom_point(aes(x = Share_Raised, y = Disease, color = "Donations"), 
             size = 4) +
  # Apply high-contrast colors for accessibility
  scale_color_manual(values = c("Deaths" = "#D55E00", "Donations" = "#0072B2")) +
  # Format the x-axis as percentages
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  # Theming and Labels
  labs(
    title = "Where We Donate vs. Diseases That Kill Us",
    subtitle = "Comparing the share of total charitable funds raised to the share of total US deaths (2011)",
    x = "Percentage of Total (%)",
    y = NULL,
    color = "Metric",
    caption = "Source: 2011 CDC report and charity data (via Vox)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )


library(ggplot2)

source("../R/thermal_functions.R")

temp <- seq(15, 40, by = 0.1)
fig_dir <- "../figures/sensitivity"
data_dir <- "../figures/sensitivity/data"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

clean_R0 <- function(x) {
  x[!is.finite(x) | x <= 0] <- NA
  x
}

percent_change <- function(new, baseline) {
  100 * (new - baseline) / baseline
}


####### Baseline traits ##########

a_aeg <- a_aegypti_mordecai_2017(temp)
b_aeg <- b_aegypti_mordecai_2017(temp)
c_aeg <- c_aegypti_mordecai_2017(temp)
lf_aeg <- lf_aegypti_mordecai_2017(temp)
PDR_aeg <- PDR_aegypti_mordecai_2017(temp)
EFD_aeg <- EFD_aegypti_mordecai_2017(temp)
pEA_aeg <- pEA_aegypti_mordecai_2017(temp)
MDR_aeg <- MDR_aegypti_mordecai_2017(temp)

a_alb <- a_albopictus_mordecai_2017(temp)
b_alb <- b_albopictus_mordecai_2017(temp)
c_alb <- c_albopictus_mordecai_2017(temp)
lf_alb <- lf_albopictus_mordecai_2017(temp)
PDR_alb <- PDR_albopictus_mordecai_2017(temp)
EFD_alb <- TFD_albopictus_mordecai_2017(temp)
pEA_alb <- pEA_albopictus_mordecai_2017(temp)
MDR_alb <- MDR_albopictus_mordecai_2017(temp)


####### Baseline R0 ##########

R0_aeg <- clean_R0(relative_R0_mordecai(
  a = a_aeg,
  b = b_aeg,
  c = c_aeg,
  mu = 1 / lf_aeg,
  PDR = PDR_aeg,
  EFD = EFD_aeg,
  pEA = pEA_aeg,
  MDR = MDR_aeg
))

R0_alb <- clean_R0(relative_R0_mordecai(
  a = a_alb,
  b = b_alb,
  c = c_alb,
  mu = 1 / lf_alb,
  PDR = PDR_alb,
  EFD = EFD_alb,
  pEA = pEA_alb,
  MDR = MDR_alb
))

peak_temp_aeg <- temp[which.max(R0_aeg)]
peak_temp_alb <- temp[which.max(R0_alb)]


####### Ae. aegypti: +10% one-at-a-time R0 changes ##########

change_aeg_a <- percent_change(clean_R0(relative_R0_mordecai(1.1 * a_aeg, b_aeg, c_aeg, 1 / lf_aeg, PDR_aeg, EFD_aeg, pEA_aeg, MDR_aeg)), R0_aeg)
change_aeg_b <- percent_change(clean_R0(relative_R0_mordecai(a_aeg, 1.1 * b_aeg, c_aeg, 1 / lf_aeg, PDR_aeg, EFD_aeg, pEA_aeg, MDR_aeg)), R0_aeg)
change_aeg_c <- percent_change(clean_R0(relative_R0_mordecai(a_aeg, b_aeg, 1.1 * c_aeg, 1 / lf_aeg, PDR_aeg, EFD_aeg, pEA_aeg, MDR_aeg)), R0_aeg)
change_aeg_lf <- percent_change(clean_R0(relative_R0_mordecai(a_aeg, b_aeg, c_aeg, 1 / (1.1 * lf_aeg), PDR_aeg, EFD_aeg, pEA_aeg, MDR_aeg)), R0_aeg)
change_aeg_PDR <- percent_change(clean_R0(relative_R0_mordecai(a_aeg, b_aeg, c_aeg, 1 / lf_aeg, 1.1 * PDR_aeg, EFD_aeg, pEA_aeg, MDR_aeg)), R0_aeg)
change_aeg_EFD <- percent_change(clean_R0(relative_R0_mordecai(a_aeg, b_aeg, c_aeg, 1 / lf_aeg, PDR_aeg, 1.1 * EFD_aeg, pEA_aeg, MDR_aeg)), R0_aeg)
change_aeg_pEA <- percent_change(clean_R0(relative_R0_mordecai(a_aeg, b_aeg, c_aeg, 1 / lf_aeg, PDR_aeg, EFD_aeg, 1.1 * pEA_aeg, MDR_aeg)), R0_aeg)
change_aeg_MDR <- percent_change(clean_R0(relative_R0_mordecai(a_aeg, b_aeg, c_aeg, 1 / lf_aeg, PDR_aeg, EFD_aeg, pEA_aeg, 1.1 * MDR_aeg)), R0_aeg)


####### Ae. albopictus: +10% one-at-a-time R0 changes ##########

change_alb_a <- percent_change(clean_R0(relative_R0_mordecai(1.1 * a_alb, b_alb, c_alb, 1 / lf_alb, PDR_alb, EFD_alb, pEA_alb, MDR_alb)), R0_alb)
change_alb_b <- percent_change(clean_R0(relative_R0_mordecai(a_alb, 1.1 * b_alb, c_alb, 1 / lf_alb, PDR_alb, EFD_alb, pEA_alb, MDR_alb)), R0_alb)
change_alb_c <- percent_change(clean_R0(relative_R0_mordecai(a_alb, b_alb, 1.1 * c_alb, 1 / lf_alb, PDR_alb, EFD_alb, pEA_alb, MDR_alb)), R0_alb)
change_alb_lf <- percent_change(clean_R0(relative_R0_mordecai(a_alb, b_alb, c_alb, 1 / (1.1 * lf_alb), PDR_alb, EFD_alb, pEA_alb, MDR_alb)), R0_alb)
change_alb_PDR <- percent_change(clean_R0(relative_R0_mordecai(a_alb, b_alb, c_alb, 1 / lf_alb, 1.1 * PDR_alb, EFD_alb, pEA_alb, MDR_alb)), R0_alb)
change_alb_EFD <- percent_change(clean_R0(relative_R0_mordecai(a_alb, b_alb, c_alb, 1 / lf_alb, PDR_alb, 1.1 * EFD_alb, pEA_alb, MDR_alb)), R0_alb)
change_alb_pEA <- percent_change(clean_R0(relative_R0_mordecai(a_alb, b_alb, c_alb, 1 / lf_alb, PDR_alb, EFD_alb, 1.1 * pEA_alb, MDR_alb)), R0_alb)
change_alb_MDR <- percent_change(clean_R0(relative_R0_mordecai(a_alb, b_alb, c_alb, 1 / lf_alb, PDR_alb, EFD_alb, pEA_alb, 1.1 * MDR_alb)), R0_alb)


####### Data for temperature line plot ##########

line_data <- rbind(
  data.frame(temp = temp, species = "Ae. aegypti", parameter = "a", percent_change_R0 = change_aeg_a),
  data.frame(temp = temp, species = "Ae. aegypti", parameter = "b", percent_change_R0 = change_aeg_b),
  data.frame(temp = temp, species = "Ae. aegypti", parameter = "c", percent_change_R0 = change_aeg_c),
  data.frame(temp = temp, species = "Ae. aegypti", parameter = "lf", percent_change_R0 = change_aeg_lf),
  data.frame(temp = temp, species = "Ae. aegypti", parameter = "PDR", percent_change_R0 = change_aeg_PDR),
  data.frame(temp = temp, species = "Ae. aegypti", parameter = "EFD", percent_change_R0 = change_aeg_EFD),
  data.frame(temp = temp, species = "Ae. aegypti", parameter = "pEA", percent_change_R0 = change_aeg_pEA),
  data.frame(temp = temp, species = "Ae. aegypti", parameter = "MDR", percent_change_R0 = change_aeg_MDR),
  data.frame(temp = temp, species = "Ae. albopictus", parameter = "a", percent_change_R0 = change_alb_a),
  data.frame(temp = temp, species = "Ae. albopictus", parameter = "b", percent_change_R0 = change_alb_b),
  data.frame(temp = temp, species = "Ae. albopictus", parameter = "c", percent_change_R0 = change_alb_c),
  data.frame(temp = temp, species = "Ae. albopictus", parameter = "lf", percent_change_R0 = change_alb_lf),
  data.frame(temp = temp, species = "Ae. albopictus", parameter = "PDR", percent_change_R0 = change_alb_PDR),
  data.frame(temp = temp, species = "Ae. albopictus", parameter = "EFD", percent_change_R0 = change_alb_EFD),
  data.frame(temp = temp, species = "Ae. albopictus", parameter = "pEA", percent_change_R0 = change_alb_pEA),
  data.frame(temp = temp, species = "Ae. albopictus", parameter = "MDR", percent_change_R0 = change_alb_MDR)
)

line_data <- line_data[is.finite(line_data$percent_change_R0), ]

write.csv(line_data, file.path(data_dir, "simple_10_percent_change_by_temperature.csv"), row.names = FALSE)


####### Plot 1: temperature vs change in R0 ##########

p_lines <- ggplot(line_data, aes(x = temp, y = percent_change_R0, color = parameter)) +
  geom_line(linewidth = 1) +
  facet_wrap(~species) +
  coord_cartesian(ylim = c(0, 25)) +
  labs(
    x = "Temperature (C)",
    y = "Change in R0 after +10% parameter increase (%)",
    color = "Parameter",
    title = "Baseline one-at-a-time sensitivity by temperature"
  ) +
  theme_bw()

ggsave(
  filename = file.path(fig_dir, "simple_10_percent_change_by_temperature.jpg"),
  plot = p_lines,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


####### Plot 2: average change across temperature range ##########

average_data <- aggregate(
  percent_change_R0 ~ species + parameter,
  data = line_data,
  FUN = mean
)

write.csv(average_data, file.path(data_dir, "simple_average_change_all_temperatures.csv"), row.names = FALSE)

p_average <- ggplot(average_data, aes(x = parameter, y = percent_change_R0, fill = species)) +
  geom_col(position = "dodge") +
  labs(
    x = "Parameter",
    y = "Average change in R0 across temperatures (%)",
    fill = "Species",
    title = "Average +10% sensitivity across the full temperature range"
  ) +
  theme_bw()

ggsave(
  filename = file.path(fig_dir, "simple_average_change_all_temperatures.jpg"),
  plot = p_average,
  width = 9,
  height = 5,
  dpi = 300,
  bg = "white"
)


####### Plot 3: change at peak R0 temperature ##########

peak_data <- rbind(
  data.frame(species = "Ae. aegypti", peak_temp = peak_temp_aeg, parameter = "a", percent_change_R0 = change_aeg_a[which.max(R0_aeg)]),
  data.frame(species = "Ae. aegypti", peak_temp = peak_temp_aeg, parameter = "b", percent_change_R0 = change_aeg_b[which.max(R0_aeg)]),
  data.frame(species = "Ae. aegypti", peak_temp = peak_temp_aeg, parameter = "c", percent_change_R0 = change_aeg_c[which.max(R0_aeg)]),
  data.frame(species = "Ae. aegypti", peak_temp = peak_temp_aeg, parameter = "lf", percent_change_R0 = change_aeg_lf[which.max(R0_aeg)]),
  data.frame(species = "Ae. aegypti", peak_temp = peak_temp_aeg, parameter = "PDR", percent_change_R0 = change_aeg_PDR[which.max(R0_aeg)]),
  data.frame(species = "Ae. aegypti", peak_temp = peak_temp_aeg, parameter = "EFD", percent_change_R0 = change_aeg_EFD[which.max(R0_aeg)]),
  data.frame(species = "Ae. aegypti", peak_temp = peak_temp_aeg, parameter = "pEA", percent_change_R0 = change_aeg_pEA[which.max(R0_aeg)]),
  data.frame(species = "Ae. aegypti", peak_temp = peak_temp_aeg, parameter = "MDR", percent_change_R0 = change_aeg_MDR[which.max(R0_aeg)]),
  data.frame(species = "Ae. albopictus", peak_temp = peak_temp_alb, parameter = "a", percent_change_R0 = change_alb_a[which.max(R0_alb)]),
  data.frame(species = "Ae. albopictus", peak_temp = peak_temp_alb, parameter = "b", percent_change_R0 = change_alb_b[which.max(R0_alb)]),
  data.frame(species = "Ae. albopictus", peak_temp = peak_temp_alb, parameter = "c", percent_change_R0 = change_alb_c[which.max(R0_alb)]),
  data.frame(species = "Ae. albopictus", peak_temp = peak_temp_alb, parameter = "lf", percent_change_R0 = change_alb_lf[which.max(R0_alb)]),
  data.frame(species = "Ae. albopictus", peak_temp = peak_temp_alb, parameter = "PDR", percent_change_R0 = change_alb_PDR[which.max(R0_alb)]),
  data.frame(species = "Ae. albopictus", peak_temp = peak_temp_alb, parameter = "EFD", percent_change_R0 = change_alb_EFD[which.max(R0_alb)]),
  data.frame(species = "Ae. albopictus", peak_temp = peak_temp_alb, parameter = "pEA", percent_change_R0 = change_alb_pEA[which.max(R0_alb)]),
  data.frame(species = "Ae. albopictus", peak_temp = peak_temp_alb, parameter = "MDR", percent_change_R0 = change_alb_MDR[which.max(R0_alb)])
)

write.csv(peak_data, file.path(data_dir, "simple_change_at_peak_R0_temperature.csv"), row.names = FALSE)

p_peak <- ggplot(peak_data, aes(x = parameter, y = percent_change_R0, fill = species)) +
  geom_col(position = "dodge") +
  labs(
    x = "Parameter",
    y = "Change in R0 at peak temperature (%)",
    fill = "Species",
    title = "Sensitivity at peak baseline R0 temperature",
    subtitle = paste0(
      "Peak temperatures: Ae. aegypti = ", round(peak_temp_aeg, 1),
      " C; Ae. albopictus = ", round(peak_temp_alb, 1), " C"
    )
  ) +
  theme_bw()

ggsave(
  filename = file.path(fig_dir, "simple_change_at_peak_R0_temperature.jpg"),
  plot = p_peak,
  width = 9,
  height = 5,
  dpi = 300,
  bg = "white"
)

print(average_data)
print(peak_data)

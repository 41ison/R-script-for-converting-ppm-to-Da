# Calculate the error in Da for a given mass and error in ppm
library(ggplot2)

ppm_to_da <- function(error_ppm, theoretical_mass) {
  error_da <- theoretical_mass / ((1 / error_ppm) * 10^6)
  return(error_da)
}

flatten <- function(l) {
  return(unlist(l))
}

generate_df <- function(masses, errors) {
  mass_error <- data.frame(Mass = rep(masses, each = length(errors)),
                           Error.ppm = rep(errors, times = length(masses)))
  mass_error$Error.Da <- sapply(1:nrow(mass_error), function(i) {
    ppm_to_da(mass_error$Error.ppm[i], mass_error$Mass[i])
  })
  return(mass_error)
}

masses <- c(400, 1200)
errors <- c(10, 20, 25, 50)

mass_error <- generate_df(masses, errors)

ggplot(mass_error, 
    aes(x = Mass, y = Error.Da, color = factor(Error.ppm))) +
  geom_line() +
  theme_bw() +
  ylim(0, 0.03) +
  labs(x = "Mass (Da)", y = "Error (Da)", color = "Error (ppm)") +
  scale_y_continuous(breaks = seq(0, 0.06, 0.002)) +
  scale_x_continuous(breaks = seq(0, 1200, 100)) +
  theme(legend.position = c(0.08, 0.9),
        text = element_text(size = 20))

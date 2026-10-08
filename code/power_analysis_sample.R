library(MASS)
library(lme4)
# Parameters
N_control <- 40
N_treatment <- 38
M_control_pre <- 3
M_treatment_pre <- 3
M_control_post <- 3
M_treatment_post <- 3.5
SD_control_pre <- 1
SD_treatment_pre <- 1
SD_control_post <- 1
SD_treatment_post <- 1
r_control <- 0.75
r_treatment <- 0.75

# Prepare parameters
mu_control <- c(M_control_pre, M_control_post)
mu_treatment <- c(M_treatment_pre, M_treatment_post)

Sigma_control <- matrix(
  ncol = 2, nrow = 2,
  c(
    SD_control_pre^2,
    SD_control_pre * SD_control_post * r_control,
    SD_control_pre * SD_control_post * r_control,
    SD_control_post^2
  )
)
Sigma_treatment <- matrix(
  ncol = 2, nrow = 2,
  c(
    SD_treatment_pre^2,
    SD_treatment_pre * SD_treatment_post * r_treatment,
    SD_control_pre * SD_control_post * r_control,
    SD_control_post^2
  )
)


samples_control <- mvrnorm(
  N_control,
  mu = mu_control, Sigma = Sigma_control,
  empirical = TRUE
)
samples_treatment <- mvrnorm(
  N_treatment,
  mu = mu_treatment, Sigma = Sigma_treatment,
  empirical = TRUE
)

# Prepare data
colnames(samples_control) <- c("pre", "post")
colnames(samples_treatment) <- c("pre", "post")

data_control <- as.data.frame(samples_control)
data_treatment <- as.data.frame(samples_treatment)

data_control <- reshape(
  data_control,
  direction = "long",
  varying = c("pre", "post"),
  v.names = "DV",
  times = c("pre", "post")
)
data_treatment <- reshape(
  data_treatment,
  direction = "long",
  varying = c("pre", "post"),
  v.names = "DV",
  times = c("pre", "post")
)

data_control$condition <- "control"
data_treatment$condition <- "treatment"
data_treatment$id <- data_treatment$id + N_control

data <- rbind(data_control, data_treatment)
data$time <- factor(data$time, levels = c("pre", "post"))

ggplot(
  data,
  aes(x = time, y = DV, color = condition)
) +
  facet_wrap(~condition) +
  geom_violin(fill = "gray90", color = "white") +
  geom_jitter(
    position = position_jitterdodge(
      dodge.width = .9, jitter.width = .25
    )
  ) +
  stat_summary(
    fun = "mean", geom = "point", position = position_dodge(.9),
    color = "black"
  ) +
  stat_summary(
    aes(group = condition),
    fun = "mean", geom = "line", color = "black",
    position = position_dodge(.9)
  ) +
  guides(fill = "none", color = "none") +
  # scale_color_manual(values = c(yellow, blue)) +
  theme_minimal()

s <- 500
p_values <- vector(length = s)

for (i in 1:s) {
  samples_control <- mvrnorm(
    N_control,
    mu = mu_control, Sigma = Sigma_control
  )
  samples_treatment <- mvrnorm(
    N_treatment,
    mu = mu_treatment, Sigma = Sigma_treatment
  )
  
  # Prepare data
  colnames(samples_control) <- c("pre", "post")
  colnames(samples_treatment) <- c("pre", "post")
  
  data_control <- as.data.frame(samples_control)
  data_treatment <- as.data.frame(samples_treatment)
  
  data_control <- reshape(
    data_control,
    direction = "long",
    varying = c("pre", "post"),
    v.names = "DV",
    times = c("pre", "post")
  )
  data_treatment <- reshape(
    data_treatment,
    direction = "long",
    varying = c("pre", "post"),
    v.names = "DV",
    times = c("pre", "post")
  )
  
  data_control$condition <- "control"
  data_treatment$condition <- "treatment"
  data_treatment$id <- data_treatment$id + N_control
  
  data <- rbind(data_control, data_treatment)
  
  model <- lmer(DV ~ condition * time + (1 | id), data = data)
  
  # p_values[i] <- coef(summary(model))[4, "Pr(>|t|)"]
  p_values[i] <- 2*pt(coef(summary(model))[4, "t value"], length(data), lower.tail = F)
  
}

power <- sum(p_values < .05) / s

print(
  paste0("Power of the interaction effect: ", power * 100, "%")
)

#####
# Loading packages
#####
library(ggplot2)
library(tidyverse)

#####
# Data preparation
#####

df_inf <- read.csv("Dataframes/final.csv", skip = 6)
df_alpha <- read.csv("Dataframes/split_alphas.csv", skip = 6)

df_end <- df_inf |> 
  rename(N = count.patches) |> 
  group_by(X.run.number.) |> 
  mutate(Finalrun = X.step. == max(X.step.)) |> 
  ungroup() |> 
  mutate(Flips_per_agent = total.flips / N) |>
  mutate(Outcome = ifelse(
    Finalrun == FALSE,
    "Running",
    ifelse(
      frac.believing == 1,
      "Consensus Believe",
      ifelse(frac.believing == 0, "Consensus Don't Believe", "No Consensus")
    )
  )) |>
  mutate(
    initial_largest_fraction = initial.largest / N,
    cluster_growth = largest.cluster.fraction - initial_largest_fraction
  )

df_alpha_end <- df_alpha |> 
  rename(N = count.patches) |> 
  group_by(X.run.number.) |> 
  mutate(Finalrun = X.step. == max(X.step.)) |> 
  ungroup() |> 
  mutate(Flips_per_agent = total.flips / N) |>
  mutate(Outcome = ifelse(
    Finalrun == FALSE,
    "Running",
    ifelse(
      frac.believing == 1,
      "Consensus Believe",
      ifelse(frac.believing == 0, "Consensus Don't Believe", "No Consensus")
    )
  )) |>
  mutate(
    initial_largest_fraction = initial.largest / N,
    cluster_growth = largest.cluster.fraction - initial_largest_fraction
  )

df_all <- df_inf |>
  rename(N = count.patches) |>
  group_by(X.run.number.) |>
  mutate(Finalrun = X.step. == max(X.step.)) |>
  ungroup() |>
  mutate(Flips_per_agent = total.flips / N,
         Outcome = case_when(
           !Finalrun            ~ "Running",
           frac.believing == 1  ~ "Consensus Believe",
           frac.believing == 0  ~ "Consensus Don't Believe",
           TRUE                 ~ "No Consensus"),
         initial_largest_fraction = initial.largest / N,
         cluster_growth = largest.cluster.fraction - initial_largest_fraction)

df_end <- df_all |> filter(Finalrun)    # one row per run
nrow(df_end)                            # should be 35000

df_alpha_all <- df_alpha |>
  rename(N = count.patches) |>
  group_by(X.run.number.) |>
  mutate(Finalrun = X.step. == max(X.step.)) |>
  ungroup() |>
  mutate(Flips_per_agent = total.flips / N,
         Outcome = case_when(
           !Finalrun            ~ "Running",
           frac.believing == 1  ~ "Consensus Believe",
           frac.believing == 0  ~ "Consensus Don't Believe",
           TRUE                 ~ "No Consensus"))

df_alpha_end <- df_alpha_all |> filter(Finalrun)

summary_cell <- df_end |>
  group_by(probability.of.spin.up) |>
  summarise(
    n_runs               = n(),
    p_consensus_believe  = mean(Outcome == "Consensus Believe"),
    p_consensus_dont     = mean(Outcome == "Consensus Don't Believe"),
    p_no_consensus       = mean(Outcome == "No Consensus"),
    mean_frac_believing  = mean(frac.believing),
    mean_time_to_stable  = mean(last.flip.tick),
    mean_flips_per_agent = mean(Flips_per_agent),
    mean_persistent      = mean(persistent.fraction),
    mean_interface       = mean(interface.density),
    mean_largest_cluster = mean(largest.cluster.fraction),
    .groups = "drop"
  )

#####
# Data visualization
#####
# Finalized run order parameters
final_order <- df_end |>
  select(
    X.run.number.,
    probability.of.spin.up,
    Outcome,
    frac.believing,
    last.flip.tick,
    Flips_per_agent,
    persistent.fraction,
    interface.density,
    largest.cluster.fraction
  ) |>
  pivot_longer(
    -c(X.run.number., probability.of.spin.up, Outcome),
    names_to = "measure",
    values_to = "value"
  ) |>
  ggplot(aes(probability.of.spin.up, value, colour = Outcome)) +
  geom_jitter(
    width = 0.4,
    height = 0,
    alpha = 0.6,
    size = 1
  ) +
  facet_wrap( ~ measure, scales = "free_y") +
  labs(x = "Probability of spin up (%)", y = NULL) +
  theme_minimal()
final_order
ggsave(".png", final_order)

#Final proportion based on initial proportion
final_initial_prop <- df_end |> 
  ggplot(aes(x = probability.of.spin.up, y = frac.believing, colour = Outcome)) +
  geom_jitter(
    width = 1,
    height = 0,
    alpha = 0.1,
    size = 1
  ) +
  labs(x = "Start probability of believing", y = "Final proportion of believing") +
  scale_x_continuous(breaks = sort(unique(df_end$probability.of.spin.up))) +
  theme_minimal()
final_initial_prop
ggsave(".png", final_initial_prop)

lv <- c("Consensus Believe", "No Consensus", "Consensus Don't Believe")
convergence_prop <- df_end |>
  mutate(Outcome = factor(Outcome, levels = lv)) |>
  count(probability.of.spin.up, Outcome) |>
  group_by(probability.of.spin.up) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(probability.of.spin.up, prop, fill = Outcome)) +
  scale_x_continuous(breaks = sort(unique(df_end$probability.of.spin.up))) +
  geom_col() +
  scale_fill_manual(values = c("Consensus Believe"       = "#F8766D",
                               "No Consensus"            = "#619CFF",
                               "Consensus Don't Believe" = "#7CAE00")) +
  labs(x = "Start probability of believing", y = "Proportion of runs") +
  theme_minimal()
convergence_prop
ggsave(".png", convergence_prop)

#####
# Effect of A on order parameters
#####

convergence_A <- df_end |>
  mutate(Outcome = factor(Outcome, levels = lv)) |>
  count(mean.init.A, Outcome) |>
  group_by(mean.init.A) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(mean.init.A, prop, fill = Outcome)) +
  geom_col() +
  scale_fill_manual(values = c("Consensus Believe"       = "#F8766D",
                               "No Consensus"            = "#619CFF",
                               "Consensus Don't Believe" = "#7CAE00")) +
  labs(x = "Mean Analytical thinking", y = "Proportion of runs") +
  theme_minimal()
convergence_A
ggsave(".png", convergence_A)

fraction_A <- df_end |>
  ggplot(aes(mean.init.A, frac.believing)) +
  geom_jitter(
    aes(colour = Outcome),
    height = 0,
    alpha = 0.15,
    size = 1
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = 1),
    colour = "black",
    linewidth = 1
  ) +
  scale_colour_manual(
    values = c(
      "Consensus Believe"       = "#F8766D",
      "No Consensus"            = "#619CFF",
      "Consensus Don't Believe" = "#7CAE00"
    )
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Mean analytical thinking", y = "Final fraction believing", colour = "Outcome") +
  scale_x_continuous(breaks = sort(unique(df_end$mean.init.A))) +
  theme_minimal()
fraction_A
ggsave(".png", fraction_A)

fraction_A_non <- df_end |>
  filter(Outcome == "No Consensus") |>
  ggplot(aes(mean.init.A, frac.believing)) +
  geom_jitter(
    aes(colour = Outcome),
    height = 0,
    alpha = 0.15,
    size = 1
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = 1),
    colour = "black",
    linewidth = 1
  ) +
  scale_colour_manual(
    values = c(
      "Consensus Believe"       = "#F8766D",
      "No Consensus"            = "#619CFF",
      "Consensus Don't Believe" = "#7CAE00"
    )
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Mean analytical thinking", y = "Final fraction believing", colour = "Outcome") +
  scale_x_continuous(breaks = sort(unique(df_end$mean.init.A))) +
  theme_minimal()
fraction_A_non
ggsave(".png", fraction_A_non)

#####
# Effect of C on order parameters
#####

convergence_C <- df_end |>
  mutate(Outcome = factor(Outcome, levels = lv)) |>
  count(probability.of.convergence, Outcome) |>
  group_by(probability.of.convergence) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(probability.of.convergence, prop, fill = Outcome)) +
  geom_col() +
  scale_fill_manual(values = c("Consensus Believe"       = "#F8766D",
                               "No Consensus"            = "#619CFF",
                               "Consensus Don't Believe" = "#7CAE00")) +
  labs(x = "Probability of Partisanship Bias", y = "Proportion of runs") +
  theme_minimal()
convergence_C
ggsave(".png", convergence_C)

fraction_C <- df_end |>
  ggplot(aes(probability.of.convergence, frac.believing)) +
  geom_jitter(
    aes(colour = Outcome),
    height = 0,
    alpha = 0.15,
    size = 1
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = 1),
    colour = "black",
    linewidth = 1
  ) +
  scale_colour_manual(
    values = c(
      "Consensus Believe"       = "#F8766D",
      "No Consensus"            = "#619CFF",
      "Consensus Don't Believe" = "#7CAE00"
    )
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Probability of Partisanship Bias", y = "Final fraction believing", colour = "Outcome") +
  scale_x_continuous(breaks = sort(unique(df_end$probability.of.convergence))) +
  theme_minimal()
fraction_C
ggsave(".png", fraction_C)

fraction_C_non <- df_end |>
  filter(Outcome == "No Consensus") |>
  ggplot(aes(probability.of.convergence, frac.believing)) +
  geom_jitter(
    aes(colour = Outcome),
    height = 0,
    alpha = 0.15,
    size = 1
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = 1),
    colour = "black",
    linewidth = 1
  ) +
  scale_colour_manual(
    values = c(
      "Consensus Believe"       = "#F8766D",
      "No Consensus"            = "#619CFF",
      "Consensus Don't Believe" = "#7CAE00"
    )
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Probability of Partisanship Bias", y = "Final fraction believing", colour = "Outcome") +
  scale_x_continuous(breaks = sort(unique(df_end$probability.of.convergence))) +
  theme_minimal()
fraction_C_non
ggsave(".png", fraction_C_non)


#####
# Effect of alpha on order parameters
#####

convergence_alpha <- df_end |>
  mutate(Outcome = factor(Outcome, levels = lv)) |>
  count(mean.init.alpha, Outcome) |>
  group_by(mean.init.alpha) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(mean.init.alpha, prop, fill = Outcome)) +
  geom_col() +
  scale_fill_manual(values = c("Consensus Believe"       = "#F8766D",
                               "No Consensus"            = "#619CFF",
                               "Consensus Don't Believe" = "#7CAE00")) +
  labs(x = "Alpha", y = "Proportion of runs") +
  theme_minimal()
convergence_alpha
ggsave(".png", convergence_alpha)

fraction_alpha <- df_end |>
  ggplot(aes(mean.init.alpha, frac.believing)) +
  geom_jitter(
    aes(colour = Outcome),
    height = 0,
    alpha = 0.15,
    size = 1
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = 1),
    colour = "black",
    linewidth = 1
  ) +
  scale_colour_manual(
    values = c(
      "Consensus Believe"       = "#F8766D",
      "No Consensus"            = "#619CFF",
      "Consensus Don't Believe" = "#7CAE00"
    )
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Receptiveness 1 / alpha", y = "Final fraction believing", colour = "Outcome") +
  scale_x_continuous(breaks = sort(unique(df_end$mean.init.alpha))) +
  theme_minimal()
fraction_alpha
ggsave(".png", fraction_alpha)


##### 
# Alpha2
#####

belief_alphas <- df_alpha_end |>
  group_by(mean.init.alpha, mean.init.alpha2, H) |>
  summarise(
    mean_belief = mean(frac.believing),
    .groups = "drop"
  ) |>
  ggplot(aes(
    x = H,
    y = mean_belief,
    colour = factor(mean.init.alpha2),
    group = mean.init.alpha2
  )) +
  geom_line() +
  geom_point() +
  facet_wrap(~ mean.init.alpha, labeller = label_both, nrow = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Maximum H",
    y = "Mean final proportion believing",
    colour = "Alpha2 (H divisor)"
  ) +
  theme_minimal()
belief_alphas
ggsave(".png", belief_alphas)

#####
# Convergence speed & probability
#####

conv_alpha <- df_all |>
  group_by(X.run.number.) |>
  summarise(t_conv = first(ticks[frac.believing %in% c(0, 1)]),  # NA if never
            .groups = "drop") |>
  left_join(df_end |> select(X.run.number., mean.init.alpha, Outcome),
            by = "X.run.number.") |>
  mutate(converged = !is.na(t_conv))

table(conv_alpha$converged, conv_alpha$Outcome)

prob_alpha <- conv_alpha |>
  group_by(mean.init.alpha) |>
  summarise(n = n(), k = sum(converged), p = k / n,
            lo = binom.test(k, n)$conf.int[1],
            hi = binom.test(k, n)$conf.int[2],
            .groups = "drop")

prob_conv_alpha <- ggplot(prob_alpha, aes(mean.init.alpha, p)) +
  geom_line() + geom_point() +
  labs(x = "Alpha", y = "P(convergence)") +
  theme_minimal()
prob_conv_alpha
ggsave(".png", prob_conv_alpha)

final_order_alpha <- df_end |>
  select(
    X.run.number.,
    mean.init.alpha,
    last.flip.tick,
    Flips_per_agent,
    Outcome
  ) |>
  pivot_longer(
    -c(X.run.number., mean.init.alpha, Outcome),
    names_to = "measure",
    values_to = "value"
  ) |>
  ggplot(aes(mean.init.alpha, value, colour = Outcome)) +
  geom_jitter(
    width = 0.4,
    height = 0,
    alpha = 0.6,
    size = 1
  ) +
  facet_wrap( ~ measure, scales = "free_y") +
  labs(x = "Alpha", y = NULL) +
  theme_minimal()
final_order_alpha
ggsave(".png", final_order_alpha)

#####
# Effect of H on order parameters
#####

convergence_H <- df_end |>
  mutate(Outcome = factor(Outcome, levels = lv)) |>
  count(H, Outcome) |>
  group_by(H) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(H, prop, fill = Outcome)) +
  geom_col() +
  scale_fill_manual(values = c("Consensus Believe"       = "#F8766D",
                               "No Consensus"            = "#619CFF",
                               "Consensus Don't Believe" = "#7CAE00")) +
  labs(x = "H", y = "Proportion of runs") +
  theme_minimal()
convergence_H
ggsave(".png", convergence_H)

convergence_Halpha <- df_end |>
  mutate(Outcome = factor(Outcome, levels = lv)) |>
  count(mean.init.alpha, H, Outcome) |>
  group_by(mean.init.alpha, H) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  ggplot(aes(H, prop, fill = Outcome)) +
  geom_col() +
  facet_wrap(~ mean.init.alpha, labeller = label_both, ncol = 5) +
  scale_fill_manual(values = c(
    "Consensus Believe"       = "#F8766D",
    "No Consensus"            = "#619CFF",
    "Consensus Don't Believe" = "#7CAE00"
  )) +
  labs(x = "H", y = "Proportion of runs", fill = "Outcome") +
  theme_minimal()
convergence_Halpha
ggsave(".png", convergence_Halpha)


fraction_H <- df_end |>
  ggplot(aes(H, frac.believing)) +
  geom_jitter(
    aes(colour = Outcome),
    height = 0,
    alpha = 0.15,
    size = 1
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = 1),
    colour = "black",
    linewidth = 1
  ) +
  scale_colour_manual(
    values = c(
      "Consensus Believe"       = "#F8766D",
      "No Consensus"            = "#619CFF",
      "Consensus Don't Believe" = "#7CAE00"
    )
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Strength of Fact-checking Campaign", y = "Final fraction believing", colour = "Outcome") +
  scale_x_continuous(breaks = sort(unique(df_end$H))) +
  theme_minimal()
fraction_H
ggsave(".png", fraction_H)

fraction_H_non <- df_end |>
  filter(Outcome == "No Consensus") |>
  ggplot(aes(H, frac.believing)) +
  geom_jitter(
    aes(colour = Outcome),
    height = 0,
    alpha = 0.15,
    size = 1
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = 1),
    colour = "black",
    linewidth = 1
  ) +
  scale_colour_manual(
    values = c(
      "Consensus Believe"       = "#F8766D",
      "No Consensus"            = "#619CFF",
      "Consensus Don't Believe" = "#7CAE00"
    )
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Strength of Fact-checking Campaign", y = "Final fraction believing", colour = "Outcome") +
  scale_x_continuous(breaks = sort(unique(df_end$H))) +
  theme_minimal()
fraction_H_non
ggsave(".png", fraction_H_non)

#####
# Effect of Net field (A, B & H)
#####

df_end <- df_end |>
  mutate(net_field = 25 * (2 * probability.of.convergence / 100 - 1) -
           50 * mean.init.A -
           H)

convergence_all <- df_end |>
  filter(probability.of.spin.up %in% c(35, 50, 65)) |>
  mutate(Outcome   = factor(Outcome, levels = lv),
         field_bin = cut_width(net_field, width = 20, center = 0),
         regime    = if_else(mean.init.alpha <= 3,
                             "Global influence (alpha 1-3)",
                             "Local influence (alpha 5-9)")) |>
  count(probability.of.spin.up, regime, field_bin, Outcome) |>
  group_by(probability.of.spin.up, regime, field_bin) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  ggplot(aes(field_bin, prop, fill = Outcome)) +
  geom_col(position = "fill", width = 0.9) +
  scale_fill_manual(values = c("Consensus Believe"       = "#F8766D",
                               "No Consensus"            = "#619CFF",
                               "Consensus Don't Believe" = "#7CAE00")) +
  labs(x = "Net effect of all A, B and H (negative = towards disbelief)",
       y = "Proportion of runs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
convergence_all
ggsave(".png", convergence_all)

fraction_all <- df_end |>
  ggplot(aes(net_field, frac.believing)) +
  geom_jitter(
    aes(colour = Outcome),
    height = 0,
    alpha = 0.15,
    size = 1
  ) +
  geom_smooth(
    aes(group = 1),
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    method.args = list(family = quasibinomial),
    colour = "black",
    linewidth = 1
  ) +
  scale_colour_manual(
    values = c(
      "Consensus Believe"       = "#F8766D",
      "No Consensus"            = "#619CFF",
      "Consensus Don't Believe" = "#7CAE00"
    )
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Net effect of all A, B and H (negative = towards disbelief)", y = "Final fraction believing", colour = "Outcome") +
  theme_minimal()
fraction_all
ggsave(".png", fraction_all)

#####
# Appendix
#####

convergence_extra <- df_end |>
  filter(probability.of.spin.up %in% c(35, 50, 65)) |>
  mutate(Outcome   = factor(Outcome, levels = lv),
         field_bin = cut_width(net_field, width = 20, center = 0)) |>
  ggplot(aes(field_bin, fill = Outcome)) +
  geom_bar(position = "fill", width = 0.9) +
  facet_wrap(~ mean.init.alpha, nrow = 1, scales = "free_x",
             labeller = labeller(mean.init.alpha = \(x) paste0("alpha = ", x))) +
  scale_fill_manual(values = c("Consensus Believe"       = "#F8766D",
                               "No Consensus"            = "#619CFF",
                               "Consensus Don't Believe" = "#7CAE00")) +
  labs(x = "Net effect of all A, B and H (negative = towards disbelief)",
       y = "Proportion of runs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
convergence_extra
ggsave(".png", convergence_extra)

coefs <- df_end |>
  group_by(mean.init.alpha) |>
  summarise(b  = cov(net_field, frac.believing) / var(net_field),
            .groups = "drop") |>
  mutate(label = sprintf("b = %.3f per 10 units", 10 * b))

fraction_extra <- df_end |>
  ggplot(aes(net_field, frac.believing)) +
  geom_jitter(
    aes(colour = Outcome),
    height = 0,
    alpha = 0.15,
    size = 1
  ) +
  geom_smooth(
    aes(group = 1),
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    method.args = list(family = quasibinomial),
    colour = "black",
    linewidth = 1
  ) +
  geom_text(
    data = coefs,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.15, vjust = 2.5, size = 3.5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ mean.init.alpha, ncol = 1,
             labeller = labeller(mean.init.alpha = \(x) paste0("alpha = ", x))) +
  scale_colour_manual(
    values = c(
      "Consensus Believe"       = "#F8766D",
      "No Consensus"            = "#619CFF",
      "Consensus Don't Believe" = "#7CAE00"
    )
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Net effect of all A, B and H (negative = towards disbelief)",
       y = "Final fraction believing", colour = "Outcome") +
  theme_minimal()
fraction_extra
ggsave(".png", fraction_extra)
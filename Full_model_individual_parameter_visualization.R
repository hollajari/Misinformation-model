#####
# Loading packages
#####
library(ggplot2)
library(tidyverse)

#####
# Data preparation
#####

list.files("Misinformation/Dataframes")   # does this folder exist from here?

df_inf <- read.csv("Misinformation/Dataframes/16Oct_full_model_H2.csv", skip = 6)

df_inf <- df_inf |> 
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

summary_cell <- df_inf |>
  group_by(probability.of.spin.up) |>       
  summarise(
    n_runs               = n(),
    p_consensus_believe  = mean(Outcome == "Consensus Believe"),
    p_consensus_dont     = mean(Outcome == "Consensus Don't Believe"),
    p_no_consensus       = mean(Outcome == "No Consensus"),
    p_running            = mean(Outcome == "Running"),
    mean_frac_believing  = mean(frac.believing),
    mean_time_to_stable  = mean(last.flip.tick[Outcome != "Running"]),
    mean_flips_per_agent = mean(Flips_per_agent),
    mean_persistent      = mean(persistent.fraction),
    mean_interface       = mean(interface.density),
    mean_largest_cluster = mean(largest.cluster.fraction),
    .groups = "drop"
  )

df_all <- df_inf |>
  rename(run = all_of("X.run.number.")) |>    # adjust to your run column
  arrange(run, ticks)

# Final state: last row of each run
df_end <- df_inf |>
  filter(Finalrun) |>
  mutate(Outcome = case_when(
    frac.believing == 1 ~ "Consensus Believe",
    frac.believing == 0 ~ "Consensus Don't Believe",
    TRUE                ~ "No Consensus"
  ))

#####
# Data visualization
#####
# Finalized run order parameters
final_order <- df_end |>
  #filter(mean.init.A == 0) |> 
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

ggsave("Misinformation/Plots/14Oct_final_order.pdf", final_order)

# Overall run order parameters
prop_vs_start_prop <- df_all |>
  left_join(df_end |> select(run = X.run.number., end_outcome = Outcome),
            by = "run") |>
  ggplot(aes(ticks, frac.believing, group = run, colour = probability.of.spin.up)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~ end_outcome) +
  scale_colour_viridis_c() +
  labs(x = "Ticks", y = "Proportion believing", colour = "Start Proportion believing") +
  theme_minimal()
prop_vs_start_prop
ggsave("Misinformation/Plots/14Oct_prop_vs_start_prop.pdf", prop_vs_start_prop)

activity <- df_all |>
  group_by(run) |>
  mutate(batch_flips = total.flips - lag(total.flips, default = 0)) |>
  ungroup() |>
  ggplot(aes(ticks, batch_flips, group = run, colour = probability.of.spin.up)) +
  labs(x = "Time (in ticks)", y = "Flips compared to the T - 1", colour = "Start Proportion believing") +
  geom_line(alpha = 0.2) +
  scale_colour_viridis_c() +
  theme_minimal()
activity
ggsave("Misinformation/Plots/14Oct_activity.pdf", activity)


run_summary <- df_all |>
  group_by(run, probability.of.spin.up) |>
  summarise(
    ever_consensus = any(frac.believing %in% c(0, 1)),
    peak           = max(frac.believing),
    trough         = min(frac.believing),
    .groups = "drop"
  )

#####
# Effect of A on order parameters
#####

final_order_A <- df_end |>
  select(
    X.run.number.,
    mean.init.A,
    Outcome,
    frac.believing,
    last.flip.tick,
    Flips_per_agent,
    persistent.fraction,
    interface.density,
    largest.cluster.fraction
  ) |>
  pivot_longer(
    -c(X.run.number., mean.init.A, Outcome),
    names_to = "measure",
    values_to = "value"
  ) |>
  ggplot(aes(mean.init.A, value, colour = Outcome)) +
  geom_jitter(
    width = 0.4,
    height = 0,
    alpha = 0.6,
    size = 1
  ) +
  facet_wrap( ~ measure, scales = "free_y") +
  labs(x = "Mean Analytical Thinking", y = NULL) +
  theme_minimal()
final_order_A

ggsave("Misinformation/Plots/14Oct_final_order.pdf", final_order)

# Overall run order parameters
prop_vs_start_prop_A <- df_all |>
  left_join(df_end |> select(run = X.run.number., end_outcome = Outcome),
            by = "run") |>
  ggplot(aes(ticks, frac.believing, group = run, colour = mean.init.A)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~ end_outcome) +
  scale_colour_viridis_c() +
  labs(x = "Ticks", y = "Proportion believing", colour = "Mean Analytical Thinking") +
  theme_minimal()
prop_vs_start_prop_A

ggsave("Misinformation/Plots/14Oct_prop_vs_start_prop.pdf", prop_vs_start_prop)

activity_A <- df_all |>
  group_by(run) |>
  mutate(batch_flips = total.flips - lag(total.flips, default = 0)) |>
  ungroup() |>
  ggplot(aes(ticks, batch_flips, group = run, colour = mean.init.A)) +
  labs(x = "Time (in ticks)", y = "Flips compared to the T - 1", colour = "Mean Analytical Thinking") +
  geom_line(alpha = 0.2) +
  scale_colour_viridis_c() +
  theme_minimal()
activity_A
ggsave("Misinformation/Plots/14Oct_activity.pdf", activity)


run_summary_A<- df_all |>
  group_by(run, mean.init.A) |>
  summarise(
    ever_consensus = any(frac.believing %in% c(0, 1)),
    peak           = max(frac.believing),
    trough         = min(frac.believing),
    .groups = "drop"
  )
run_summary_A

lv <- c("Consensus Believe", "No Consensus", "Consensus Don't Believe")

df_end |>
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

#####
# Convergence speed & probability
#####

conv_A <- df_all |>
  group_by(run) |>
  summarise(t_conv = first(ticks[frac.believing %in% c(0, 1)]),  # NA if never
            .groups = "drop") |>
  left_join(df_end |> select(run = X.run.number., mean.init.A, Outcome),
            by = "run") |>
  mutate(converged = !is.na(t_conv))

table(conv_A$converged, conv_A$Outcome)

prob_A <- conv_A |>
  group_by(mean.init.A) |>
  summarise(n = n(), k = sum(converged), p = k / n,
            lo = binom.test(k, n)$conf.int[1],
            hi = binom.test(k, n)$conf.int[2],
            .groups = "drop")

ggplot(prob_A, aes(mean.init.A, p)) +
  geom_line() + geom_point() +
  labs(x = "Mean Analytical Thinking", y = "P(convergence)") +
  theme_minimal()

max_t_A <- max(df_all$ticks)
conv_A |>
  mutate(t_plot = coalesce(t_conv, max_t_A)) |>
  ggplot(aes(mean.init.A, t_plot, colour = Outcome, shape = converged)) +
  geom_jitter(width = 0.4, height = 0, alpha = 0.6) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1)) +
  labs(x = "Mean Analytical Thinking", y = "Ticks to consensus",
       shape = "Converged") +
  theme_minimal()

#####
# Effect of C on order parameters
#####

final_order_C <- df_end |>
  select(
    X.run.number.,
    probability.of.convergence,
    Outcome,
    frac.believing,
    last.flip.tick,
    Flips_per_agent,
    persistent.fraction,
    interface.density,
    largest.cluster.fraction
  ) |>
  pivot_longer(
    -c(X.run.number., probability.of.convergence, Outcome),
    names_to = "measure",
    values_to = "value"
  ) |>
  ggplot(aes(probability.of.convergence, value, colour = Outcome)) +
  geom_jitter(
    width = 0.4,
    height = 0,
    alpha = 0.6,
    size = 1
  ) +
  facet_wrap( ~ measure, scales = "free_y") +
  labs(x = "Probability of a Patch having converging beliefs", y = NULL) +
  theme_minimal()
final_order_C

ggsave("Misinformation/Plots/14Oct_final_order.pdf", final_order)

# Overall run order parameters
prop_vs_start_prop_C <- df_all |>
  left_join(df_end |> select(run = X.run.number., end_outcome = Outcome),
            by = "run") |>
  ggplot(aes(ticks, frac.believing, group = run, colour = probability.of.convergence)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~ end_outcome) +
  scale_colour_viridis_c() +
  labs(x = "Ticks", y = "Proportion believing", colour = "Probability of a Patch having converging beliefs") +
  theme_minimal()
prop_vs_start_prop_C

ggsave("Misinformation/Plots/14Oct_prop_vs_start_prop.pdf", prop_vs_start_prop)

activity_C <- df_all |>
  group_by(run) |>
  mutate(batch_flips = total.flips - lag(total.flips, default = 0)) |>
  ungroup() |>
  ggplot(aes(ticks, batch_flips, group = run, colour = probability.of.convergence)) +
  labs(x = "Time (in ticks)", y = "Flips compared to the T - 1", colour = "Probability of a Patch having converging beliefs") +
  geom_line(alpha = 0.2) +
  scale_colour_viridis_c() +
  theme_minimal()
activity_C
ggsave("Misinformation/Plots/14Oct_activity.pdf", activity)


run_summary_C <- df_all |>
  group_by(run, probability.of.convergence) |>
  summarise(
    ever_consensus = any(frac.believing %in% c(0, 1)),
    peak           = max(frac.believing),
    trough         = min(frac.believing),
    .groups = "drop"
  )
run_summary_C

lv <- c("Consensus Believe", "No Consensus", "Consensus Don't Believe")

df_end |>
  mutate(Outcome = factor(Outcome, levels = lv)) |>
  count(probability.of.convergence, Outcome) |>
  group_by(probability.of.convergence) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(probability.of.convergence, prop, fill = Outcome)) +
  geom_col() +
  scale_fill_manual(values = c("Consensus Believe"       = "#F8766D",
                               "No Consensus"            = "#619CFF",
                               "Consensus Don't Believe" = "#7CAE00")) +
  labs(x = "Probability of a patch having converging beliefs", y = "Proportion of runs") +
  theme_minimal()

#####
# Convergence speed & probability
#####

conv_C <- df_all |>
  group_by(run) |>
  summarise(t_conv = first(ticks[frac.believing %in% c(0, 1)]),  # NA if never
            .groups = "drop") |>
  left_join(df_end |> select(run = X.run.number., probability.of.convergence, Outcome),
            by = "run") |>
  mutate(converged = !is.na(t_conv))

table(conv_C$converged, conv_C$Outcome)

prob_C <- conv_C |>
  group_by(probability.of.convergence) |>
  summarise(n = n(), k = sum(converged), p = k / n,
            lo = binom.test(k, n)$conf.int[1],
            hi = binom.test(k, n)$conf.int[2],
            .groups = "drop")

ggplot(prob_C, aes(probability.of.convergence, p)) +
  geom_line() + geom_point() +
  labs(x = "Probability of a Patch having converging beliefs", y = "P(convergence)") +
  theme_minimal()

max_t_C <- max(df_all$ticks)
conv_C |>
  mutate(t_plot = coalesce(t_conv, max_t_C)) |>
  ggplot(aes(probability.of.convergence, t_plot, colour = Outcome, shape = converged)) +
  geom_jitter(width = 0.4, height = 0, alpha = 0.6) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1)) +
  labs(x = "Probability of a Patch having converging beliefs", y = "Ticks to consensus",
       shape = "Converged") +
  theme_minimal()

#####
# Effect of alpha on order parameters
#####

final_order_alpha <- df_end |>
  select(
    X.run.number.,
    mean.init.alpha,
    Outcome,
    frac.believing,
    last.flip.tick,
    Flips_per_agent,
    persistent.fraction,
    interface.density,
    largest.cluster.fraction
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

ggsave("Misinformation/Plots/14Oct_final_order.pdf", final_order)

# Overall run order parameters
prop_vs_start_prop_alpha <- df_all |>
  left_join(df_end |> select(run = X.run.number., end_outcome = Outcome),
            by = "run") |>
  ggplot(aes(ticks, frac.believing, group = run, colour = mean.init.alpha)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~ end_outcome) +
  scale_colour_viridis_c() +
  labs(x = "Ticks", y = "Proportion believing", colour = "Alpha") +
  theme_minimal()
prop_vs_start_prop_alpha

ggsave("Misinformation/Plots/14Oct_prop_vs_start_prop.pdf", prop_vs_start_prop)

activity_alpha <- df_all |>
  group_by(run) |>
  mutate(batch_flips = total.flips - lag(total.flips, default = 0)) |>
  ungroup() |>
  ggplot(aes(ticks, batch_flips, group = run, colour = mean.init.alpha)) +
  labs(x = "Time (in ticks)", y = "Flips compared to the T - 1", colour = "Alpha") +
  geom_line(alpha = 0.2) +
  scale_colour_viridis_c() +
  theme_minimal()
activity_alpha
ggsave("Misinformation/Plots/14Oct_activity.pdf", activity)


run_summary_alpha <- df_all |>
  group_by(run, mean.init.alpha) |>
  summarise(
    ever_consensus = any(frac.believing %in% c(0, 1)),
    peak           = max(frac.believing),
    trough         = min(frac.believing),
    .groups = "drop"
  )
run_summary_alpha

lv <- c("Consensus Believe", "No Consensus", "Consensus Don't Believe")

df_end |>
  mutate(Outcome = factor(Outcome, levels = lv)) |>
  count(mean.init.alpha, Outcome) |>
  group_by(mean.init.alpha) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(mean.init.alpha, prop, fill = Outcome)) +
  geom_col() +
  scale_fill_manual(values = c("Consensus Believe"       = "#F8766D",
                               "No Consensus"            = "#619CFF",
                               "Consensus Don't Believe" = "#7CAE00")) +
  labs(x = "1 / Alpha --> Receptiveness of distant information ", y = "Proportion of runs") +
  theme_minimal()

#####
# Convergence speed & probability
#####

conv_alpha <- df_all |>
  group_by(run) |>
  summarise(t_conv = first(ticks[frac.believing %in% c(0, 1)]),  # NA if never
            .groups = "drop") |>
  left_join(df_end |> select(run = X.run.number., mean.init.alpha, Outcome),
            by = "run") |>
  mutate(converged = !is.na(t_conv))

table(conv_alpha$converged, conv_alpha$Outcome)

prob_alpha <- conv_alpha |>
  group_by(mean.init.alpha) |>
  summarise(n = n(), k = sum(converged), p = k / n,
            lo = binom.test(k, n)$conf.int[1],
            hi = binom.test(k, n)$conf.int[2],
            .groups = "drop")

ggplot(prob_alpha, aes(mean.init.alpha, p)) +
  geom_line() + geom_point() +
  labs(x = "Alpha", y = "P(convergence)") +
  theme_minimal()

max_t_alpha <- max(df_all$ticks)
conv_alpha |>
  mutate(t_plot = coalesce(t_conv, max_t_alpha)) |>
  ggplot(aes(mean.init.alpha, t_plot, colour = Outcome, shape = converged)) +
  geom_jitter(width = 0.4, height = 0, alpha = 0.6) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1)) +
  labs(x = "Alpha", y = "Ticks to consensus",
       shape = "Converged") +
  theme_minimal()

#####
# Effect of H on order parameters
#####

final_order_H <- df_end |>
  select(
    X.run.number.,
    max.h,
    Outcome,
    frac.believing,
    last.flip.tick,
    Flips_per_agent,
    persistent.fraction,
    interface.density,
    largest.cluster.fraction
  ) |>
  pivot_longer(
    -c(X.run.number., max.h, Outcome),
    names_to = "measure",
    values_to = "value"
  ) |>
  ggplot(aes(max.h, value, colour = Outcome)) +
  geom_jitter(
    width = 0.4,
    height = 0,
    alpha = 0.6,
    size = 1
  ) +
  facet_wrap( ~ measure, scales = "free_y") +
  labs(x = "H", y = NULL) +
  theme_minimal()
final_order_H


# Overall run order parameters
prop_vs_start_prop_H <- df_all |>
  left_join(df_end |> select(run = X.run.number., end_outcome = Outcome),
            by = "run") |>
  ggplot(aes(ticks, frac.believing, group = run, colour = max.h)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~ end_outcome) +
  scale_colour_viridis_c() +
  labs(x = "Ticks", y = "Proportion believing", colour = "H") +
  theme_minimal()
prop_vs_start_prop_H


activity_H <- df_all |>
  group_by(run) |>
  mutate(batch_flips = total.flips - lag(total.flips, default = 0)) |>
  ungroup() |>
  ggplot(aes(ticks, batch_flips, group = run, colour = max.h)) +
  labs(x = "Time (in ticks)", y = "Flips compared to the T - 1", colour = "H") +
  geom_line(alpha = 0.2) +
  scale_colour_viridis_c() +
  theme_minimal()
activity_H


run_summary_H <- df_all |>
  group_by(run, max.h) |>
  summarise(
    ever_consensus = any(frac.believing %in% c(0, 1)),
    peak           = max(frac.believing),
    trough         = min(frac.believing),
    .groups = "drop"
  )
run_summary_H

lv <- c("Consensus Believe", "No Consensus", "Consensus Don't Believe")

df_end |>
  mutate(Outcome = factor(Outcome, levels = lv)) |>
  count(max.h, Outcome) |>
  group_by(max.h) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(max.h, prop, fill = Outcome)) +
  geom_col() +
  scale_fill_manual(values = c("Consensus Believe"       = "#F8766D",
                               "No Consensus"            = "#619CFF",
                               "Consensus Don't Believe" = "#7CAE00")) +
  labs(x = "H", y = "Proportion of runs") +
  theme_minimal()


#####
# Convergence speed & probability
#####

conv_H <- df_all |>
  group_by(run) |>
  summarise(t_conv = first(ticks[frac.believing %in% c(0, 1)]),  # NA if never
            .groups = "drop") |>
  left_join(df_end |> select(run = X.run.number., max.h, Outcome),
            by = "run") |>
  mutate(converged = !is.na(t_conv))

table(conv_H$converged, conv_H$Outcome)

max_t_H <- max(df_all$ticks)

prob_H <- conv_H |>
  group_by(max.h) |>
  summarise(n = n(), k = sum(converged), p = k / n,
            lo = binom.test(k, n)$conf.int[1],
            hi = binom.test(k, n)$conf.int[2],
            .groups = "drop")

ggplot(prob_H, aes(max.h, p)) +
  geom_line() + geom_point() +
  labs(x = "H", y = "P(convergence)") +
  theme_minimal()


conv_H |>
  mutate(t_plot = coalesce(t_conv, max_t_H)) |>
  ggplot(aes(max.h, t_plot, colour = Outcome, shape = converged)) +
  geom_jitter(width = 0.4, height = 0, alpha = 0.6) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1)) +
  labs(x = "H", y = "Ticks to consensus",
       shape = "Converged") +
  theme_minimal()

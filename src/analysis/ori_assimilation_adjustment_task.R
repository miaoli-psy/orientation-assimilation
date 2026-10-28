# ------------library-----------------
library(dplyr)
library(ggplot2)


# ------------set project path---------
setwd("D:/OneDrive/projects/orientation_assimilation")


# ------------read data---------------
my_data <- read.csv(
  "data/orientation_assimilation_alldata.csv",
  colClasses = c(participant = "character"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    SubID = participant,
    correct_num = set_size,
    number_deviation = reported_number - correct_num
  )


# # check participants info
# df_check_participants <- my_data %>% 
#   group_by(participant, age, sex) %>% 
#   tally()
# 
# # mean age == 27.7 years
# mean(df_check_participants$age)


# ------------plot theme--------------
my_plot_theme <- theme(
  axis.title.x = element_text(
    color = "black",
    size = 14,
    face = "bold"
  ),
  axis.title.y = element_text(
    color = "black",
    size = 14,
    face = "bold"
  ),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "grey"),
  axis.text.x = element_text(size = 12, face = "bold"),
  axis.text.y = element_text(size = 12, face = "bold"),
  legend.title = element_text(size = 12, face = "bold"),
  legend.text = element_text(size = 10),
  strip.text.x = element_text(size = 12, face = "bold")
)


# Fixed scale for every diverging-color matrix. Values beyond the displayed
# range are clamped to the endpoint colors so cells remain visible.
matrix_color_limits <- c(-4, 4)
matrix_color_breaks <- seq(-4, 4, by = 2)


# ------------plot 1: number deviation by set size---------------
# First calculate each participant's mean deviation for each set size.
data_number_deviation_by_pp <- my_data %>%
  group_by(SubID, correct_num) %>%
  summarise(
    mean_number_deviation = mean(number_deviation, na.rm = TRUE),
    .groups = "drop"
  )


# Then calculate the average and 95% CI across participants.
data_number_deviation_across_pp <- data_number_deviation_by_pp %>%
  group_by(correct_num) %>%
  summarise(
    n = sum(!is.na(mean_number_deviation)),
    number_deviation_mean = mean(mean_number_deviation, na.rm = TRUE),
    number_deviation_sd = sd(mean_number_deviation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    number_deviation_SEM = number_deviation_sd / sqrt(n),
    number_deviation_CI = number_deviation_SEM * qt(0.975, n - 1)
  )


my_plot_number_deviation <- ggplot() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey"
  ) +
  geom_point(
    data = data_number_deviation_by_pp,
    aes(
      x = factor(correct_num),
      y = mean_number_deviation
    ),
    position = position_jitter(
      width = 0.08,
      height = 0,
      seed = 20261028
    ),
    size = 2,
    alpha = 0.5,
    color = "#A0A0A0"
  ) +
  geom_point(
    data = data_number_deviation_across_pp,
    aes(
      x = factor(correct_num),
      y = number_deviation_mean
    ),
    size = 3,
    color = "black"
  ) +
  geom_errorbar(
    data = data_number_deviation_across_pp,
    aes(
      x = factor(correct_num),
      y = number_deviation_mean,
      ymin = number_deviation_mean - number_deviation_CI,
      ymax = number_deviation_mean + number_deviation_CI
    ),
    linewidth = 0.5,
    width = 0.0,
    color = "black"
  ) +
  labs(
    x = "Set Size",
    y = "Number Deviation"
  ) +
  my_plot_theme


my_plot_number_deviation


# ------------plot 2: percentage of trials by number deviation---------------
# Calculate percentages within each participant and set size first.
number_deviation_levels <- seq(
  min(my_data$number_deviation, na.rm = TRUE),
  max(my_data$number_deviation, na.rm = TRUE)
)

data_number_percentage_by_pp <- my_data %>%
  count(SubID, correct_num, number_deviation, name = "n_trials") %>%
  group_by(SubID, correct_num) %>%
  tidyr::complete(
    number_deviation = number_deviation_levels,
    fill = list(n_trials = 0)
  ) %>%
  mutate(
    percentage_trials = n_trials / sum(n_trials) * 100
  ) %>%
  ungroup()


# Average the participant percentages and calculate 95% CIs.
data_number_percentage_across_pp <- data_number_percentage_by_pp %>%
  group_by(correct_num, number_deviation) %>%
  summarise(
    n = n(),
    percentage_trials_mean = mean(percentage_trials),
    percentage_trials_sd = sd(percentage_trials),
    .groups = "drop"
  ) %>%
  mutate(
    percentage_trials_SEM = percentage_trials_sd / sqrt(n),
    percentage_trials_CI = percentage_trials_SEM * qt(0.975, n - 1)
  )


my_plot_number_percentage <- ggplot(
  data_number_percentage_across_pp,
  aes(
    x = factor(number_deviation),
    y = percentage_trials_mean,
    fill = factor(correct_num)
  )
) +
  geom_col(
    position = position_dodge(0.7),
    width = 0.6,
    alpha = 0.5
  ) +
  geom_errorbar(
    aes(
      ymin = percentage_trials_mean - percentage_trials_CI,
      ymax = percentage_trials_mean + percentage_trials_CI
    ),
    position = position_dodge(0.7),
    width = 0.0,
    color = "black"
  ) +
  labs(
    x = "Number Deviation",
    y = "Percentage of Trials",
    fill = "Set Size"
  ) +
  scale_fill_manual(
    values = c("3" = "#0072B2", "4" = "#D55E00")
  ) +
  my_plot_theme


my_plot_number_percentage


# ------------data for orientation adjustment task---------------
# Keep trials where the reported number was correct or one line too few.
n_trials_before_adjustment_filter <- nrow(my_data)

my_data_adjustment <- my_data %>%
  filter(number_deviation %in% c(-1, 0))

n_trials_after_adjustment_filter <- nrow(my_data_adjustment)
n_trials_removed_adjustment <- n_trials_before_adjustment_filter -
  n_trials_after_adjustment_filter

# Removed 230 of 16,199 trials (1.42%).
percentage_trials_removed_adjustment <-
  n_trials_removed_adjustment / n_trials_before_adjustment_filter * 100


# ------------plot 3: number of bars adjusted---------------
# RM = number deviation -1; Correct = number deviation 0.
data_adjusted_bars_by_pp <- my_data_adjustment %>%
  mutate(
    trial_type = factor(
      number_deviation,
      levels = c(-1, 0),
      labels = c("RM", "Correct")
    )
  ) %>%
  count(
    SubID,
    correct_num,
    trial_type,
    n_bars_adjusted,
    name = "n_trials"
  ) %>%
  group_by(SubID, correct_num, trial_type) %>%
  tidyr::complete(
    n_bars_adjusted = 1:4,
    fill = list(n_trials = 0)
  ) %>%
  mutate(
    percentage_trials = n_trials / sum(n_trials) * 100
  ) %>%
  ungroup() %>%
  mutate(
    n_bars_adjusted = factor(
      n_bars_adjusted,
      levels = c(1, 2, 3, 4)
    )
  )


# ------------plot 3a: one plot for each participant---------------
n_trials_plot_3a <- my_data_adjustment %>%
  mutate(
    trial_type = factor(
      number_deviation,
      levels = c(-1, 0),
      labels = c("RM", "Correct")
    )
  ) %>%
  count(
    SubID,
    correct_num,
    trial_type,
    name = "n_trials"
  )

participant_adjustment_data_list <- split(
  data_adjusted_bars_by_pp,
  data_adjusted_bars_by_pp$SubID,
  drop = TRUE
)

my_plot_adjusted_bars_by_participant <- lapply(
  participant_adjustment_data_list,
  function(participant_data) {
    participant_id <- unique(participant_data$SubID)

    ggplot(
      participant_data,
      aes(
        x = trial_type,
        y = percentage_trials,
        fill = n_bars_adjusted
      )
    ) +
      geom_col(width = 0.65) +
      facet_wrap(
        ~ correct_num,
        labeller = as_labeller(
          c("3" = "Set size 3", "4" = "Set size 4")
        )
      ) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20),
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(0, 0.02))
      ) +
      scale_fill_manual(
        values = c(
          "1" = "#56B4E9",
          "2" = "#009E73",
          "3" = "#E69F00",
          "4" = "#CC79A7"
        ),
        drop = FALSE,
        name = "Bars Adjusted"
      ) +
      labs(
        title = paste("Participant", participant_id),
        x = "Number Response",
        y = "Percentage of Trials"
      ) +
      my_plot_theme
  }
)


# Display the participant plots one by one.
for (participant_id in names(my_plot_adjusted_bars_by_participant)) {
  print(
    n_trials_plot_3a %>%
      filter(SubID == participant_id)
  )

  print(my_plot_adjusted_bars_by_participant[[participant_id]])

  readline(
    prompt = paste0(
      "participant ",
      participant_id,
      "enter for next participant."
    )
  )
}


# ------------plot 3b: average across participants---------------
n_trials_plot_3b <- my_data_adjustment %>%
  mutate(
    trial_type = factor(
      number_deviation,
      levels = c(-1, 0),
      labels = c("RM", "Correct")
    )
  ) %>%
  count(
    correct_num,
    trial_type,
    name = "n_trials"
  )

n_trials_plot_3b


# Each participant contributes one percentage distribution to each condition.
data_adjusted_bars_across_pp <- data_adjusted_bars_by_pp %>%
  group_by(correct_num, trial_type, n_bars_adjusted) %>%
  summarise(
    n_participants = n(),
    percentage_trials_mean = mean(percentage_trials),
    .groups = "drop"
  )


my_plot_adjusted_bars_average <- ggplot(
  data_adjusted_bars_across_pp,
  aes(
    x = trial_type,
    y = percentage_trials_mean,
    fill = n_bars_adjusted
  )
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(
      label = ifelse(
        percentage_trials_mean >= 3,
        paste0(round(percentage_trials_mean, 1), "%"),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black"
  ) +
  geom_text(
    data = n_trials_plot_3b,
    aes(
      x = trial_type,
      y = 106,
      label = paste0("n = ", n_trials)
    ),
    inherit.aes = FALSE,
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  facet_wrap(
    ~ correct_num,
    labeller = as_labeller(
      c("3" = "Set size 3", "4" = "Set size 4")
    )
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 112),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(
    values = c(
      "1" = "#56B4E9",
      "2" = "#009E73",
      "3" = "#E69F00",
      "4" = "#CC79A7"
    ),
    drop = FALSE,
    name = "Bars Adjusted"
  ) +
  labs(
    x = "Number Response",
    y = "Average Percentage of Trials"
  ) +
  my_plot_theme


my_plot_adjusted_bars_average


# ------------data for perceiving one tilted line---------------
# Keep trials where participants reported the correct number and adjusted
# exactly one bar. The location is converted from screen left-to-right order
# into inner-to-outer order, so left- and right-field trials can be combined.
parse_number_list <- function(x) {
  if (is.na(x) || trimws(x) == "") {
    return(numeric(0))
  }

  x <- gsub("\\[|\\]", "", x)

  if (trimws(x) == "") {
    return(numeric(0))
  }

  as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1]]))
}


get_first_adjusted_index <- function(x) {
  values <- parse_number_list(x)

  if (length(values) == 0) {
    return(NA_real_)
  }

  values[1]
}


get_adjusted_orientation <- function(x, adjusted_location) {
  values <- parse_number_list(x)

  if (
    is.na(adjusted_location) ||
      adjusted_location < 1 ||
      adjusted_location > length(values)
  ) {
    return(NA_real_)
  }

  values[as.integer(adjusted_location)]
}


n_trials_before_one_bar_filter <- nrow(my_data)

data_one_bar_perception <- my_data %>%
  filter(
    number_deviation == 0,
    n_bars_adjusted == 1
  ) %>%
  mutate(
    adjusted_index_LtoR = vapply(
      adjusted_indices,
      get_first_adjusted_index,
      numeric(1)
    ),
    adjusted_location = if_else(
      hemifield == "left",
      correct_num - adjusted_index_LtoR,
      adjusted_index_LtoR + 1
    ),
    adjusted_orientation_folded = mapply(
      get_adjusted_orientation,
      adjusted_ori_inner_to_outer_folded,
      adjusted_location
    ),
    target_orientation_magnitude = abs(target_orientation_folded),
    target_direction_folded = factor(
      tilt_direction_folded,
      levels = c("toward", "away"),
      labels = c("Toward fixation", "Away from fixation")
    ),
    adjusted_location_correct = adjusted_location == tilted_slot,

    # Positive = overshoot; negative = undershoot.
    orientation_error_aligned =
      (adjusted_orientation_folded - target_orientation_folded) *
      sign(target_orientation_folded),

    # Positive values are on the target side of vertical. Negative values
    # mean the response crossed vertical into the opposite direction.
    reproduced_orientation_aligned =
      adjusted_orientation_folded * sign(target_orientation_folded),

    direction_correct =
      sign(adjusted_orientation_folded) ==
      sign(target_orientation_folded) &
      adjusted_orientation_folded != 0,

    # Positive location error = farther from fixation; negative = inward.
    location_error = adjusted_location - tilted_slot
  ) %>%
  filter(
    !is.na(adjusted_location),
    !is.na(adjusted_orientation_folded),
    !is.na(target_orientation_folded),
    target_orientation_folded != 0
  )


n_trials_after_one_bar_filter <- nrow(data_one_bar_perception)

percentage_trials_kept_one_bar <-
  n_trials_after_one_bar_filter /
  n_trials_before_one_bar_filter * 100

# Kept 5,928 of 16,199 trials (36.59%).
# Of these trials, 45.77% adjusted the correct inner-to-outer location.


# # ------------plot 4: target location and adjusted location---------------
# # Calculate the distribution of adjusted locations separately for every
# # participant and target location first.
# data_location_confusion_by_pp <- data_one_bar_perception %>%
#   count(
#     SubID,
#     correct_num,
#     tilted_slot,
#     adjusted_location,
#     name = "n_trials"
#   ) %>%
#   group_by(SubID, correct_num, tilted_slot) %>%
#   tidyr::complete(
#     adjusted_location = 1:4,
#     fill = list(n_trials = 0)
#   ) %>%
#   filter(adjusted_location <= correct_num) %>%
#   mutate(
#     percentage_trials = n_trials / sum(n_trials) * 100
#   ) %>%
#   ungroup()
# 
# 
# data_location_confusion_across_pp <- data_location_confusion_by_pp %>%
#   group_by(correct_num, tilted_slot, adjusted_location) %>%
#   summarise(
#     n_participants = n(),
#     percentage_trials_mean = mean(percentage_trials),
#     .groups = "drop"
#   )
# 
# 
# n_trials_plot_4 <- data_one_bar_perception %>%
#   count(correct_num, name = "n_trials")
# 
# n_trials_plot_4
# 
# 
# my_plot_location_confusion <- ggplot(
#   data_location_confusion_across_pp,
#   aes(
#     x = adjusted_location,
#     y = tilted_slot,
#     fill = percentage_trials_mean
#   )
# ) +
#   geom_tile(
#     color = "white",
#     linewidth = 1
#   ) +
#   geom_text(
#     aes(
#       label = paste0(round(percentage_trials_mean, 1), "%")
#     ),
#     size = 3.5,
#     color = "black"
#   ) +
#   facet_wrap(
#     ~ correct_num,
#     labeller = as_labeller(
#       c("3" = "Set size 3", "4" = "Set size 4")
#     )
#   ) +
#   scale_x_continuous(
#     breaks = 1:4
#   ) +
#   scale_y_continuous(
#     breaks = 1:4,
#     trans = "reverse"
#   ) +
#   scale_fill_gradient(
#     low = "white",
#     high = "#0072B2",
#     limits = c(0, 100),
#     name = "Trials"
#   ) +
#   coord_equal() +
#   labs(
#     x = "Adjusted Location (Inner to Outer)",
#     y = "Target Location (Inner to Outer)"
#   ) +
#   my_plot_theme
# 
# 
# my_plot_location_confusion


# # ------------plot 5: correct adjusted location---------------
# # Calculate each participant's proportion of correct adjusted locations.
# data_location_correct_by_pp <- data_one_bar_perception %>%
#   group_by(
#     SubID,
#     correct_num,
#     tilted_slot,
#     target_direction_folded
#   ) %>%
#   summarise(
#     proportion_location_correct = mean(adjusted_location_correct),
#     .groups = "drop"
#   )
# 
# 
# data_location_correct_across_pp <- data_location_correct_by_pp %>%
#   group_by(
#     correct_num,
#     tilted_slot,
#     target_direction_folded
#   ) %>%
#   summarise(
#     n = sum(!is.na(proportion_location_correct)),
#     proportion_correct_mean = mean(
#       proportion_location_correct,
#       na.rm = TRUE
#     ),
#     proportion_correct_sd = sd(
#       proportion_location_correct,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     proportion_correct_SEM = proportion_correct_sd / sqrt(n),
#     proportion_correct_CI =
#       proportion_correct_SEM * qt(0.975, n - 1)
#   )
# 
# 
# data_location_chance <- data.frame(
#   correct_num = c(3, 4),
#   chance = c(1 / 3, 1 / 4)
# )
# 
# 
# n_trials_plot_5 <- data_one_bar_perception %>%
#   count(
#     correct_num,
#     target_direction_folded,
#     name = "n_trials"
#   )
# 
# n_trials_plot_5
# 
# 
# my_plot_location_correct <- ggplot(
#   data_location_correct_across_pp,
#   aes(
#     x = factor(tilted_slot),
#     y = proportion_correct_mean,
#     color = target_direction_folded,
#     group = target_direction_folded
#   )
# ) +
#   geom_hline(
#     data = data_location_chance,
#     aes(yintercept = chance),
#     inherit.aes = FALSE,
#     linetype = "dashed",
#     color = "grey"
#   ) +
#   geom_line(
#     linewidth = 0.7
#   ) +
#   geom_point(
#     position = position_dodge(width = 0.3),
#     size = 3
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = proportion_correct_mean - proportion_correct_CI,
#       ymax = proportion_correct_mean + proportion_correct_CI
#     ),
#     position = position_dodge(width = 0.3),
#     width = 0.12,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ correct_num,
#     labeller = as_labeller(
#       c("3" = "Set size 3", "4" = "Set size 4")
#     )
#   ) +
#   scale_color_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     name = "Target Direction"
#   ) +
#   scale_y_continuous(
#     limits = c(0, 1),
#     breaks = seq(0, 1, 0.2),
#     labels = function(x) paste0(x * 100, "%")
#   ) +
#   labs(
#     x = "Target Location (Inner to Outer)",
#     y = "Correct Adjusted Location"
#   ) +
#   my_plot_theme
# 
# 
# my_plot_location_correct


# # ------------plot 6: signed location error by target location---------------
# # Positive errors are outward; negative errors are inward. Target locations
# # are kept separate because the possible errors depend on target location.
# # For example, an innermost target cannot have a negative location error.
# data_location_error_by_pp <- data_one_bar_perception %>%
#   count(
#     SubID,
#     correct_num,
#     tilted_slot,
#     location_error,
#     name = "n_trials"
#   ) %>%
#   group_by(SubID, correct_num, tilted_slot) %>%
#   tidyr::complete(
#     location_error = -3:3,
#     fill = list(n_trials = 0)
#   ) %>%
#   filter(
#     location_error >= 1 - tilted_slot,
#     location_error <= correct_num - tilted_slot
#   ) %>%
#   mutate(
#     percentage_trials = n_trials / sum(n_trials) * 100
#   ) %>%
#   ungroup()
# 
# 
# data_location_error_across_pp <- data_location_error_by_pp %>%
#   group_by(correct_num, tilted_slot, location_error) %>%
#   summarise(
#     n = n(),
#     percentage_trials_mean = mean(percentage_trials),
#     percentage_trials_sd = sd(percentage_trials),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     percentage_trials_SEM = percentage_trials_sd / sqrt(n),
#     percentage_trials_CI =
#       percentage_trials_SEM * qt(0.975, n - 1)
#   )
# 
# 
# #−1 = adjusted one position closer to fixation than the target.
# # 0 = adjusted the target position.
# #+1 = adjusted one position farther from fixation than the target.
# 
# n_trials_plot_6 <- data_one_bar_perception %>%
#   count(
#     correct_num,
#     tilted_slot,
#     name = "n_trials"
#   )
# 
# n_trials_plot_6
# 
# 
# my_plot_location_error <- ggplot(
#   data_location_error_across_pp,
#   aes(
#     x = factor(
#       location_error,
#       levels = -3:3
#     ),
#     y = percentage_trials_mean
#   )
# ) +
#   geom_col(
#     width = 0.65,
#     fill = "#707070",
#     alpha = 0.75
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = percentage_trials_mean - percentage_trials_CI,
#       ymax = percentage_trials_mean + percentage_trials_CI
#     ),
#     width = 0.12,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ correct_num + tilted_slot,
#     ncol = 4,
#     axes = "all_x",
#     axis.labels = "all_x",
#     labeller = labeller(
#       correct_num = as_labeller(
#         c("3" = "Set size 3", "4" = "Set size 4")
#       ),
#       tilted_slot = function(x) paste("Target location", x)
#     )
#   ) +
#   scale_x_discrete(
#     limits = as.character(-3:3),
#     drop = FALSE
#   ) +
#   labs(
#     x = "Location Error (Negative = Inward; Positive = Outward)",
#     y = "Percentage of Trials"
#   ) +
#   my_plot_theme
# 
# 
# my_plot_location_error


# # ------------plot 7a: reproduced orientation at correct location---------------
# # Use only trials where the participant adjusted the correct location.
# # The response is aligned to the target direction, so the identity line is
# # the correct response for both CW and CCW targets in both visual fields.
# data_reproduced_orientation_by_pp <- data_one_bar_perception %>%
#   filter(adjusted_location_correct) %>%
#   group_by(
#     SubID,
#     correct_num,
#     target_orientation_magnitude,
#     target_direction_folded
#   ) %>%
#   summarise(
#     reproduced_orientation_mean = mean(
#       reproduced_orientation_aligned,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   )
# 
# 
# data_reproduced_orientation_across_pp <-
#   data_reproduced_orientation_by_pp %>%
#   group_by(
#     correct_num,
#     target_orientation_magnitude,
#     target_direction_folded
#   ) %>%
#   summarise(
#     n = sum(!is.na(reproduced_orientation_mean)),
#     reproduced_orientation_sd = sd(
#       reproduced_orientation_mean,
#       na.rm = TRUE
#     ),
#     reproduced_orientation_mean = mean(
#       reproduced_orientation_mean,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     reproduced_orientation_SEM =
#       reproduced_orientation_sd / sqrt(n),
#     reproduced_orientation_CI =
#       reproduced_orientation_SEM * qt(0.975, n - 1)
#   )
# 
# 
# n_trials_plot_7a <- data_one_bar_perception %>%
#   filter(adjusted_location_correct) %>%
#   count(correct_num, name = "n_trials")
# 
# n_trials_plot_7a
# 
# 
# 
# my_plot_reproduced_orientation <- ggplot(
#   data_reproduced_orientation_across_pp,
#   aes(
#     x = target_orientation_magnitude,
#     y = reproduced_orientation_mean,
#     color = target_direction_folded,
#     group = target_direction_folded
#   )
# ) +
#   geom_abline(
#     intercept = 0,
#     slope = 1,
#     linetype = "dashed",
#     color = "grey"
#   ) +
#   annotate(
#     "text",
#     x = 6,
#     y = 7.5,
#     label = "Overshot",
#     size = 4,
#     fontface = "bold",
#     color = "#505050"
#   ) +
#   annotate(
#     "text",
#     x = 6,
#     y = 4.5,
#     label = "Undershot",
#     size = 4,
#     fontface = "bold",
#     color = "#505050"
#   ) +
#   geom_ribbon(
#     aes(
#       ymin = reproduced_orientation_mean - reproduced_orientation_CI,
#       ymax = reproduced_orientation_mean + reproduced_orientation_CI,
#       fill = target_direction_folded
#     ),
#     alpha = 0.15,
#     color = NA
#   ) +
#   geom_line(
#     linewidth = 0.7
#   ) +
#   geom_point(
#     position = position_dodge(width = 0.3),
#     size = 3
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = reproduced_orientation_mean - reproduced_orientation_CI,
#       ymax = reproduced_orientation_mean + reproduced_orientation_CI
#     ),
#     position = position_dodge(width = 0.3),
#     width = 0.12,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ correct_num,
#     labeller = as_labeller(
#       c("3" = "Set size 3", "4" = "Set size 4")
#     )
#   ) +
#   scale_color_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     name = "Target Direction"
#   ) +
#   scale_fill_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     guide = "none"
#   ) +
#   labs(
#     title = "Reproduced Orientation: Correct-Location Trials Only",
#     subtitle = paste(
#       "Correct number response; one bar adjusted",
#       "at the target location"
#     ),
#     x = "Presented Orientation Magnitude (Degrees)",
#     y = "Reproduced Orientation (Degrees)"
#   ) +
#   my_plot_theme
# 
# 
# my_plot_reproduced_orientation
# 
# 
# # ------------plot 7b: reproduced orientation at all locations---------------
# # Include trials where the participant adjusted either the correct or the
# # wrong location. The plotted response is the folded orientation of the one
# # bar that the participant adjusted.
# data_reproduced_orientation_all_locations_by_pp <-
#   data_one_bar_perception %>%
#   group_by(
#     SubID,
#     correct_num,
#     target_orientation_magnitude,
#     target_direction_folded
#   ) %>%
#   summarise(
#     reproduced_orientation_mean = mean(
#       reproduced_orientation_aligned,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   )
# 
# 
# data_reproduced_orientation_all_locations_across_pp <-
#   data_reproduced_orientation_all_locations_by_pp %>%
#   group_by(
#     correct_num,
#     target_orientation_magnitude,
#     target_direction_folded
#   ) %>%
#   summarise(
#     n = sum(!is.na(reproduced_orientation_mean)),
#     reproduced_orientation_sd = sd(
#       reproduced_orientation_mean,
#       na.rm = TRUE
#     ),
#     reproduced_orientation_mean = mean(
#       reproduced_orientation_mean,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     reproduced_orientation_SEM =
#       reproduced_orientation_sd / sqrt(n),
#     reproduced_orientation_CI =
#       reproduced_orientation_SEM * qt(0.975, n - 1)
#   )
# 
# 
# n_trials_plot_7b <- data_one_bar_perception %>%
#   count(correct_num, name = "n_trials")
# 
# 
# n_trials_plot_7b
# 
# 
# my_plot_reproduced_orientation_all_locations <- ggplot(
#   data_reproduced_orientation_all_locations_across_pp,
#   aes(
#     x = target_orientation_magnitude,
#     y = reproduced_orientation_mean,
#     color = target_direction_folded,
#     group = target_direction_folded
#   )
# ) +
#   geom_abline(
#     intercept = 0,
#     slope = 1,
#     linetype = "dashed",
#     color = "grey"
#   ) +
#   annotate(
#     "text",
#     x = 6,
#     y = 7.5,
#     label = "Overshot",
#     size = 4,
#     fontface = "bold",
#     color = "#505050"
#   ) +
#   annotate(
#     "text",
#     x = 6,
#     y = 4.5,
#     label = "Undershot",
#     size = 4,
#     fontface = "bold",
#     color = "#505050"
#   ) +
#   geom_ribbon(
#     aes(
#       ymin = reproduced_orientation_mean - reproduced_orientation_CI,
#       ymax = reproduced_orientation_mean + reproduced_orientation_CI,
#       fill = target_direction_folded
#     ),
#     alpha = 0.15,
#     color = NA
#   ) +
#   geom_line(
#     linewidth = 0.7
#   ) +
#   geom_point(
#     position = position_dodge(width = 0.3),
#     size = 3
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = reproduced_orientation_mean - reproduced_orientation_CI,
#       ymax = reproduced_orientation_mean + reproduced_orientation_CI
#     ),
#     position = position_dodge(width = 0.3),
#     width = 0.12,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ correct_num,
#     labeller = as_labeller(
#       c("3" = "Set size 3", "4" = "Set size 4")
#     )
#   ) +
#   scale_color_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     name = "Target Direction"
#   ) +
#   scale_fill_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     guide = "none"
#   ) +
#   labs(
#     title = "Reproduced Orientation: All Adjusted Locations",
#     subtitle = paste(
#       "Correct number response; one bar adjusted",
#       "at either the correct or wrong location"
#     ),
#     x = "Presented Orientation Magnitude (Degrees)",
#     y = "Reproduced Orientation (Degrees)"
#   ) +
#   my_plot_theme
# 
# 
# my_plot_reproduced_orientation_all_locations
# 

# # ------------plot 8a: orientation error at correct location---------------
# # This is the error definition selected from the example table:
# # (folded response - folded target) * sign(folded target).
# # Use only trials where the participant adjusted the correct location.
# data_orientation_error_by_pp <- data_one_bar_perception %>%
#   filter(adjusted_location_correct) %>%
#   group_by(
#     SubID,
#     correct_num,
#     target_orientation_magnitude,
#     target_direction_folded
#   ) %>%
#   summarise(
#     orientation_error_mean = mean(
#       orientation_error_aligned,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   )
# 
# 
# data_orientation_error_across_pp <- data_orientation_error_by_pp %>%
#   group_by(
#     correct_num,
#     target_orientation_magnitude,
#     target_direction_folded
#   ) %>%
#   summarise(
#     n = sum(!is.na(orientation_error_mean)),
#     orientation_error_sd = sd(
#       orientation_error_mean,
#       na.rm = TRUE
#     ),
#     orientation_error_mean = mean(
#       orientation_error_mean,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     orientation_error_SEM = orientation_error_sd / sqrt(n),
#     orientation_error_CI =
#       orientation_error_SEM * qt(0.975, n - 1)
#   )
# 
# 
# n_trials_plot_8a <- data_one_bar_perception %>%
#   filter(adjusted_location_correct) %>%
#   count(correct_num, name = "n_trials")
# 
# n_trials_plot_8a
# 
# 
# my_plot_orientation_error <- ggplot(
#   data_orientation_error_across_pp,
#   aes(
#     x = target_orientation_magnitude,
#     y = orientation_error_mean,
#     color = target_direction_folded,
#     group = target_direction_folded
#   )
# ) +
#   geom_hline(
#     yintercept = 0,
#     linetype = "dashed",
#     color = "grey"
#   ) +
#   geom_ribbon(
#     aes(
#       ymin = orientation_error_mean - orientation_error_CI,
#       ymax = orientation_error_mean + orientation_error_CI,
#       fill = target_direction_folded
#     ),
#     alpha = 0.15,
#     color = NA
#   ) +
#   geom_line(
#     linewidth = 0.7
#   ) +
#   geom_point(
#     position = position_dodge(width = 0.3),
#     size = 3
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = orientation_error_mean - orientation_error_CI,
#       ymax = orientation_error_mean + orientation_error_CI
#     ),
#     position = position_dodge(width = 0.3),
#     width = 0.12,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ correct_num,
#     labeller = as_labeller(
#       c("3" = "Set size 3", "4" = "Set size 4")
#     )
#   ) +
#   scale_color_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     name = "Target Direction"
#   ) +
#   scale_fill_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     guide = "none"
#   ) +
#   labs(
#     title = "Orientation Error: Correct-Location Trials Only",
#     subtitle = paste(
#       "Correct number response; one bar adjusted",
#       "at the target location"
#     ),
#     x = "Presented Orientation Magnitude (Degrees)",
#     y = "Orientation Error (Degrees)\nNegative = Undershoot; Positive = Overshoot"
#   ) +
#   my_plot_theme
# 
# 
# 
# my_plot_orientation_error


# # ------------data for plots 8b and 8c: orientation error at all locations---------------
# # Include trials where the participant adjusted either the correct or the
# # wrong location. The error is based on the folded orientation of the one
# # bar that the participant adjusted.
# data_orientation_error_all_locations_by_pp <-
#   data_one_bar_perception %>%
#   group_by(
#     SubID,
#     correct_num,
#     target_orientation_magnitude,
#     target_direction_folded
#   ) %>%
#   summarise(
#     orientation_error_mean = mean(
#       orientation_error_aligned,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   )
# 
# 
# data_orientation_error_all_locations_across_pp <-
#   data_orientation_error_all_locations_by_pp %>%
#   group_by(
#     correct_num,
#     target_orientation_magnitude,
#     target_direction_folded
#   ) %>%
#   summarise(
#     n = sum(!is.na(orientation_error_mean)),
#     orientation_error_sd = sd(
#       orientation_error_mean,
#       na.rm = TRUE
#     ),
#     orientation_error_mean = mean(
#       orientation_error_mean,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     orientation_error_SEM = orientation_error_sd / sqrt(n),
#     orientation_error_CI =
#       orientation_error_SEM * qt(0.975, n - 1)
#   )
# 
# 
# n_trials_plot_8b <- data_one_bar_perception %>%
#   count(correct_num, name = "n_trials")
# 
# n_trials_plot_8b
# 
# 
# # ------------plot 8b: orientation error at all locations, direction combined---------------
# # This is the main plot. Toward-fixation and away-from-fixation trials are
# # combined within each participant before calculating the group mean and CI.
# data_orientation_error_all_locations_combined_by_pp <-
#   data_one_bar_perception %>%
#   group_by(
#     SubID,
#     correct_num,
#     target_orientation_magnitude
#   ) %>%
#   summarise(
#     orientation_error_mean = mean(
#       orientation_error_aligned,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   )
# 
# 
# data_orientation_error_all_locations_combined_across_pp <-
#   data_orientation_error_all_locations_combined_by_pp %>%
#   group_by(
#     correct_num,
#     target_orientation_magnitude
#   ) %>%
#   summarise(
#     n = sum(!is.na(orientation_error_mean)),
#     orientation_error_sd = sd(
#       orientation_error_mean,
#       na.rm = TRUE
#     ),
#     orientation_error_mean = mean(
#       orientation_error_mean,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     orientation_error_SEM = orientation_error_sd / sqrt(n),
#     orientation_error_CI =
#       orientation_error_SEM * qt(0.975, n - 1)
#   )
# 
# 
# my_plot_orientation_error_all_locations_combined <- ggplot(
#   data_orientation_error_all_locations_combined_across_pp,
#   aes(
#     x = target_orientation_magnitude,
#     y = orientation_error_mean
#   )
# ) +
#   geom_hline(
#     yintercept = 0,
#     linetype = "dashed",
#     color = "grey"
#   ) +
#   geom_ribbon(
#     aes(
#       ymin = orientation_error_mean - orientation_error_CI,
#       ymax = orientation_error_mean + orientation_error_CI
#     ),
#     fill = "#707070",
#     alpha = 0.15
#   ) +
#   geom_line(
#     linewidth = 0.7,
#     color = "#707070"
#   ) +
#   geom_point(
#     size = 3,
#     color = "#707070"
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = orientation_error_mean - orientation_error_CI,
#       ymax = orientation_error_mean + orientation_error_CI
#     ),
#     width = 0.12,
#     linewidth = 0.5,
#     color = "#707070"
#   ) +
#   facet_wrap(
#     ~ correct_num,
#     labeller = as_labeller(
#       c("3" = "Set size 3", "4" = "Set size 4")
#     )
#   ) +
#   scale_x_continuous(
#     breaks = c(1, 2, 4, 6, 8)
#   ) +
#   labs(
#     title = "Orientation Error: All Adjusted Locations",
#     subtitle = paste(
#       "Correct number response; one bar adjusted;",
#       "target directions combined"
#     ),
#     x = "Presented Orientation Magnitude (Degrees)",
#     y = "Orientation Error (Degrees)\nNegative = Undershoot; Positive = Overshoot"
#   ) +
#   my_plot_theme
# 
# 
# my_plot_orientation_error_all_locations_combined
# 

# # ------------plot 8c: orientation error at all locations, direction separated---------------
# my_plot_orientation_error_all_locations <- ggplot(
#   data_orientation_error_all_locations_across_pp,
#   aes(
#     x = target_orientation_magnitude,
#     y = orientation_error_mean,
#     color = target_direction_folded,
#     group = target_direction_folded
#   )
# ) +
#   geom_hline(
#     yintercept = 0,
#     linetype = "dashed",
#     color = "grey"
#   ) +
#   geom_ribbon(
#     aes(
#       ymin = orientation_error_mean - orientation_error_CI,
#       ymax = orientation_error_mean + orientation_error_CI,
#       fill = target_direction_folded
#     ),
#     alpha = 0.15,
#     color = NA
#   ) +
#   geom_line(
#     linewidth = 0.7
#   ) +
#   geom_point(
#     position = position_dodge(width = 0.3),
#     size = 3
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = orientation_error_mean - orientation_error_CI,
#       ymax = orientation_error_mean + orientation_error_CI
#     ),
#     position = position_dodge(width = 0.3),
#     width = 0.12,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ correct_num,
#     labeller = as_labeller(
#       c("3" = "Set size 3", "4" = "Set size 4")
#     )
#   ) +
#   scale_color_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     name = "Target Direction"
#   ) +
#   scale_fill_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     guide = "none"
#   ) +
#   labs(
#     title = "Orientation Error: All Adjusted Locations",
#     subtitle = paste(
#       "Correct number response; one bar adjusted",
#       "at either the correct or wrong location"
#     ),
#     x = "Presented Orientation Magnitude (Degrees)",
#     y = "Orientation Error (Degrees)\nNegative = Undershoot; Positive = Overshoot"
#   ) +
#   my_plot_theme
# 
# 
# my_plot_orientation_error_all_locations


# ------------plot 8d: orientation error including multiple adjusted bars---------------
# Use correct-number trials with one or more adjusted bars. Calculate the
# target-aligned error for every adjusted bar, then average the adjusted bars
# within each trial so that every trial has the same weight.
data_orientation_error_all_adjustments <- my_data %>%
  filter(
    number_deviation == 0,
    n_bars_adjusted >= 1,
    n_bars_adjusted <= correct_num
  ) %>%
  mutate(
    trial_id = row_number(),
    adjusted_index_LtoR = lapply(
      adjusted_indices,
      parse_number_list
    )
  ) %>%
  tidyr::unnest_longer(
    adjusted_index_LtoR
  ) %>%
  mutate(
    adjusted_location = if_else(
      hemifield == "left",
      correct_num - adjusted_index_LtoR,
      adjusted_index_LtoR + 1
    ),
    adjusted_orientation_folded = mapply(
      get_adjusted_orientation,
      adjusted_ori_inner_to_outer_folded,
      adjusted_location
    ),
    target_orientation_magnitude = abs(
      target_orientation_folded
    ),
    orientation_error_aligned =
      (adjusted_orientation_folded - target_orientation_folded) *
      sign(target_orientation_folded)
  ) %>%
  filter(
    !is.na(adjusted_location),
    !is.na(adjusted_orientation_folded),
    !is.na(target_orientation_folded),
    target_orientation_folded != 0
  )


# Average adjusted bars within each trial first.
data_orientation_error_all_adjustments_by_trial <-
  data_orientation_error_all_adjustments %>%
  group_by(
    trial_id,
    SubID,
    correct_num,
    n_bars_adjusted,
    target_orientation_magnitude
  ) %>%
  summarise(
    orientation_error_trial_mean = mean(
      orientation_error_aligned,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


n_trials_plot_8d <-
  data_orientation_error_all_adjustments_by_trial %>%
  count(
    correct_num,
    n_bars_adjusted,
    name = "n_trials"
  )

n_trials_plot_8d


# Then average trials within each participant.
data_orientation_error_all_adjustments_by_pp <-
  data_orientation_error_all_adjustments_by_trial %>%
  group_by(
    SubID,
    correct_num,
    target_orientation_magnitude
  ) %>%
  summarise(
    orientation_error_mean = mean(
      orientation_error_trial_mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Finally calculate the group mean and 95% CI across participants.
data_orientation_error_all_adjustments_across_pp <-
  data_orientation_error_all_adjustments_by_pp %>%
  group_by(
    correct_num,
    target_orientation_magnitude
  ) %>%
  summarise(
    n = sum(!is.na(orientation_error_mean)),
    orientation_error_sd = sd(
      orientation_error_mean,
      na.rm = TRUE
    ),
    orientation_error_mean = mean(
      orientation_error_mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    orientation_error_SEM = orientation_error_sd / sqrt(n),
    orientation_error_CI =
      orientation_error_SEM * qt(0.975, n - 1)
  )


my_plot_orientation_error_all_adjustments <- ggplot(
  data_orientation_error_all_adjustments_across_pp,
  aes(
    x = target_orientation_magnitude,
    y = orientation_error_mean
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey"
  ) +
  geom_ribbon(
    aes(
      ymin = orientation_error_mean - orientation_error_CI,
      ymax = orientation_error_mean + orientation_error_CI
    ),
    fill = "#707070",
    alpha = 0.15
  ) +
  geom_line(
    linewidth = 0.7,
    color = "#707070"
  ) +
  geom_point(
    size = 3,
    color = "#707070"
  ) +
  geom_errorbar(
    aes(
      ymin = orientation_error_mean - orientation_error_CI,
      ymax = orientation_error_mean + orientation_error_CI
    ),
    width = 0.12,
    linewidth = 0.5,
    color = "#707070"
  ) +
  facet_wrap(
    ~ correct_num,
    labeller = as_labeller(
      c("3" = "Set size 3", "4" = "Set size 4")
    )
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 4, 6, 8)
  ) +
  labs(
    title = "Orientation Error Across All Adjusted Bars",
    subtitle = paste(
      "Correct number responses; one or more bars adjusted;",
      "each trial weighted equally"
    ),
    x = "Presented Target Orientation Magnitude (Degrees)",
    y = paste(
      "Mean Adjustment Error \nAcross Adjusted Bars (Degrees)",
      sep = ""
    )
  ) +
  my_plot_theme


my_plot_orientation_error_all_adjustments



# # ------------plot 9: location and direction outcomes---------------
# data_location_direction_outcome_by_pp <- data_one_bar_perception %>%
#   mutate(
#     outcome = case_when(
#       adjusted_location_correct & direction_correct ~
#         "Correct location + correct direction",
#       adjusted_location_correct & !direction_correct ~
#         "Correct location + wrong direction",
#       !adjusted_location_correct & direction_correct ~
#         "Wrong location + correct direction",
#       TRUE ~ "Wrong location + wrong direction"
#     ),
#     outcome = factor(
#       outcome,
#       levels = c(
#         "Correct location + correct direction",
#         "Correct location + wrong direction",
#         "Wrong location + correct direction",
#         "Wrong location + wrong direction"
#       )
#     )
#   ) %>%
#   count(
#     SubID,
#     correct_num,
#     target_direction_folded,
#     outcome,
#     name = "n_trials"
#   ) %>%
#   group_by(SubID, correct_num, target_direction_folded) %>%
#   tidyr::complete(
#     outcome,
#     fill = list(n_trials = 0)
#   ) %>%
#   mutate(
#     percentage_trials = n_trials / sum(n_trials) * 100
#   ) %>%
#   ungroup()
# 
# 
# 
# data_location_direction_outcome_across_pp <-
#   data_location_direction_outcome_by_pp %>%
#   group_by(correct_num, target_direction_folded, outcome) %>%
#   summarise(
#     n_participants = n(),
#     percentage_trials_mean = mean(percentage_trials),
#     .groups = "drop"
#   )
# 
# 
# n_trials_plot_9 <- data_one_bar_perception %>%
#   count(
#     correct_num,
#     target_direction_folded,
#     name = "n_trials"
#   )
# 
# n_trials_plot_9
# 
# 
# 
# my_plot_location_direction_outcome <- ggplot(
#   data_location_direction_outcome_across_pp,
#   aes(
#     x = target_direction_folded,
#     y = percentage_trials_mean,
#     fill = outcome
#   )
# ) +
#   geom_col(width = 0.65) +
#   facet_wrap(
#     ~ correct_num,
#     labeller = as_labeller(
#       c("3" = "Set size 3", "4" = "Set size 4")
#     )
#   ) +
#   scale_y_continuous(
#     limits = c(0, 100),
#     breaks = seq(0, 100, 20),
#     labels = function(x) paste0(x, "%"),
#     expand = expansion(mult = c(0, 0.02))
#   ) +
#   scale_fill_manual(
#     values = c(
#       "Correct location + correct direction" = "#009E73",
#       "Correct location + wrong direction" = "#56B4E9",
#       "Wrong location + correct direction" = "#E69F00",
#       "Wrong location + wrong direction" = "#CC79A7"
#     ),
#     name = "Outcome"
#   ) +
#   labs(
#     x = "Target Direction",
#     y = "Average Percentage of Trials"
#   ) +
#   my_plot_theme
# 
# 
# my_plot_location_direction_outcome


# # ------------plot 10: visual field check---------------
# # This diagnostic verifies that location performance is comparable after
# # converting both visual fields into the same inner-to-outer location system.
# data_visual_field_check_by_pp <- data_one_bar_perception %>%
#   mutate(
#     visual_field = factor(
#       hemifield,
#       levels = c("left", "right"),
#       labels = c("Left visual field", "Right visual field")
#     )
#   ) %>%
#   group_by(
#     SubID,
#     correct_num,
#     tilted_slot,
#     visual_field
#   ) %>%
#   summarise(
#     proportion_location_correct = mean(adjusted_location_correct),
#     .groups = "drop"
#   )
# 
# 
# data_visual_field_check_across_pp <- data_visual_field_check_by_pp %>%
#   group_by(correct_num, tilted_slot, visual_field) %>%
#   summarise(
#     n = sum(!is.na(proportion_location_correct)),
#     proportion_correct_mean = mean(
#       proportion_location_correct,
#       na.rm = TRUE
#     ),
#     proportion_correct_sd = sd(
#       proportion_location_correct,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     proportion_correct_SEM = proportion_correct_sd / sqrt(n),
#     proportion_correct_CI =
#       proportion_correct_SEM * qt(0.975, n - 1)
#   )
# 
# 
# n_trials_plot_10 <- data_one_bar_perception %>%
#   mutate(
#     visual_field = factor(
#       hemifield,
#       levels = c("left", "right"),
#       labels = c("Left visual field", "Right visual field")
#     )
#   ) %>%
#   count(
#     correct_num,
#     visual_field,
#     name = "n_trials"
#   )
# 
# n_trials_plot_10
# 
# 
# my_plot_visual_field_check <- ggplot(
#   data_visual_field_check_across_pp,
#   aes(
#     x = factor(tilted_slot),
#     y = proportion_correct_mean,
#     color = visual_field,
#     group = visual_field
#   )
# ) +
#   geom_line(
#     linewidth = 0.7
#   ) +
#   geom_point(
#     position = position_dodge(width = 0.3),
#     size = 3
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = proportion_correct_mean - proportion_correct_CI,
#       ymax = proportion_correct_mean + proportion_correct_CI
#     ),
#     position = position_dodge(width = 0.3),
#     width = 0.12,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ correct_num,
#     labeller = as_labeller(
#       c("3" = "Set size 3", "4" = "Set size 4")
#     )
#   ) +
#   scale_color_manual(
#     values = c(
#       "Left visual field" = "#009E73",
#       "Right visual field" = "#CC79A7"
#     ),
#     name = "Visual Field"
#   ) +
#   scale_y_continuous(
#     limits = c(0, 1),
#     breaks = seq(0, 1, 0.2),
#     labels = function(x) paste0(x * 100, "%")
#   ) +
#   labs(
#     x = "Target Location (Inner to Outer)",
#     y = "Correct Adjusted Location"
#   ) +
#   my_plot_theme
# 
# 
# my_plot_visual_field_check




# ------------data for multiple adjusted bars---------------
# Keep correct number responses where participants adjusted 2, 3, or 4 bars.
data_multiple_adjustments <- my_data %>%
  filter(
    number_deviation == 0,
    n_bars_adjusted %in% c(2, 3, 4)
  )


# Show how many trials enter plot 11 and the percentage of two denominators.
data_multiple_adjustments_percentage_all <- my_data %>%
  summarise(
    set_size = "All",
    n_all_trials = n(),
    n_correct_number_trials = sum(
      number_deviation == 0,
      na.rm = TRUE
    ),
    n_multiple_adjustment_trials = sum(
      number_deviation == 0 &
        n_bars_adjusted %in% c(2, 3, 4),
      na.rm = TRUE
    ),
    percentage_of_all_trials =
      n_multiple_adjustment_trials / n_all_trials * 100,
    percentage_of_correct_number_trials =
      n_multiple_adjustment_trials /
      n_correct_number_trials * 100
  )


data_multiple_adjustments_percentage_by_set_size <- my_data %>%
  group_by(correct_num) %>%
  summarise(
    n_all_trials = n(),
    n_correct_number_trials = sum(
      number_deviation == 0,
      na.rm = TRUE
    ),
    n_multiple_adjustment_trials = sum(
      number_deviation == 0 &
        n_bars_adjusted %in% c(2, 3, 4),
      na.rm = TRUE
    ),
    percentage_of_all_trials =
      n_multiple_adjustment_trials / n_all_trials * 100,
    percentage_of_correct_number_trials =
      n_multiple_adjustment_trials /
      n_correct_number_trials * 100,
    .groups = "drop"
  ) %>%
  mutate(
    set_size = as.character(correct_num)
  ) %>%
  select(
    set_size,
    n_all_trials,
    n_correct_number_trials,
    n_multiple_adjustment_trials,
    percentage_of_all_trials,
    percentage_of_correct_number_trials
  )


data_multiple_adjustments_percentage <- bind_rows(
  data_multiple_adjustments_percentage_all,
  data_multiple_adjustments_percentage_by_set_size
) %>%
  mutate(
    percentage_of_all_trials = round(
      percentage_of_all_trials,
      2
    ),
    percentage_of_correct_number_trials = round(
      percentage_of_correct_number_trials,
      2
    )
  )

data_multiple_adjustments_percentage

# Overall: 2,694 trials; 16.63% of all trials and 31.25% of trials with a
# correct number response.


# Number of correct-number trials in each participant-level condition. This
# includes trials with 1, 2, 3, or 4 adjusted bars and is the denominator for
# calculating how often each location was adjusted.
data_adjustment_trials_by_pp <- my_data %>%
  filter(
    number_deviation == 0,
    n_bars_adjusted >= 1,
    n_bars_adjusted <= correct_num
  ) %>%
  count(
    SubID,
    correct_num,
    n_bars_adjusted,
    tilted_slot,
    name = "n_trials"
  )

# Make one row for every adjusted location on every correct-number trial.
# adjusted_indices is in screen left-to-right order, so left-field locations
# are reversed into the same inner-to-outer order used by tilted_slot.
data_adjustment_locations <- my_data %>%
  filter(
    number_deviation == 0,
    n_bars_adjusted >= 1,
    n_bars_adjusted <= correct_num
  ) %>%
  mutate(
    adjusted_index_LtoR = lapply(
      adjusted_indices,
      parse_number_list
    )
  ) %>%
  tidyr::unnest_longer(
    adjusted_index_LtoR
  ) %>%
  mutate(
    adjusted_location = if_else(
      hemifield == "left",
      correct_num - adjusted_index_LtoR,
      adjusted_index_LtoR + 1
    )
  )


# ------------plot 11: locations selected by number of adjusted bars---------------
# Each cell is the percentage of trials in which that adjusted location was
# selected. Rows sum to 100%, 200%, 300%, or 400% when 1, 2, 3, or 4 bars
# were adjusted, respectively.
data_adjusted_location_matrix_by_pp <-
  data_adjustment_locations %>%
  count(
    SubID,
    correct_num,
    n_bars_adjusted,
    tilted_slot,
    adjusted_location,
    name = "n_trials_location_selected"
  ) %>%
  group_by(
    SubID,
    correct_num,
    n_bars_adjusted,
    tilted_slot
  ) %>%
  tidyr::complete(
    adjusted_location = 1:4,
    fill = list(n_trials_location_selected = 0)
  ) %>%
  ungroup() %>%
  filter(adjusted_location <= correct_num) %>%
  left_join(
    data_adjustment_trials_by_pp,
    by = c(
      "SubID",
      "correct_num",
      "n_bars_adjusted",
      "tilted_slot"
    )
  ) %>%
  mutate(
    percentage_trials_location_selected =
      n_trials_location_selected / n_trials * 100
  )


data_adjusted_location_matrix_across_pp <-
  data_adjusted_location_matrix_by_pp %>%
  group_by(
    correct_num,
    n_bars_adjusted,
    tilted_slot,
    adjusted_location
  ) %>%
  summarise(
    n_participants = n(),
    percentage_trials_mean = mean(
      percentage_trials_location_selected
    ),
    .groups = "drop"
  )


# ------------plot 11a: set size 3---------------
data_adjusted_location_matrix_set_size_3 <-
  data_adjusted_location_matrix_across_pp %>%
  filter(correct_num == 3)



n_trials_plot_11a <- my_data %>%
  filter(
    number_deviation == 0,
    correct_num == 3,
    n_bars_adjusted %in% 1:3
  ) %>%
  count(
    n_bars_adjusted,
    tilted_slot,
    name = "n_trials"
  )

n_trials_plot_11a


my_plot_multiple_adjusted_locations_set_size_3 <- ggplot(
  data_adjusted_location_matrix_set_size_3,
  aes(
    x = adjusted_location,
    y = tilted_slot,
    fill = percentage_trials_mean
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = paste0(round(percentage_trials_mean, 1), "%")
    ),
    size = 4,
    color = "black"
  ) +
  facet_wrap(
    ~ n_bars_adjusted,
    labeller = as_labeller(
      c(
        "1" = "1 bar adjusted",
        "2" = "2 bars adjusted",
        "3" = "3 bars adjusted"
      )
    )
  ) +
  scale_x_continuous(
    breaks = 1:3
  ) +
  scale_y_continuous(
    breaks = 1:3,
    trans = "reverse"
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#0072B2",
    limits = c(0, 100),
    name = "Trials"
  ) +
  coord_equal() +
  labs(
    title = "Adjusted Locations: Set Size 3",
    subtitle = paste(
      "Correct number responses; cell = percentage of trials",
      "in which the location was adjusted; rows sum to 100% x bars adjusted"
    ),
    x = "Adjusted Location (Inner to Outer)",
    y = "Target Location (Inner to Outer)"
  ) +
  my_plot_theme


my_plot_multiple_adjusted_locations_set_size_3


# ------------plot 11b: set size 4---------------
data_adjusted_location_matrix_set_size_4 <-
  data_adjusted_location_matrix_across_pp %>%
  filter(correct_num == 4)


n_trials_plot_11b <- my_data %>%
  filter(
    number_deviation == 0,
    correct_num == 4,
    n_bars_adjusted %in% 1:4
  ) %>%
  count(
    n_bars_adjusted,
    tilted_slot,
    name = "n_trials"
  )

n_trials_plot_11b


my_plot_multiple_adjusted_locations_set_size_4 <- ggplot(
  data_adjusted_location_matrix_set_size_4,
  aes(
    x = adjusted_location,
    y = tilted_slot,
    fill = percentage_trials_mean
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = paste0(round(percentage_trials_mean, 1), "%")
    ),
    size = 3.5,
    color = "black"
  ) +
  facet_wrap(
    ~ n_bars_adjusted,
    labeller = as_labeller(
      c(
        "1" = "1 bar adjusted",
        "2" = "2 bars adjusted",
        "3" = "3 bars adjusted",
        "4" = "4 bars adjusted"
      )
    )
  ) +
  scale_x_continuous(
    breaks = 1:4
  ) +
  scale_y_continuous(
    breaks = 1:4,
    trans = "reverse"
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#0072B2",
    limits = c(0, 100),
    name = "Trials"
  ) +
  coord_equal() +
  labs(
    title = "Adjusted Locations: Set Size 4",
    subtitle = paste(
      "Correct number responses; cell = percentage of trials",
      "in which the location was adjusted; rows sum to 100% x bars adjusted"
    ),
    x = "Adjusted Location (Inner to Outer)",
    y = "Target Location (Inner to Outer)"
  ) +
  my_plot_theme


my_plot_multiple_adjusted_locations_set_size_4



# ------------data for orientation-error matrix---------------
# Make one row for every final bar location. Correct-number trials have the
# same number of final response bars as presented bars, so response locations
# can be compared directly with tilted_slot in inner-to-outer order.
data_orientation_error_by_adjustment_count <- my_data %>%
  filter(number_deviation == 0) %>%
  mutate(
    response_orientation_folded = lapply(
      adjusted_ori_inner_to_outer_folded,
      parse_number_list
    ),
    response_location = lapply(
      response_orientation_folded,
      seq_along
    )
  ) %>%
  tidyr::unnest(
    c(response_location, response_orientation_folded)
  ) %>%
  mutate(
    aligned_response_orientation =
      response_orientation_folded *
      sign(target_orientation_folded),

    # The target location should reproduce the target magnitude. Every
    # non-target location should remain vertical at zero degrees.
    expected_orientation = if_else(
      response_location == tilted_slot,
      abs(target_orientation_folded),
      0
    ),
    orientation_error =
      aligned_response_orientation - expected_orientation
  )


# Calculate each participant's mean error first.
data_orientation_error_matrix_by_pp <-
  data_orientation_error_by_adjustment_count %>%
  group_by(
    SubID,
    correct_num,
    n_bars_adjusted,
    tilted_slot,
    response_location
  ) %>%
  summarise(
    n_trials = sum(!is.na(orientation_error)),
    orientation_error_mean = mean(
      orientation_error,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Then average participants and calculate 95% CIs.
data_orientation_error_matrix_across_pp <-
  data_orientation_error_matrix_by_pp %>%
  group_by(
    correct_num,
    n_bars_adjusted,
    tilted_slot,
    response_location
  ) %>%
  summarise(
    n = sum(!is.na(orientation_error_mean)),
    n_trials = sum(n_trials),
    orientation_error_sd = sd(
      orientation_error_mean,
      na.rm = TRUE
    ),
    orientation_error_mean = mean(
      orientation_error_mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    orientation_error_SEM = orientation_error_sd / sqrt(n),
    orientation_error_CI =
      orientation_error_SEM * qt(0.975, n - 1)
  )


# How to read plots 12, 13, and 14:
# Diagonal cell (response location = target location):
#   positive = overshoot; zero = no mean error; negative = undershoot or an
#   opposite-direction response.
# Off-diagonal cell (response location != target location):
#   positive = non-target tilted in the target direction;
#   zero = non-target remained vertical;
#   negative = non-target tilted opposite to the target direction.
correct_orientation_matrix_caption <- paste(
  "Diagonal: positive = overshoot; negative = undershoot.",
  "Off-diagonal: positive = target-direction tilt,",
  "negative = opposite-direction tilt; zero = correct."
)


# ------------plot 12a: orientation-error matrix, set size 3---------------
data_orientation_error_matrix_set_size_3 <-
  data_orientation_error_matrix_across_pp %>%
  filter(correct_num == 3)


n_trials_plot_12a <- my_data %>%
  filter(
    number_deviation == 0,
    correct_num == 3
  ) %>%
  count(
    n_bars_adjusted,
    tilted_slot,
    name = "n_trials"
  )

n_trials_plot_12a


my_plot_orientation_error_matrix_set_size_3 <- ggplot(
  data_orientation_error_matrix_set_size_3,
  aes(
    x = response_location,
    y = tilted_slot,
    fill = orientation_error_mean
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = paste0(round(orientation_error_mean, 1), "°")
    ),
    size = 4,
    color = "black"
  ) +
  geom_text(
    aes(label = paste0("trials = ", n_trials)),
    nudge_x = 0.43,
    nudge_y = 0.43,
    hjust = 1,
    vjust = 0,
    size = 2.3,
    color = "black"
  ) +
  facet_wrap(
    ~ n_bars_adjusted,
    labeller = as_labeller(
      c(
        "1" = "1 bar adjusted",
        "2" = "2 bars adjusted",
        "3" = "3 bars adjusted"
      )
    )
  ) +
  scale_x_continuous(
    breaks = 1:3
  ) +
  scale_y_continuous(
    breaks = 1:3,
    trans = "reverse"
  ) +
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "white",
    high = "#D55E00",
    midpoint = 0,
    limits = matrix_color_limits,
    breaks = matrix_color_breaks,
    oob = scales::squish,
    name = "Errors (Degrees)"
  ) +
  coord_equal() +
  labs(
    title = "Orientation-Error Matrix: Set Size 3",
    subtitle = paste(
      "Correct number responses separated by bars adjusted;",
      "averaged across target magnitudes and directions"
    ),
    x = "Response Location (Inner to Outer)",
    y = "Target Location (Inner to Outer)",
    caption = correct_orientation_matrix_caption
  ) +
  my_plot_theme

my_plot_orientation_error_matrix_set_size_3

# ------------plot 12b: orientation-error matrix, set size 4---------------
data_orientation_error_matrix_set_size_4 <-
  data_orientation_error_matrix_across_pp %>%
  filter(correct_num == 4)


n_trials_plot_12b <- my_data %>%
  filter(
    number_deviation == 0,
    correct_num == 4
  ) %>%
  count(
    n_bars_adjusted,
    tilted_slot,
    name = "n_trials"
  )

n_trials_plot_12b


my_plot_orientation_error_matrix_set_size_4 <- ggplot(
  data_orientation_error_matrix_set_size_4,
  aes(
    x = response_location,
    y = tilted_slot,
    fill = orientation_error_mean
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = paste0(round(orientation_error_mean, 1), "°")
    ),
    size = 3.5,
    color = "black"
  ) +
  geom_text(
    aes(label = paste0("trials = ", n_trials)),
    nudge_x = 0.43,
    nudge_y = 0.43,
    hjust = 1,
    vjust = 0,
    size = 2.1,
    color = "black"
  ) +
  facet_wrap(
    ~ n_bars_adjusted,
    labeller = as_labeller(
      c(
        "1" = "1 bar adjusted",
        "2" = "2 bars adjusted",
        "3" = "3 bars adjusted",
        "4" = "4 bars adjusted"
      )
    )
  ) +
  scale_x_continuous(
    breaks = 1:4
  ) +
  scale_y_continuous(
    breaks = 1:4,
    trans = "reverse"
  ) +
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "white",
    high = "#D55E00",
    midpoint = 0,
    limits = matrix_color_limits,
    breaks = matrix_color_breaks,
    oob = scales::squish,
    name = "Errors (Degrees)"
  ) +
  coord_equal() +
  labs(
    title = "Orientation-Error Matrix: Set Size 4",
    subtitle = paste(
      "Correct number responses separated by bars adjusted;",
      "averaged across target magnitudes and directions"
    ),
    x = "Response Location (Inner to Outer)",
    y = "Target Location (Inner to Outer)",
    caption = correct_orientation_matrix_caption
  ) +
  my_plot_theme

my_plot_orientation_error_matrix_set_size_4


# ------------analysis 1: adjacent orientation-assimilation gain---------------
# This analysis uses all correct-number trials, regardless of how many bars were
# adjusted. Every final response location is retained, including locations whose
# final orientation is zero. Keeping those zero responses is important: excluding
# them would select only visibly assimilated responses and inflate the estimated
# spread of the target orientation.
#
# Orientation-transfer gain is the zero-intercept slope relating the final
# target-aligned orientation at a response location (y) to the magnitude of
# the presented target orientation (x):
#
#   gain = sum(x * y) / sum(x^2)
#
# At the target location, gain = 1 means exact reproduction of the target.
# At a non-target location, gain = 0 means that the line remained vertical,
# gain > 0 means assimilation in the target direction, gain = 1 means a full
# copy of the target orientation, and gain < 0 means tilt in the opposite
# direction. Thus, adjacent assimilation is tested directly by asking whether
# the participant-level adjacent gain is reliably greater than zero.


calculate_orientation_gain <- function(
    target_orientation_magnitude,
    aligned_response_orientation
) {
  complete_values <-
    is.finite(target_orientation_magnitude) &
    is.finite(aligned_response_orientation)

  target_orientation_magnitude <-
    target_orientation_magnitude[complete_values]

  aligned_response_orientation <-
    aligned_response_orientation[complete_values]

  denominator <- sum(target_orientation_magnitude^2)

  if (length(target_orientation_magnitude) == 0 || denominator == 0) {
    return(NA_real_)
  }

  sum(
    target_orientation_magnitude * aligned_response_orientation
  ) / denominator
}


data_orientation_gain <-
  data_orientation_error_by_adjustment_count %>%
  filter(
    !is.na(target_orientation_folded),
    target_orientation_folded != 0
  ) %>%
  mutate(
    target_orientation_magnitude = abs(
      target_orientation_folded
    ),
    relative_location = response_location - tilted_slot,
    location_class = case_when(
      relative_location == 0 ~ "target",
      relative_location == -1 ~ "adjacent_inward",
      relative_location == 1 ~ "adjacent_outward",
      TRUE ~ "other_non_target"
    )
  )


# Calculate gains separately for the two folded target directions first. This
# prevents a direction with more trials from dominating a participant's gain.
data_orientation_gain_by_pp_direction <-
  data_orientation_gain %>%
  group_by(
    SubID,
    location_class,
    tilt_direction_folded
  ) %>%
  summarise(
    n_observations = n(),
    orientation_gain = calculate_orientation_gain(
      target_orientation_magnitude,
      aligned_response_orientation
    ),
    .groups = "drop"
  )


# Give the toward and away target directions equal weight within participant.
data_orientation_gain_by_pp <-
  data_orientation_gain_by_pp_direction %>%
  group_by(SubID, location_class) %>%
  summarise(
    n_target_directions = sum(!is.na(orientation_gain)),
    orientation_gain = mean(
      orientation_gain,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Average inward- and outward-adjacent gains within each participant. Requiring
# both adjacent classes makes the participant-level contrast comparable across
# participants and avoids treating one spatial direction as the whole effect.
data_adjacent_orientation_gain_by_pp <-
  data_orientation_gain_by_pp %>%
  filter(
    location_class %in% c(
      "adjacent_inward",
      "adjacent_outward"
    )
  ) %>%
  group_by(SubID) %>%
  summarise(
    n_adjacent_location_classes = n_distinct(location_class),
    adjacent_orientation_gain = mean(
      orientation_gain,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  filter(n_adjacent_location_classes == 2)


# The participant is the inferential unit. A two-sided one-sample t test asks
# whether the mean adjacent gain differs from zero. The positive estimate and
# confidence interval establish that the difference is target-direction
# assimilation. Cohen's dz describes the standardized participant-level
# effect size.
test_adjacent_orientation_gain <- t.test(
  data_adjacent_orientation_gain_by_pp$adjacent_orientation_gain,
  mu = 0,
  alternative = "two.sided"
)


result_adjacent_orientation_gain <- data.frame(
  n_participants = nrow(data_adjacent_orientation_gain_by_pp),
  adjacent_gain_mean = mean(
    data_adjacent_orientation_gain_by_pp$adjacent_orientation_gain
  ),
  adjacent_gain_sd = sd(
    data_adjacent_orientation_gain_by_pp$adjacent_orientation_gain
  ),
  confidence_interval_low =
    test_adjacent_orientation_gain$conf.int[1],
  confidence_interval_high =
    test_adjacent_orientation_gain$conf.int[2],
  t_statistic = unname(test_adjacent_orientation_gain$statistic),
  degrees_freedom = unname(test_adjacent_orientation_gain$parameter),
  p_value = test_adjacent_orientation_gain$p.value
) %>%
  mutate(
    cohen_dz = adjacent_gain_mean / adjacent_gain_sd
  )


adjacent_gain_p_text <- if (
  result_adjacent_orientation_gain$p_value < 0.001
) {
  "< .001"
} else {
  paste0(
    "= ",
    sprintf(
      "%.3f",
      result_adjacent_orientation_gain$p_value
    )
  )
}


adjacent_orientation_assimilation_result <- paste0(
  "There was strong evidence for adjacent orientation assimilation: ",
  "mean gain = ",
  sprintf(
    "%.3f",
    result_adjacent_orientation_gain$adjacent_gain_mean
  ),
  ", 95% CI [",
  sprintf(
    "%.3f",
    result_adjacent_orientation_gain$confidence_interval_low
  ),
  ", ",
  sprintf(
    "%.3f",
    result_adjacent_orientation_gain$confidence_interval_high
  ),
  "], t(",
  result_adjacent_orientation_gain$degrees_freedom,
  ") = ",
  sprintf(
    "%.2f",
    result_adjacent_orientation_gain$t_statistic
  ),
  ", p ",
  adjacent_gain_p_text,
  ", Cohen's dz = ",
  sprintf(
    "%.2f",
    result_adjacent_orientation_gain$cohen_dz
  ),
  "."
)

print(result_adjacent_orientation_gain)
cat(adjacent_orientation_assimilation_result, "\n")


# Summarize all spatial classes for the gain plot. The adjacent-gain test above
# remains the primary inferential test; the other classes provide context.
orientation_gain_location_levels <- c(
  "target",
  "adjacent_inward",
  "adjacent_outward",
  "other_non_target"
)


orientation_gain_location_labels <- c(
  target = "Target",
  adjacent_inward = "Adjacent Inward",
  adjacent_outward = "Adjacent Outward",
  other_non_target = "Other Non-Target"
)


data_orientation_gain_across_pp <-
  data_orientation_gain_by_pp %>%
  group_by(location_class) %>%
  summarise(
    n = sum(!is.na(orientation_gain)),
    orientation_gain_mean = mean(
      orientation_gain,
      na.rm = TRUE
    ),
    orientation_gain_sd = sd(
      orientation_gain,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    orientation_gain_SEM = orientation_gain_sd / sqrt(n),
    orientation_gain_CI =
      orientation_gain_SEM * qt(0.975, n - 1),
    location_class = factor(
      location_class,
      levels = orientation_gain_location_levels,
      labels = unname(
        orientation_gain_location_labels[
          orientation_gain_location_levels
        ]
      )
    )
  )


data_orientation_gain_plot <-
  data_orientation_gain_by_pp %>%
  mutate(
    location_class = factor(
      location_class,
      levels = orientation_gain_location_levels,
      labels = unname(
        orientation_gain_location_labels[
          orientation_gain_location_levels
        ]
      )
    )
  )




my_plot_orientation_transfer_gain <- ggplot(
  data_orientation_gain_plot,
  aes(
    x = location_class,
    y = orientation_gain
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    color = "black"
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "grey"
  ) +
  geom_jitter(
    width = 0.08,
    height = 0,
    size = 2,
    alpha = 0.55,
    color = "#707070"
  ) +
  geom_errorbar(
    data = data_orientation_gain_across_pp,
    aes(
      x = location_class,
      y = orientation_gain_mean,
      ymin = orientation_gain_mean - orientation_gain_CI,
      ymax = orientation_gain_mean + orientation_gain_CI
    ),
    inherit.aes = FALSE,
    width = 0.0,
    linewidth = 0.5,
    color = "black"
  ) +
  geom_point(
    data = data_orientation_gain_across_pp,
    aes(
      x = location_class,
      y = orientation_gain_mean
    ),
    inherit.aes = FALSE,
    size = 4,
    color = "black"
  ) +
  labs(x = "Location Relative to Target",
       y = "Orientation Assimilation"
  ) +
  
  scale_y_continuous(limits = c(-2, 2)) +
  
  my_plot_theme


my_plot_orientation_transfer_gain


# ------------correct-trial gains separately by target location---------------
# Estimate gain within each set size and presented target position so target-
# location heterogeneity is visible rather than averaged into one overall mean.
correct_target_location_labels <- c(
  target = "Target",
  adjacent_inward = "Adj.\nInward",
  adjacent_outward = "Adj.\nOutward",
  other_non_target = "Other\nNon-Target"
)


data_correct_gain_by_target_direction <-
  data_orientation_gain %>%
  group_by(
    SubID,
    correct_num,
    tilted_slot,
    location_class,
    tilt_direction_folded
  ) %>%
  summarise(
    n_observations = n(),
    orientation_gain = calculate_orientation_gain(
      target_orientation_magnitude,
      aligned_response_orientation
    ),
    .groups = "drop"
  )


data_correct_gain_by_target_by_pp <-
  data_correct_gain_by_target_direction %>%
  group_by(
    SubID,
    correct_num,
    tilted_slot,
    location_class
  ) %>%
  summarise(
    n_target_directions = sum(
      is.finite(orientation_gain)
    ),
    orientation_gain = mean(
      orientation_gain,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  filter(is.finite(orientation_gain))


# Show the contributing participant count in every panel because correct-number
# trials were not available for every participant in every target condition.
data_correct_gain_target_condition_counts <-
  data_correct_gain_by_target_by_pp %>%
  distinct(
    SubID,
    correct_num,
    tilted_slot
  ) %>%
  count(
    correct_num,
    tilted_slot,
    name = "n_participants_condition"
  ) %>%
  arrange(
    correct_num,
    tilted_slot
  ) %>%
  mutate(
    target_condition_label = paste0(
      "Set Size ",
      correct_num,
      ": Target P",
      tilted_slot,
      "\n",
      "n = ",
      n_participants_condition
    )
  )


correct_target_condition_label_levels <-
  data_correct_gain_target_condition_counts %>%
  pull(target_condition_label)


data_correct_gain_by_target_across_pp <-
  data_correct_gain_by_target_by_pp %>%
  group_by(
    correct_num,
    tilted_slot,
    location_class
  ) %>%
  summarise(
    n = sum(!is.na(orientation_gain)),
    orientation_gain_mean = mean(
      orientation_gain,
      na.rm = TRUE
    ),
    orientation_gain_sd = sd(
      orientation_gain,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    orientation_gain_SEM =
      orientation_gain_sd / sqrt(n),
    orientation_gain_CI =
      orientation_gain_SEM * qt(0.975, n - 1)
  ) %>%
  left_join(
    data_correct_gain_target_condition_counts,
    by = c(
      "correct_num",
      "tilted_slot"
    )
  ) %>%
  mutate(
    location_class = factor(
      location_class,
      levels = orientation_gain_location_levels,
      labels = unname(
        correct_target_location_labels[
          orientation_gain_location_levels
        ]
      )
    ),
    target_condition_label = factor(
      target_condition_label,
      levels = correct_target_condition_label_levels
    )
  )


data_correct_gain_by_target_plot <-
  data_correct_gain_by_target_by_pp %>%
  left_join(
    data_correct_gain_target_condition_counts,
    by = c(
      "correct_num",
      "tilted_slot"
    )
  ) %>%
  mutate(
    location_class = factor(
      location_class,
      levels = orientation_gain_location_levels,
      labels = unname(
        correct_target_location_labels[
          orientation_gain_location_levels
        ]
      )
    ),
    target_condition_label = factor(
      target_condition_label,
      levels = correct_target_condition_label_levels
    )
  )


my_plot_correct_orientation_gain_by_target_location <- ggplot(
  data_correct_gain_by_target_plot,
  aes(
    x = location_class,
    y = orientation_gain
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    color = "black"
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "grey"
  ) +
  geom_jitter(
    width = 0.08,
    height = 0,
    size = 1.8,
    alpha = 0.5,
    color = "#707070"
  ) +
  geom_errorbar(
    data = data_correct_gain_by_target_across_pp,
    aes(
      x = location_class,
      y = orientation_gain_mean,
      ymin = orientation_gain_mean - orientation_gain_CI,
      ymax = orientation_gain_mean + orientation_gain_CI
    ),
    inherit.aes = FALSE,
    width = 0.0,
    linewidth = 0.5,
    color = "black"
  ) +
  geom_point(
    data = data_correct_gain_by_target_across_pp,
    aes(
      x = location_class,
      y = orientation_gain_mean
    ),
    inherit.aes = FALSE,
    size = 3.5,
    color = "black"
  ) +
  facet_wrap(
    ~ target_condition_label,
    ncol = 4,
    scales = "free_x"
  ) +
  scale_y_continuous(limits = c(-2, 2)) +
  scale_x_discrete(drop = TRUE) +
  labs(
    title = "Correct-Trial Orientation Gain by Target Location",
    x = "Location Relative to Target",
    y = "Orientation Gain/Assimilation") +
  my_plot_theme +
  theme(
    axis.text.x = element_text(
      size = 8,
      face = "bold"
    )
  )


my_plot_correct_orientation_gain_by_target_location


# Test every plotted gain against its theoretical reference. Target gain is
# tested against 1; all non-target gains are tested against 0. Holm correction
# controls the complete family of 23 target-location/category tests.
result_correct_gain_by_target_reference_tests <-
  data_correct_gain_by_target_by_pp %>%
  group_by(
    correct_num,
    tilted_slot,
    location_class
  ) %>%
  group_modify(
    ~ {
      gain_values <- .x$orientation_gain
      reference_value <- if (
        .y$location_class[[1]] == "target"
      ) {
        1
      } else {
        0
      }

      test_result <- t.test(
        gain_values,
        mu = reference_value,
        alternative = "two.sided"
      )

      data.frame(
        reference_value = reference_value,
        n_participants = length(gain_values),
        orientation_gain_mean = mean(gain_values),
        orientation_gain_sd = sd(gain_values),
        mean_difference_from_reference =
          mean(gain_values) - reference_value,
        confidence_interval_low = test_result$conf.int[1],
        confidence_interval_high = test_result$conf.int[2],
        t_statistic = unname(test_result$statistic),
        degrees_freedom = unname(test_result$parameter),
        p_value = test_result$p.value,
        cohen_dz_vs_reference =
          (mean(gain_values) - reference_value) /
            sd(gain_values),
        stringsAsFactors = FALSE
      )
    }
  ) %>%
  ungroup() %>%
  mutate(
    p_value_holm_23 = p.adjust(
      p_value,
      method = "holm"
    ),
    significance = case_when(
      p_value_holm_23 < 0.001 ~ "***",
      p_value_holm_23 < 0.01 ~ "**",
      p_value_holm_23 < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    target_condition = paste0(
      "Set Size ",
      correct_num,
      ", Target P",
      tilted_slot
    ),
    location_category = recode(
      location_class,
      target = "Target",
      adjacent_inward = "Adjacent Inward",
      adjacent_outward = "Adjacent Outward",
      other_non_target = "Other Non-Target"
    )
  ) %>%
  arrange(
    correct_num,
    tilted_slot,
    match(
      location_class,
      orientation_gain_location_levels
    )
  ) %>%
  select(
    target_condition,
    location_category,
    reference_value,
    n_participants,
    orientation_gain_mean,
    orientation_gain_sd,
    mean_difference_from_reference,
    confidence_interval_low,
    confidence_interval_high,
    t_statistic,
    degrees_freedom,
    p_value,
    p_value_holm_23,
    significance,
    cohen_dz_vs_reference,
    correct_num,
    tilted_slot,
    location_class
  )


print(result_correct_gain_by_target_reference_tests)


# Current-data interpretation using the full 23-test family:
# Middle targets show attenuated target gain, especially set-size-3 P2 and
# set-size-4 P2/P3. Reliable non-target transfer appears at set-size-3 P2/P3,
# set-size-4 P1/P2/P4, and at the farther inward locations for set-size-4 P4.
# Thus, the combined correct-trial plot hides target-position heterogeneity.


# ------------reference tests for orientation-transfer gains---------------
# Every participant contributes one gain to each of the four location classes.
# The question here is not whether the four means differ generally. Instead,
# each test asks whether a location contains its theoretically expected amount
# of target-orientation information. The participant is the inferential unit.


orientation_gain_condition_names <- c(
  "target",
  "adjacent_inward",
  "adjacent_outward",
  "other_non_target"
)


data_orientation_gain_wide <-
  data_orientation_gain_by_pp %>%
  select(
    SubID,
    location_class,
    orientation_gain
  ) %>%
  tidyr::pivot_wider(
    names_from = location_class,
    values_from = orientation_gain
  ) %>%
  filter(
    if_all(
      all_of(orientation_gain_condition_names),
      is.finite
    )
  )


# Convert adjusted p-values to symbols for later plot annotation. The full
# adjusted p-values remain in the result tables.
get_significance_symbol <- function(p_value) {
  case_when(
    is.na(p_value) ~ NA_character_,
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  )
}


# Reference-value tests: target gain is tested against 1 (perfect target
# preservation); the three non-target gains are tested against 0 (no transfer).
# Holm correction controls family-wise error across these four tests.
orientation_gain_reference_values <- c(
  target = 1,
  adjacent_inward = 0,
  adjacent_outward = 0,
  other_non_target = 0
)


run_orientation_gain_reference_test <- function(condition_name) {
  gain_values <- data_orientation_gain_wide[[condition_name]]
  reference_value <-
    orientation_gain_reference_values[[condition_name]]

  test_result <- t.test(
    gain_values,
    mu = reference_value,
    alternative = "two.sided"
  )

  data.frame(
    condition = condition_name,
    condition_label = gsub(
      "\n",
      " ",
      orientation_gain_location_labels[[condition_name]]
    ),
    n_participants = length(gain_values),
    mean_gain = mean(gain_values),
    gain_sd = sd(gain_values),
    reference_value = reference_value,
    mean_difference = mean(gain_values) - reference_value,
    cohen_dz =
      (mean(gain_values) - reference_value) /
      sd(gain_values),
    confidence_interval_low = test_result$conf.int[1],
    confidence_interval_high = test_result$conf.int[2],
    t_statistic = unname(test_result$statistic),
    degrees_freedom = unname(test_result$parameter),
    p_value = test_result$p.value,
    stringsAsFactors = FALSE
  )
}


result_orientation_gain_reference_tests <-
  bind_rows(
    lapply(
      orientation_gain_condition_names,
      run_orientation_gain_reference_test
    )
  ) %>%
  mutate(
    p_value_holm = p.adjust(
      p_value,
      method = "holm"
    ),
    significance = get_significance_symbol(
      p_value_holm
    )
  )


cat(
  paste(
    "Orientation-gain reference tests",
    "(two-sided one-sample t tests; Holm correction across four tests):"
  ),
  "\n"
)


print(result_orientation_gain_reference_tests)


# ------------adjacent inward versus adjacent outward gain---------------
# Both gains come from the same participants, so the analysis uses a paired
# t test. The difference is defined as inward minus outward: a positive value
# means stronger orientation transfer toward fixation.
data_adjacent_inward_outward_difference <-
  data_orientation_gain_wide %>%
  transmute(
    SubID,
    adjacent_inward_gain = adjacent_inward,
    adjacent_outward_gain = adjacent_outward,
    inward_minus_outward =
      adjacent_inward - adjacent_outward
  )


test_adjacent_inward_vs_outward <- t.test(
  data_adjacent_inward_outward_difference$adjacent_inward_gain,
  data_adjacent_inward_outward_difference$adjacent_outward_gain,
  paired = TRUE,
  alternative = "two.sided"
)


result_adjacent_inward_vs_outward <- data.frame(
  n_participants =
    nrow(data_adjacent_inward_outward_difference),
  adjacent_inward_gain_mean = mean(
    data_adjacent_inward_outward_difference$adjacent_inward_gain
  ),
  adjacent_outward_gain_mean = mean(
    data_adjacent_inward_outward_difference$adjacent_outward_gain
  ),
  mean_difference_inward_minus_outward = mean(
    data_adjacent_inward_outward_difference$inward_minus_outward
  ),
  difference_sd = sd(
    data_adjacent_inward_outward_difference$inward_minus_outward
  ),
  confidence_interval_low =
    test_adjacent_inward_vs_outward$conf.int[1],
  confidence_interval_high =
    test_adjacent_inward_vs_outward$conf.int[2],
  t_statistic = unname(
    test_adjacent_inward_vs_outward$statistic
  ),
  degrees_freedom = unname(
    test_adjacent_inward_vs_outward$parameter
  ),
  p_value = test_adjacent_inward_vs_outward$p.value,
  participants_inward_greater = sum(
    data_adjacent_inward_outward_difference$inward_minus_outward > 0
  )
) %>%
  mutate(
    cohen_dz =
      mean_difference_inward_minus_outward /
      difference_sd
  )


adjacent_inward_outward_p_text <- if (
  result_adjacent_inward_vs_outward$p_value < 0.001
) {
  "< .001"
} else {
  paste0(
    "= ",
    sprintf(
      "%.3f",
      result_adjacent_inward_vs_outward$p_value
    )
  )
}


adjacent_inward_outward_statement <- paste0(
  "Assimilation was stronger inward than outward, ",
  "mean difference = ",
  sprintf(
    "%.3f",
    result_adjacent_inward_vs_outward$mean_difference_inward_minus_outward
  ),
  ", 95% CI [",
  sprintf(
    "%.3f",
    result_adjacent_inward_vs_outward$confidence_interval_low
  ),
  ", ",
  sprintf(
    "%.3f",
    result_adjacent_inward_vs_outward$confidence_interval_high
  ),
  "], t(",
  result_adjacent_inward_vs_outward$degrees_freedom,
  ") = ",
  sprintf(
    "%.2f",
    result_adjacent_inward_vs_outward$t_statistic
  ),
  ", p ",
  adjacent_inward_outward_p_text,
  ", Cohen's dz = ",
  sprintf(
    "%.2f",
    result_adjacent_inward_vs_outward$cohen_dz
  ),
  "."
)


print(result_adjacent_inward_vs_outward)
cat(adjacent_inward_outward_statement, "\n")



# # ------------all correct-number trials for orientation matrices---------------
# # These data include trials where 1, 2, 3, or 4 bars were adjusted. Each row
# # below represents one final response location on one trial.
# data_correct_number_orientation <- my_data %>%
#   filter(number_deviation == 0) %>%
#   mutate(
#     response_orientation_folded = lapply(
#       adjusted_ori_inner_to_outer_folded,
#       parse_number_list
#     ),
#     response_location = lapply(
#       response_orientation_folded,
#       seq_along
#     )
#   ) %>%
#   tidyr::unnest(
#     c(response_location, response_orientation_folded)
#   ) %>%
#   mutate(
#     aligned_response_orientation =
#       response_orientation_folded *
#       sign(target_orientation_folded),
#     expected_orientation = if_else(
#       response_location == tilted_slot,
#       abs(target_orientation_folded),
#       0
#     ),
#     orientation_error =
#       aligned_response_orientation - expected_orientation
#   )


# Hypothetical reported-correct matrices were moved to:
# src/analysis/hypothetical_orientation_matrices_reported_correct.R

# # Calculate the observed matrix using all correct-number trials. Adjustment
# # count is deliberately combined here to show the overall final orientation
# # pattern; plot 12 keeps multiple-adjustment strategies separate.
# data_all_correct_orientation_error_by_pp <-
#   data_correct_number_orientation %>%
#   group_by(
#     SubID,
#     correct_num,
#     tilted_slot,
#     response_location
#   ) %>%
#   summarise(
#     n_trials = sum(!is.na(orientation_error)),
#     orientation_error_mean = mean(
#       orientation_error,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   )
# 
# 
# data_all_correct_orientation_error_across_pp <-
#   data_all_correct_orientation_error_by_pp %>%
#   group_by(
#     correct_num,
#     tilted_slot,
#     response_location
#   ) %>%
#   summarise(
#     n = sum(!is.na(orientation_error_mean)),
#     n_trials = sum(n_trials),
#     orientation_error_sd = sd(
#       orientation_error_mean,
#       na.rm = TRUE
#     ),
#     orientation_error_mean = mean(
#       orientation_error_mean,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     orientation_error_SEM = orientation_error_sd / sqrt(n),
#     orientation_error_CI =
#       orientation_error_SEM * qt(0.975, n - 1)
#   )


# # ------------plot 14a: observed matrix, all correct trials, set size 3---------------
# data_all_correct_orientation_error_set_size_3 <-
#   data_all_correct_orientation_error_across_pp %>%
#   filter(correct_num == 3)
# 
# 
# n_trials_plot_14a <- my_data %>%
#   filter(
#     number_deviation == 0,
#     correct_num == 3
#   ) %>%
#   count(n_bars_adjusted, name = "n_trials")
# 
# n_trials_plot_14a

# 
# my_plot_all_correct_orientation_error_set_size_3 <- ggplot(
#   data_all_correct_orientation_error_set_size_3,
#   aes(
#     x = response_location,
#     y = tilted_slot,
#     fill = orientation_error_mean
#   )
# ) +
#   geom_tile(
#     color = "white",
#     linewidth = 1
#   ) +
#   geom_text(
#     aes(
#       label = paste0(round(orientation_error_mean, 1), "°")
#     ),
#     size = 4,
#     color = "black"
#   ) +
#   geom_text(
#     aes(label = paste0("trials = ", n_trials)),
#     nudge_x = 0.43,
#     nudge_y = 0.43,
#     hjust = 1,
#     vjust = 0,
#     size = 2.3,
#     color = "black"
#   ) +
#   scale_x_continuous(breaks = 1:3) +
#   scale_y_continuous(
#     breaks = 1:3,
#     trans = "reverse"
#   ) +
#   scale_fill_gradient2(
#     low = "#0072B2",
#     mid = "white",
#     high = "#D55E00",
#     midpoint = 0,
#     limits = matrix_color_limits,
#     breaks = matrix_color_breaks,
#     oob = scales::squish,
#     name = "Errors (Degrees)"
#   ) +
#   coord_equal() +
#   labs(
#     title = "Observed Orientation-Error Matrix: Set Size 3",
#     subtitle = paste(
#       "All correct-number trials combined across",
#       "1, 2, and 3 bars adjusted"
#     ),
#     x = "Response Location (Inner to Outer)",
#     y = "Target Location (Inner to Outer)"
#   ) +
#   my_plot_theme
# 
# # ------------plot 14b: observed matrix, all correct trials, set size 4---------------
# data_all_correct_orientation_error_set_size_4 <-
#   data_all_correct_orientation_error_across_pp %>%
#   filter(correct_num == 4)
# 
# 
# n_trials_plot_14b <- my_data %>%
#   filter(
#     number_deviation == 0,
#     correct_num == 4
#   ) %>%
#   count(n_bars_adjusted, name = "n_trials")
# 
# n_trials_plot_14b
# 
# 
# my_plot_all_correct_orientation_error_set_size_4 <- ggplot(
#   data_all_correct_orientation_error_set_size_4,
#   aes(
#     x = response_location,
#     y = tilted_slot,
#     fill = orientation_error_mean
#   )
# ) +
#   geom_tile(
#     color = "white",
#     linewidth = 1
#   ) +
#   geom_text(
#     aes(
#       label = paste0(round(orientation_error_mean, 1), "°")
#     ),
#     size = 3.5,
#     color = "black"
#   ) +
#   geom_text(
#     aes(label = paste0("trials = ", n_trials)),
#     nudge_x = 0.43,
#     nudge_y = 0.43,
#     hjust = 1,
#     vjust = 0,
#     size = 2.1,
#     color = "black"
#   ) +
#   scale_x_continuous(breaks = 1:4) +
#   scale_y_continuous(
#     breaks = 1:4,
#     trans = "reverse"
#   ) +
#   scale_fill_gradient2(
#     low = "#0072B2",
#     mid = "white",
#     high = "#D55E00",
#     midpoint = 0,
#     limits = matrix_color_limits,
#     breaks = matrix_color_breaks,
#     oob = scales::squish,
#     name = "Errors (Degrees)"
#   ) +
#   coord_equal() +
#   labs(
#     title = "Observed Orientation-Error Matrix: Set Size 4",
#     subtitle = paste(
#       "All correct-number trials combined across",
#       "1, 2, 3, and 4 bars adjusted"
#     ),
#     x = "Response Location (Inner to Outer)",
#     y = "Target Location (Inner to Outer)"
#   ) +
#   my_plot_theme
# 
# 
# # Display the set-size-3 and set-size-4 observed matrices in one figure. Free
# # facet scales keep each matrix fitted to its available locations.
# my_plot_all_correct_orientation_error <- ggplot(
#   data_all_correct_orientation_error_across_pp,
#   aes(
#     x = response_location,
#     y = tilted_slot,
#     fill = orientation_error_mean
#   )
# ) +
#   geom_tile(
#     color = "white",
#     linewidth = 1
#   ) +
#   geom_text(
#     aes(
#       label = paste0(round(orientation_error_mean, 1), "°")
#     ),
#     size = 3.5,
#     color = "black"
#   ) +
#   geom_text(
#     aes(label = paste0("trials = ", n_trials)),
#     nudge_x = 0.43,
#     nudge_y = 0.43,
#     hjust = 1,
#     vjust = 0,
#     size = 2.1,
#     color = "black"
#   ) +
#   facet_wrap(
#     ~ correct_num,
#     nrow = 1,
#     scales = "free",
#     labeller = as_labeller(
#       c(
#         "3" = "Set Size 3",
#         "4" = "Set Size 4"
#       )
#     )
#   ) +
#   scale_x_continuous(breaks = 1:4) +
#   scale_y_continuous(
#     breaks = 1:4,
#     trans = "reverse"
#   ) +
#   scale_fill_gradient2(
#     low = "#0072B2",
#     mid = "white",
#     high = "#D55E00",
#     midpoint = 0,
#     limits = matrix_color_limits,
#     breaks = matrix_color_breaks,
#     oob = scales::squish,
#     name = "Errors (Degrees)"
#   ) +
#   labs(
#     title = "Observed Orientation-Error Matrices",
#     subtitle = paste(
#       "Correct-number trials; all adjustment counts combined"
#     ),
#     x = "Response Location (Inner to Outer)",
#     y = "Target Location (Inner to Outer)"
#   ) +
#   my_plot_theme
# 
# my_plot_all_correct_orientation_error



# ------------data for RM response-location selection matrix---------------
# RM trials contain one fewer response bar than presented lines. Therefore,
# the response locations are defined within the smaller response array. This
# is an ordinal mapping, not a correct-versus-incorrect location analysis.
data_RM_location <- my_data %>%
  filter(number_deviation == -1) %>%
  mutate(
    adjusted_index_LtoR = lapply(
      adjusted_indices,
      parse_number_list
    )
  ) %>%
  tidyr::unnest_longer(
    adjusted_index_LtoR
  ) %>%
  mutate(
    adjusted_location = if_else(
      hemifield == "left",
      reported_number - adjusted_index_LtoR,
      adjusted_index_LtoR + 1
    )
  ) %>%
  filter(
    !is.na(adjusted_location),
    adjusted_location >= 1,
    adjusted_location <= reported_number
  )


# The denominator is the number of RM trials before adjusted_indices is
# expanded. Within each target location and bars-adjusted panel, a cell is the
# percentage of trials where that response location was adjusted.
data_RM_location_trials_by_pp <- my_data %>%
  filter(number_deviation == -1) %>%
  count(
    SubID,
    correct_num,
    reported_number,
    n_bars_adjusted,
    tilted_slot,
    name = "n_trials"
  )


data_RM_location_matrix_by_pp <- data_RM_location %>%
  count(
    SubID,
    correct_num,
    reported_number,
    n_bars_adjusted,
    tilted_slot,
    adjusted_location,
    name = "n_trials_location_selected"
  ) %>%
  group_by(
    SubID,
    correct_num,
    reported_number,
    n_bars_adjusted,
    tilted_slot
  ) %>%
  tidyr::complete(
    adjusted_location = 1:3,
    fill = list(n_trials_location_selected = 0)
  ) %>%
  ungroup() %>%
  filter(adjusted_location <= reported_number) %>%
  left_join(
    data_RM_location_trials_by_pp,
    by = c(
      "SubID",
      "correct_num",
      "reported_number",
      "n_bars_adjusted",
      "tilted_slot"
    )
  ) %>%
  mutate(
    percentage_trials_location_selected =
      n_trials_location_selected / n_trials * 100
  )


data_RM_location_matrix_across_pp <-
  data_RM_location_matrix_by_pp %>%
  group_by(
    correct_num,
    reported_number,
    n_bars_adjusted,
    tilted_slot,
    adjusted_location
  ) %>%
  summarise(
    n_participants = n(),
    percentage_trials_mean = mean(
      percentage_trials_location_selected,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ------------plot 15a: RM response-location matrix, set size 3---------------
data_RM_location_matrix_set_size_3 <-
  data_RM_location_matrix_across_pp %>%
  filter(correct_num == 3)


n_trials_plot_15a <- my_data %>%
  filter(
    number_deviation == -1,
    correct_num == 3
  ) %>%
  count(
    n_bars_adjusted,
    tilted_slot,
    name = "n_trials"
  )

n_trials_plot_15a


my_plot_RM_response_location_matrix_set_size_3 <- ggplot(
  data_RM_location_matrix_set_size_3,
  aes(
    x = adjusted_location,
    y = tilted_slot,
    fill = percentage_trials_mean
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = paste0(round(percentage_trials_mean, 1), "%")
    ),
    size = 4,
    color = "black"
  ) +
  facet_wrap(
    ~ n_bars_adjusted,
    labeller = as_labeller(
      c(
        "1" = "1 Bar Adjusted",
        "2" = "2 Bars Adjusted"
      )
    )
  ) +
  scale_x_continuous(breaks = 1:2) +
  scale_y_continuous(
    breaks = 1:3,
    trans = "reverse"
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#0072B2",
    limits = c(0, 100),
    name = "Trials"
  ) +
  coord_equal() +
  labs(
    title = "RM Response-Location Selection Matrix: Set Size 3",
    subtitle = paste(
      "Presented 3 lines; reported 2 lines;",
      "cell = percentage of trials where the response location was adjusted"
    ),
    x = "Response Location (Inner to Outer)",
    y = "Presented Target Location (Inner to Outer)"
  ) +
  my_plot_theme


my_plot_RM_response_location_matrix_set_size_3


# ------------plot 15b: RM response-location matrix, set size 4---------------
data_RM_location_matrix_set_size_4 <-
  data_RM_location_matrix_across_pp %>%
  filter(correct_num == 4)


n_trials_plot_15b <- my_data %>%
  filter(
    number_deviation == -1,
    correct_num == 4
  ) %>%
  count(
    n_bars_adjusted,
    tilted_slot,
    name = "n_trials"
  )

n_trials_plot_15b


my_plot_RM_response_location_matrix_set_size_4 <- ggplot(
  data_RM_location_matrix_set_size_4,
  aes(
    x = adjusted_location,
    y = tilted_slot,
    fill = percentage_trials_mean
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = paste0(round(percentage_trials_mean, 1), "%")
    ),
    size = 4,
    color = "black"
  ) +
  facet_wrap(
    ~ n_bars_adjusted,
    labeller = as_labeller(
      c(
        "1" = "1 Bar Adjusted",
        "2" = "2 Bars Adjusted",
        "3" = "3 Bars Adjusted"
      )
    )
  ) +
  scale_x_continuous(breaks = 1:3) +
  scale_y_continuous(
    breaks = 1:4,
    trans = "reverse"
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#0072B2",
    limits = c(0, 100),
    name = "Trials"
  ) +
  coord_equal() +
  labs(
    title = "RM Response-Location Selection Matrix: Set Size 4",
    subtitle = paste(
      "Presented 4 lines; reported 3 lines;",
      "cell = percentage of trials where the response location was adjusted"
    ),
    x = "Response Location (Inner to Outer)",
    y = "Presented Target Location (Inner to Outer)"
  ) +
  my_plot_theme



my_plot_RM_response_location_matrix_set_size_4


# ------------data for RM ordinal location offset---------------
# To match plot 6, keep only trials where one response bar was adjusted.
# The offset compares inner-to-outer ranks across arrays of different sizes:
# response location - presented target location.
# It describes inward/outward response placement, not localization accuracy.
data_RM_location_error <- data_RM_location %>%
  filter(n_bars_adjusted == 1) %>%
  mutate(
    location_offset = adjusted_location - tilted_slot
  )


data_RM_location_error_by_pp <- data_RM_location_error %>%
  count(
    SubID,
    correct_num,
    reported_number,
    tilted_slot,
    location_offset,
    name = "n_trials"
  ) %>%
  group_by(
    SubID,
    correct_num,
    reported_number,
    tilted_slot
  ) %>%
  tidyr::complete(
    location_offset = -3:2,
    fill = list(n_trials = 0)
  ) %>%
  filter(
    location_offset >= 1 - tilted_slot,
    location_offset <= reported_number - tilted_slot
  ) %>%
  mutate(
    percentage_trials = n_trials / sum(n_trials) * 100
  ) %>%
  ungroup()


data_RM_location_error_across_pp <-
  data_RM_location_error_by_pp %>%
  group_by(
    correct_num,
    reported_number,
    tilted_slot,
    location_offset
  ) %>%
  summarise(
    n = n(),
    percentage_trials_mean = mean(percentage_trials),
    percentage_trials_sd = sd(percentage_trials),
    .groups = "drop"
  ) %>%
  mutate(
    percentage_trials_SEM = percentage_trials_sd / sqrt(n),
    percentage_trials_CI =
      percentage_trials_SEM * qt(0.975, n - 1)
  )


# -1 = response was one ordinal position closer to fixation than the target.
#  0 = response and target had the same inner-to-outer rank.
# +1 = response was one ordinal position farther from fixation than the target.
# Zero is impossible for an outermost target because the RM response array has
# one fewer location than the presented array.


# ------------plot 16a: RM ordinal location offset, set size 3---------------
data_RM_location_error_set_size_3 <-
  data_RM_location_error_across_pp %>%
  filter(correct_num == 3)


n_trials_plot_16a <- data_RM_location_error %>%
  filter(correct_num == 3) %>%
  count(
    tilted_slot,
    name = "n_trials"
  )

n_trials_plot_16a


my_plot_RM_location_error_set_size_3 <- ggplot(
  data_RM_location_error_set_size_3,
  aes(
    x = factor(location_offset),
    y = percentage_trials_mean
  )
) +
  geom_col(
    width = 0.65,
    fill = "#707070",
    alpha = 0.75
  ) +
  geom_errorbar(
    aes(
      ymin = percentage_trials_mean - percentage_trials_CI,
      ymax = percentage_trials_mean + percentage_trials_CI
    ),
    width = 0.12,
    linewidth = 0.5
  ) +
  facet_wrap(
    ~ tilted_slot,
    scales = "free_x",
    ncol = 3,
    labeller = labeller(
      tilted_slot = function(x) paste("Target location", x)
    )
  ) +
  labs(
    title = "RM Ordinal Location Offset: Set Size 3",
    subtitle = paste(
      "One-bar-adjusted trials; offset = response location",
      "minus presented target location"
    ),
    x = "Ordinal Location Offset (Negative = Inward; Positive = Outward)",
    y = "Percentage of Trials"
  ) +
  my_plot_theme


my_plot_RM_location_error_set_size_3


# ------------plot 16b: RM ordinal location offset, set size 4---------------
data_RM_location_error_set_size_4 <-
  data_RM_location_error_across_pp %>%
  filter(correct_num == 4)


n_trials_plot_16b <- data_RM_location_error %>%
  filter(correct_num == 4) %>%
  count(
    tilted_slot,
    name = "n_trials"
  )

n_trials_plot_16b


my_plot_RM_location_error_set_size_4 <- ggplot(
  data_RM_location_error_set_size_4,
  aes(
    x = factor(location_offset),
    y = percentage_trials_mean
  )
) +
  geom_col(
    width = 0.65,
    fill = "#707070",
    alpha = 0.75
  ) +
  geom_errorbar(
    aes(
      ymin = percentage_trials_mean - percentage_trials_CI,
      ymax = percentage_trials_mean + percentage_trials_CI
    ),
    width = 0.12,
    linewidth = 0.5
  ) +
  facet_wrap(
    ~ tilted_slot,
    scales = "free_x",
    ncol = 4,
    labeller = labeller(
      tilted_slot = function(x) paste("Target location", x)
    )
  ) +
  labs(
    title = "RM Ordinal Location Offset: Set Size 4",
    subtitle = paste(
      "One-bar-adjusted trials; offset = response location",
      "minus presented target location"
    ),
    x = "Ordinal Location Offset (Negative = Inward; Positive = Outward)",
    y = "Percentage of Trials"
  ) +
  my_plot_theme


my_plot_RM_location_error_set_size_4


# ------------data for RM target-aligned orientation matrices---------------
# Keep every final response bar, including response bars that remained
# vertical. Positive values point in the target direction, zero is vertical,
# and negative values point in the direction opposite to the target.
data_RM_response_orientation <- my_data %>%
  filter(number_deviation == -1) %>%
  mutate(
    response_orientation_folded = lapply(
      adjusted_ori_inner_to_outer_folded,
      parse_number_list
    ),
    response_location = lapply(
      response_orientation_folded,
      seq_along
    )
  ) %>%
  tidyr::unnest(
    c(response_location, response_orientation_folded)
  ) %>%
  mutate(
    aligned_response_orientation =
      response_orientation_folded *
      sign(target_orientation_folded)
  ) %>%
  filter(
    !is.na(response_orientation_folded),
    !is.na(target_orientation_folded),
    target_orientation_folded != 0
  )


# Calculate the mean orientation within each participant first and then take
# the group mean across participants. Target magnitudes and directions are
# combined after aligning every response to its target direction.
data_RM_response_orientation_by_pp <-
  data_RM_response_orientation %>%
  group_by(
    SubID,
    correct_num,
    reported_number,
    n_bars_adjusted,
    tilted_slot,
    response_location
  ) %>%
  summarise(
    n_trials = sum(!is.na(aligned_response_orientation)),
    aligned_response_orientation_mean = mean(
      aligned_response_orientation,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


data_RM_response_orientation_across_pp <-
  data_RM_response_orientation_by_pp %>%
  group_by(
    correct_num,
    reported_number,
    n_bars_adjusted,
    tilted_slot,
    response_location
  ) %>%
  summarise(
    n_participants = sum(
      !is.na(aligned_response_orientation_mean)
    ),
    n_trials = sum(n_trials),
    aligned_response_orientation_mean = mean(
      aligned_response_orientation_mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# How to read plots 17a and 17b:
# Every cell is the mean final orientation at one response location after its
# sign has been aligned to the target direction. Positive means the response
# points in the target direction, and negative means the response points
# opposite to the target. A mean near zero indicates no average aligned tilt;
# it can reflect vertical responses or cancellation. These values are
# orientations, not over- or undershoot errors, because an RM response has no
# uniquely correct target-to-response location match.
RM_response_orientation_matrix_caption <- paste(
  "Cell = mean final target-aligned response orientation.",
  "Positive = target direction; negative = opposite direction;",
  "zero = no mean aligned tilt. Not an over/undershoot error."
)


# ------------plot 17a: RM response-orientation matrix, set size 3---------------
data_RM_response_orientation_set_size_3 <-
  data_RM_response_orientation_across_pp %>%
  filter(correct_num == 3)


n_trials_plot_17a <- my_data %>%
  filter(
    number_deviation == -1,
    correct_num == 3
  ) %>%
  count(
    n_bars_adjusted,
    tilted_slot,
    name = "n_trials"
  )

n_trials_plot_17a


my_plot_RM_response_orientation_matrix_set_size_3 <- ggplot(
  data_RM_response_orientation_set_size_3,
  aes(
    x = response_location,
    y = tilted_slot,
    fill = aligned_response_orientation_mean
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = paste0(
        round(aligned_response_orientation_mean, 1),
        "°"
      )
    ),
    size = 4,
    color = "black"
  ) +
  geom_text(
    aes(label = paste0("trials = ", n_trials)),
    nudge_x = 0.43,
    nudge_y = 0.43,
    hjust = 1,
    vjust = 0,
    size = 2.3,
    color = "black"
  ) +
  facet_wrap(
    ~ n_bars_adjusted,
    labeller = as_labeller(
      c(
        "1" = "1 Bar Adjusted",
        "2" = "2 Bars Adjusted"
      )
    )
  ) +
  scale_x_continuous(breaks = 1:2) +
  scale_y_continuous(
    breaks = 1:3,
    trans = "reverse"
  ) +
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "white",
    high = "#D55E00",
    midpoint = 0,
    limits = matrix_color_limits,
    breaks = matrix_color_breaks,
    oob = scales::squish,
    name = "Target-Aligned\nOrientation (Degrees)"
  ) +
  coord_equal() +
  labs(
    title = "RM Target-Aligned Response Orientation: Set Size 3",
    subtitle = paste(
      "Presented 3 lines; reported 2 lines;",
      "positive = target direction, negative = opposite direction"
    ),
    x = "Response Location (Inner to Outer)",
    y = "Presented Target Location (Inner to Outer)",
  ) +
  my_plot_theme


my_plot_RM_response_orientation_matrix_set_size_3


# ------------plot 17b: RM response-orientation matrix, set size 4---------------
data_RM_response_orientation_set_size_4 <-
  data_RM_response_orientation_across_pp %>%
  filter(correct_num == 4)


n_trials_plot_17b <- my_data %>%
  filter(
    number_deviation == -1,
    correct_num == 4
  ) %>%
  count(
    n_bars_adjusted,
    tilted_slot,
    name = "n_trials"
  )

n_trials_plot_17b


my_plot_RM_response_orientation_matrix_set_size_4 <- ggplot(
  data_RM_response_orientation_set_size_4,
  aes(
    x = response_location,
    y = tilted_slot,
    fill = aligned_response_orientation_mean
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = paste0(
        round(aligned_response_orientation_mean, 1),
        "°"
      )
    ),
    size = 4,
    color = "black"
  ) +
  geom_text(
    aes(label = paste0("trials = ", n_trials)),
    nudge_x = 0.43,
    nudge_y = 0.43,
    hjust = 1,
    vjust = 0,
    size = 2.1,
    color = "black"
  ) +
  facet_wrap(
    ~ n_bars_adjusted,
    labeller = as_labeller(
      c(
        "1" = "1 Bar Adjusted",
        "2" = "2 Bars Adjusted",
        "3" = "3 Bars Adjusted"
      )
    )
  ) +
  scale_x_continuous(breaks = 1:3) +
  scale_y_continuous(
    breaks = 1:4,
    trans = "reverse"
  ) +
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "white",
    high = "#D55E00",
    midpoint = 0,
    limits = matrix_color_limits,
    breaks = matrix_color_breaks,
    oob = scales::squish,
    name = "Target-Aligned\nOrientation (Degrees)"
  ) +
  coord_equal() +
  labs(
    title = "RM Target-Aligned Response Orientation: Set Size 4",
    subtitle = paste(
      "Presented 4 lines; reported 3 lines;",
      "positive = target direction, negative = opposite direction"
    ),
    x = "Response Location (Inner to Outer)",
    y = "Presented Target Location (Inner to Outer)",
  ) +
  my_plot_theme


my_plot_RM_response_orientation_matrix_set_size_4


# # ------------data for RM one-bar reproduced orientation---------------
# # Data arrangement:
# # 1. Keep RM trials where the reported number is one below the presented
# #    number and exactly one response bar was adjusted.
# # 2. Use the adjusted location already converted to inner-to-outer order.
# # 3. Extract the final folded orientation at that adjusted response location.
# # 4. Align its sign to the target direction, so positive responses point in
# #    the target direction and negative responses point in the opposite one.
# # 5. Average trials within each participant first, then calculate the group
# #    mean and participant-based 95% CI.
# data_RM_one_bar_orientation <- data_RM_location %>%
#   filter(n_bars_adjusted == 1) %>%
#   mutate(
#     adjusted_orientation_folded = mapply(
#       get_adjusted_orientation,
#       adjusted_ori_inner_to_outer_folded,
#       adjusted_location
#     ),
#     target_orientation_magnitude = abs(
#       target_orientation_folded
#     ),
#     target_direction_folded = factor(
#       tilt_direction_folded,
#       levels = c("toward", "away"),
#       labels = c("Toward fixation", "Away from fixation")
#     ),
#     reproduced_orientation_aligned =
#       adjusted_orientation_folded *
#       sign(target_orientation_folded)
#   ) %>%
#   filter(
#     !is.na(adjusted_orientation_folded),
#     !is.na(target_orientation_folded),
#     target_orientation_folded != 0
#   )
# 
# 
# data_RM_one_bar_orientation_by_pp <-
#   data_RM_one_bar_orientation %>%
#   group_by(
#     SubID,
#     correct_num,
#     tilted_slot,
#     target_orientation_magnitude,
#     target_direction_folded
#   ) %>%
#   summarise(
#     reproduced_orientation_mean = mean(
#       reproduced_orientation_aligned,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   )
# 
# 
# data_RM_one_bar_orientation_across_pp <-
#   data_RM_one_bar_orientation_by_pp %>%
#   group_by(
#     correct_num,
#     tilted_slot,
#     target_orientation_magnitude,
#     target_direction_folded
#   ) %>%
#   summarise(
#     n = sum(!is.na(reproduced_orientation_mean)),
#     reproduced_orientation_sd = sd(
#       reproduced_orientation_mean,
#       na.rm = TRUE
#     ),
#     reproduced_orientation_mean = mean(
#       reproduced_orientation_mean,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     reproduced_orientation_SEM =
#       reproduced_orientation_sd / sqrt(n),
#     reproduced_orientation_CI =
#       reproduced_orientation_SEM * qt(0.975, n - 1)
#   )
# 
# 
# RM_one_bar_orientation_caption <- paste(
#   "Dashed line = exact reproduction of target magnitude.",
#   "Above = overshoot; below = undershoot;",
#   "negative responses point opposite to the target direction."
# )
# 
# 
# # ------------plot 18a: RM one-bar reproduced orientation, set size 3---------------
# data_RM_one_bar_orientation_set_size_3 <-
#   data_RM_one_bar_orientation_across_pp %>%
#   filter(correct_num == 3)
# 
# 
# n_trials_plot_18a <- data_RM_one_bar_orientation %>%
#   filter(correct_num == 3) %>%
#   count(
#     tilted_slot,
#     target_direction_folded,
#     name = "n_trials"
#   )
# 
# n_trials_plot_18a
# 
# 
# my_plot_RM_reproduced_orientation_set_size_3 <- ggplot(
#   data_RM_one_bar_orientation_set_size_3,
#   aes(
#     x = target_orientation_magnitude,
#     y = reproduced_orientation_mean,
#     color = target_direction_folded,
#     group = target_direction_folded
#   )
# ) +
#   geom_abline(
#     intercept = 0,
#     slope = 1,
#     linetype = "dashed",
#     color = "grey"
#   ) +
#   geom_line(
#     position = position_dodge(width = 0.25),
#     linewidth = 0.7
#   ) +
#   geom_point(
#     position = position_dodge(width = 0.25),
#     size = 3
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = reproduced_orientation_mean - reproduced_orientation_CI,
#       ymax = reproduced_orientation_mean + reproduced_orientation_CI
#     ),
#     position = position_dodge(width = 0.25),
#     width = 0.12,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ tilted_slot,
#     ncol = 3,
#     labeller = labeller(
#       tilted_slot = function(x) paste("Target location", x)
#     )
#   ) +
#   scale_x_continuous(
#     breaks = c(1, 2, 4, 6, 8)
#   ) +
#   scale_color_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     name = "Target Direction"
#   ) +
#   labs(
#     title = "RM One-Bar Reproduced Orientation: Set Size 3",
#     subtitle = paste(
#       "Presented 3 lines; reported 2 lines;",
#       "exactly one response bar adjusted"
#     ),
#     x = "Presented Orientation Magnitude (Degrees)",
#     y = "Target-Aligned Reproduced Orientation (Degrees)",
#     caption = RM_one_bar_orientation_caption
#   ) +
#   my_plot_theme
# 
# 
# my_plot_RM_reproduced_orientation_set_size_3
# 
# 
# # ------------plot 18b: RM one-bar reproduced orientation, set size 4---------------
# data_RM_one_bar_orientation_set_size_4 <-
#   data_RM_one_bar_orientation_across_pp %>%
#   filter(correct_num == 4)
# 
# 
# n_trials_plot_18b <- data_RM_one_bar_orientation %>%
#   filter(correct_num == 4) %>%
#   count(
#     tilted_slot,
#     target_direction_folded,
#     name = "n_trials"
#   )
# 
# n_trials_plot_18b
# 
# 
# my_plot_RM_reproduced_orientation_set_size_4 <- ggplot(
#   data_RM_one_bar_orientation_set_size_4,
#   aes(
#     x = target_orientation_magnitude,
#     y = reproduced_orientation_mean,
#     color = target_direction_folded,
#     group = target_direction_folded
#   )
# ) +
#   geom_abline(
#     intercept = 0,
#     slope = 1,
#     linetype = "dashed",
#     color = "grey"
#   ) +
#   geom_line(
#     position = position_dodge(width = 0.25),
#     linewidth = 0.7
#   ) +
#   geom_point(
#     position = position_dodge(width = 0.25),
#     size = 3
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = reproduced_orientation_mean - reproduced_orientation_CI,
#       ymax = reproduced_orientation_mean + reproduced_orientation_CI
#     ),
#     position = position_dodge(width = 0.25),
#     width = 0.12,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ tilted_slot,
#     ncol = 4,
#     labeller = labeller(
#       tilted_slot = function(x) paste("Target location", x)
#     )
#   ) +
#   scale_x_continuous(
#     breaks = c(1, 2, 4, 6, 8)
#   ) +
#   scale_color_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     name = "Target Direction"
#   ) +
#   labs(
#     title = "RM One-Bar Reproduced Orientation: Set Size 4",
#     subtitle = paste(
#       "Presented 4 lines; reported 3 lines;",
#       "exactly one response bar adjusted"
#     ),
#     x = "Presented Orientation Magnitude (Degrees)",
#     y = "Target-Aligned Reproduced Orientation (Degrees)",
#     caption = RM_one_bar_orientation_caption
#   ) +
#   my_plot_theme
# 
# 
# my_plot_RM_reproduced_orientation_set_size_4


# # ------------data for RM one-bar orientation error---------------
# # Use the same one-bar RM trials as plot 18. Error is calculated at the trial
# # level before averaging:
# # target-aligned reproduced orientation - presented target magnitude.
# # Positive = overshoot. Zero = exact magnitude. Negative = undershoot or an
# # opposite-direction response.
# data_RM_one_bar_orientation_error <-
#   data_RM_one_bar_orientation %>%
#   mutate(
#     orientation_error =
#       reproduced_orientation_aligned -
#       target_orientation_magnitude
#   )
# 
# 
# data_RM_one_bar_orientation_error_by_pp <-
#   data_RM_one_bar_orientation_error %>%
#   group_by(
#     SubID,
#     correct_num,
#     tilted_slot,
#     target_orientation_magnitude,
#     target_direction_folded
#   ) %>%
#   summarise(
#     orientation_error_mean = mean(
#       orientation_error,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   )
# 
# 
# data_RM_one_bar_orientation_error_across_pp <-
#   data_RM_one_bar_orientation_error_by_pp %>%
#   group_by(
#     correct_num,
#     tilted_slot,
#     target_orientation_magnitude,
#     target_direction_folded
#   ) %>%
#   summarise(
#     n = sum(!is.na(orientation_error_mean)),
#     orientation_error_sd = sd(
#       orientation_error_mean,
#       na.rm = TRUE
#     ),
#     orientation_error_mean = mean(
#       orientation_error_mean,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     orientation_error_SEM = orientation_error_sd / sqrt(n),
#     orientation_error_CI =
#       orientation_error_SEM * qt(0.975, n - 1)
#   )
# 
# 
# RM_one_bar_orientation_error_caption <- paste(
#   "Zero = exact target magnitude; positive = overshoot;",
#   "negative = undershoot or an opposite-direction response.",
#   "The adjusted response location is not treated as correct or incorrect."
# )
# 
# 
# # ------------plot 19a: RM one-bar orientation error, set size 3---------------
# data_RM_one_bar_orientation_error_set_size_3 <-
#   data_RM_one_bar_orientation_error_across_pp %>%
#   filter(correct_num == 3)
# 
# 
# n_trials_plot_19a <- data_RM_one_bar_orientation_error %>%
#   filter(correct_num == 3) %>%
#   count(
#     tilted_slot,
#     target_direction_folded,
#     name = "n_trials"
#   )
# 
# n_trials_plot_19a
# 
# 
# my_plot_RM_orientation_error_set_size_3 <- ggplot(
#   data_RM_one_bar_orientation_error_set_size_3,
#   aes(
#     x = target_orientation_magnitude,
#     y = orientation_error_mean,
#     color = target_direction_folded,
#     group = target_direction_folded
#   )
# ) +
#   geom_hline(
#     yintercept = 0,
#     linetype = "dashed",
#     color = "grey"
#   ) +
#   geom_line(
#     position = position_dodge(width = 0.25),
#     linewidth = 0.7
#   ) +
#   geom_point(
#     position = position_dodge(width = 0.25),
#     size = 3
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = orientation_error_mean - orientation_error_CI,
#       ymax = orientation_error_mean + orientation_error_CI
#     ),
#     position = position_dodge(width = 0.25),
#     width = 0.12,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ tilted_slot,
#     ncol = 3,
#     labeller = labeller(
#       tilted_slot = function(x) paste("Target location", x)
#     )
#   ) +
#   scale_x_continuous(
#     breaks = c(1, 2, 4, 6, 8)
#   ) +
#   scale_color_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     name = "Target Direction"
#   ) +
#   labs(
#     title = "RM One-Bar Orientation Error: Set Size 3",
#     subtitle = paste(
#       "Presented 3 lines; reported 2 lines;",
#       "exactly one response bar adjusted"
#     ),
#     x = "Presented Orientation Magnitude (Degrees)",
#     y = paste(
#       "Orientation Error (Degrees)",
#       "Negative = Under/Opposite; Positive = Overshoot",
#       sep = "\n"
#     ),
#     caption = RM_one_bar_orientation_error_caption
#   ) +
#   my_plot_theme
# 
# 
# 
# my_plot_RM_orientation_error_set_size_3
# 
# 
# # ------------plot 19b: RM one-bar orientation error, set size 4---------------
# data_RM_one_bar_orientation_error_set_size_4 <-
#   data_RM_one_bar_orientation_error_across_pp %>%
#   filter(correct_num == 4)
# 
# 
# n_trials_plot_19b <- data_RM_one_bar_orientation_error %>%
#   filter(correct_num == 4) %>%
#   count(
#     tilted_slot,
#     target_direction_folded,
#     name = "n_trials"
#   )
# 
# n_trials_plot_19b
# 
# 
# my_plot_RM_orientation_error_set_size_4 <- ggplot(
#   data_RM_one_bar_orientation_error_set_size_4,
#   aes(
#     x = target_orientation_magnitude,
#     y = orientation_error_mean,
#     color = target_direction_folded,
#     group = target_direction_folded
#   )
# ) +
#   geom_hline(
#     yintercept = 0,
#     linetype = "dashed",
#     color = "grey"
#   ) +
#   geom_line(
#     position = position_dodge(width = 0.25),
#     linewidth = 0.7
#   ) +
#   geom_point(
#     position = position_dodge(width = 0.25),
#     size = 3
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = orientation_error_mean - orientation_error_CI,
#       ymax = orientation_error_mean + orientation_error_CI
#     ),
#     position = position_dodge(width = 0.25),
#     width = 0.12,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ tilted_slot,
#     ncol = 4,
#     labeller = labeller(
#       tilted_slot = function(x) paste("Target location", x)
#     )
#   ) +
#   scale_x_continuous(
#     breaks = c(1, 2, 4, 6, 8)
#   ) +
#   scale_color_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     name = "Target Direction"
#   ) +
#   labs(
#     title = "RM One-Bar Orientation Error: Set Size 4",
#     subtitle = paste(
#       "Presented 4 lines; reported 3 lines;",
#       "exactly one response bar adjusted"
#     ),
#     x = "Presented Orientation Magnitude (Degrees)",
#     y = paste(
#       "Orientation Error (Degrees)",
#       "Negative = Under/Opposite; Positive = Overshoot",
#       sep = "\n"
#     ),
#     caption = RM_one_bar_orientation_error_caption
#   ) +
#   my_plot_theme
# 
# 
# my_plot_RM_orientation_error_set_size_4
# 
# 
# # ------------data for total target signal across the RM response array---------------
# # For every RM trial:
# # 1. Align every final response orientation to the target direction.
# # 2. Sum the aligned orientations across the entire response array.
# # 3. Divide this sum by the presented target magnitude.
# #
# # Ratio = 1: one target magnitude of net orientation signal is preserved.
# # Ratio = 0: no net target-aligned signal remains.
# # Ratio > 1: excess target-aligned signal, from overshoot and/or spreading the
# #            target orientation across multiple response bars.
# # Ratio < 0: the response array has a net tilt opposite to the target.
# data_RM_total_target_signal_by_trial <-
#   data_RM_response_orientation %>%
#   mutate(
#     target_orientation_magnitude = abs(
#       target_orientation_folded
#     ),
#     target_direction_folded = factor(
#       tilt_direction_folded,
#       levels = c("toward", "away"),
#       labels = c("Toward fixation", "Away from fixation")
#     )
#   ) %>%
#   group_by(
#     SubID,
#     block,
#     trial_in_block,
#     correct_num,
#     reported_number,
#     n_bars_adjusted,
#     tilted_slot,
#     target_orientation_magnitude,
#     target_direction_folded
#   ) %>%
#   summarise(
#     total_aligned_orientation = sum(
#       aligned_response_orientation,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     total_target_signal_ratio =
#       total_aligned_orientation /
#       target_orientation_magnitude
#   )
# 
# 
# # Average trials within each participant first. Target magnitude is not kept
# # as a plotting factor because division by target magnitude puts all trials on
# # the same relative scale.
# data_RM_total_target_signal_by_pp <-
#   data_RM_total_target_signal_by_trial %>%
#   group_by(
#     SubID,
#     correct_num,
#     n_bars_adjusted,
#     tilted_slot,
#     target_direction_folded
#   ) %>%
#   summarise(
#     total_target_signal_ratio_mean = mean(
#       total_target_signal_ratio,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   )
# 
# 
# data_RM_total_target_signal_across_pp <-
#   data_RM_total_target_signal_by_pp %>%
#   group_by(
#     correct_num,
#     n_bars_adjusted,
#     tilted_slot,
#     target_direction_folded
#   ) %>%
#   summarise(
#     n = sum(!is.na(total_target_signal_ratio_mean)),
#     total_target_signal_ratio_mean = mean(
#       total_target_signal_ratio_mean,
#       na.rm = TRUE
#     ),
#     total_target_signal_ratio_sd = sd(
#       total_target_signal_ratio_mean,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     total_target_signal_ratio_SEM =
#       total_target_signal_ratio_sd / sqrt(n),
#     total_target_signal_ratio_CI =
#       total_target_signal_ratio_SEM * qt(0.975, n - 1)
#   )
# 
# 
# RM_total_target_signal_caption <- paste(
#   "Dashed line at 1 = one target magnitude preserved;",
#   "dotted line at 0 = no net target-aligned signal.",
#   "Values above 1 may reflect overshoot and/or signal spread across bars."
# )
# 
# 
# # ------------plot 20a: total RM target signal, set size 3---------------
# data_RM_total_target_signal_set_size_3 <-
#   data_RM_total_target_signal_across_pp %>%
#   filter(correct_num == 3)
# 
# 
# n_trials_plot_20a <-
#   data_RM_total_target_signal_by_trial %>%
#   filter(correct_num == 3) %>%
#   count(
#     n_bars_adjusted,
#     tilted_slot,
#     target_direction_folded,
#     name = "n_trials"
#   )
# 
# n_trials_plot_20a
# 
# 
# my_plot_RM_total_target_signal_set_size_3 <- ggplot(
#   data_RM_total_target_signal_set_size_3,
#   aes(
#     x = factor(tilted_slot),
#     y = total_target_signal_ratio_mean,
#     color = target_direction_folded,
#     group = target_direction_folded
#   )
# ) +
#   geom_hline(
#     yintercept = 1,
#     linetype = "dashed",
#     color = "grey"
#   ) +
#   geom_hline(
#     yintercept = 0,
#     linetype = "dotted",
#     color = "grey"
#   ) +
#   geom_line(
#     position = position_dodge(width = 0.3),
#     linewidth = 0.7
#   ) +
#   geom_point(
#     position = position_dodge(width = 0.3),
#     size = 3
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = total_target_signal_ratio_mean -
#         total_target_signal_ratio_CI,
#       ymax = total_target_signal_ratio_mean +
#         total_target_signal_ratio_CI
#     ),
#     position = position_dodge(width = 0.3),
#     width = 0.12,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ n_bars_adjusted,
#     labeller = as_labeller(
#       c(
#         "1" = "1 Bar Adjusted",
#         "2" = "2 Bars Adjusted"
#       )
#     )
#   ) +
#   scale_color_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     name = "Target Direction"
#   ) +
#   labs(
#     title = "RM Total Target Signal Across Response Array: Set Size 3",
#     subtitle = paste(
#       "Presented 3 lines; reported 2 lines;",
#       "sum of all target-aligned response orientations"
#     ),
#     x = "Presented Target Location (Inner to Outer)",
#     y = paste(
#       "Total Target Signal Ratio",
#       "Sum of Aligned Responses / Target Magnitude",
#       sep = "\n"
#     ),
#     caption = RM_total_target_signal_caption
#   ) +
#   my_plot_theme
# 
# 
# my_plot_RM_total_target_signal_set_size_3
# 
# 
# # ------------plot 20b: total RM target signal, set size 4---------------
# data_RM_total_target_signal_set_size_4 <-
#   data_RM_total_target_signal_across_pp %>%
#   filter(correct_num == 4)
# 
# 
# n_trials_plot_20b <-
#   data_RM_total_target_signal_by_trial %>%
#   filter(correct_num == 4) %>%
#   count(
#     n_bars_adjusted,
#     tilted_slot,
#     target_direction_folded,
#     name = "n_trials"
#   )
# 
# n_trials_plot_20b
# 
# 
# my_plot_RM_total_target_signal_set_size_4 <- ggplot(
#   data_RM_total_target_signal_set_size_4,
#   aes(
#     x = factor(tilted_slot),
#     y = total_target_signal_ratio_mean,
#     color = target_direction_folded,
#     group = target_direction_folded
#   )
# ) +
#   geom_hline(
#     yintercept = 1,
#     linetype = "dashed",
#     color = "grey"
#   ) +
#   geom_hline(
#     yintercept = 0,
#     linetype = "dotted",
#     color = "grey"
#   ) +
#   geom_line(
#     position = position_dodge(width = 0.3),
#     linewidth = 0.7
#   ) +
#   geom_point(
#     position = position_dodge(width = 0.3),
#     size = 3
#   ) +
#   geom_errorbar(
#     aes(
#       ymin = total_target_signal_ratio_mean -
#         total_target_signal_ratio_CI,
#       ymax = total_target_signal_ratio_mean +
#         total_target_signal_ratio_CI
#     ),
#     position = position_dodge(width = 0.3),
#     width = 0.12,
#     linewidth = 0.5
#   ) +
#   facet_wrap(
#     ~ n_bars_adjusted,
#     labeller = as_labeller(
#       c(
#         "1" = "1 Bar Adjusted",
#         "2" = "2 Bars Adjusted",
#         "3" = "3 Bars Adjusted"
#       )
#     )
#   ) +
#   scale_color_manual(
#     values = c(
#       "Toward fixation" = "#E69F00",
#       "Away from fixation" = "#56B4E9"
#     ),
#     name = "Target Direction"
#   ) +
#   labs(
#     title = "RM Total Target Signal Across Response Array: Set Size 4",
#     subtitle = paste(
#       "Presented 4 lines; reported 3 lines;",
#       "sum of all target-aligned response orientations"
#     ),
#     x = "Presented Target Location (Inner to Outer)",
#     y = paste(
#       "Total Target Signal Ratio",
#       "Sum of Aligned Responses / Target Magnitude",
#       sep = "\n"
#     ),
#     caption = RM_total_target_signal_caption
#   ) +
#   my_plot_theme
# 
# 
# my_plot_RM_total_target_signal_set_size_4


# ------------RM orientation-assimilation gain---------------
# It summarizes the already plotted target-aligned RM matrices in the same
# participant-gain style as my_plot_orientation_transfer_gain. It does not make
# any comparison with correct-number trials.
#
# Map the shorter response array onto the full presented array. Every response
# is classified as position-compatible, inward, or outward assimilation. No
# response bar is assumed to be the original target because the reported array
# contains fewer bars than the presented array.
#
# P1, P2, P3 example: three bars were presented but only response P1 and P2 exist
# because the participant reported two bars.
#
#   Presented target P1:
#     - response P1 = position-compatible assimilation;
#     - response P2 = outward-assimilation location.
#
#   Presented target P2:
#     - response P1 = inward-assimilation location;
#     - response P2 = outward-assimilation location.
#
#   Presented target P3:
#     - response P1 = inward-assimilation location;
#     - response P2 = position-compatible assimilation.
#
# There is therefore no "other location" for set size 3.
#
# P1, P2, P3, P4 example: four bars were presented but only response P1, P2,
# and P3 exist because the participant reported three bars.
#
#   Presented target P1:
#     - response P1 = position-compatible assimilation;
#     - response P2 and P3 = outward-assimilation locations.
#
#   Presented target P2:
#     - response P1 = inward-assimilation location;
#     - response P2 and P3 = outward-assimilation locations.
#
#   Presented target P3:
#     - response P1 and P2 = inward-assimilation locations;
#     - response P3 = outward-assimilation location.
#
#   Presented target P4:
#     - response P1 and P2 = inward-assimilation locations;
#     - response P3 = position-compatible assimilation.
#
# There is therefore no "other location" for set size 4 either.


RM_assimilation_location_levels <- c(
  "position_compatible_assimilation",
  "inward_assimilation",
  "outward_assimilation"
)


RM_assimilation_location_labels <- c(
  position_compatible_assimilation = "Position-Compatible\nAssimilation",
  inward_assimilation = "Inward\nAssimilation",
  outward_assimilation = "Outward\nAssimilation"
)


# Match the current correct-number gain analysis by pooling set sizes 3 and 4
# and using all RM trials, regardless of how many bars were adjusted. Retain
# zero-orientation responses and align every response to the target direction
# before estimating gain.
data_RM_orientation_assimilation <-
  data_RM_response_orientation %>%
  mutate(
    target_orientation_magnitude = abs(
      target_orientation_folded
    ),
    RM_location_class = case_when(
      correct_num == 3 &
        tilted_slot == 1 &
        response_location == 1 ~
        "position_compatible_assimilation",
      correct_num == 3 &
        tilted_slot == 3 &
        response_location == 2 ~
        "position_compatible_assimilation",
      correct_num == 4 &
        tilted_slot == 1 &
        response_location == 1 ~
        "position_compatible_assimilation",
      correct_num == 4 &
        tilted_slot == 4 &
        response_location == 3 ~
        "position_compatible_assimilation",
      response_location < tilted_slot ~
        "inward_assimilation",
      TRUE ~ "outward_assimilation"
    )
  )


# Estimate gain separately for target directions toward and away from fixation.
data_RM_orientation_assimilation_by_pp_direction <-
  data_RM_orientation_assimilation %>%
  group_by(
    SubID,
    RM_location_class,
    tilt_direction_folded
  ) %>%
  summarise(
    n_observations = n(),
    orientation_gain = calculate_orientation_gain(
      target_orientation_magnitude,
      aligned_response_orientation
    ),
    .groups = "drop"
  )


# Give the two folded target directions equal weight within participant.
data_RM_orientation_assimilation_by_pp <-
  data_RM_orientation_assimilation_by_pp_direction %>%
  group_by(
    SubID,
    RM_location_class
  ) %>%
  summarise(
    n_target_directions = sum(
      is.finite(orientation_gain)
    ),
    orientation_gain = mean(
      orientation_gain,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  filter(is.finite(orientation_gain))


data_RM_orientation_assimilation_across_pp <-
  data_RM_orientation_assimilation_by_pp %>%
  group_by(RM_location_class) %>%
  summarise(
    n = sum(!is.na(orientation_gain)),
    orientation_gain_mean = mean(
      orientation_gain,
      na.rm = TRUE
    ),
    orientation_gain_sd = sd(
      orientation_gain,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    orientation_gain_SEM =
      orientation_gain_sd / sqrt(n),
    orientation_gain_CI =
      orientation_gain_SEM * qt(0.975, n - 1),
    RM_location_class = factor(
      RM_location_class,
      levels = RM_assimilation_location_levels,
      labels = unname(
        RM_assimilation_location_labels[
          RM_assimilation_location_levels
        ]
      )
    )
  )


data_RM_orientation_assimilation_plot <-
  data_RM_orientation_assimilation_by_pp %>%
  mutate(
    RM_location_class = factor(
      RM_location_class,
      levels = RM_assimilation_location_levels,
      labels = unname(
        RM_assimilation_location_labels[
          RM_assimilation_location_levels
        ]
      )
    )
  )


RM_orientation_assimilation_caption <- paste(
  "Gain is the zero-intercept slope from target magnitude to target-aligned",
  "response orientation. Because response identity is unknown, every response",
  "is classified as position-compatible, inward, or outward assimilation."
)


my_plot_RM_orientation_assimilation_gain <- ggplot(
  data_RM_orientation_assimilation_plot,
  aes(
    x = RM_location_class,
    y = orientation_gain
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    color = "black"
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "grey"
  ) +
  geom_jitter(
    width = 0.08,
    height = 0,
    size = 2,
    alpha = 0.55,
    color = "#707070"
  ) +
  geom_errorbar(
    data = data_RM_orientation_assimilation_across_pp,
    aes(
      x = RM_location_class,
      y = orientation_gain_mean,
      ymin = orientation_gain_mean - orientation_gain_CI,
      ymax = orientation_gain_mean + orientation_gain_CI
    ),
    inherit.aes = FALSE,
    width = 0.0,
    linewidth = 0.5,
    color = "black"
  ) +
  geom_point(
    data = data_RM_orientation_assimilation_across_pp,
    aes(
      x = RM_location_class,
      y = orientation_gain_mean
    ),
    inherit.aes = FALSE,
    size = 4,
    color = "black"
  ) +
  scale_y_continuous(limits = c(-2, 2)) +
  scale_x_discrete(drop = FALSE) +
  labs(
    title = "RM Target-Orientation Assimilation Across Response Positions",
    subtitle = paste("set sizes 3 and 4 combined; all RM trials"
    ),
    x = "Assimilation Category",
    y = "Orientation Assimilation"
  ) +
  my_plot_theme


my_plot_RM_orientation_assimilation_gain


# ------------RM response-location gain tests---------------
# Test all three assimilation gains against 0, meaning no target-orientation
# information in that response-position category. The participant is the
# inferential unit, and Holm correction controls the family of three tests.
RM_gain_reference_values <- c(
  position_compatible_assimilation = 0,
  inward_assimilation = 0,
  outward_assimilation = 0
)


run_RM_location_class_test <- function(location_name) {
  gain_values <-
    data_RM_orientation_assimilation_by_pp %>%
    filter(RM_location_class == location_name) %>%
    pull(orientation_gain)

  reference_gain <- unname(
    RM_gain_reference_values[location_name]
  )

  test_result <- t.test(
    gain_values,
    mu = reference_gain,
    alternative = "two.sided"
  )

  data.frame(
    RM_location_class = location_name,
    reference_gain = reference_gain,
    n_participants = length(gain_values),
    orientation_gain_mean = mean(gain_values),
    mean_difference_from_reference =
      mean(gain_values) - reference_gain,
    orientation_gain_sd = sd(gain_values),
    confidence_interval_low = test_result$conf.int[1],
    confidence_interval_high = test_result$conf.int[2],
    t_statistic = unname(test_result$statistic),
    degrees_freedom = unname(test_result$parameter),
    p_value = test_result$p.value,
    cohen_dz_vs_reference =
      (mean(gain_values) - reference_gain) /
        sd(gain_values),
    stringsAsFactors = FALSE
  )
}


result_RM_location_class_gain_tests <-
  bind_rows(
    lapply(
      RM_assimilation_location_levels,
      run_RM_location_class_test
    )
  ) %>%
  mutate(
    p_value_holm = p.adjust(
      p_value,
      method = "holm"
    ),
    significance = get_significance_symbol(
      p_value_holm
    )
  )


print(result_RM_location_class_gain_tests)


# Direction-specific paired comparison: is assimilation stronger on the inward
# or outward side of the target?
data_RM_inward_vs_outward_gain_by_pp <-
  data_RM_orientation_assimilation_by_pp %>%
  filter(
    RM_location_class %in% c(
      "inward_assimilation",
      "outward_assimilation"
    )
  ) %>%
  select(
    SubID,
    RM_location_class,
    orientation_gain
  ) %>%
  tidyr::pivot_wider(
    names_from = RM_location_class,
    values_from = orientation_gain
  ) %>%
  filter(
    is.finite(inward_assimilation),
    is.finite(outward_assimilation)
  ) %>%
  mutate(
    inward_minus_outward =
      inward_assimilation - outward_assimilation
  )


test_RM_inward_vs_outward_gain <- t.test(
  data_RM_inward_vs_outward_gain_by_pp$inward_assimilation,
  data_RM_inward_vs_outward_gain_by_pp$outward_assimilation,
  paired = TRUE,
  alternative = "two.sided"
)


result_RM_inward_vs_outward_gain <- data.frame(
  set_sizes = "3 and 4 combined",
  n_participants = nrow(
    data_RM_inward_vs_outward_gain_by_pp
  ),
  inward_gain_mean = mean(
    data_RM_inward_vs_outward_gain_by_pp$inward_assimilation
  ),
  outward_gain_mean = mean(
    data_RM_inward_vs_outward_gain_by_pp$outward_assimilation
  ),
  mean_difference = mean(
    data_RM_inward_vs_outward_gain_by_pp$inward_minus_outward
  ),
  confidence_interval_low =
    test_RM_inward_vs_outward_gain$conf.int[1],
  confidence_interval_high =
    test_RM_inward_vs_outward_gain$conf.int[2],
  t_statistic = unname(
    test_RM_inward_vs_outward_gain$statistic
  ),
  degrees_freedom = unname(
    test_RM_inward_vs_outward_gain$parameter
  ),
  p_value = test_RM_inward_vs_outward_gain$p.value,
  cohen_dz = mean(
    data_RM_inward_vs_outward_gain_by_pp$inward_minus_outward
  ) / sd(
    data_RM_inward_vs_outward_gain_by_pp$inward_minus_outward
  ),
  stringsAsFactors = FALSE
)


print(result_RM_inward_vs_outward_gain)


# Current-data interpretation:
# Position-compatible assimilation is reliably greater than zero. Inward and
# outward assimilation gains are both descriptively positive, but neither is
# reliably different from zero after Holm correction across the three tests.
# Their paired difference is small and not reliable, so there is no evidence that
# inward assimilation is stronger than outward assimilation. Comparison with
# correct-number trials remains a later step.


# # ------------RM set size 3, middle-target assimilation---------------
# # Focus only on trials with three presented bars, P2 as the target, and two
# # reported response bars. Under the current mapping:
# #
# #   response P1 = inward assimilation;
# #   response P2 = outward assimilation.
# #
# # There is no position-compatible category in this middle-target condition.
# RM_middle_target_location_levels <- c(
#   "inward_assimilation",
#   "outward_assimilation"
# )
# 
# 
# RM_middle_target_location_labels <- c(
#   inward_assimilation = "Inward\nAssimilation",
#   outward_assimilation = "Outward\nAssimilation"
# )
# 
# 
# data_RM_set_size_3_middle_target <-
#   data_RM_orientation_assimilation %>%
#   filter(
#     correct_num == 3,
#     tilted_slot == 2
#   )
# 
# 
# # Estimate the two gains separately for target directions toward and away from
# # fixation, then give those target directions equal weight within participant.
# data_RM_set_size_3_middle_target_by_pp_direction <-
#   data_RM_set_size_3_middle_target %>%
#   group_by(
#     SubID,
#     RM_location_class,
#     tilt_direction_folded
#   ) %>%
#   summarise(
#     n_observations = n(),
#     orientation_gain = calculate_orientation_gain(
#       target_orientation_magnitude,
#       aligned_response_orientation
#     ),
#     .groups = "drop"
#   )
# 
# 
# data_RM_set_size_3_middle_target_by_pp <-
#   data_RM_set_size_3_middle_target_by_pp_direction %>%
#   group_by(
#     SubID,
#     RM_location_class
#   ) %>%
#   summarise(
#     n_target_directions = sum(
#       is.finite(orientation_gain)
#     ),
#     orientation_gain = mean(
#       orientation_gain,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   filter(is.finite(orientation_gain))
# 
# 
# data_RM_set_size_3_middle_target_across_pp <-
#   data_RM_set_size_3_middle_target_by_pp %>%
#   group_by(RM_location_class) %>%
#   summarise(
#     n = sum(!is.na(orientation_gain)),
#     orientation_gain_mean = mean(
#       orientation_gain,
#       na.rm = TRUE
#     ),
#     orientation_gain_sd = sd(
#       orientation_gain,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     orientation_gain_SEM =
#       orientation_gain_sd / sqrt(n),
#     orientation_gain_CI =
#       orientation_gain_SEM * qt(0.975, n - 1),
#     RM_location_class = factor(
#       RM_location_class,
#       levels = RM_middle_target_location_levels,
#       labels = unname(
#         RM_middle_target_location_labels[
#           RM_middle_target_location_levels
#         ]
#       )
#     )
#   )
# 
# 
# data_RM_set_size_3_middle_target_plot <-
#   data_RM_set_size_3_middle_target_by_pp %>%
#   mutate(
#     RM_location_class = factor(
#       RM_location_class,
#       levels = RM_middle_target_location_levels,
#       labels = unname(
#         RM_middle_target_location_labels[
#           RM_middle_target_location_levels
#         ]
#       )
#     )
#   )
# 
# 
# my_plot_RM_set_size_3_middle_target_assimilation_gain <- ggplot(
#   data_RM_set_size_3_middle_target_plot,
#   aes(
#     x = RM_location_class,
#     y = orientation_gain
#   )
# ) +
#   geom_hline(
#     yintercept = 0,
#     linetype = "dotted",
#     color = "black"
#   ) +
#   geom_hline(
#     yintercept = 1,
#     linetype = "dashed",
#     color = "grey"
#   ) +
#   geom_jitter(
#     width = 0.08,
#     height = 0,
#     size = 2,
#     alpha = 0.55,
#     color = "#707070"
#   ) +
#   geom_errorbar(
#     data = data_RM_set_size_3_middle_target_across_pp,
#     aes(
#       x = RM_location_class,
#       y = orientation_gain_mean,
#       ymin = orientation_gain_mean - orientation_gain_CI,
#       ymax = orientation_gain_mean + orientation_gain_CI
#     ),
#     inherit.aes = FALSE,
#     width = 0.0,
#     linewidth = 0.5,
#     color = "black"
#   ) +
#   geom_point(
#     data = data_RM_set_size_3_middle_target_across_pp,
#     aes(
#       x = RM_location_class,
#       y = orientation_gain_mean
#     ),
#     inherit.aes = FALSE,
#     size = 4,
#     color = "black"
#   ) +
#   scale_y_continuous(limits = c(-2, 2)) +
#   scale_x_discrete(drop = FALSE) +
#   labs(
#     title = "RM Assimilation With a Middle Target",
#     subtitle = paste(
#       "Three bars presented, middle target, two bars reported;",
#       "RM trials"
#     ),
#     x = "Assimilation Direction",
#     y = "Orientation Assimilation",
#   ) +
#   my_plot_theme
# 
# 
# my_plot_RM_set_size_3_middle_target_assimilation_gain


# ------------RM assimilation separately by target location---------------
# Estimate gains within each set size and presented target position so the
# strong middle-target pattern is not diluted by edge-target trials.
data_RM_assimilation_by_target_direction <-
  data_RM_orientation_assimilation %>%
  group_by(
    SubID,
    correct_num,
    tilted_slot,
    RM_location_class,
    tilt_direction_folded
  ) %>%
  summarise(
    n_observations = n(),
    orientation_gain = calculate_orientation_gain(
      target_orientation_magnitude,
      aligned_response_orientation
    ),
    .groups = "drop"
  )


data_RM_assimilation_by_target_by_pp <-
  data_RM_assimilation_by_target_direction %>%
  group_by(
    SubID,
    correct_num,
    tilted_slot,
    RM_location_class
  ) %>%
  summarise(
    n_target_directions = sum(
      is.finite(orientation_gain)
    ),
    orientation_gain = mean(
      orientation_gain,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  filter(is.finite(orientation_gain))


# Display the number of contributing participants in every target-position
# panel because the RM condition did not occur for every participant.
data_RM_assimilation_target_condition_counts <-
  data_RM_assimilation_by_target_by_pp %>%
  distinct(
    SubID,
    correct_num,
    tilted_slot
  ) %>%
  count(
    correct_num,
    tilted_slot,
    name = "n_participants_condition"
  ) %>%
  arrange(
    correct_num,
    tilted_slot
  ) %>%
  mutate(
    target_condition_label = paste0(
      "Set Size ",
      correct_num,
      ": Target P",
      tilted_slot,
      "\n",
      "n = ",
      n_participants_condition
    )
  )


RM_target_condition_label_levels <-
  data_RM_assimilation_target_condition_counts %>%
  pull(target_condition_label)


data_RM_assimilation_by_target_across_pp <-
  data_RM_assimilation_by_target_by_pp %>%
  group_by(
    correct_num,
    tilted_slot,
    RM_location_class
  ) %>%
  summarise(
    n = sum(!is.na(orientation_gain)),
    orientation_gain_mean = mean(
      orientation_gain,
      na.rm = TRUE
    ),
    orientation_gain_sd = sd(
      orientation_gain,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    orientation_gain_SEM =
      orientation_gain_sd / sqrt(n),
    orientation_gain_CI =
      orientation_gain_SEM * qt(0.975, n - 1)
  ) %>%
  left_join(
    data_RM_assimilation_target_condition_counts,
    by = c(
      "correct_num",
      "tilted_slot"
    )
  ) %>%
  mutate(
    RM_location_class = factor(
      RM_location_class,
      levels = RM_assimilation_location_levels,
      labels = unname(
        RM_assimilation_location_labels[
          RM_assimilation_location_levels
        ]
      )
    ),
    target_condition_label = factor(
      target_condition_label,
      levels = RM_target_condition_label_levels
    )
  )


data_RM_assimilation_by_target_plot <-
  data_RM_assimilation_by_target_by_pp %>%
  left_join(
    data_RM_assimilation_target_condition_counts,
    by = c(
      "correct_num",
      "tilted_slot"
    )
  ) %>%
  mutate(
    RM_location_class = factor(
      RM_location_class,
      levels = RM_assimilation_location_levels,
      labels = unname(
        RM_assimilation_location_labels[
          RM_assimilation_location_levels
        ]
      )
    ),
    target_condition_label = factor(
      target_condition_label,
      levels = RM_target_condition_label_levels
    )
  )


my_plot_RM_orientation_assimilation_by_target_location <- ggplot(
  data_RM_assimilation_by_target_plot,
  aes(
    x = RM_location_class,
    y = orientation_gain
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    color = "black"
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "grey"
  ) +
  geom_jitter(
    width = 0.08,
    height = 0,
    size = 1.8,
    alpha = 0.5,
    color = "#707070"
  ) +
  geom_errorbar(
    data = data_RM_assimilation_by_target_across_pp,
    aes(
      x = RM_location_class,
      y = orientation_gain_mean,
      ymin = orientation_gain_mean - orientation_gain_CI,
      ymax = orientation_gain_mean + orientation_gain_CI
    ),
    inherit.aes = FALSE,
    width = 0.0,
    linewidth = 0.5,
    color = "black"
  ) +
  geom_point(
    data = data_RM_assimilation_by_target_across_pp,
    aes(
      x = RM_location_class,
      y = orientation_gain_mean
    ),
    inherit.aes = FALSE,
    size = 3.5,
    color = "black"
  ) +
  facet_wrap(
    ~ target_condition_label,
    ncol = 4,
    scales = "free_x"
  ) +
  scale_y_continuous(limits = c(-2, 2)) +
  scale_x_discrete(drop = TRUE) +
  labs(
    title = "RM Orientation Assimilation by Target Location",
    x = "Assimilation Category",
    y = "Orientation Assimilation "
  ) +
  my_plot_theme +
  theme(
    axis.text.x = element_text(
      size = 9,
      face = "bold"
    )
  )


my_plot_RM_orientation_assimilation_by_target_location


# ------------RM target-location gains tested against zero---------------
# The direct question is whether each of the 14 plotted target-location/category
# gains contains target-orientation information. Participant-level one-sample
# tests answer that question transparently. Holm correction is applied across
# the complete family of 14 tests, rather than separately within each panel.
#
# An observation-level mixed model could be added later as a robustness analysis
# for unequal trial counts, but it is not needed for these cell-versus-zero tests.
result_RM_assimilation_by_target_zero_tests <-
  data_RM_assimilation_by_target_by_pp %>%
  group_by(
    correct_num,
    tilted_slot,
    RM_location_class
  ) %>%
  group_modify(
    ~ {
      gain_values <- .x$orientation_gain

      test_result <- t.test(
        gain_values,
        mu = 0,
        alternative = "two.sided"
      )

      data.frame(
        reference_gain = 0,
        n_participants = length(gain_values),
        orientation_gain_mean = mean(gain_values),
        orientation_gain_sd = sd(gain_values),
        confidence_interval_low = test_result$conf.int[1],
        confidence_interval_high = test_result$conf.int[2],
        t_statistic = unname(test_result$statistic),
        degrees_freedom = unname(test_result$parameter),
        p_value = test_result$p.value,
        cohen_dz = mean(gain_values) / sd(gain_values),
        stringsAsFactors = FALSE
      )
    }
  ) %>%
  ungroup() %>%
  mutate(
    p_value_holm_14 = p.adjust(
      p_value,
      method = "holm"
    ),
    significance = get_significance_symbol(
      p_value_holm_14
    ),
    target_condition = paste0(
      "Set Size ",
      correct_num,
      ", Target P",
      tilted_slot
    ),
    assimilation_category = recode(
      RM_location_class,
      position_compatible_assimilation =
        "Position-Compatible Assimilation",
      inward_assimilation = "Inward Assimilation",
      outward_assimilation = "Outward Assimilation"
    )
  ) %>%
  arrange(
    correct_num,
    tilted_slot,
    match(
      RM_location_class,
      RM_assimilation_location_levels
    )
  ) %>%
  select(
    target_condition,
    assimilation_category,
    reference_gain,
    n_participants,
    orientation_gain_mean,
    orientation_gain_sd,
    confidence_interval_low,
    confidence_interval_high,
    t_statistic,
    degrees_freedom,
    p_value,
    p_value_holm_14,
    significance,
    cohen_dz,
    correct_num,
    tilted_slot,
    RM_location_class
  )







# ------------library-----------------
library(dplyr)
library(ggplot2)


# ------------set project path---------
setwd("D:/OneDrive/projects/orientation_assimilation")


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


# ------------design target magnitudes---------------
# These are the five target magnitudes used in the experiment. Hypothetical
# matrices should use equal design weighting rather than observed frequencies.
target_orientation_magnitudes_design <- c(1, 2, 4, 6, 8)

target_orientation_magnitude_design_mean <- mean(
  target_orientation_magnitudes_design
)


# ------------design matrix locations---------------
# This grid comes only from the design. Each target location is crossed with
# every response location for set sizes 3 and 4.
data_hypothetical_design_grid <- bind_rows(
  tidyr::expand_grid(
    correct_num = 3,
    tilted_slot = 1:3,
    response_location = 1:3
  ),
  tidyr::expand_grid(
    correct_num = 4,
    tilted_slot = 1:4,
    response_location = 1:4
  )
)


# ------------hypothetical orientation-error matrices---------------
# Perfect reconstruction:
# The target location has the target orientation and non-targets remain
# vertical. Every cell has zero error.
#
# Full assimilation / all lines = target:
# Every response location has the target magnitude. The target-location
# diagonal has zero error. Every non-target cell has +4.2 degrees error.
#
# Pooling:
# The target magnitude is divided equally across all response locations. The
# target-location diagonal is the pooled response minus the target magnitude;
# every non-target cell is the pooled response relative to vertical.
#
# For hypotheses containing a randomly selected non-target location, a cell is
# the expected value across all equally likely non-target locations. Therefore,
# the target magnitude is divided by set size minus one.
data_hypothetical_orientation_error <-
  data_hypothetical_design_grid %>%
  mutate(
    perfect_reconstruction = 0,
    full_assimilation = if_else(
      response_location == tilted_slot,
      0,
      target_orientation_magnitude_design_mean
    ),
    pooling = if_else(
      response_location == tilted_slot,
      target_orientation_magnitude_design_mean / correct_num -
        target_orientation_magnitude_design_mean,
      target_orientation_magnitude_design_mean / correct_num
    ),
    one_non_target_also_target = if_else(
      response_location == tilted_slot,
      0,
      target_orientation_magnitude_design_mean /
        (correct_num - 1)
    ),
    target_mislocalized_to_one_non_target = if_else(
      response_location == tilted_slot,
      -target_orientation_magnitude_design_mean,
      target_orientation_magnitude_design_mean /
        (correct_num - 1)
    ),
    target_orientation_moved_outward = case_when(
      tilted_slot == correct_num ~ 0,
      response_location == tilted_slot ~
        -target_orientation_magnitude_design_mean,
      response_location == tilted_slot + 1 ~
        target_orientation_magnitude_design_mean,
      TRUE ~ 0
    ),
    target_orientation_moved_inward = case_when(
      tilted_slot == 1 ~ 0,
      response_location == tilted_slot ~
        -target_orientation_magnitude_design_mean,
      response_location == tilted_slot - 1 ~
        target_orientation_magnitude_design_mean,
      TRUE ~ 0
    ),
    target_orientation_copied_outward = case_when(
      tilted_slot == correct_num ~ 0,
      response_location == tilted_slot ~ 0,
      response_location == tilted_slot + 1 ~
        target_orientation_magnitude_design_mean,
      TRUE ~ 0
    ),
    target_orientation_copied_inward = case_when(
      tilted_slot == 1 ~ 0,
      response_location == tilted_slot ~ 0,
      response_location == tilted_slot - 1 ~
        target_orientation_magnitude_design_mean,
      TRUE ~ 0
    )
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      perfect_reconstruction,
      full_assimilation,
      pooling,
      one_non_target_also_target,
      target_mislocalized_to_one_non_target,
      target_orientation_moved_outward,
      target_orientation_moved_inward,
      target_orientation_copied_outward,
      target_orientation_copied_inward
    ),
    names_to = "hypothetical_condition",
    values_to = "orientation_error_mean"
  ) %>%
  mutate(
    hypothetical_condition = factor(
      hypothetical_condition,
      levels = c(
        "perfect_reconstruction",
        "full_assimilation",
        "pooling",
        "one_non_target_also_target",
        "target_mislocalized_to_one_non_target",
        "target_orientation_moved_outward",
        "target_orientation_moved_inward",
        "target_orientation_copied_outward",
        "target_orientation_copied_inward"
      ),
      labels = c(
        "Perfect reconstruction",
        "Full assimilation\n(all lines = target)",
        "Pooling\n(target averaged across locations)",
        "One additional\ntarget line",
        "Target at random\nnon-target",
        "Target moved\noutward",
        "Target moved\ninward",
        "Target copied\noutward",
        "Target copied\ninward"
      )
    )
  )


# Match the observed correct-number matrices so colors can be compared across
# figures. Values outside this displayed range use the endpoint colors.
matrix_color_limits <- c(-4, 4)
matrix_color_breaks <- seq(-4, 4, by = 2)


hypothetical_orientation_matrix_caption <- paste(
  "Diagonal: positive = overshoot; negative = undershoot.",
  "Off-diagonal: positive = target-direction tilt,",
  "negative = opposite-direction tilt; zero = no mean deviation."
)


# ------------plot function---------------
make_hypothetical_orientation_error_plot <- function(
    plot_data,
    set_size
) {
  expected_locations <- seq_len(set_size)
  matrix_cell_counts <- plot_data %>%
    count(hypothetical_condition, name = "n_cells")

  stopifnot(
    identical(
      sort(unique(plot_data$tilted_slot)),
      expected_locations
    ),
    identical(
      sort(unique(plot_data$response_location)),
      expected_locations
    ),
    all(matrix_cell_counts$n_cells == set_size^2)
  )

  n_matrices <- n_distinct(plot_data$hypothetical_condition)
  matrices_per_row <- if (n_matrices > 3) 4 else n_matrices

  ggplot(
    plot_data,
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
        label = paste0(
          round(orientation_error_mean, 1),
          "°"
        )
      ),
      size = if_else(set_size == 3, 4, 3.5),
      color = "black"
    ) +
    facet_wrap(
      ~ hypothetical_condition,
      ncol = matrices_per_row,
      labeller = labeller(
        hypothetical_condition = label_wrap_gen(width = 24)
      )
    ) +
    scale_x_continuous(
      breaks = seq_len(set_size)
    ) +
    scale_y_continuous(
      breaks = seq_len(set_size),
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
      title = paste0(
        "Hypothetical Orientation-Error Matrices\n",
        "Set Size ",
        set_size
      ),
      subtitle = paste0(
        "Expected cells use equally weighted design magnitudes ",
        "1, 2, 4, 6, and 8 degrees (mean = 4.2 degrees)"
      ),
      x = "Response Location (Inner to Outer)",
      y = "Target Location (Inner to Outer)"
    ) +
    my_plot_theme +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      strip.text.x = element_text(
        size = 8.5,
        lineheight = 0.9,
        margin = margin(3, 2, 3, 2)
      ),
      axis.title.x = element_text(size = 11, face = "bold"),
      axis.title.y = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 9),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8.5)
    ) +
    guides(
      fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 10,
        barheight = 0.6
      )
    )
}


# ------------plot: set size 3---------------
data_hypothetical_orientation_error_set_size_3 <-
  data_hypothetical_orientation_error %>%
  filter(correct_num == 3)


my_plot_hypothetical_orientation_error_set_size_3 <-
  make_hypothetical_orientation_error_plot(
    data_hypothetical_orientation_error_set_size_3,
    set_size = 3
  )


my_plot_hypothetical_orientation_error_set_size_3


# ------------plot: set size 4---------------
data_hypothetical_orientation_error_set_size_4 <-
  data_hypothetical_orientation_error %>%
  filter(correct_num == 4)


my_plot_hypothetical_orientation_error_set_size_4 <-
  make_hypothetical_orientation_error_plot(
    data_hypothetical_orientation_error_set_size_4,
    set_size = 4
  )


my_plot_hypothetical_orientation_error_set_size_4


# Suggested wide export size:
# ggsave(
#   file.path("src", "analysis", "hypothetical_correct_set_size_3.png"),
#   plot = my_plot_hypothetical_orientation_error_set_size_3,
#   width = 16,
#   height = 9,
#   units = "in",
#   dpi = 300
# )
# ggsave(
#   file.path("src", "analysis", "hypothetical_correct_set_size_4.png"),
#   plot = my_plot_hypothetical_orientation_error_set_size_4,
#   width = 16,
#   height = 9,
#   units = "in",
#   dpi = 300
# )

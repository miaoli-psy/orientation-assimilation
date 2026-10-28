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
# Use equal design weighting, as in the hypothetical correct-number matrices.
# The five target magnitudes have a design mean of 4.2 degrees.
target_orientation_magnitudes_design <- c(1, 2, 4, 6, 8)

target_orientation_magnitude_design_mean <- mean(
  target_orientation_magnitudes_design
)


# ------------design matrix locations---------------
# RM means reported number = presented set size - 1. Therefore, the matrix has
# one row for every possible PRESENTED target rank, but one column for every
# REPORTED response rank in the smaller response array.
#
# Set size 3: 3 target rows x 2 response columns.
# Set size 4: 4 target rows x 3 response columns.
#
# Rows and columns are both ordered inner to outer, but they refer to different
# arrays. A matching row and column number is not a uniquely correct response,
# because the omitted presented line is unknown.
data_hypothetical_RM_design_grid <- bind_rows(
  tidyr::expand_grid(
    correct_num = 3,
    reported_number = 2,
    tilted_slot = 1:3,
    response_location = 1:2
  ),
  tidyr::expand_grid(
    correct_num = 4,
    reported_number = 3,
    tilted_slot = 1:4,
    response_location = 1:3
  )
)


# ------------hypothetical RM orientation matrices---------------
# These matrices show expected TARGET-ALIGNED RESPONSE ORIENTATION, not error.
# Positive cells contain orientation in the target direction; zero cells contain
# no expected target-direction signal.
#
# Target omitted:
# The tilted target is the omitted line, so no reported line carries its signal.
#
# Random presented line omitted:
# Any presented line, including the target, is equally likely to be omitted.
# A cell is the expected target signal after averaging over those possibilities.
#
# Random non-target omitted:
# The target is always retained, but its response rank depends on whether the
# omitted non-target was inward or outward of it.
#
# Inward/outward non-target omitted:
# The target shifts or stays according to the side on which a non-target is
# omitted. At a boundary, the available non-target must be omitted instead.
#
# Target at one random response location:
# On each trial, one randomly chosen response location carries the full target.
# Its EXPECTED mean matrix is target magnitude / reported number in every cell.
#
# Pooling:
# On each trial, the target magnitude is divided equally across the REPORTED
# response locations. Therefore:
#
#   pooled orientation = target magnitude / reported number
#
# Set size 3 has two reported lines, so the mean pooling cell is 4.2 / 2 = 2.1.
# Set size 4 has three reported lines, so the mean pooling cell is 4.2 / 3 = 1.4.
#
# Pooling and "target at one random response location" have identical EXPECTED
# mean matrices. They differ at the trial level: pooling predicts a partial
# orientation at every response location, whereas random location predicts one
# full target response and zeros elsewhere. Mean matrices alone cannot separate
# these two hypotheses.
data_hypothetical_RM_orientation <-
  data_hypothetical_RM_design_grid %>%
  mutate(
    target_omitted = 0,

    one_presented_line_omitted_randomly = case_when(
      tilted_slot > 1 &
        response_location == tilted_slot - 1 ~
        target_orientation_magnitude_design_mean *
        (tilted_slot - 1) / correct_num,
      tilted_slot <= reported_number &
        response_location == tilted_slot ~
        target_orientation_magnitude_design_mean *
        (correct_num - tilted_slot) / correct_num,
      TRUE ~ 0
    ),

    target_retained_random_non_target_omitted = case_when(
      tilted_slot > 1 &
        response_location == tilted_slot - 1 ~
        target_orientation_magnitude_design_mean *
        (tilted_slot - 1) / (correct_num - 1),
      tilted_slot <= reported_number &
        response_location == tilted_slot ~
        target_orientation_magnitude_design_mean *
        (correct_num - tilted_slot) / (correct_num - 1),
      TRUE ~ 0
    ),

    inward_non_target_omitted = if_else(
      response_location == pmax(tilted_slot - 1, 1),
      target_orientation_magnitude_design_mean,
      0
    ),

    outward_non_target_omitted = if_else(
      response_location == pmin(tilted_slot, reported_number),
      target_orientation_magnitude_design_mean,
      0
    ),

    target_at_one_random_response_location =
      target_orientation_magnitude_design_mean /
      reported_number,

    pooling =
      target_orientation_magnitude_design_mean /
      reported_number,

    every_response_bar_has_target_orientation =
      target_orientation_magnitude_design_mean
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      target_omitted,
      one_presented_line_omitted_randomly,
      target_retained_random_non_target_omitted,
      inward_non_target_omitted,
      outward_non_target_omitted,
      target_at_one_random_response_location,
      pooling,
      every_response_bar_has_target_orientation
    ),
    names_to = "hypothetical_condition",
    values_to = "aligned_response_orientation_mean"
  ) %>%
  mutate(
    hypothetical_condition = factor(
      hypothetical_condition,
      levels = c(
        "target_omitted",
        "one_presented_line_omitted_randomly",
        "target_retained_random_non_target_omitted",
        "inward_non_target_omitted",
        "outward_non_target_omitted",
        "target_at_one_random_response_location",
        "pooling",
        "every_response_bar_has_target_orientation"
      ),
      labels = c(
        "Target omitted",
        "Random presented\nline omitted",
        "Random non-target\nomitted",
        "Inward non-target\nomitted",
        "Outward non-target\nomitted",
        "Target at random\nresponse location",
        "Pooling\n(divided by reported number)",
        "All response bars\n= target"
      )
    )
  )


# Match the hypothetical correct-number matrices and the observed diverging
# matrices. Values beyond the displayed range use the endpoint colors.
matrix_color_limits <- c(-4, 4)
matrix_color_breaks <- seq(-4, 4, by = 2)


hypothetical_RM_orientation_caption <- paste(
  "Cell = expected final target-aligned response orientation, not error.",
  "Rows are presented target ranks; columns are reported response ranks.",
  "Positive = target-direction signal; zero = no expected target signal."
)


# ------------how to read set size 3---------------
# For set size 3, three lines were presented but only two were reported.
#
#   Row 1: the target was the innermost presented line.
#   Row 2: the target was the middle presented line.
#   Row 3: the target was the outermost presented line.
#
#   Column 1: the innermost line in the two-line response array.
#   Column 2: the outermost line in the two-line response array.
#
# Example: in the pooling panel, every cell is 2.1 degrees because the mean
# target magnitude (4.2 degrees) is divided across the two REPORTED locations.
# For an individual 8-degree target, pooling predicts 8 / 2 = 4 degrees at
# both reported response locations, regardless of the presented target row.
RM_set_size_3_matrix_interpretation <- paste(
  "Set size 3 has three presented-target rows and two reported-response",
  "columns. Each cell gives the expected target-aligned orientation at that",
  "reported rank. Under pooling, target magnitude is divided by two, so the",
  "design-mean matrix contains 2.1 degrees in every cell."
)


# ------------plot function---------------
make_hypothetical_RM_orientation_plot <- function(
    plot_data,
    set_size
) {
  reported_size <- set_size - 1
  expected_target_locations <- seq_len(set_size)
  expected_response_locations <- seq_len(reported_size)

  matrix_cell_counts <- plot_data %>%
    count(hypothetical_condition, name = "n_cells")

  stopifnot(
    identical(
      sort(unique(plot_data$tilted_slot)),
      expected_target_locations
    ),
    identical(
      sort(unique(plot_data$response_location)),
      expected_response_locations
    ),
    all(
      matrix_cell_counts$n_cells ==
        set_size * reported_size
    )
  )

  n_matrices <- n_distinct(plot_data$hypothetical_condition)
  matrices_per_row <- if (n_matrices > 3) 4 else n_matrices

  ggplot(
    plot_data,
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
      breaks = expected_response_locations
    ) +
    scale_y_continuous(
      breaks = expected_target_locations,
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
      title = paste0(
        "Hypothetical RM Orientation Matrices\n",
        "Set Size ",
        set_size
      ),
      subtitle = paste0(
        "Presented ",
        set_size,
        " lines; reported ",
        reported_size,
        " lines. Expected cells use equally weighted target magnitudes ",
        "1, 2, 4, 6, and 8 degrees (mean = 4.2 degrees)."
      ),
      x = "Reported Response Location (Inner to Outer)",
      y = "Presented Target Location (Inner to Outer)"
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


# ------------plot: RM set size 3---------------
data_hypothetical_RM_orientation_set_size_3 <-
  data_hypothetical_RM_orientation %>%
  filter(correct_num == 3)


my_plot_hypothetical_RM_orientation_set_size_3 <-
  make_hypothetical_RM_orientation_plot(
    data_hypothetical_RM_orientation_set_size_3,
    set_size = 3
  )


cat(RM_set_size_3_matrix_interpretation, "\n")
my_plot_hypothetical_RM_orientation_set_size_3


 # ------------plot: RM set size 4---------------
data_hypothetical_RM_orientation_set_size_4 <-
  data_hypothetical_RM_orientation %>%
  filter(correct_num == 4)


my_plot_hypothetical_RM_orientation_set_size_4 <-
  make_hypothetical_RM_orientation_plot(
    data_hypothetical_RM_orientation_set_size_4,
    set_size = 4
  )


my_plot_hypothetical_RM_orientation_set_size_4


# Suggested wide export size: eight panels, four matrices per row.
# ggsave(
#   file.path("src", "analysis", "hypothetical_RM_set_size_3.png"),
#   plot = my_plot_hypothetical_RM_orientation_set_size_3,
#   width = 16,
#   height = 9,
#   units = "in",
#   dpi = 300
# )
# ggsave(
#   file.path("src", "analysis", "hypothetical_RM_set_size_4.png"),
#   plot = my_plot_hypothetical_RM_orientation_set_size_4,
#   width = 16,
#   height = 9,
#   units = "in",
#   dpi = 300
# )

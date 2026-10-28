# ------------library-----------------
library(dplyr)
library(readr)


# ------------settings----------------
# Run from the project folder, src, or src/preprocess.
project_dir <- c(".", "..", "../..")
project_dir <- project_dir[dir.exists(file.path(project_dir, "mydata"))][1]
if (is.na(project_dir)) stop("Set your working directory to the project folder.")

data_dir <- file.path(project_dir, "mydata")
output_file <- file.path(project_dir, "src/preprocess/data_preprocessed.csv")
failed_list <- c("507", "516", "517", "518", "525")

# The older files record ori accuracy as zero for every trial.
# Until their key mapping is confirmed, leave their derived accuracy as NA.
# If negative tilt = a and positive tilt = d, replace NA below with "a" and "d".
legacy_correct_keys <- c(negative = NA_character_, positive = NA_character_)


# ------------read and combine data----------------
data_files <- sort(list.files(data_dir, pattern = "\\.csv$", full.names = TRUE))
if (length(data_files) == 0) stop("No CSV files found in mydata.")

read_one_file <- function(file) {
  data <- read_csv(
    file,
    col_types = cols(.default = col_character()),
    col_select = any_of(c(
      "age", "gender", "date", "task_order", "block", "blocks.thisN", "loop.thisN",
      "hemisphere", "amount", "response_number", "ori", "oriented_line_location",
      "key_resp.rt", "rotation_response.keys", "rotation_response.rt",
      "orientation_correct_key"
    )),
    na = c("", "NA", "NaN", "nan", "None"),
    name_repair = "minimal",
    show_col_types = FALSE
  )

  if (nrow(problems(data)) > 0) stop("CSV parsing problem in ", basename(file))

  # Only the newer files provide the expected orientation key.
  if (!"orientation_correct_key" %in% names(data)) {
    data$orientation_correct_key <- NA_character_
  }

  # Use filename IDs: file 522 has participant = 512 inside the CSV.
  data$SubID <- sub("_.*$", "", basename(file))
  data
}

my_data <- bind_rows(lapply(data_files, read_one_file))


# ------------keep main trials and remove failed participants----------------
# Practice also has block = 1 in the older files; use blocks.thisN instead.
my_data <- my_data %>%
  filter(!is.na(blocks.thisN), !SubID %in% failed_list)


# ------------useful columns and derived variables----------------
my_data <- my_data %>%
  transmute(
    SubID,
    data_year = as.integer(substr(date, 1, 4)),
    age = as.numeric(age),
    gender,
    block = as.integer(block),
    trial = (block - 1L) * 60L + as.integer(loop.thisN) + 1L,
    task_order,
    hemisphere,
    correct_num = as.numeric(amount),
    response_number = as.numeric(response_number),
    count_error = response_number - correct_num,
    number_correct = as.integer(count_error == 0),
    # RM currently means any underestimation; count_error keeps its exact size.
    RM = case_when(
      count_error < 0 ~ "RM",
      count_error == 0 ~ "Correct",
      count_error > 0 ~ "Overestimate"
    ),
    ori_deg = as.numeric(ori),
    abs_ori = abs(ori_deg),
    oriented_line_location,
    ori_response = rotation_response.keys,
    ori_correct = as.integer(ori_response == coalesce(
      orientation_correct_key,
      if_else(ori_deg < 0, legacy_correct_keys[["negative"]], legacy_correct_keys[["positive"]])
    )),
    number_rt = as.numeric(key_resp.rt),
    ori_rt = as.numeric(rotation_response.rt)
  ) %>%
  arrange(SubID, trial)

my_data$RM <- factor(my_data$RM, levels = c("RM", "Correct", "Overestimate"))
my_data$task_order <- factor(my_data$task_order, levels = c("num_first", "ori_first"))
my_data$hemisphere <- factor(my_data$hemisphere, levels = c("left", "right"))
my_data$oriented_line_location <- factor(
  my_data$oriented_line_location,
  levels = c("inner", "middle_inner", "middle", "middle_outer", "outer")
)


# ------------check and save----------------
stopifnot(
  all(table(my_data$SubID) == 600L),
  !anyDuplicated(my_data[c("SubID", "trial")]),
  all(my_data$correct_num %in% c(3, 4)),
  all(my_data$abs_ori %in% c(2, 4, 6, 8, 10))
)

write_csv(my_data, output_file, na = "")
message("Saved ", nrow(my_data), " trials from ", n_distinct(my_data$SubID), " IDs to ", output_file)

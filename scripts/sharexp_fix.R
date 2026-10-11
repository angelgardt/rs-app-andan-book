library(tidyverse)

files_raw <- dir("data/sharexp/raw", full.names = TRUE)
share_raw <- tibble()

read_csv(files_raw[1], show_col_types = FALSE) |> colnames()
readxl::read_xlsx("data/sharexp/targetpositions.xlsx") |> colnames()


for (i in 1:length(files_raw)) {
  read_csv(files_raw[i]) |> 
    slice(1:450) |> 
    mutate(file = files_raw[i]) |> 
    bind_rows(share_raw) -> share_raw
}

share_raw |> View()

socdem_raw <- tibble()

for (i in 1:length(files_raw)) {
  n_rows <- nrow(read_csv(files_raw[i]))
  read_csv(files_raw[i]) |> 
    slice(451:n_rows) |> 
    mutate(file = files_raw[i]) |> 
    bind_rows(socdem_raw) -> socdem_raw
}

socdem_raw |> View()

socdem_raw |> 
  select(stim1, stim2, file) |> 
  filter(!is.na(stim1)) |> 
  pivot_wider(names_from = stim1, values_from = stim2)


for (i in 1:length(files_raw)) {
  read_csv(files_raw[i], show_col_types = FALSE) |> nrow() |> print()
}
read_csv(files_raw[1]) |> View()
filter(stim1 == "surname") |> 
  print()

read_csv(files_raw[i], show_col_types = FALSE) |>
  slice(1:453, 455:459) |> View()

for (i in 1:length(files_raw)) {
  read_csv(files_raw[i], show_col_types = FALSE) |>
    slice(1:453, 455:459) |> 
    write_csv(paste0("/Users/antonangelgardt/Desktop/sharexp_anon/", 
                     files_raw[i] |> str_extract("\\d{2}\\.csv$")))
}


read_csv("data/sharexp/share_platform.csv") |> 
  write_delim("data/sharexp/share_platform.csv", delim = " ")

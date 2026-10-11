# sharexp: подготовка единого датасета из сырых данных PsychoPy
#
# Что делает скрипт:
#   1. читает 20 файлов data/sharexp/raw/*.csv (по одному на испытуемого);
#   2. оставляет только пробы и нужные столбцы, переименовывает их;
#   3. достаёт из служебных строк возраст и дату сессии;
#   4. присоединяет платформу смартфона (share_platform.csv)
#      и координаты целевых стимулов (targetpositions.xlsx);
#   5. считает правильность ответа (попал ли первый клик в целевой стимул);
#   6. сохраняет результат в data/sharexp/share_clean.csv.
#
# Это тот же код, который разбирается в главе «Продвинутая предобработка данных».
# Запускать из корня проекта (рабочая директория = папка с rs-app-andan-book.Rproj).

library(tidyverse)
library(readxl)

files <- dir("data/sharexp/raw", full.names = TRUE)

# --- пробы -------------------------------------------------------------------
preprocess <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    filter(!is.na(numtrial)) |>
    select(numtrial, trialtype, setsize,
           starts_with("mouse") & ends_with("_raw") & !contains("Button")) |>
    rename(time1 = mouse_main1.time_raw,
           click1x = mouse_main1.x_raw,
           click1y = mouse_main1.y_raw,
           time2 = mouse_main2.time_raw,
           click2x = mouse_main2.x_raw,
           click2y = mouse_main2.y_raw) |>
    mutate(id = parse_number(basename(path)), .before = 1)
}

share <- files |> map(preprocess) |> list_rbind()

# --- информация об испытуемых (служебные строки в конце файлов) --------------
read_meta <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    filter(is.na(numtrial), !is.na(stim2)) |>
    select(key = stim1, value = stim2) |>
    filter(key %in% c("age", "date")) |>   # только нужное; персональные данные не берём
    pivot_wider(names_from = key, values_from = value) |>
    mutate(id = parse_number(basename(path)), .before = 1)
}

meta <- files |> map(read_meta) |> list_rbind() |>
  mutate(age = as.numeric(age))

platform <- read_delim("data/sharexp/share_platform.csv", delim = " ", show_col_types = FALSE)

subjects <- meta |> left_join(platform, by = "id")

# --- координаты целей ---------------------------------------------------------
targets <- read_xlsx("data/sharexp/targetpositions.xlsx") |>
  select(!ends_with("tr")) |>
  drop_na(numtrial)

# --- объединение и расчёт точности -------------------------------------------
share <- share |>
  left_join(subjects, by = "id") |>
  left_join(targets, by = "numtrial") |>
  mutate(hit1 = between(click1x, posxmin1, posxmax1) & between(click1y, posymin1, posymax1),
         hit2 = between(click1x, posxmin2, posxmax2) & between(click1y, posymin2, posymax2),
         correct = hit1 | hit2) |>
  select(id, age, platform, date, numtrial, trialtype, setsize,
         time1, click1x, click1y, correct)

stopifnot(nrow(share) == 20 * 450)

write_csv(share, "data/sharexp/share_clean.csv")

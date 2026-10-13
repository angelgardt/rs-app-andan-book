# Генерация расширенного датасета diamonds для тренировки предобработки и визуализации
# Результат: data/diamonds-ext/*.csv
# Запуск из корня проекта: Rscript scripts/diamonds-ext-generate.R

library(tidyverse)

Sys.setenv(TZ = "UTC")
set.seed(20261013)
out_dir <- "data/diamonds-ext"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

n <- nrow(diamonds)

# ---- id ----
d <- diamonds |>
  mutate(id = sprintf("DM-%05d", row_number()), .before = 1)

# ---- лаборатории (с «грязным» написанием) ----
lab_codes <- c("GIA", "IGI", "HRD", "MGL")
lab_true <- sample(lab_codes, n, replace = TRUE, prob = c(.45, .30, .15, .10))
lab_messy <- lab_true
k <- sample(n, round(n * .12))
lab_messy[k] <- map_chr(lab_true[k], \(x) sample(c(
  str_to_lower(x), str_to_title(x), paste0(x, " "), paste0(" ", x), paste0(str_to_lower(x), "  ")
), 1))

# ---- даты ----
# дата поступления образца: 2023-01-01 ... 2025-12-31, будни чаще
all_days <- seq(as.Date("2023-01-01"), as.Date("2025-12-31"), by = "day")
w <- if_else(wday(all_days, week_start = 1) >= 6, .25, 1)
# лёгкая сезонность: перед Новым годом и 8 Марта поступает больше
w <- w * (1 + .6 * (month(all_days) %in% c(11, 12)) + .3 * (month(all_days) == 2))
recv <- sample(all_days, n, replace = TRUE, prob = w)

# дата экспертизы: через 1–45 дней, у HRD и MGL дольше; дорогие камни изучают дольше
lag_exam <- round(rgamma(n, shape = 2, rate = .25)) + 1 +
  if_else(lab_true %in% c("HRD", "MGL"), 5, 0) + round(log(d$price) - 5)
lag_exam <- pmax(lag_exam, 1)
exam <- recv + lag_exam
# ~0.2% ошибок ввода: экспертиза раньше поступления
err <- sample(n, 120)
exam[err] <- recv[err] - sample(1:20, 120, replace = TRUE)

# продажа: вероятность продажи выше у хорошей огранки и умеренной цены
cut_num <- as.integer(d$cut)
p_sold <- plogis(1.2 + .25 * (cut_num - 3) - .00004 * d$price)
sold_flag <- runif(n) < p_sold
lag_sale <- round(rexp(n, rate = 1 / (40 + d$price / 120))) + 1
sale_date <- exam + lag_sale
sold_flag <- sold_flag & sale_date <= as.Date("2026-06-30")
sale_time <- as.POSIXct(sale_date) +
  hours(sample(10:20, n, replace = TRUE)) + minutes(sample(0:59, n, replace = TRUE))
sale_time[!sold_flag] <- NA

# цена продажи: скидка/наценка относительно прайсовой цены
disc <- rnorm(n, mean = -.04 - .002 * pmin(lag_sale, 300) / 10, sd = .04)
sale_price <- if_else(sold_flag, round(d$price * (1 + disc)), NA_real_)

# ---- текстовая колонка: заключение геммолога ----
fluor <- sample(c("нет", "слабая", "средняя", "сильная"), n, replace = TRUE,
                prob = c(.55, .25, .14, .06))
origin <- sample(c("Ботсвана", "Россия, Якутия", "Канада", "ЮАР", "Австралия", "Ангола", NA),
                 n, replace = TRUE, prob = c(.25, .25, .12, .12, .06, .05, .15))
cert <- sprintf("%010.0f", runif(n, 1e9, 1e10 - 1))
defects_pool <- c("Небольшой скол на рундисте.",
                  "Включение у павильона.",
                  "Лёгкий коричневатый оттенок.",
                  "Царапина на площадке.",
                  "Рекомендована переогранка.",
                  "Природная грань (натурал) на рундисте.")
defect <- map_chr(seq_len(n), \(i) {
  # у камней с худшей чистотой дефекты отмечают чаще
  p <- c(.45, .35, .3, .2, .15, .1, .08, .05)[as.integer(d$clarity[i])]
  if (runif(1) < p) paste(sample(defects_pool, sample(1:2, 1, prob = c(.8, .2))), collapse = " ")
  else "Видимых дефектов нет."
})
fluor_word <- sample(c("Флуоресценция", "флуоресценция", "Флюоресценция"), n,
                     replace = TRUE, prob = c(.8, .12, .08))
comment <- str_c(
  "Сертификат ", lab_true, " № ", cert, ". ",
  fluor_word, ": ", fluor, ". ",
  if_else(is.na(origin), "", str_c("Происхождение: ", origin, ". ")),
  defect
)
# немного «шума»: лишние пробелы, пропуски
k <- sample(n, round(n * .05)); comment[k] <- str_replace_all(comment[k], " ", "  ")
k <- sample(n, round(n * .03)); comment[k] <- str_c("  ", comment[k], " ")
comment[sample(n, round(n * .02))] <- NA

# ---- основная таблица ----
d_ext <- d |>
  mutate(
    lab        = lab_messy,
    recv_date  = format(recv, "%Y-%m-%d"),          # ISO
    exam_date  = format(exam, "%d.%m.%Y"),          # «русский» формат
    sale_dttm  = format(sale_time, "%d/%m/%Y %H:%M"), # дата-время
    sale_price = sale_price,
    comment    = comment
  )

# дубликаты строк (ошибка выгрузки)
dup <- d_ext |> slice_sample(n = 75)
d_ext <- bind_rows(d_ext, dup) |> slice_sample(prop = 1)

d_ext <- d_ext |> 
  unite(col = "color_clarity", color, clarity)


write_csv(d_ext, file.path(out_dir, "diamonds_ext.csv"), na = "")

# ---- оценки трёх независимых экспертов (широкий формат) ----
ids <- sample(d$id, 1500)
appr <- d |>
  filter(id %in% ids) |>
  transmute(id,
            appraiser_A = round(price * rnorm(n(), 1.00, .06), -1),
            appraiser_B = round(price * rnorm(n(), 0.93, .05), -1),
            appraiser_C = round(price * rnorm(n(), 1.08, .10), -1))
appr$appraiser_B[sample(nrow(appr), 120)] <- NA
appr$appraiser_C[sample(nrow(appr), 60)] <- NA
write_csv(appr, file.path(out_dir, "diamonds_appraisals.csv"), na = "")

# ---- справочники ----
color_scale <- tibble(
  color = c("D", "E", "F", "G", "H", "I", "J", "K"),
  color_group = c(rep("бесцветный", 3), rep("почти бесцветный", 4), "слабо окрашенный")
)
write_csv(color_scale, file.path(out_dir, "color_scale.csv"))

clarity_scale <- tibble(
  clarity = c("FL", "IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2", "I1", "I2", "I3"),
  clarity_rank = 1:11,
  clarity_desc = c("безупречный", "внутренне безупречный",
                   "очень-очень мелкие включения", "очень-очень мелкие включения",
                   "очень мелкие включения", "очень мелкие включения",
                   "мелкие включения", "мелкие включения",
                   "включения, видимые глазом", "включения, видимые глазом",
                   "включения, видимые глазом")
)
write_csv(clarity_scale, file.path(out_dir, "clarity_scale.csv"))

labs <- tibble(
  lab_code = c("GIA", "IGI", "HRD", "MGL", "AGS"),
  lab_name = c("Gemological Institute of America",
               "International Gemological Institute",
               "Hoge Raad voor Diamant",
               "Московская геммологическая лаборатория",
               "American Gem Society Laboratories"),
  lab_country = c("США", "Бельгия", "Бельгия", "Россия", "США")
)
write_csv(labs, file.path(out_dir, "labs.csv"))


set.seed(123)
d_ext10000 <- d_ext |> slice_sample(n = 10000)
write_csv(d_ext10000, file.path(out_dir, "diamonds_ext_small.csv"), na = "")
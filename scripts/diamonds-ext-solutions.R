# Решения к заданиям «Предобработка и визуализация: бриллианты»
# Запуск из корня проекта: Rscript scripts/diamonds-ext-solutions.R

library(tidyverse)
Sys.setenv(TZ = "UTC")
path <- "data/diamonds-ext/"

# ==== Часть 1. Импорт и первое знакомство ====

# 1
raw <- read_csv(file.path(path, "diamonds_ext.csv"))
spec(raw)

# 2
ncol(raw); nrow(raw); dim(raw)

# 3
tibble(var = names(raw),
       class = map_chr(raw, \(x) str_c(class(x), collapse = "/")),
       typeof = map_chr(raw, typeof))
tibble(var = names(diamonds),
       class = map_chr(diamonds, \(x) str_c(class(x), collapse = "/")),
       typeof = map_chr(diamonds, typeof))

# 4
colSums(is.na(raw))
names(raw)[colSums(is.na(raw)) > 0]
all(is.na(raw$sale_dttm) == is.na(raw$sale_price))

# 5
raw |> filter(x == 0 | y == 0 | z == 0) |> distinct() |> nrow()

# 6
sum(duplicated(raw))
raw |> count(id) |> filter(n > 1) |> nrow()
dm <- raw |> distinct()
nrow(dm)

# 7
dm <- dm |>
  mutate(cut = factor(cut, levels = levels(diamonds$cut), ordered = TRUE),
         color = factor(color, levels = levels(diamonds$color), ordered = TRUE),
         clarity = factor(clarity, levels = levels(diamonds$clarity), ordered = TRUE))
dm |> count(cut)

# ==== Часть 2. select, rename, filter, arrange ====

# 8
dm <- dm |>
  rename(date_received = recv_date, date_examined = exam_date,
         datetime_sold = sale_dttm,
         length_mm = x, width_mm = y, height_mm = z)
names(dm)

# 9
dm |> select(id, cut:clarity, starts_with("date"))

# 10
dm |> relocate(price, .before = sale_price) |> names()

# 11
dm |> filter(cut == "Good", color == "D") |> nrow()

# 12
q12 <- dm |>
  filter(between(carat, 1, 1.5), color %in% c("D", "E", "F"), is.na(sale_price)) |>
  arrange(desc(price))
nrow(q12); q12 |> slice(1) |> select(id, carat, color, price)

# 13
dm |> slice_max(carat, n = 10) |> select(id, carat, price)
dm |> mutate(ppc = price / carat) |> slice_min(ppc, n = 10) |> select(id, carat, price, ppc)

# 14
dm |> arrange(desc(cut), price) |> select(id, cut, price) |> head()
dm |> arrange(desc(as.character(cut)), price) |> select(id, cut, price) |> head()

# ==== Часть 3. mutate, group_by, summarise ====

# 15
dm |> count(cut, sort = TRUE) |> mutate(pct = round(n / sum(n) * 100, 1))

# 16
dm |> group_by(cut, color, clarity) |>
  summarise(mean_price = mean(price), n = n(), .groups = "drop") |>
  arrange(mean_price) |> head(5)

# 17
dm |> group_by(color) |>
  summarise(sd_depth = sd(depth), iqr_depth = IQR(depth)) |>
  arrange(desc(sd_depth))

# 18
dm |> slice_head(n = 1000) |>
  filter(clarity != "IF") |>
  mutate(price_cat = case_when(
    price < mean(price) - sd(price) ~ "low",
    price > mean(price) + sd(price) ~ "high",
    .default = "medium"),
    price_cat = factor(price_cat, levels = c("low", "medium", "high"))) |>
  group_by(price_cat) |>
  summarise(n = n(), min_price = min(price), max_price = max(price))

# 19
dm <- dm |> mutate(price_per_carat = price / carat,
                   volume = length_mm * width_mm * height_mm)
dm |> group_by(clarity) |>
  summarise(median_price = median(price),
            median_ppc = median(price_per_carat),
            median_carat = median(carat))

# 20
dm |> filter(!is.na(sale_price)) |>
  mutate(discount_pct = (sale_price - price) / price * 100) |>
  group_by(cut) |>
  summarise(across(c(price, sale_price), list(mean = mean, median = median)),
            mean_discount = mean(discount_pct))

# ==== Часть 4. Строки ====

# 21
n_distinct(dm$lab)
dm <- dm |> mutate(lab = str_to_upper(str_trim(lab)))
n_distinct(dm$lab); dm |> count(lab)

# 22
sum(str_detect(dm$comment, "\\s{2,}|^\\s|\\s$"), na.rm = TRUE)
dm <- dm |> mutate(comment = str_squish(comment))

# 23
dm <- dm |> mutate(cert_number = str_extract(comment, "\\d{10}"))
sum(is.na(dm$cert_number)); n_distinct(dm$cert_number, na.rm = TRUE)

# 24
dm |> mutate(lab_from_comment = str_extract(comment, "(?<=Сертификат )[A-Z]+")) |>
  filter(!is.na(comment)) |>
  summarise(all_equal = all(lab == lab_from_comment), n_mismatch = sum(lab != lab_from_comment))

# 25
dm <- dm |>
  mutate(fluorescence = str_match(str_to_lower(comment), "фл[ую]оресценция: (\\w+)")[, 2],
         fluorescence = factor(fluorescence, levels = c("нет", "слабая", "средняя", "сильная"),
                               ordered = TRUE))
dm |> count(fluorescence)

# 26
dm <- dm |>
  mutate(origin = str_extract(comment, "(?<=Происхождение: )[^.]+")) |>
  separate_wider_delim(origin, delim = ", ", names = c("origin_country", "origin_region"),
                       too_few = "align_start", cols_remove = FALSE)
dm |> count(origin_country, origin_region, sort = TRUE)
sum(is.na(dm$origin) & !is.na(dm$comment))

# 27
dm <- dm |> mutate(has_defect = !str_detect(comment, "Видимых дефектов нет"))
dm |> group_by(clarity) |> summarise(defect_share = mean(has_defect, na.rm = TRUE))
tibble(defect = c("скол", "включение", "оттенок", "царапина", "переогранка", "натурал")) |>
  mutate(n = map_int(defect, \(p) sum(str_detect(str_to_lower(dm$comment), p), na.rm = TRUE))) |>
  arrange(desc(n))

# 28
dm |> mutate(lot = str_glue("{format(carat, nsmall = 2)} ct, {cut}, {color}/{clarity}")) |>
  select(id, lot) |> head(3)

# ==== Часть 5. Даты ====

# 29
dm <- dm |> mutate(date_examined = dmy(date_examined),
                   datetime_sold = dmy_hm(datetime_sold))
colSums(is.na(select(dm, starts_with("date"))))

# 30
dm |> filter(date_examined < date_received) |> nrow()
dm <- dm |> filter(date_examined >= date_received)
nrow(dm)

# 31
dm <- dm |> mutate(exam_days = as.numeric(date_examined - date_received))
dm |> group_by(lab) |> summarise(mean_days = mean(exam_days), median_days = median(exam_days)) |>
  arrange(desc(mean_days))

# 32
dm <- dm |> mutate(days_to_sale = as.numeric(as_date(datetime_sold) - date_examined))
cor(dm$price, dm$days_to_sale, use = "complete.obs")
dm |> mutate(price_q = ntile(price, 4)) |> group_by(price_q) |>
  summarise(median_days_to_sale = median(days_to_sale, na.rm = TRUE))

# 33
dm |> count(month = month(date_received, label = TRUE, abbr = FALSE), sort = TRUE) |> head(3)
dm |> filter(!is.na(datetime_sold)) |>
  count(wday = wday(datetime_sold, label = TRUE, week_start = 1), sort = TRUE)
dm |> filter(!is.na(datetime_sold)) |>
  mutate(part = case_when(hour(datetime_sold) < 13 ~ "до 13",
                          hour(datetime_sold) < 17 ~ "13–17",
                          .default = "после 17")) |>
  count(part)

# 34
dm |> filter(!is.na(datetime_sold)) |>
  group_by(quarter = floor_date(datetime_sold, "quarter")) |>
  summarise(revenue = sum(sale_price), n = n()) |> print(n = 20)

# 35
d0 <- ymd("2025-01-01")
dm |> filter(date_received < d0, is.na(datetime_sold) | as_date(datetime_sold) >= d0) |> nrow()

# 36
dm |> filter(is.na(datetime_sold)) |>
  mutate(days_waiting = as.numeric(ymd("2026-06-30") - date_examined)) |>
  summarise(n = n(), median = median(days_waiting), max = max(days_waiting))

# ==== Часть 6. Объединение таблиц ====

color_scale <- read_csv(file.path(path, "color_scale.csv"))
clarity_scale <- read_csv(file.path(path, "clarity_scale.csv"))
labs <- read_csv(file.path(path, "labs.csv"))
appr <- read_csv(file.path(path, "diamonds_appraisals.csv"))

# 37
dm <- dm |> left_join(color_scale, by = "color")
dm |> count(color_group)
color_scale |> anti_join(dm |> mutate(color = as.character(color)), by = "color")

# 38
dmc <- dm |> mutate(clarity = as.character(clarity))
c(left = nrow(left_join(dmc, clarity_scale, by = "clarity")),
  inner = nrow(inner_join(dmc, clarity_scale, by = "clarity")),
  right = nrow(right_join(dmc, clarity_scale, by = "clarity")),
  full = nrow(full_join(dmc, clarity_scale, by = "clarity")))

# 39
raw |> left_join(labs, by = join_by(lab == lab_code)) |> filter(is.na(lab_name)) |> nrow()
dm <- dm |> left_join(labs, by = join_by(lab == lab_code))
sum(is.na(dm$lab_name))
labs |> anti_join(dm, by = join_by(lab_code == lab))

# 40
dm |> semi_join(appr, by = "id") |> nrow()
dm |> anti_join(appr, by = "id") |> nrow()
appr |> anti_join(dm, by = "id") |> nrow()  # попали в ошибочные строки из задания 30

# ==== Часть 7. Широкий и длинный формат ====

# 41
appr_long <- appr |>
  pivot_longer(starts_with("appraiser_"), names_to = "appraiser", values_to = "estimate",
               names_prefix = "appraiser_", values_drop_na = TRUE)
nrow(appr_long)

# 42
appr_long |> inner_join(select(dm, id, price), by = "id") |>
  mutate(dev_pct = (estimate - price) / price * 100) |>
  group_by(appraiser) |>
  summarise(mean_dev = mean(dev_pct), sd_dev = sd(dev_pct), n = n())

# 43
pivot_cc <- dm |> group_by(cut, color) |>
  summarise(median_price = median(price), .groups = "drop") |>
  pivot_wider(names_from = color, values_from = median_price)
pivot_cc

# 44
dm |> filter(!is.na(datetime_sold)) |>
  count(year = year(datetime_sold), quarter = str_c("Q", quarter(datetime_sold))) |>
  pivot_wider(names_from = quarter, values_from = n, values_fill = 0)

# 45
pivot_cc |> pivot_longer(-cut, names_to = "color", values_to = "median_price")

# ==== Часть 8. Визуализация ====

dir.create("img/diamonds-ext", showWarnings = FALSE, recursive = TRUE)
sv <- \(name) ggsave(file.path("img/diamonds-ext", name), width = 7, height = 4.5, dpi = 100)

# 46
ggplot(dm, aes(cut)) + geom_bar(); sv("46.png")
# 47
ggplot(dm, aes(cut, fill = clarity)) + geom_bar(position = "fill"); sv("47.png")
# 48
dm |> filter(clarity == "IF") |>
  ggplot(aes(depth, price)) + geom_point(alpha = .3) + geom_smooth(); sv("48.png")
# 49
dm |> filter(length_mm > 0) |>
  ggplot(aes(color, length_mm, fill = cut)) + geom_boxplot(); sv("49.png")
# 50
ggplot(dm, aes(price)) + geom_histogram(bins = 50) + scale_x_log10() + facet_wrap(~cut); sv("50.png")
# 51
dm |> count(month = floor_date(date_received, "month"), lab) |>
  ggplot(aes(month, n, color = lab)) + geom_line(); sv("51.png")
# 52
ggplot(dm, aes(lab, exam_days)) + geom_violin() + geom_boxplot(width = .1); sv("52.png")
# 53
dm |> filter(!is.na(datetime_sold)) |>
  count(wday = wday(datetime_sold, label = TRUE, week_start = 1), hour = hour(datetime_sold)) |>
  ggplot(aes(hour, wday, fill = n)) + geom_tile(); sv("53.png")
# 54
appr_long |> inner_join(select(dm, id, price), by = "id") |>
  ggplot(aes(price, estimate, color = appraiser)) +
  geom_point(alpha = .3) + geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_log10() + scale_y_log10(); sv("54.png")
# 55
dm |> group_by(cut, clarity, color_group) |>
  summarise(med_ppc = median(price_per_carat), .groups = "drop") |>
  ggplot(aes(clarity, med_ppc, color = color_group, group = color_group)) +
  geom_line() + geom_point() +
  facet_wrap(~cut, nrow = 1) +
  labs(x = "Чистота (от худшей к лучшей)", y = "Медианная цена за карат, $",
       color = "Группа цвета",
       title = "Медианная цена за карат по чистоте, огранке и группе цвета") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5), legend.position = "bottom")
sv("55.png")

# ==== Часть 9. Итоговое задание ====
# 56
dm |> filter(!is.na(days_to_sale), !is.na(fluorescence)) |>
  group_by(fluorescence) |>
  summarise(median_days = median(days_to_sale), mean_disc = mean((sale_price - price) / price * 100), n = n())
write_csv(dm, file.path(path, "diamonds_clean.csv"))

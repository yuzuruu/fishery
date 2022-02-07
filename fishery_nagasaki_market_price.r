############################################################################## 
# おさかな天国：長崎魚市鮮魚価格日報データを取得する
# 2020年1月27日
# 宇都宮　譲　（長崎大学経済学部）
# 
# 工程
# 1.鮮魚速報pdfをスクレイピングする：手動にてダウンロード済。
# 2.鮮魚速報pdfファイルを読み込んで使いやすいデータ形式に整える。
# 3.いろいろ処理する。
# 
# オブジェクト階層図
# readyと書いてある箇所はコードが完成した。
# 書いてない場所はつくっている途中。
# 001. fish_price --- 011. fish_price_live --- 111. fish_price_live_floor (ready)
#                 |                        | 
#                 |                        --- 112. fish_price_live_swim
#                 |
#                 --- 021. fish_price_box
#                 |
#                 --- 031. fish_price_trawl
#                 |
#                 --- 041. fish_price_purse
# 
# 
### ---- read.library ----
# ライブラリを読み込む
library(tidyverse)
library(tabulizer)
library(janitor)
library(ragtop)
library(rvest)
library(cmdstanr)
library(bayesplot)
# 
# ### ---- read.pdf.files ---- 
# 使わないときはコメントアウトする
# 処理に時間がかかるから
# # ファイル名を読み込む。
# # 自動読み込みに備えて、ファイル名はオブジェクトに収納する。
# # 複数になってもおなじこと。
# # とりあえず単数ファイルで試して読み込む作業がうまくいってから複数ファイル自動読み込み化へ移行する。
# file_names <- 
#   data.frame(file_name = list.files("fish_price_nagasaki")) %>% 
#   dplyr::mutate(
#     file_name = paste0("./fish_price_nagasaki/", file_name)
#   )
# # 複数ファイルをいっぺんに読み込む。
# # かなり時間がかかるから注意。
# # どのくらい？それはわからんというくらい。
# # 17時40分から17時52分までかかった。
# # 001
# fish_price <- 
#   file_names %>% 
#   group_by(file_name) %>% 
#   dplyr::mutate(
#     price_file = purrr::map(
#       .,
#       ~
#         tabulizer::extract_tables(
#           file = file_name, 
#           output = "data.frame"
#         ) %>% 
#         purrr::map_dfr(as.data.frame)
#         # purrr::map(as.data.frame)
#     )
#   ) %>% 
#   dplyr::mutate(
#     year_month_date = lubridate::ymd(
#       str_sub(
#         file_name,
#         start = -12,
#         end = -5
#       )
#     )
#   ) %>% 
#   dplyr::select(file_name, year_month_date, price_file)
# 
# 読んだデータを保存する。
# 上記コードはかなり時間がかかるからねぇ。
# ファイルサイズ事態は大したことない。388kくらい。
# 使わないときはコメントアウトする。
# saveRDS(
#   fish_price,
#   "fish_price.rds"
#   )
fish_price <-
  readRDS(
    "fish_price.rds"
    )
# 
### ---- read.data.by.part ----
# 必要な箇所に分割して読み込む。
# 近海物と底びき、まき網は含まれる情報が異なるから、切り分ける。
# 近海物も活魚台物と泳ぎ、箱値に、その他も底曳箱値と旋網箱値とに切り分ける。
# 参考ページ
# 空白blankを除去する方法
# どういうわけか、pdfファイルを読み込んだとき、blankができちゃった。
# blankを除去すれば、ファイルサイズも見た目もよくできるから除去する。
# https://community.rstudio.com/t/removing-blanks-nas/27887/3
# ここは活魚。台物と泳ぎを含む
# 011
fish_price_live <- 
  fish_price %>%
  dplyr::mutate(
    price_file_live = purrr::map(
      price_file,
      ~
        # 見た目に不要な列は使わない。
        dplyr::select(.data = ., c(1, 3, 4))%>% 
        # 読み込んだ列に名前をつける。
        # 第2列は空白列になりがちだから除去。
        data.table::setnames(
          c(
            "species", # 魚種
            "v2", # いろいろ混ざった列。後から切り分ける
            "low" # 底値
          )
        ) %>% 
        # 全列から空白blankを探してNAに置換
        mutate(
          dplyr::across(
            everything(), 
            ~ ifelse(
              . %in% c("NA", "null", ""), # NAかnullかblankかいずれかを見つけたら 
              NA,  # NAに置き換える
              . # 条件に当てはまらないものはそのまま。
            )
          )
        ) %>% 
        # すべての要素がNAである行のみ除去
        dplyr::filter(
          Reduce(`|`, 
                 dplyr::across(
                   dplyr::all_of(colnames(.)
                   ), 
                   ~
                     !is.na(.)
                 )
          )
        )
    )
  )
# 
# カテゴリ毎にデータを整理する
# まずは近海活魚台物
# 参考ページ
# 条件にあてはまる要素があるデータの行番号を取得する
# https://stackoverflow.com/questions/68843681/extracting-data-by-row-number-after-a-set-condition
# 
# https://stackoverflow.com/questions/42744274/string-manipulation-in-a-column
# 111
# 
# write_excel_csv(bind_rows(fish_price_live_floor$price_file_live_floor), "hoge.csv")
fish_price_live_floor <- 
  fish_price_live %>%
  dplyr::mutate(
    price_file_live_floor = purrr::map(
      price_file_live,
      ~
        dplyr::slice(.data = ., -1) %>% 
        # 行番号を含む列を新規に挿入する
        dplyr::mutate(
          rownumber = row_number()
        ) %>% 
        # 「泳」という文字を含む行の番号を含む列を新規に挿入する。
        # 「泳」という文字を含む行にて、台物と泳ぎ物が区分される。
        # なので、上記行よりも上（小さい行番号）か下（大きい行番号）かで
        # 台物か泳ぎかを区分する。
        dplyr::mutate(
          targetnumber = dplyr::nth(
            .$rownumber, 
            (
              which(
                str_detect(
                  .$v2, 
                  "泳"
                )
              )
            )
          )
        ) %>% 
        # ここがポイントです
        # 家族のポイント　釣りのポイント
        # 台物：<
        # 泳ぎ：>
        dplyr::filter(rownumber < targetnumber) %>% 
        # 重さをあらわす単位Kという文字を除去（Kをなにもない文字に置換）する。
        dplyr::mutate(
          v2 = stringr::str_replace(
            v2, 
            # _Kを
            pattern = "[:blank:]K",
            # なにもない文字に置き換える
            replacement = ""
          )
        ) %>% 
        # 変数v2に含まれる文字列を、半角空白を用いて切り分ける
        tidyr::separate(
          col = v2,
          # 3つに切り分ける
          into = c(
            "volume",
            "high",
            "medium"
          ),
          # 区切るために用いる半角空白
          sep = " "
        ) %>% 
        dplyr::select(
          species, 
          volume, 
          high, 
          medium, 
          low
        ) %>% 
        # 1列目だけ除去する。1列目には余計な文字列がある
        dplyr::slice(-1) %>% 
        # 桁区切りカンマのみを、数量と価格が入っている列から除去する
        dplyr::mutate(
          across(
            .cols = -species, 
            ~ as.numeric(
              stringr::str_replace(
                .,
                pattern = "[:punct:]", 
                replacement = ""
              )
            )
          )
        ) %>%
        # 台物floorであることを示す変数と、年月日を示す変数とを各々新規に挿入する。
        dplyr::mutate(
          type = factor("floor"),
          year_month_date = year_month_date
          # year_month_date = lubridate::ymd(
          #   str_sub(
          #     file_names, 
          #     start = 1, 
          #     end = 8
          #   )
          # ) 
        ) %>% 
        # 変数順序を並び替える
        dplyr::select(
          year_month_date, 
          species, 
          type, 
          volume, 
          high, 
          medium, 
          low
        ) %>% 
        as_tibble() %>% 
        # 整然とさせる
        tidyr::pivot_longer(
          cols = c("high", "medium", "low"),
          names_to = "status",
          values_to = "price"
        )
    )
  )
# 魚価データのみデータフレームにまとめる
# ここは台物。
# これまでいろいろ工夫してdfにしてきたけど、
# dplyr::bind_rows()だけでできるとは知らなかった。
fish_price_live_floor_df <- 
  fish_price_live_floor$price_file_live_floor %>%
  dplyr::bind_rows(.)
# 
# 
# 近海活魚泳ぎ物
# 112
# 
fish_price_live_swim <- 
  fish_price_live %>%
  dplyr::mutate(
    price_file_live_swim = purrr::map(
      price_file_live,
      ~
        dplyr::slice(.data = ., -1) %>% 
        # 行番号を含む列を新規に挿入する
        dplyr::mutate(
          rownumber = row_number()
        ) %>% 
        # 「活」という文字を含む行の番号を含む列を新規に挿入する。
        # 「活」という文字を含む行にて、台物と泳ぎ物が区分される。
        # なので、上記行よりも上（小さい行番号）か下（大きい行番号）かで
        # 台物か泳ぎかを区分する。
        dplyr::mutate(
          targetnumber = dplyr::nth(
            .$rownumber, 
            (
              which(
                str_detect(
                  .$v2, 
                  "活"
                )
              )
            )
          )
        ) %>% 
        # ここがポイントです
        # 家族のポイント　釣りのポイント
        # 台物：<
        # 泳ぎ：>
        dplyr::filter(rownumber > targetnumber) %>% 
        # 重さをあらわす単位Kという文字を除去（Kをなにもない文字に置換）する。
        dplyr::mutate(
          v2 = stringr::str_replace(
            v2, 
            # _Kを
            pattern = "[:blank:]K",
            # なにもない文字に置き換える
            replacement = ""
          )
        ) %>% 
        # 変数v2に含まれる文字列を、半角空白を用いて切り分ける
        tidyr::separate(
          col = v2,
          # 3つに切り分ける
          into = c(
            "volume",
            "high",
            "medium"
          ),
          # 区切るために用いる半角空白
          sep = " "
        ) %>% 
        dplyr::select(
          species, 
          volume, 
          high, 
          medium, 
          low
        ) %>% 
        # 1列目だけ除去する。1列目には余計な文字列がある
        dplyr::slice(-1) %>% 
        # 桁区切りカンマのみを、数量と価格が入っている列から除去する
        dplyr::mutate(
          across(
            .cols = -species, 
            ~ as.numeric(
              stringr::str_replace(
                .,
                pattern = "[:punct:]", 
                replacement = ""
              )
            )
          )
        ) %>%
        # 泳ぎ物swimであることを示す変数と、年月日を示す変数とを各々新規に挿入する。
        dplyr::mutate(
          type = factor("swim"),
          year_month_date = year_month_date
        ) %>% 
        # 変数順序を並び替える
        dplyr::select(
          year_month_date, 
          species, 
          type, 
          volume, 
          high, 
          medium, 
          low
        ) %>% 
        as_tibble() %>% 
        # 整然とさせる
        tidyr::pivot_longer(
          cols = c("high", "medium", "low"),
          names_to = "status",
          values_to = "price"
        )
    )
  )
# 魚価データのみデータフレームにまとめる
# ここは泳ぎ物。
fish_price_live_swim_df <- 
  fish_price_live_swim$price_file_live_swim %>%
  dplyr::bind_rows(.)
# 活魚データを結合・保存
# 活魚については以降このデータを利用すべし
fish_price_live_df <- 
  dplyr::bind_rows(
    fish_price_live_floor_df,
    fish_price_live_swim_df
  ) %>% 
  # データがない日についてもデータを挿入する
  # 作図や処理をするにも、そのほうが便利
  # ついでに曜日も挿入するか。
  # グループ毎に切り分けて
  group_by(species, type, status) %>%
  # 欠けた日付を、上記グループ毎に埋めて
  # 関数使い方参考ページ：
  # https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5
  tidyr::complete(
    year_month_date = seq.Date(
      min(.$year_month_date), 
      max(.$year_month_date), 
      # 単位は日。
      by = "day"
      )
    ) %>% 
  # グループ化を解いて
  ungroup() %>% 
  # なぜかある魚種がNAなデータを除去して
  drop_na(species) %>% 
  # 曜日列も加えて
  dplyr::mutate(
    # lubridate::wday()は、日付データから曜日を取得する
    week_of_day = lubridate::wday(
      year_month_date,
      # 曜日名を入れる。
      # FALSEにすると番号だけが残る
      label = TRUE
    ),
    n_week_of_day = lubridate::wday(
      year_month_date,
      # 曜日番号を入れる。
      # FALSEにすると番号だけが残る
      label = FALSE
    ),
    n_week = lubridate::week(year_month_date),
    year = lubridate::year(year_month_date),
    month = lubridate::month(year_month_date, label = TRUE, abbr = TRUE),
    day = lubridate::day(year_month_date)
  ) %>% 
  # 変数（列）順番を並び替えて
  dplyr::select(
    species, type, status, year_month_date, year, month, day, week_of_day, n_week_of_day, n_week, volume, price
  )
# 保存する
write_excel_csv(fish_price_live_df, "fish_price_live_df.csv")
saveRDS(fish_price_live_df, "fish_price_live_df.rds")
# 
# 
#
##
### END
##
#
### ---- read.library.live ----
# Circularプロット作図
# とりあえずタイ台物中位価格のみ
# 他はいろいろ考えてからやりましょう
fish_price_live_circle <- 
  fish_price_live_df %>%
  # 必要なデータを選ぶ
  dplyr::filter(species == "タイ" &type == "floor" & status == "medium") %>% 
  ggplot2::ggplot(
    aes(
      x = week_of_day,
      y = log(volume+0.5),
      group = factor(n_week),
      color = month
    )
  ) + 
  geom_line(fill = NA, na.rm = TRUE) +
  labs(
    title = "タイ数量曜日別推移",
    subtitle = "長崎魚市株式会社「鮮魚速報」2019年1月25日-2022年1月25日分より。活魚・台物。",
    x = "曜日",
    y = "数量（単位：kg。log+0.5）",
    color = "月次"
  ) +
  scale_colour_smoothrainbow(discrete = TRUE, reverse = TRUE) +
  coord_polar() +
  facet_wrap(~ year) +
  theme_bw() +
  theme(
    strip.background = element_blank()
  )

ggsave(
  "fish_price_live_circle.pdf",
  plot = fish_price_live_circle,
  width = 200,
  height = 200,
  units = "mm",
  device = cairo_pdf
)
# 折れ線グラフで「数量」日推移を表現する。
# とりあえずタイ台物、中位価格だけ。
fish_price_live_line_01 <- 
  fish_price_live_df %>%
  # 必要なデータを選ぶ
  dplyr::filter(
    species == "タイ" & type == "floor" & status == "medium") %>% 
  ggplot2::ggplot(
    aes(
      x = year_month_date,
      y = volume
    )
  ) + 
  geom_line(na.rm = TRUE) +
  # x軸目盛りを4ヶ月毎に振る
  scale_x_date(
    date_breaks = "4 months"
    ) +
  labs(
    title = "タイ数量日別推移",
    subtitle = "長崎魚市株式会社「鮮魚速報」2019年1月25日-2022年1月25日分より。活魚・台物。",
    x = "日",
    y = "数量（単位：kg）"
  ) +
  theme_classic()
fish_price_live_line_01

# 状態空間モデル用おためしデータセット
# タイとヒラスを選ぶ
# 必要に応じて、データ範囲は拡大できる
fish_price_live_df_selected <- 
  fish_price_live_df %>%
  # 必要なデータを選ぶ
  dplyr::filter(
    species %in% c("タイ", "ヒラス") & type == "floor" & status == "medium"
    ) %>% 
  dplyr::select(
    species, year_month_date, volume 
  ) %>% 
  tidyr::pivot_wider(
    names_from = "species",
    values_from = "volume"
  ) %>% 
  data.table::setnames(c("year_month_date","tai","hirasu")) 
  
# 行列にしてひっくりかえす
# tidyなままだとstanが受け付けてくれない。
# brmsだといいけれど、stanネイティブだとやめておいたほうが無難。特に複数は
Y <- 
  fish_price_live_df_selected %>% 
  dplyr::select(tai, hirasu) %>% 
  as.matrix(.) %>% 
  t(.)
# stanに食べさせる用にデータを加工する
# NAではないデータを取り出す
ypos <- Y[!is.na(Y)]
# NAではないデータ個数を数える
n_pos <- length(ypos)  # number on non-NA ys
# NAではないデータにインデックスをつける
indx_pos <- which(!is.na(Y), arr.ind = TRUE)  # index on the non-NAs
col_indx_pos <- as.vector(indx_pos[, "col"])
row_indx_pos <- as.vector(indx_pos[, "row"])
# stan用データリスト作成
data_volume <-
  list(
    y = ypos,
    TT = ncol(Y), 
    N = nrow(Y), 
    n_pos = n_pos, 
    col_indx_pos = col_indx_pos,
    row_indx_pos = row_indx_pos
  )
# 
# レベルと時間・魚種変動に、季節変動と週変動を組み込んだモデル
# 02
# 季節変動：
# stanモデルをコンパイルする
model_volume <-
  cmdstan_model(
    # モデルは別ファイルに書く
    stan_file = "fishery_nagasaki_market_price_02.stan",
    compile = TRUE
    )
# stanモデルをあてはめる
# けっこう時間がかかるから注意。
fit_volume <-
  model_volume$sample(
    data_volume,
    iter_warmup = 500,
    iter_sampling = 500,
    chains = 4,
    parallel_chains = 4,
    refresh = 200
  )
# 結果を保存する
saveRDS(fit_volume, "fit_volume.rds")
# 結果要約をcsvファイルにて保存
fit_volume <- readRDS("fit_volume.rds")
summary_volume <- fit_volume$summary()
write_excel_csv(data.frame(summary_volume), "summary_volume.csv")
# ここからは作図
# 推定結果から、推定された「数量」を取り出す
fish_price_live_df_estimated <- 
  summary_volume %>% 
  dplyr::filter(
    ., 
    stringr::str_detect(
      variable, 
      "yhat"
      )
    ) %>% 
  dplyr::select(mean, median, sd, q5, q95)
# 縦長データに戻す
fish_price_live_df_selected_02 <- 
  fish_price_live_df_selected %>% 
  tidyr::pivot_longer(
    cols = c("tai","hirasu"),
    names_to = "species",
    values_to = "volume"
  ) %>% 
  # 結合しやすいように並び替える
  dplyr::arrange(desc(species)) %>% 
  # 横方向に元データと結合する
  dplyr::bind_cols(fish_price_live_df_estimated) %>% 
  # タイやヒラスを日本語に戻す
  dplyr::mutate(
    species = dplyr::case_when(
      species == "tai" ~ "タイ",
      species == "hirasu" ~ "ヒラス",
      TRUE ~ "hoge"
    )
  )

fish_price_live_line_02 <- 
  fish_price_live_df_selected_02 %>% 
  ggplot2::ggplot(
    aes(
      x = year_month_date, 
      y = volume,
      color = species
    )
  ) +
  geom_line(
    aes(
      x = year_month_date,
      y = mean,
      color = species
    )
  ) +
  geom_point(size = 0.5) +
  scale_color_okabeito() +
  facet_wrap(~ species, scales = "free_y") +
  theme_classic() +
  theme(
    strip.background = element_blank()
  )
fish_price_live_line_02
ggsave("fish_price_live_line_02.pdf", width = 200, height = 200, units = "mm")
# 
# レベルと時間・魚種変動に、季節変動と週変動を組み込んだモデル
# 03
# 季節変動：
# 週変動：
# stanモデルをコンパイルする
model_volume_03 <-
  cmdstan_model(
    # モデルは別ファイルに書く
    stan_file = "fishery_nagasaki_market_price_03.stan",
    compile = TRUE
  )
# stanモデルをあてはめる
# けっこう時間がかかるから注意。
fit_volume_03 <-
  model_volume$sample(
    data_volume,
    iter_warmup = 500,
    iter_sampling = 500,
    chains = 4,
    parallel_chains = 4,
    refresh = 200
  )
# 結果を保存する
saveRDS(fit_volume_03, "fit_volume_03.rds")
# 結果要約をcsvファイルにて保存
fit_volume_03 <- readRDS("fit_volume_03.rds")
summary_volume_03 <- fit_volume_03$summary()
write_excel_csv(data.frame(summary_volume_03), "summary_volume_03.csv")
# ここからは作図
# 推定結果から、推定された「数量」を取り出す
fish_price_live_df_estimated_03 <- 
  summary_volume_03 %>% 
  dplyr::filter(
    ., 
    stringr::str_detect(
      variable, 
      "yhat"
    )
  ) %>% 
  dplyr::select(mean, median, sd, q5, q95)
# 縦長データに戻す
fish_price_live_df_selected_03 <- 
  fish_price_live_df_selected %>% 
  tidyr::pivot_longer(
    cols = c("tai","hirasu"),
    names_to = "species",
    values_to = "volume"
  ) %>% 
  # 結合しやすいように並び替える
  dplyr::arrange(desc(species)) %>% 
  # 横方向に元データと結合する
  dplyr::bind_cols(fish_price_live_df_estimated_03) %>% 
  # タイやヒラスを日本語に戻す
  dplyr::mutate(
    species = dplyr::case_when(
      species == "tai" ~ "タイ",
      species == "hirasu" ~ "ヒラス",
      TRUE ~ "hoge"
    )
  )
# 作図
fish_price_live_line_03 <- 
  fish_price_live_df_selected_03 %>% 
  ggplot2::ggplot(
    aes(
      x = year_month_date, 
      y = volume,
      color = species
    )
  ) +
  geom_line(
    aes(
      x = year_month_date,
      y = mean,
      color = species
    )
  ) +
  geom_point(size = 0.5) +
  scale_color_okabeito() +
  facet_wrap(~ species, scales = "free_y") +
  theme_classic() +
  theme(
    strip.background = element_blank()
  )
fish_price_live_line_03
# 保存
ggsave(
  "fish_price_live_line_03.pdf", 
  width = 200, 
  height = 200, 
  units = "mm",
  device = cairo_pdf
  )
# 
#
##
### END
##
#


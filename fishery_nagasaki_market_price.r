############################################################################## 
# おさかな天国：長崎魚市鮮魚価格日報データを取得する
# 2020年1月27日
# 宇都宮　譲　（長崎大学経済学部）
# 
# 工程
# 1.鮮魚速報pdfをスクレイピングする。
# 2.鮮魚速報pdfファイルを読み込んで使いやすいデータ形式に整える。
# 
# 3.いろいろ処理する。

### ---- read.library ----
# ライブラリを読み込む
library(tidyverse)
library(tabulizer)
library(janitor)
library(ragtop)

### ---- read.pdf.files ---- 
# ファイル名を読み込む。
# 自動読み込みに備えて、ファイル名はオブジェクトに収納する。
# 複数になってもおなじこと。
# とりあえず単数ファイルで試して読み込む作業がうまくいってから複数ファイル自動読み込み化へ移行する。
file_names <- c("20220124.pdf")
# ファイルを読み込む。
fish_price <- 
  # pdfファイルを読み込む
  # tabulizer()があることを知って、自動化できそうと確信を得た。
  tabulizer::extract_tables(
    file_names, 
    output = "data.frame"
    ) %>% 
  # 使うところだけ取り出す。
  map(., 
      ~ 
        {
          colname <- 
            .x %>% 
            slice(1) %>% 
            t() %>% 
            data.frame() %>% 
            pull(.)
          .x %>% 
            slice(-1) %>% 
            setNames(colname)
          }
      ) %>% 
  bind_rows()
# 
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
fish_price_live <- 
  fish_price %>%
  # 見た目に不要な列は使わない。
  dplyr::select(c(1, 3, 4)) %>% 
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
# 
# カテゴリ毎にデータを整理する
# まずは近海活魚台物
# 参考ページ
# 条件にあてはまる要素があるデータの行番号を取得する
# https://stackoverflow.com/questions/68843681/extracting-data-by-row-number-after-a-set-condition
# 
# https://stackoverflow.com/questions/42744274/string-manipulation-in-a-column
# 
fish_price_live_floor <- 
  fish_price_live %>%
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
    year_month_date = lubridate::ymd(
      str_sub(
        file_names, 
        start = 1, 
        end = 8
        )
      ) 
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
# 
#
##
### END
##
#

// データ
data {
  int<lower=0> TT; // length of ts。期間の数
  int<lower=0> N; // num of ts; rows of y。魚種数
  int<lower=0> n_pos; // number of non-NA values in y。期間の数のうち欠損がない部分の数
  int<lower=0> col_indx_pos[n_pos]; // col index of non-NA vals。欠損ではない部分の列（期間）方向インデックス
  int<lower=0> row_indx_pos[n_pos]; // row index of non-NA vals。欠損ではない部分の行（魚種）方向インデックス
  vector[n_pos] y; // 値がある箇所に入っていた値
}
// パラメータ
parameters {
  vector[N] x0; // initial states。状態の初期値。
  real u; // 平均成長率
  vector[N] pro_dev[TT]; // refed as pro_dev[TT,N]。ある年TTに魚種iにみられる偏差
  vector [N] season[TT]; // refed as season[TT,N]
  vector [N] week[TT]; // refed as season[TT,N]
  real<lower=0> sd_q; // pro_devの分布のSD。時点・魚種を通じて共通。
  real<lower=0> sd_r[N]; // obs variances are different。観測空間のばらつき(SD)。魚種毎に異なる。時点を通じて共通
}
transformed parameters {
  vector[N] x[TT]; // refed as x[TT,N]。魚種・時点別状態=見えないところにある「数量」。
  // 
  for(i in 1:N){
    // 初期値
    x[1,i] = x0[i] + u + pro_dev[1,i];
    // 2日目以降の状態
    // ここがモデルのキモでしょう。
    for(t in 2:TT) {
      x[t,i] = x[t-1,i] + u + pro_dev[t,i];
    }
  }
}
// モデル
model {
  // 事前分布
  // 平均成長率の事前分布
  u ~ normal(0,100);
  sd_q ~ student_t(3, 0, 10);
  // 
  for(i in 1:N){
    // 初期値。左端というか最初の日の直前の状態を推定する
    x0[i] ~ normal(y[i],10); // assume no missing y[1]
    sd_r[i] ~ student_t(3, 0, 10);
    // // ある年TTに魚種iにみられる偏差=年・魚種毎に異なるばらつき
    for(t in 1:TT){
      pro_dev[t,i] ~ normal(0, sd_q);
    }
    //　季節変動。魚種毎に異なる
    // とりあえず28日（月齢）にしてます。
    // 図を見る限り180日くらいでもよさそうな
    for(t in 180:TT){
      season[t, i] ~ normal(-sum(season[(t-179):(t-1),i]), sd_q);
    }
    //　週変動。魚種毎に異なる
    // とりあえず7日（月齢）にしてます。
    // 図を見る限り180日くらいでもよさそうな
    for(t in 7:TT){
      week[t, i] ~ normal(-sum(season[(t-6):(t-1),i]), sd_q);
    }
  }
  // 観測空間
  for(i in 1:n_pos){
    y[i] ~ normal(x[col_indx_pos[i], row_indx_pos[i]] + season[col_indx_pos[i], row_indx_pos[i]] + week[col_indx_pos[i], row_indx_pos[i]], sd_r[row_indx_pos[i]]);
  }
}
// 尤度計算と推定。
generated quantities {
  // 変数宣言
  vector[n_pos] log_lik;
  vector[N] yhat[TT]; // refed as muhat[TT,N]。
  // 尤度計算
  for (n in 1:n_pos) log_lik[n] = normal_lpdf(y[n] | x[col_indx_pos[n], row_indx_pos[n]] + season[col_indx_pos[n], row_indx_pos[n]] + week[col_indx_pos[n], row_indx_pos[n]], sd_r[row_indx_pos[n]]);
  // yを推定する
  for(i in 1:N){
    for(t in 1:TT){
      yhat[t,i] = normal_rng(x[t,i] + season[t,i] + week[t,i], sd_r[i]);
      }
      }
    }

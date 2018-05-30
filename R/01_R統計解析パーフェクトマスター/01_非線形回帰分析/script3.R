file <- "/mine/git_repo/examples/R/01_R統計解析パーフェクトマスター/01_非線形回帰分析/普及率.txt" # 読み込むファイル名を設定

data <- read.delim(                # ファイルを読み込んでdataに格納
  file,
  header=T,                        # 1行目は列名であることを指定
  fileEncoding="CP932"             # 文字コードをShift_JISに指定
)

# 非線形回帰分析
nls.relation. <- nls(
  pene ~ a / (1 + b * exp(c * year)), # 関係式
  start=c(a=1, b=1, c=-1),            # 初期値を設定
  trace="TRUE"                        # 計算過程をトレース
)

year <-(1:19)       																 # 年度を1～19にする

# 非線形回帰分析
relat.nls <- nls(
  pene ~ a / (1 + b * exp(c * year)), # 関係式
  start=c(a=1, b=1, c=-1),            # 初期値を設定
  trace="TRUE"                        # 計算過程をトレース
)

# SSlogis()をh奇数にして非線形回帰分析する
func.nls <- nls(pene ~
                    SSlogis(year, Asym, xmid, scal)
                )

summary(func.nls)                      # 分析結果を表示

relat.coef <- coefficients(relat.nls)   # 係数を取得

# 係数を取り出す
Asym <- as.vector(relat.coef[1])            # aの値
xmid <- as.vector(relat.coef[2])            # bの値
scal <- as.vector(relat.coef[3])            # cの値
x <- 10                                 # xの値

# ロジスティック曲線モデル
func.y <- Asym / (1 + exp((xmid - x) / scal))

expe_2 <- cbind(data,
                fitted(relat.nls),
                fitted(func.nls))
plot(year, pene, cex=1)  # 実測値の散布図を作成
lines(year,              # x座標は年代
      fitted(relat.nls), # y座標は予測値
      col="RED",         # 赤にする
      lty="dotted",      # ドットで描画
      lwd=2)             # 太さは2

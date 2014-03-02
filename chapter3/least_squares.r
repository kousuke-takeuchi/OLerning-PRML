library(MASS)

n <- 50
N <- 1000

# Conj() は複素共役を行う
# -3~3 までの範囲で、length 個に等分したデータを作成し、データ列に複素共役転置を行う
x <- seq(-3, 3, length=n)
X <- seq(-3, 3, length=N)

# 基底関数ベクトル(今回では三角多項式を実行する回数に影響するので山の数が変動する)
b <- 100

pix <- pi*x

# y は標本化関数(アナログデジタル変換等信号処理に用いる)、x は入力値
# rnorm によって平均 0、偏差 1 を元に、ランダム数で正規分布を作成
y <- sin(pix)/(pix)+0.1*x+0.05*rnorm(n)

# 1~n 迄のデータ列作成且つ 1 で初期化
p <- 1:n*0+1
P <- 1:N*0+1

# n 行 1 列の行列に変換
p <- matrix(p, n, 1)
P <- matrix(P, N, 1)

# p,P と matrix を結合
p <- cbind(p, matrix(0, n, b*2))
P <- cbind(P, matrix(0, N, b*2))

# 三角多項式基底関数 φ(x) の適用
for (j in 1:b) {
  p[,2*j]   <- sin((j/2) * x)
  p[,2*j+1] <- cos((j/2) * x)
  P[,2*j]   <- sin((j/2) * X)
  P[,2*j+1] <- cos((j/2) * X)
}

# 式 φTφθ = φTy を計算
t <- ginv(p) %*% y
F <- P %*% t

# png で保存
png('~/ml_graph_data/ls_b100.png')

# X, F についてプロットする
# x, y 軸のラベルは無し
# 描画範囲は -2.8<x<2.8, -0.5<y<1.2
plot(X, F, type='l', ylab='', xlab='', xlim=c(-2.8,2.8), ylim=c(-0.5, 1.2))

# 次にプロットするグラフを重ねて描画する
par(new=T)
plot(x, y, xlim=c(-2.8,2.8), ylim=c(-0.5, 1.2))

# グラフの保存を終了させる
dev.off()

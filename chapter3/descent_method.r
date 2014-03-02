repmat <- function(mat, m, n) {
  rmat <- mat
  for (i in 1:m) {
    if (i == m) break
    rmat <- rbind(rmat, mat)
  }
  crmat <- rmat
  for (i in 1:n) {
    if (i == n) break
    crmat <- cbind(crmat, rmat)
  }
  crmat
}

n <- 50   # 訓練標本数
N <- 1000 # ベクトル数
tt <- 10000 # トレーニング回数

x <- seq(-3, 3, length=n)
X <- seq(-3, 3, length=N)

pix <- pi*x

# 標本 yi を計算
y <- sin(pix)/pix+0.1*x+0.05*rnorm(n)

# ガウスカーネルモデルの計算式 2h^2 の事を指す
# この場合、0.3 が h (バンド幅) に相当する
hh <- 2*0.3^2

# 初期値 θ を適当に設定
t0 <- matrix(rnorm(n), n, 1)

# 勾配の歩幅
# 学習させる際にどの程度一度に勾配を下らせるかを設定
e <- 0.1

# 反復
for (i in 1:tt) {

  # 学習
  for (o in 1:n*N) {
    i <- ceiling( runif(1)*n )
    ki <- matrix(exp( -(x-x[i])^2/hh ), n, 1)

    t <- t0-e*ki %*% (Conj(t( ki )) %*% t0-y[i])
    if (norm(t-t0) < 0.000001) break
    t0 <- t
  }

}

K <- exp( -(repmat(X^2, 1, n)+repmat(Conj(t(x^2)), N, 1) - 2*X %*% Conj(t(x)))/hh )
F <- K %*% t


png( paste('~/ml_graph_data/', 'dmg_kernel_ls_tt', tt, '.png', sep='') )

plot(X, F, xlab='', ylab='', xlim=c(-2.8,2.8), ylim=c(-0.5,1.2), type='l')
par(new=T)
plot(x, y, xlim=c(-2.8,2.8), ylim=c(-0.5,1.2))

dev.off()

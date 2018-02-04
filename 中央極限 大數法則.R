#常態分佈的大數法則
#選取n個樣本之平均
sample_mean <- function(n=30){
  sum(rnorm(n))/n
}


#n個樣本 做i次取樣的柱狀圖(大數法則)
LLN <- function(n,i=1000){
normal=0
for (x in 1:i){
  normal[x] <- sample_mean(n)
}
hist(normal,xlim = range(-2,2),col="blue")
}



par(mfrow=c(2,2))
LLN(50)
LLN(100)
LLN(1000)
LLN(10000)

#指數分布的大數法則
sample_mean_exp <- function(n=30){
  sum(rexp(n))/n
}
LLN_exp <- function(n,i=1000){
  exp=0
  for (x in 1:i){
    exp[x] <- sample_mean_exp(n)
  }
  hist(exp,xlim = range(-2,2),col="red")
}
LLN_exp(50)
LLN_exp(100)
LLN_exp(1000)
LLN_exp(10000)

#中央極限定理(以指數函數當範例)

clt_exp_normal <- function(n,i=1000){
  exp=0
  for (x in 1:i){
    exp[x] <- sample_mean_exp(n)
  }
  sd <- sd(exp)
  mean <- mean(exp)
  par(mfrow=c(1,1))
  hist(exp,xlim = range(-2,2),col="red",main="red=exp_shape\nblue=nor_shape")
  par(new=T)
  hist(rnorm(i,mean,sd),col="blue",xlim = range(-2,2),main = "")

}

#n是抽樣的個體數 i是抽幾次 兩種分配的形狀會隨著N增加而趨近
clt_exp_normal(n=10,i=1000)
clt_exp_normal(50)
clt_exp_normal(100)
clt_exp_normal(1000)



clt_math <- function(n,i=1000,pop_mean=0,pop_sd=100){
  sample=0
  for (x in 1 :i) {
  sample[x] <- sum( rnorm(n,pop_mean,pop_sd))/n}
  
  
  hist(sample,xlim = range(pop_mean-15,pop_mean+15),main= paste(n,"個取樣的常態"))

}


#在不變的區間下，樣本平均數的分布，會以根號n的速度收斂

par(mfrow=c(2,2))
clt_math(100)
clt_math(500)
clt_math(1000)
clt_math(10000)

#母體是指數分配也一樣



clt_math_exp <- function(n,i=1000,pop_mean=1){
  sample=0
  for (x in 1 :i) {
    sample[x] <- sum( rexp(n,1/pop_mean))/n}
  
  
  hist(sample,xlim = range(0,2),main= paste(n,"個取樣的指數"))
  
}


par(mfrow=c(2,2))
clt_math_exp(100)
clt_math_exp(500)
clt_math_exp(1000)
clt_math_exp(10000)



#假設樣本平均是以根號n收斂，在將其乘上根號n則n就無法影響樣本平均之分佈

clt_math_n <- function(n,i=1000,pop_mean=0,pop_sd=100){
  sample=0
  for (x in 1 :i) {
    sample[x] <- sum( rnorm(n,pop_mean,pop_sd))/n}
hist((sample-pop_mean)/pop_sd*n^0.5,xlim = range(-5,5),main= paste(n,"個取樣的常態","\n(除上根號n)"))
  
}

par(mfrow=c(2,2))
clt_math_n(100)
clt_math_n(500)
clt_math_n(1000)
clt_math_n(10000)


#試驗若母體為指數是否有相同性質


clt_math_n_exp <- function(n,i=1000,pop_mean=1){
  sample=0
  for (x in 1 :i) {
    sample[x] <- sum( rexp(n,1/pop_mean))/n}
  hist((sample-pop_mean)/0.2*n^0.5,xlim = range(-13,13),main= paste(n,"個取樣的指數","\n(除上根號n)"),breaks=30)
}
  

par(mfrow=c(2,2))
clt_math_n_exp(100)
clt_math_n_exp(500)
clt_math_n_exp(1000)
clt_math_n_exp(10000)




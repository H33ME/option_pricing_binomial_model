library(derivmkts)
library(ggplot2)
library(dplyr)
butterfly <- function(s,  k1,  k2,  k3,  v,  r,  tt,  d) {
  bscall(s, k1,  v,  r,  tt,  d) -
    2*bscall(s,  k2,  v,  r,  tt,  d) +
    bscall(s,  k3,  v,  r,  tt,  d)
}
s0 <- 100
s <- seq(10,100,  by = 0.5)
itmk <- 95; atmk <- 100; otmk <- 120
v <- 0.2
d <- 0
r <- 0.06
tt <- c(0.0001, .04, .5, 1)
ttexp <- 0.0001
x <- expand.grid(s = s, tt = tt)
sl <- unlist(x$s)
sl
ttl <- unlist(x$tt)
bf <- greeks(binomial_tree_option(type='call', sigma=0.15, t=1, r=0.1, K=100, S=110, Tn=5)
)
group_by(bf, tt) %>%
  mutate(profit = butterfly(s, itmk, atmk, otmk, v, r, tt, d))  %>%
  ggplot(aes(x = s,  y = profit, color = as.factor(tt), group = tt)) +
  geom_line() + labs(color = 'Expiration')
bf<- greeks(butterfly(sl,itmk,atmk,otmk,v,r,ttl,d),complete = TRUE)

group_by(bf,tt)%>%
  mutate(profit=binomial_tree_option(type='call', sigma=0.15, t=1, r=0.1, K=100, S=110, Tn=5)
)%>%
  ggplot(aes(x=s,y=profit,color=as.factor(tt),group=tt))+
  geom_line()+labs(color="Expiration")


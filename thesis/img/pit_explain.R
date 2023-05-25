#!/usr/bin/env Rscript
CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/img/"
setwd(CWD)

library("data.table")
library("ggplot2")
theme_set(theme_classic())

# functions ---------------------------------------------------------------

F_nb <- function(r, p){
  func <- function(y) {
    result <- vector(mode = "numeric", length = length(y))
    result[y < 0] <- 0
    result[y>=0] <- pnbinom(y[y>=0], size=r, prob=p)
    return(result)
  }
  return(func)
}

F_u <- function(x, u, func){
  f_u <- vector(mode="numeric", length = length(x))
  p <- func(x)
  p_1 <- func(x-1)
  f_u[u < p_1] <- 0
  index <- (u >= p_1) & (u <= p)
  f_u[index] <- (u - p_1[index]) / (p[index] - p_1[index])
  f_u[u > p] <- 1
  return(f_u)
} 

# generer data ----------------------------------------------------------
N <- 200
s <- 5
prob <- 1/2
sim_data <- rnbinom(N, size=s, prob=prob)
u <- seq(0, 1, length.out=11)
pit_data <- data.table(r=numeric(), mod=character(), u=numeric(),Fu=numeric())
mean_pit <- data.table(mod=character(), u=numeric(), mean_FU=numeric())

for (params in list(c(1,1/6), c(s, prob), c(25, 5/6))){
  r = params[1]
  p = params[2]
  mod <- paste("NB(", r, ", ", round(p,digits = 2),")", sep="")
  PIT <- unlist(lapply(u, F_u, x=sim_data, func=F_nb(r, p)))
  PIT <- data.table(r=r, mod=mod, u=rep(u, each=N), Fu=PIT)
  pit_data <- rbind(pit_data, PIT)
  mean_pit <- rbind(mean_pit, PIT[,.(mean_FU=mean(Fu, na.rm = TRUE)),by=u][,mod:=mod])
}
# plot --------------------------------------------------------------------


pit_plot <- ggplot(pit_data[,type:="F(u)"], aes(x=u, y=Fu)) +
  geom_col(data=mean_pit[,.(u,type="PIT", pit= mean_FU-shift(mean_FU,n=1)),by=mod],
              aes(y=pit),just=1) +
  geom_point(data=mean_pit[,type:="F(u)"],aes(y=mean_FU, color="Mean F(u)")) +
  geom_line(aes(y=u), linetype=2, color="lightgrey") +
  facet_grid(cols=vars(mod), rows = vars(type), scale="free_y") +
  theme(axis.title.y = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank())
 
ggsave("pit_explain.pdf", pit_plot, units="cm", width = 15, height = 10)
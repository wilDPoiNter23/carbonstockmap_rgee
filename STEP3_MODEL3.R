library('rlist')
library("pipeR")
library(plyr)
library(modelr)
library(lmodel2)
library(car)
library(purrr)
########### HHMC ALLOMETRIC GROWTH #####
hhmc_table <- read_excel("~/Downloads/code/06data/hhmc.xlsx")
data<-hhmc_table
data2<-tibble(logun=data$logun,
              logab=data$logab,
              dw_un=data$dw_un,
              dw_ab=data$dw_ab)

# model2
allo_m<-lmodel2(logun~logab,data=data,"relative","relative",99)
allo_m
a<-allo_m$regression.results[3,3]
b<-allo_m$regression.results[3,2]

########## LW ALLOMETRIC GROWTH######
lw_table <- read_excel("~/Downloads/code/06data/lw.xlsx")
data<-lw_table
data2<-tibble(logun=data$logun,
              logab=data$logab,
              dw_un=data$dw_un,
              dw_ab=data$dw_ab)

allo_m<-lmodel2(logun~logab,data=data,"relative","relative",99)
allo_m
a<-allo_m$regression.results[3,3]
b<-allo_m$regression.results[3,2]
######## YDJP ALLOMETRIC GROWTH######
ydjp_table <- read_csv("~/Downloads/code/06data/ydjp.csv")
data<-ydjp_table
data2<-tibble(logun=data$logun,
              logab=data$logab,
              dw_un=data$dw_un,
              dw_ab=data$dw_ab)

allo_m<-lmodel2(logun~logab,data=data,"relative","relative",99)
allo_m
a<-allo_m$regression.results[3,3]
b<-allo_m$regression.results[3,2]

#########PLOT########
ALLO_model<-function(x){
  y=a*x+b
  D<-data.frame(x=data$logab,y=y)
  return(D)
}
print(D)
ALLO_model(data$logab)
logun_pre= a*data$logab+b
# plot

library("latex2exp")
dat<- tibble(data2,logun_pre)
reverse_model<-function(x)
{
  return(y<- 10^b*(x^a))
}
f_plot<-reverse_model(dat$dw_ab)
new_grid<- tibble(dat,f_plot)
#plot is 50cm*50cm, zoom the scale in the 1m*1m
yrng <- range(new_grid[[3]])
xrng <- range(new_grid[[4]])
caption <- paste(strwrap("A. ;Back-transformed
                         BGB=10^", 40), collapse = "\n")

ggplot(new_grid,aes(x=dw_ab, y=dw_un)) +
  geom_function(fun=reverse_model, colour="#999999",size=1)+
  geom_point(colour= "#5F97D2",size=2.5)+
  theme_grey()+
  labs(x=TeX('Aboveground biomass $\\g/m^2$'),
       y=TeX('Belowground biomass $\\g/m^2$'),)+
  scale_y_continuous(breaks = scales::breaks_extended(n=7))+
  scale_x_continuous(breaks = scales::breaks_extended(n=7))+
  theme(aspect.ratio = 1)
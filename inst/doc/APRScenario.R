## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.show = "hide",
  cache = FALSE
)

## ----Set-directory,echo=FALSE,include=FALSE-----------------------------------
# Following is because of the German way of typing dates etc.
Sys.setlocale("LC_TIME", "English.UTF-8") # Windows

## ----Load-necessary-packages,eval=TRUE----------------------------------------
# Load required packages
library(APRScenario)
library(bsvarSIGNs)
library(bsvars)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(gridExtra)
library(tidyr)

# Note: During package development, you can reload the package if needed


## ----DATA, fig.show='hide'----------------------------------------------------
data(NKdata)
X0<-NKdata[,-1] %>% as.data.frame()
varbls<-names(X0)
p<-autoplot(X0 %>% ts(.,frequency=4,start=round(NKdata$year[1])),facets=T)+ylab('')+xlab('')


# To save the plot, use:
# ggsave('data.png', plot=p, device='png', path=tempdir(), width=18, height=16, units = 'cm')


## ----Sign-Restriction, fig.show='hide'----------------------------------------

sr <- matrix(NA, nrow = length(varbls), ncol = length(varbls))

# Fill the matrix with sign restrictions as per the previous setup
sr <- matrix(c(
1,-1,-1,# GDP
1,1,-1, # infl
1,NA,1 # interest rate
), nrow = 3, byrow = TRUE)


## ----data-plot, echo=FALSE, eval=FALSE----------------------------------------
# # The data plot can be displayed with:
# # print(p)
# #
# # To save the plot:
# # ggsave('data.png', plot=p, device='png', path=tempdir(), width=18, height=16, units = 'cm')

## ----estimate-BVAR, fig.show='hide'-------------------------------------------


# ############# Subset of variables
# 1. Baseline VAR --------
subset_<-c(1:3) # start with subsets of variables
#/////////////////////////////
n_draws=200 # increase for final estimation
p=4 # lags
# possible subsampling
X=X0[1:(nrow(X0)-4),]
# specify the model
specification  = bsvarSIGNs::specify_bsvarSIGN$new(data=as.matrix(X,3,3),
                                      p        = p,
                                       sign_irf = sr[1:3,1:3])

# estimate the model
posterior      = estimate(specification, S = n_draws) # SEE FORKED VERSION FOR PARALLEL Q ROTATION DRAWS

# compute and plot impulse responses
irf            = bsvars::compute_impulse_responses(posterior, horizon = 40)
# {
#   X11()
# plot(irf, probability = 0.68)
# }
{
  p<-plot_bvars(irf, significance_level = 0.32,
                central_tendency = 'median', variable_names = varbls, shock_names = varbls)

  {dev.new()
  grid::grid.newpage()  
  grid_arranged_plots <- arrangeGrob(grobs = p[1:9], nrow =3, ncol =3)
  grid.arrange(grobs = p[1:9], nrow =3, ncol =3)

  }
  }

# To save the IRF plots:
# ggsave('irf_bvars.png', plot=grid_arranged_plots, device='png', path=tempdir(), width=18, height=16, units = 'cm')


sr1<-sr %>% as.data.frame()
rownames(sr1)<-varbls 
names(sr1)<-varbls
# To display the sign restrictions table:
# knitr::kable(sr1, row.names = TRUE, caption = 'Sign Restrictions: IRF 1 (shocks in col.)')
# sr1 %>% cat(file='figures/sr_bvars.tex')


f_bvar<-bsvars::forecast(
  posterior,
  horizon = 3,
  exogenous_forecast = NULL,
  conditional_forecast = NULL
)
# VAR structure Y'=X'*B+u' (see specification$data_matrices) 


## ----irf-plot, echo=FALSE, eval=FALSE-----------------------------------------
# # The IRF plots can be displayed with:
# # grid.arrange(grobs = p[1:9], nrow = 3, ncol = 3)
# #
# # To save the arranged plot:
# # ggsave('irf_bvars.png', plot=grid_arranged_plots, device='png', path=tempdir())

## ----Matrices,echo=TRUE-------------------------------------------------------
# Actual function ---------------

matrices<-gen_mats(posterior, specification);

# Unpack the matrices from the returned list
M <- matrices$M
M_inv <- matrices$M_inv
M_list <- matrices$M_list
B <- matrices$B
B_list <- matrices$B_list
intercept <- matrices$intercept
n_var <- matrices$n_var
n_p <- matrices$n_p
Y <- matrices$Y
XX <- matrices$X
Z <- matrices$Z


## ----Run-code-for-unconditional-forecast, fig.show='hide'---------------------
{# run all block 
h=4
y_h_all<-forc_h(h,n_sim=200,data_=Z,posterior=posterior,matrices=matrices)
y_h<-y_h_all[[1]]
hist_h<-y_h_all[[3]]
b_h<-y_h_all[[4]]
{# extract quantiles
y_h_m<-apply(y_h,c(1,2),FUN=function(x)quantile(x,0.5))
y_h_l<-apply(y_h,c(1,2),FUN=function(x)quantile(x,0.16))
y_h_u<-apply(y_h,c(1,2),FUN=function(x)quantile(x,0.84))


hist_h_l<-apply(hist_h,c(1,2),FUN=function(x)quantile(x,0.16))
hist_h_u<-apply(hist_h,c(1,2),FUN=function(x)quantile(x,0.84))

}
# convert to data frame for better manipulation
{
y_h_m<-as.data.frame(t(y_h_m))
y_h_u<-as.data.frame(t(y_h_u))
y_h_l<-as.data.frame(t(y_h_l))

hist_h_u<-as.data.frame(t(hist_h_u))
hist_h_l<-as.data.frame(t(hist_h_l))


names(y_h_m)<-varbls
y_h_m$h<-1:nrow(y_h_m)
y_h_tot<-pivot_longer(y_h_m,cols=all_of(varbls),names_to='vars',values_to='Median')
names(y_h_l)<-varbls
y_h_l$h<-1:nrow(y_h_l)
y_h_tot<-left_join(y_h_tot,
                   pivot_longer(y_h_l,cols=all_of(varbls),names_to='vars',values_to='LB'),
                   by=c('h','vars'))

names(y_h_u)<-varbls
y_h_u$h<-1:nrow(y_h_u)
y_h_tot<-left_join(y_h_tot,
                   pivot_longer(y_h_u,cols=all_of(varbls),names_to='vars',values_to='UB'),
by=c('h','vars'))

names(hist_h_l)<-varbls
hist_h_l$h<-1:nrow(hist_h_l)
hist_h_tot<- pivot_longer(hist_h_l,cols=all_of(varbls),names_to='vars',values_to='LB_s')

names(hist_h_u)<-varbls
hist_h_u$h<-1:nrow(hist_h_u)
hist_h_tot<-left_join(hist_h_tot,
                   pivot_longer(hist_h_u,cols=all_of(varbls),names_to='vars',values_to='UB_s'),
by=c('h','vars'))


y_h_tot<-left_join(y_h_tot,hist_h_tot,by=c('h','vars'))

}
# inspect result
dt_t<-as.data.frame(t(Y))
dt_t$h=1:nrow(dt_t)
y_data<-pivot_longer(dt_t,cols =all_of(1:n_var),values_to =names(y_h_tot)[3],names_to = "vars" )
y_data<-y_data[y_data$h>=last(y_data$h)-4,]  
  
y_data$h<-y_data$h-max(y_data$h)
y_data$LB<-NA
y_data$UB<-NA
y_data$LB_s<-NA
y_data$UB_s<-NA

y_h_tot<-rbind(y_data,y_h_tot)
y_h_tot<-y_h_tot[order(y_h_tot$h),]
head(y_h_tot)

uncond.forc<-y_h_tot
# plot 


p <- ggplot(uncond.forc[uncond.forc$vars == varbls[1], ], aes(x = h)) +
  # Median line (solid line)
  geom_line(aes(y = Median, color = "med"), linewidth = 1, show.legend = TRUE) +
  
  # Shaded area for 68% HDI
  geom_ribbon(aes(ymin = LB, ymax = UB, fill = "bnd"), 
              alpha = 0.5, show.legend = TRUE) +
  
  # Dashed lines for 68% high credibility band of history
  geom_line(aes(y = LB_s, color = "hist"), 
            linetype = "dashed", linewidth = 0.8, show.legend = TRUE) +
  geom_line(aes(y = UB_s, color = "hist"), 
            linetype = "dashed", linewidth = 0.8, show.legend = FALSE) +
  
  # Labels and theme
  labs(title = "Unconditional Forecast", x = "h", y = varbls[1]) +
  theme_minimal() +
  
  # Custom legend for colors
  scale_color_manual(
    name = "Legend",
    labels=c('med'="Median",'hist'="no shock unc."),
    values = c("med" = "blue",  'hist'= "red")
  ) +
  
  # # Custom legend for fill
  # scale_fill_manual(
  #   name = "Legend",
  #   labels=c('bnd'="no shock unc."),
  #   values = c('bnd' = "lightblue")
  # )+
  theme(legend.position = 'bottom')+
  theme_minimal()


} # end run all block
p
# To save the unconditional forecast plot:
# ggsave('uncond_forc.png', plot=p, device='png', path=tempdir(), width=18, height=16, units = 'cm')

## ----uncond-forecast-plot, echo=FALSE, eval=FALSE-----------------------------
# # The unconditional forecast plot can be displayed with:
# # print(p)
# #
# # To save the plot:
# # ggsave('uncond_forc.png', plot=p, device='png', path=tempdir())

## ----Run-Scenario, fig.show='hide'--------------------------------------------
#NB: Y contans the data n_var x T
h=4 # horizon
n_sim=200 # number of shock draws
obs=c(2) #position of observables
pos_cond_vars=obs 
# given the path of the observables
TT=nrow(X0)
path=X0[(TT-h+1):TT,obs]
# used for bult in conditional forecast
bvarSign_path=X0[(TT-h+1):TT,]
bvarSign_path[,-obs]<-NA

# give the shocks that are not driving the scenario: NA if all driving
shocks=NA#c(1,2,3)
tmp<-scenarios(h = h,
               path = path,obs = obs,
               free_shocks = shocks,
               n_sample = NULL,
               data_ = Z,g=NULL,Sigma_g=NULL,
               posterior=posterior,matrices=matrices)
mu_eps<-tmp[[1]]
Sigma_eps<-tmp[[2]]
mu_y<-tmp[[3]]
Sigma_y<-tmp[[4]]
big_b<-tmp[[5]]
big_M<-tmp[[6]]

y_h<-simulate_conditional_forecasts(mu_y=mu_y, Sigma_y=Sigma_y, varnames = varbls, n_sim = n_sim)
  

# convert to data frames of central and HPD
y_h_m<-apply(y_h,c(1),FUN=function(x)quantile(x,0.5))
y_h_l<-apply(y_h,c(1),FUN=function(x)quantile(x,0.16))
y_h_u<-apply(y_h,c(1),FUN=function(x)quantile(x,0.84))

cond.for<-data.frame(center=y_h_m,variable=rep(varbls,h),hor=sort(rep(1:h,n_var)))
cond.for$lower<-y_h_l
cond.for$upper<-y_h_u
cond.for<-cond.for[,c("hor","variable","lower","center","upper")]




# inspect result
dt_t<-as.data.frame(t(Y))
dt_t$h=1:nrow(dt_t)
y_data<-pivot_longer(dt_t,cols =all_of(1:n_var),values_to =names(cond.for)[4],names_to = "variable" )
y_data<-y_data[y_data$h>=last(y_data$h)-4,]  
  
y_data$hor<-y_data$h-max(y_data$h)
y_data$lower<-NA
y_data$upper<-NA
y_data<-y_data[,names(cond.for)]
cond.for<-rbind(y_data,cond.for)
cond.for<-cond.for[order(cond.for$hor),]

cond.for$hist<-1
cond.for[cond.for$hor<=0,'hist']<-0
library(lubridate)

T.start='2016-01-01'
T.end=as.Date(T.start)%m+%months(9)
y_data<-t(Y) %>% as.data.frame()
dt_tmp<-zoo::as.yearqtr(NKdata$year, format = "%Y Q%q")
dates_date<-zoo::as.Date(dt_tmp)
y_data$hor<-dates_date[(2*specification$p+1):length(dates_date)]

p<-plot_cond_forc(y_h_cond =y_h, varbls[1],T.start = T.start,T.end = T.end,y_data =y_data )
p[[1]]

# To save the conditional forecast plot:
# ggsave('cond_forc.png', plot=p[[1]], device='png', path=tempdir(), width=18, height=16, units = 'cm')

threshold=last(y_data[,1])*(1.01)

p<-plot_cond_histo(variable=varbls[1],horizon=1,
                   threshold = threshold,data =y_h,above=F,size=5)+
labs(title='')+theme_minimal()+ylab('')+xlab('')
p

# To save the conditional histogram plot:
# ggsave('cond_histo.png', plot=p, device='png', path=tempdir(), width=18, height=16, units = 'cm')


## ----cond-forecast-plots, echo=FALSE, eval=FALSE------------------------------
# # The conditional forecast and histogram plots can be displayed with:
# # print(p[[1]])  # for the forecast plot
# # print(p)       # for the histogram plot
# #
# # To save the plots:
# # ggsave('cond_forc.png', plot=p[[1]], device='png', path=tempdir())
# # ggsave('cond_histo.png', plot=p, device='png', path=tempdir())

## ----Kullback-Leibler-measure, fig.show='hide'--------------------------------

q<-KL(Sigma_eps,mu_eps,h,plot_ = T)
hist(q[[1]],main='KL measure (ref value 0.5)')



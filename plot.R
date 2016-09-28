# boxplot
# http://docs.ggplot2.org/0.9.3.1/geom_boxplot.html

library(dplyr)
library(plotly)

ICS_Data_slim %>% 
  filter(tpf_cust_mod_tenure == 0) %>% 
  filter(referral_rsn_grp_desc == "Credit") %>% 
  filter(Avg_OOP < 160) %>% group_by(cntry_cd, referral_rsn_desc) %>% 
  filter(referral_rsn_desc == "NB OOP") %>% plot_ly(type = "box", y=Avg_OOP, color=cntry_cd)

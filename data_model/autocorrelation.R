hh_df$e <- m3$residuals

aux_1 <-   lm(e ~-1 + lag(e), data = hh_df)
aux_1 %>%
  tidy()

aux_2 <- plm(e ~  lag(e) +
              DV_hh_ls_pc_lag1 + 
               IV_lending_pc + IV_lending_pc_lag1 + #IV_lending_pc_lag2 + 
               IV_gov_exp_pc + IV_gov_exp_pc_lag1 + #IV_gov_exp_pc_lag2 +
               IV_trade_balance_pc + IV_trade_balance_pc_lag1 + #IV_trade_balance_pc_lag2 +                            
               IV_fdi_net_pc + IV_fdi_net_pc_lag1 + #IV_fdi_outflow_pc_lag2 +
               C_REER + C_wgdp + C_cpi + C_wgini,
             data = hh_df, model = 'within',
             effect = 'twoways', index = c('Country', 'Year'))

nR2 <- 
  aux_2 %>% 
  r.squared *
  aux_2$df.residual 
nR2 %>% 
  pchisq(1, lower.tail = F)

aux_1 %>%  
  tidy()
# 0.253

aux_2 %>%  
  tidy()
# 0.0614


roh <- aux_2$coefficients[1]

m3_trans  <- plm(I(DV_hh_ls_pc- roh*lag(DV_hh_ls_pc)) ~  
           I(DV_hh_ls_pc_lag1- roh*lag(DV_hh_ls_pc_lag1)) + 
           I(IV_lending_pc- roh*lag(IV_lending_pc)) + I(IV_lending_pc_lag1-roh*lag(IV_lending_pc_lag1)) +
            I(IV_gov_exp_pc- roh*lag(IV_gov_exp_pc)) + I(IV_gov_exp_pc_lag1-roh*lag(IV_gov_exp_pc_lag1)) +
            I(IV_trade_balance_pc- roh*lag(IV_trade_balance_pc)) + I(IV_trade_balance_pc_lag1-roh*lag(IV_trade_balance_pc_lag1)) +
             I(IV_fdi_net_pc- roh*lag(IV_fdi_net_pc)) + I(IV_fdi_net_pc_lag1-roh*lag(IV_fdi_net_pc_lag1)) +
             I(C_REER-roh*lag(C_REER)) + 
             I(C_wgdp-roh*lag(C_wgdp)) + 
             I(C_cpi-roh*lag(C_cpi)) + 
             I(C_wgini-roh*lag(C_wgini)), 
           data = hh_df, model = 'within',
           effect = 'twoways', index = c('Country', 'Year'))
summary(m3_trans)
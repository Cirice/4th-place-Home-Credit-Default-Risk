library(xgboost)
library(magrittr)
set.seed(0)
library(data.table)
library(readr)
library(dplyr)
library(e1071)

setwd('/home/suresh/Downloads/HomeCreditDefault')

#---------------------------
cat("Loading data...\n")

bbalance <- read_csv("bureau_balance.csv") 
bureau <- read_csv("bureau.csv")
cc_balance <- read_csv("credit_card_balance.csv")
payments <- read_csv("installments_payments.csv") 
pc_balance <- read_csv("POS_CASH_balance.csv")
prev <- read_csv("previous_application.csv")
tr <- read_csv("application_train.csv") 
te <- read_csv("application_test.csv")

#################################### Pre processing ###########################################################################
tr$OCCUPATION_TYPE_EMPTY = 0
tr$OCCUPATION_TYPE_EMPTY[is.na(tr$OCCUPATION_TYPE)] = 1
te$OCCUPATION_TYPE_EMPTY = 0
te$OCCUPATION_TYPE_EMPTY[is.na(te$OCCUPATION_TYPE)] = 1

tr$OCCUPATION_TYPE_LABORERS = 0
tr$OCCUPATION_TYPE_LABORERS[tr$OCCUPATION_TYPE %in% c('Cleaning staff', 'Cooking staff', 'Low-skill Laborers', 'Waiters/barmen staff', 'Drivers')] = 1

te$OCCUPATION_TYPE_LABORERS = 0
te$OCCUPATION_TYPE_LABORERS[te$OCCUPATION_TYPE %in% c('Cleaning staff', 'Cooking staff', 'Low-skill Laborers', 'Waiters/barmen staff', 'Drivers')] = 1


tr$OWN_CAR_AGE[tr$OWN_CAR_AGE > 80] = NaN
te$OWN_CAR_AGE[te$OWN_CAR_AGE > 80] = NaN
tr$REGION_RATING_CLIENT_W_CITY[tr$REGION_RATING_CLIENT_W_CITY < 0] = NaN
te$REGION_RATING_CLIENT_W_CITY[te$REGION_RATING_CLIENT_W_CITY < 0] = NaN

tr$AMT_INCOME_TOTAL[tr$AMT_INCOME_TOTAL > 1000000] = 1000000
te$AMT_INCOME_TOTAL[te$AMT_INCOME_TOTAL > 1000000] = 1000000

tr$AMT_REQ_CREDIT_BUREAU_QRT[tr$AMT_REQ_CREDIT_BUREAU_QRT > 5] = 5
te$AMT_REQ_CREDIT_BUREAU_QRT[te$AMT_REQ_CREDIT_BUREAU_QRT > 5] = 5

te$OBS_30_CNT_SOCIAL_CIRCLE[te$OBS_30_CNT_SOCIAL_CIRCLE > 20] = 20
tr$OBS_30_CNT_SOCIAL_CIRCLE[tr$OBS_30_CNT_SOCIAL_CIRCLE > 20] = 20


tr$DAYS_LAST_PHONE_CHANGE[is.na(tr$DAYS_LAST_PHONE_CHANGE)] = NaN
te$DAYS_LAST_PHONE_CHANGE[is.na(te$DAYS_LAST_PHONE_CHANGE)] = NaN

tr$CODE_GENDER[tr$CODE_GENDER == 'XNA'] = NaN
te$CODE_GENDER[te$CODE_GENDER == 'XNA'] = NaN

tr$DAYS_EMPLOYED[tr$DAYS_EMPLOYED == 365243] = NaN
te$DAYS_EMPLOYED[te$DAYS_EMPLOYED == 365243] = NaN

tr$NAME_FAMILY_STATUS[tr$NAME_FAMILY_STATUS == 'Unknown'] = NaN
te$NAME_FAMILY_STATUS[te$NAME_FAMILY_STATUS == 'Unknown'] = NaN

tr$ORGANIZATION_TYPE[tr$ORGANIZATION_TYPE == 'XNA'] = NaN
te$ORGANIZATION_TYPE[te$ORGANIZATION_TYPE == 'XNA'] = NaN

tr$ORGANIZATION_TYPE[tr$ORGANIZATION_TYPE == 'XNA'] = NaN
te$ORGANIZATION_TYPE[te$ORGANIZATION_TYPE == 'XNA'] = NaN

tr$NAME_INCOME_TYPE[!(tr$NAME_INCOME_TYPE %in% c('Commercial associate','Pensioner','State servant','Working'))] = 'Other'
te$NAME_INCOME_TYPE[!(te$NAME_INCOME_TYPE %in% c('Commercial associate','Pensioner','State servant','Working'))] = 'Other'

tr$NAME_FAMILY_STATUS[!(tr$NAME_FAMILY_STATUS == 'Unknown')] = NaN

tr$POPULAR_AMT_GOODS_PRICE = 0
tr$POPULAR_AMT_GOODS_PRICE[tr$AMT_GOODS_PRICE %in% c(225000, 450000, 675000, 900000)] = 1
te$POPULAR_AMT_GOODS_PRICE = 0
te$POPULAR_AMT_GOODS_PRICE[te$AMT_GOODS_PRICE %in% c(225000, 450000, 675000, 900000)] = 1

tr$POPULAR_MOST_AMT_GOODS_PRICE = 0
tr$POPULAR_MOST_AMT_GOODS_PRICE[tr$AMT_GOODS_PRICE %in% c(1125000, 1350000, 1575000, 1800000, 2250000)] = 1
te$POPULAR_MOST_AMT_GOODS_PRICE = 0
te$POPULAR_MOST_AMT_GOODS_PRICE[te$AMT_GOODS_PRICE %in% c(1125000, 1350000, 1575000, 1800000, 2250000)] = 1

cc_balance$AMT_DRAWINGS_ATM_CURRENT[cc_balance$AMT_DRAWINGS_ATM_CURRENT < 0] = NaN
cc_balance$AMT_DRAWINGS_CURRENT[cc_balance$AMT_DRAWINGS_CURRENT < 0] = NaN

cc_balance$AMT_PAYMENT_CURRENT[cc_balance$AMT_PAYMENT_CURRENT > 4000000] = NaN
cc_balance$AMT_CREDIT_LIMIT_ACTUAL[cc_balance$AMT_CREDIT_LIMIT_ACTUAL > 1000000] = NaN


prev$DAYS_FIRST_DRAWING[prev$DAYS_FIRST_DRAWING == 365243]  = NaN
prev$DAYS_FIRST_DUE[prev$DAYS_FIRST_DUE == 365243]  = NaN
prev$DAYS_LAST_DUE[prev$DAYS_LAST_DUE == 365243]  = NaN
prev$DAYS_TERMINATION[prev$DAYS_TERMINATION == 365243]  = NaN
prev$DAYS_LAST_DUE_1ST_VERSION[prev$DAYS_LAST_DUE_1ST_VERSION == 365243]  = NaN

#dim(bureau)
#bureau <- bureau %>% filter(DAYS_ENDDATE_FACT >= DAYS_CREDIT)

bureau$DAYS_CREDIT_ENDDATE[bureau$DAYS_CREDIT_ENDDATE < -40000] = NaN
bureau$DAYS_CREDIT_UPDATE[bureau$DAYS_CREDIT_UPDATE < -40000] = NaN
bureau$DAYS_ENDDATE_FACT[bureau$DAYS_ENDDATE_FACT < -40000] = NaN
dim(bureau)

bureau$AMT_ANNUITY[bureau$AMT_ANNUITY > 8e7] = NaN
bureau$AMT_CREDIT_SUM[bureau$AMT_CREDIT_SUM > 3e8] = NaN
bureau$AMT_CREDIT_SUM_DEBT[bureau$AMT_CREDIT_SUM_DEBT > 1e8] = NaN
bureau$AMT_CREDIT_MAX_OVERDUE[bureau$AMT_CREDIT_MAX_OVERDUE > 8e7] = NaN
##########################################################################################################################################
cat("Preprocessing...\n")
fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))

sum_bbalance <- bbalance %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_BUREAU) %>% 
  summarise_all(fn)
gc()

# get min and max of months balance
sum_bbalance_agg1 <- bbalance %>%
  arrange(SK_ID_BUREAU, MONTHS_BALANCE)  %>%
  group_by(SK_ID_BUREAU)  %>%
  summarise(x_min_months_balance = min(MONTHS_BALANCE),
            x_max_months_balance = max(MONTHS_BALANCE))

# get status when months_balance is equal to x_max_months_balance as Last_status
bb_balance_x_status <- bbalance %>% left_join(sum_bbalance_agg1) %>%
  filter(MONTHS_BALANCE == x_max_months_balance) %>%
  mutate(Last_status = STATUS)  %>%
  select(SK_ID_BUREAU, Last_status) %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer))

# get status when months_balance is equal to x_min_months_balance as First_status
bb_balance_y_status <- bbalance %>% left_join(sum_bbalance_agg1) %>%
  filter(MONTHS_BALANCE == x_min_months_balance) %>%
  mutate(First_status = STATUS)  %>%
  select(SK_ID_BUREAU, First_status)  %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer))

### GET maximum of months of balance as Months_balance
bb_balance_w_status <- bbalance %>% left_join(sum_bbalance_agg1) %>%
  filter(MONTHS_BALANCE == x_max_months_balance) %>%
  mutate(Month_bbalance = abs(MONTHS_BALANCE))  %>%
  select(SK_ID_BUREAU, Month_bbalance)

##### get CLOSED loans and then get maximum months balance by SK_ID_bureau
sum_bbalance_aggx <- bbalance %>%
  arrange(SK_ID_BUREAU, MONTHS_BALANCE)  %>%
  group_by(SK_ID_BUREAU) %>%
  summarise(When_closed = max(MONTHS_BALANCE))

#bb_balance_z_status <- bbalance %>% left_join(sum_bbalance_aggx) %>%
#  filter(When_closed == MONTHS_BALANCE)   %>%
#  select(SK_ID_BUREAU, When_closed)

sum_bbalance <- sum_bbalance %>% left_join(bb_balance_y_status)
sum_bbalance <- sum_bbalance %>% left_join(bb_balance_x_status)
sum_bbalance <- sum_bbalance %>% left_join(bb_balance_w_status)
sum_bbalance <- sum_bbalance %>% left_join(sum_bbalance_aggx)

sum_bbalance <- sum_bbalance %>% mutate (newbbalance_top = When_closed -  Month_bbalance)

sum_bureau <- bureau %>% 
  left_join(sum_bbalance, by = "SK_ID_BUREAU") %>% 
  select(-SK_ID_BUREAU) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
gc()

####### bureau features #######################################################################
#bureau <- read_csv("bureau.csv")
dim(sum_bureau)

bureau$DAYS_CREDIT_ENDDATE[bureau$DAYS_CREDIT_ENDDATE < -10000] = NaN
bureau$DAYS_CREDIT_UPDATE[bureau$DAYS_CREDIT_UPDATE < -10000] = NaN
bureau$DAYS_ENDDATE_FACT[bureau$DAYS_ENDDATE_FACT < -10000] = NaN


dim(sum_bureau)

bureau$bureau_credit_active_binary = 0
bureau$bureau_credit_active_binary[bureau$CREDIT_ACTIVE != 'Closed'] = 1
bureau$bureau_credit_enddate_binary = 0
bureau$bureau_credit_enddate_binary[bureau$DAYS_CREDIT_ENDDATE > 0] = 1
bureau$DAYS_CREDIT_UPDATE[bureau$DAYS_CREDIT_UPDATE > 0] = NaN

bureau_active <- bureau %>%
  filter(CREDIT_ACTIVE == 'Active') %>%
  group_by(SK_ID_CURR) %>%
  summarise(ACTIVE_DAYS_CREDIT_MAX = max(DAYS_CREDIT, na.rm=TRUE),
            ACTIVE_DAYS_CREDIT_END_DATE = min(DAYS_CREDIT_ENDDATE, na.rm=TRUE),
            ACTIVE_DAYS_CREDIT_UPDATE_MEAN = mean(DAYS_CREDIT_UPDATE, na.rm=TRUE),
            ACTIVE_AMT_CREDIT_SUM_SUM = sum(AMT_CREDIT_SUM, na.rm=TRUE),
            ACTIVE_CNT_CREDIT_PROLONG_SUM = sum())

bureau_closed <- bureau %>%
  filter(CREDIT_ACTIVE == 'Closed') %>%
  group_by(SK_ID_CURR) %>%
  summarise(CLOSED_DAYS_CREDIT_MAX = max(DAYS_CREDIT, na.rm=TRUE),
            CLOSED_AMT_CREDIT_SUM_SUM = sum(AMT_CREDIT_SUM, na.rm=TRUE),
            CLOSED_DAYS_CREDIT_END_DATE_MAX = max(DAYS_CREDIT_ENDDATE, na.rm=TRUE),
            CLOSED_AMT_CREDIT_SUM_MEAN = mean(AMT_CREDIT_SUM, na.rm=TRUE))

bureau_agg <- bureau %>%
  group_by(SK_ID_CURR) %>%
  summarize(bureau_credit_active_binary_mean = mean(bureau_credit_active_binary))

sum_bureau <- sum_bureau %>%
  left_join(bureau_agg, by = 'SK_ID_CURR')

dim(sum_bureau)

bureau_agg <- bureau %>%
  group_by(SK_ID_CURR) %>%
  summarize(bureau_credit_enddate_mean = mean(bureau_credit_enddate_binary))

sum_bureau <- sum_bureau %>%
  left_join(bureau_agg, by = 'SK_ID_CURR')

dim(sum_bureau)

bureau_agg <- bureau %>%
  group_by(SK_ID_CURR) %>%
  summarize(bureau_total_customer_credit = sum(AMT_CREDIT_SUM))

sum_bureau <- sum_bureau %>%
  left_join(bureau_agg, by = 'SK_ID_CURR')

dim(sum_bureau)

bureau_agg <- bureau %>%
  group_by(SK_ID_CURR) %>%
  summarize(bureau_total_customer_debt = sum(AMT_CREDIT_SUM_DEBT))

sum_bureau <- sum_bureau %>%
  left_join(bureau_agg, by = 'SK_ID_CURR')

dim(sum_bureau)

bureau_agg <- bureau %>%
  group_by(SK_ID_CURR) %>%
  summarize(bureau_total_customer_overdue = sum(AMT_CREDIT_SUM_OVERDUE))

sum_bureau <- sum_bureau %>%
  left_join(bureau_agg, by = 'SK_ID_CURR')

sum_bureau$bureau_debt_credit_ratio  = sum_bureau$bureau_total_customer_debt / sum_bureau$bureau_total_customer_credit
sum_bureau$bureau_overdue_debt_ratio  = sum_bureau$bureau_total_customer_overdue / sum_bureau$bureau_total_customer_debt

sum_bureau <- sum_bureau %>%
  select(-c(bureau_total_customer_debt, bureau_total_customer_credit, bureau_total_customer_overdue))

bureau_agg4 <- bureau %>%
  group_by(SK_ID_CURR) %>%
  summarize(bureau_number_of_past_loans = n_distinct(SK_ID_BUREAU),
            bureau_number_of_loan_types = n_distinct(CREDIT_TYPE),
            n_loans_per_loan_style = bureau_number_of_past_loans/bureau_number_of_loan_types)

sum_bureau <- sum_bureau %>% left_join(bureau_agg4)

dim(sum_bureau)

#########################################################################################################################################################
cc_balance <- cc_balance %>%
  mutate(card_SK_DPD_MONTHS_BALANCE = SK_DPD - MONTHS_BALANCE,
         card_SK_DPD_DEF_MONTHS_BALANCE = SK_DPD_DEF - MONTHS_BALANCE,
         card_AMT_TOTAL_RECEIVABLE_AMT_RECIVABLEE = AMT_TOTAL_RECEIVABLE - AMT_RECIVABLE,
         card_AMT_RECIVABLE_AMT_RECEIVABLE_PRINCIPAL = AMT_RECIVABLE - AMT_RECEIVABLE_PRINCIPAL,
         card_AMT_BALANCE_AMT_RECIVABLE = AMT_BALANCE - AMT_RECIVABLE,
         card_AMT_DRAWINGS_CURRENT_AMT_DRAWINGS_ATM_CURRENT = AMT_DRAWINGS_CURRENT - AMT_DRAWINGS_ATM_CURRENT,
         card_AMT_DRAWINGS_CURRENT_AMT_DRAWINGS_OTHER_CURRENT = AMT_DRAWINGS_CURRENT - AMT_DRAWINGS_OTHER_CURRENT,
         card_AMT_DRAWINGS_CURRENT_AMT_DRAWINGS_POS_CURRENT = AMT_DRAWINGS_CURRENT - AMT_DRAWINGS_POS_CURRENT)


sum_cc_balance <- cc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
gc()

dim(sum_cc_balance)

cc_balance_agg <- cc_balance %>% 
  group_by(SK_ID_CURR, SK_ID_PREV) %>%
  summarise(number_of_paid_installments = max(CNT_INSTALMENT_MATURE_CUM, na.rm=TRUE))

cc_balance_agg <- cc_balance_agg %>% 
  group_by(SK_ID_CURR) %>%
  summarise(total_number_of_installments = sum(number_of_paid_installments, na.rm=TRUE))

sum_cc_balance <- sum_cc_balance %>%
  left_join(cc_balance_agg)

dim(sum_cc_balance)

cc_balance_agg <- cc_balance %>% 
  group_by(SK_ID_CURR) %>%
  summarise(credit_card_max_loading_of_credit_limit = max(AMT_BALANCE, na.rm=TRUE)/max(AMT_CREDIT_LIMIT_ACTUAL, na.rm=TRUE))

sum_cc_balance <- sum_cc_balance %>%
  left_join(cc_balance_agg)

dim(sum_cc_balance)

sum_cc_balance$credit_card_cash_card_ratio = sum_cc_balance$AMT_DRAWINGS_ATM_CURRENT_sum/sum_cc_balance$AMT_DRAWINGS_CURRENT_sum


cc_balance_agg <- cc_balance %>%
  arrange(SK_ID_CURR, MONTHS_BALANCE)

cc_balance_agg$diff_amount_balance = c(0, diff(cc_balance_agg$AMT_BALANCE))
cc_balance_agg$diff_sk_id_curr = c(0,diff(cc_balance_agg$SK_ID_CURR))
cc_balance_agg$diff_amount_balance[cc_balance_agg$diff_sk_id_curr > 0] = 0

cc_balance_agg <- cc_balance_agg %>% group_by(SK_ID_CURR) %>%
  summarise(mean_payment_cc = mean(diff_amount_balance, na.rm=TRUE))

sum_cc_balance <- sum_cc_balance %>%
  left_join(cc_balance_agg)

##################################################################################################################################################
payments$NUM_INSTALMENT_VERSION[payments$NUM_INSTALMENT_VERSION > 70] = NaN
payments$DAYS_ENTRY_PAYMENT[payments$DAYS_ENTRY_PAYMENT < -4000] = NaN

payments$NUM_INSTALMENT_NUMBER_100 = 0
payments$NUM_INSTALMENT_NUMBER_100[payments$NUM_INSTALMENT_NUMBER == 100] = 1

payments <- payments %>%
  mutate(ins_DAYS_INSTALMENT_more_NUM_INSTALMENT_NUMBER = DAYS_INSTALMENT > (NUM_INSTALMENT_NUMBER * 50/3)  - (11500/3))

sum_payments <- payments %>% 
  select(-SK_ID_PREV) %>% 
  filter(DAYS_INSTALMENT > -730) %>%
  filter(DAYS_INSTALMENT > -730) %>%
  mutate(PAYMENT_PERC = AMT_PAYMENT / AMT_INSTALMENT,
         PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
         DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
         DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
         DPD = ifelse(DPD > 0, DPD, 0),
         DBD = ifelse(DBD > 0, DBD, 0)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
gc()

fnp <- funs(mean, sd, min, max, sum, median, skewness, kurtosis, IQR, .args = list(na.rm = TRUE))
agg_payments12_months <- payments %>% 
  filter(DAYS_INSTALMENT > -366) %>%
  select(-SK_ID_PREV) %>%
  mutate(installment_paid_late_in_days_12_months = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
         installment_paid_late_12_months = installment_paid_late_in_days_12_months > 0,
         installment_paid_over_amount_12_months = AMT_PAYMENT - AMT_INSTALMENT,
         installment_paid_over_12_months = installment_paid_over_amount_12_months > 0) %>%
  select(c(SK_ID_CURR, NUM_INSTALMENT_VERSION, installment_paid_late_in_days_12_months, installment_paid_over_amount_12_months)) %>%
  group_by(SK_ID_CURR) %>%
  summarise_all(fnp)

agg_payments12_months2 <- payments %>% 
  filter(DAYS_INSTALMENT > -366) %>%
  select(-SK_ID_PREV) %>%
  mutate(installment_paid_late_in_days_12_months = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
         installment_paid_late_12_months = installment_paid_late_in_days_12_months > 0,
         installment_paid_over_amount_12_months = AMT_PAYMENT - AMT_INSTALMENT,
         installment_paid_over_12_months = installment_paid_over_amount_12_months > 0)  %>%
  select(c(SK_ID_CURR, installment_paid_late_12_months, installment_paid_over_12_months)) %>%
  group_by(SK_ID_CURR) %>%
  summarise(installment_paid_late_12_months_mean = mean(installment_paid_late_12_months),
            installment_paid_over_12_months_mean = mean(installment_paid_over_12_months),
            installment_paid_late_12_months_sum = sum(installment_paid_late_12_months),
            installment_paid_over_12_months_sum = sum(installment_paid_over_12_months))

agg_payments12_months <- agg_payments12_months %>% left_join(agg_payments12_months2)

fnp <- funs(mean, sd, min, max, sum, median, skewness, kurtosis, .args = list(na.rm = TRUE))
agg_payments_last_loan1 <- payments %>% 
  group_by(SK_ID_CURR) %>%
  summarise(DAYS_INSTALMENT_MAX = max(DAYS_INSTALMENT)) %>%
  left_join(payments) %>%
  filter(DAYS_INSTALMENT_MAX == DAYS_INSTALMENT)  %>%
  select(SK_ID_CURR, SK_ID_PREV) 
colnames(agg_payments_last_loan1) = c('SK_ID_CURR', 'SK_ID_PREV_X')

fnp <- funs(mean, sd, min, max, sum, .args = list(na.rm = TRUE))
agg_payments_last_loan2 <- payments %>% 
  left_join(agg_payments_last_loan1)  %>%
  filter(SK_ID_PREV_X == SK_ID_PREV)  %>%
  mutate(installment_paid_late_days_last_loan = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
         installment_paid_over_amount_last_loan = AMT_PAYMENT - AMT_INSTALMENT) %>%
  select(SK_ID_CURR, installment_paid_late_days_last_loan, installment_paid_over_amount_last_loan) %>%
  group_by(SK_ID_CURR)  %>%
  summarise_all(fnp)

fnp <- funs(mean, sum, .args = list(na.rm = TRUE))
agg_payments_last_loan3 <- payments %>% 
  left_join(agg_payments_last_loan1)  %>%
  filter(SK_ID_PREV_X == SK_ID_PREV)  %>%
  mutate(installment_paid_late_last_loan = (DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT) > 0,
         installment_paid_over_last_loan = (AMT_PAYMENT - AMT_INSTALMENT) > 0) %>%
  select(SK_ID_CURR, installment_paid_late_last_loan, installment_paid_over_last_loan) %>%
  group_by(SK_ID_CURR)  %>%
  summarise_all(fnp)

agg_payments_last_loan <- agg_payments_last_loan2 %>% left_join(agg_payments_last_loan3)

#############################################################################################################################
pc_balance$CNT_INSTALMENT_FUTURE[pc_balance$CNT_INSTALMENT_FUTURE > 60] =NaN

sum_pc_balance <- pc_balance %>%
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
gc()

pc_balance$pos_CNT_INSTALMENT_more_CNT_INSTALMENT_FUTURE = pc_balance$CNT_INSTALMENT > pc_balance$CNT_INSTALMENT_FUTURE

pc_agg <- pc_balance %>%
  group_by(SK_ID_CURR, SK_ID_PREV)  %>%
  summarize(min_remaining_pos_installments = min(CNT_INSTALMENT_FUTURE, na.rm = TRUE))

pc_agg1 <- pc_agg %>%
  group_by(SK_ID_CURR) %>%
  summarise(total_remaining_pos_installments = sum(min_remaining_pos_installments, na.rm = TRUE))

pc_agg2 <- pc_balance %>%
  group_by(SK_ID_CURR)  %>%
  summarize(total_completed_tran = sum(NAME_CONTRACT_STATUS == 'Completed', na.rm = TRUE))

pc_balance <- pc_balance %>%
  mutate(pos_cash_paid_late = SK_DPD > 0,
         pos_cash_paid_late_with_tolerance = SK_DPD_DEF > 0)

pc_agg3 <- pc_balance %>%
  filter(MONTHS_BALANCE > -19) %>%
  group_by(SK_ID_CURR)  %>%
  summarize(pos_cash_paid_late_total_18_months = sum(pos_cash_paid_late, na.rm=TRUE),
            pos_cash_paid_late_with_tolerance_18_months = sum(pos_cash_paid_late_with_tolerance, na.rm=TRUE),
            pos_cash_paid_late_total_18_months = mean(pos_cash_paid_late, na.rm=TRUE),
            pos_cash_paid_late_with_tolerance_18_months = mean(pos_cash_paid_late_with_tolerance, na.rm=TRUE),
            SK_DPD_SUMX = sum(SK_DPD, na.rm=TRUE),
            SK_DPD_MEANX = mean(SK_DPD, na.rm=TRUE),
            SK_DPD_MAX = max(SK_DPD, na.rm=TRUE),
            SK_DPD_STD = sd(SK_DPD, na.rm=TRUE),
            SK_DPD_SKEW = skewness(SK_DPD, na.rm=TRUE),
            SK_DPD_KURT = kurtosis(SK_DPD, na.rm=TRUE),
            SK_DPD_DEF_SUMX = sum(SK_DPD_DEF, na.rm=TRUE),
            SK_DPD_DEF_MEANX = mean(SK_DPD_DEF, na.rm=TRUE),
            SK_DPD_DEF_MAX = max(SK_DPD_DEF, na.rm=TRUE),
            SK_DPD_DEF_STD = sd(SK_DPD_DEF, na.rm=TRUE),
            SK_DPD_DEF_SKEW = skewness(SK_DPD_DEF, na.rm=TRUE),
            SK_DPD_DEF_KURT = kurtosis(SK_DPD_DEF, na.rm=TRUE))

pc_agg44 <- pc_balance %>%
  group_by(SK_ID_CURR)  %>%
  summarize(pos_cash_paid_late_all_months_sum = sum(pos_cash_paid_late, na.rm=TRUE),
            pos_cash_paid_late_with_tolerance_all_months_sum = sum(pos_cash_paid_late_with_tolerance, na.rm=TRUE),
            pos_cash_paid_late_all_months_mean = mean(pos_cash_paid_late, na.rm=TRUE),
            pos_cash_paid_late_with_tolerance_all_months_mean = mean(pos_cash_paid_late_with_tolerance, na.rm=TRUE))


pc_last_loan_id <- pc_balance %>%
  group_by(SK_ID_CURR)  %>%
  filter(MONTHS_BALANCE == min(MONTHS_BALANCE, na.rm=TRUE)) %>%
  select(SK_ID_CURR, SK_ID_PREV)

colnames(pc_last_loan_id) = c('SK_ID_CURR', 'LAST_LOAN_ID')

pc_last_loan_idx <- pc_balance %>%
  left_join(pc_last_loan_id)

pc_agg4 <- pc_last_loan_idx %>%
  filter(SK_ID_PREV==LAST_LOAN_ID)  %>%
  group_by(SK_ID_CURR)  %>%
  summarise(pos_cash_paid_late_last_loan_sum = sum(pos_cash_paid_late, na.rm=TRUE),
            pos_cash_paid_late_last_loan_mean = mean(pos_cash_paid_late, na.rm=TRUE),
            pos_cash_paid_late_with_tolerance_last_loan_mean = mean(pos_cash_paid_late_with_tolerance, na.rm=TRUE),
            SK_DPD_last_loan_mean = mean(SK_DPD, na.rm=TRUE),
            SK_DPD_last_loan_max = max(SK_DPD, na.rm=TRUE),
            SK_DPD_last_loan_sd = sd(SK_DPD, na.rm=TRUE),
            SK_DPD_last_loan_sum = sum(SK_DPD, na.rm=TRUE),
            SK_DPD_DEF_last_loan_mean = mean(SK_DPD_DEF, na.rm=TRUE),
            SK_DPD_DEF_last_loan_max = max(SK_DPD_DEF, na.rm=TRUE),
            SK_DPD_DEF_last_loan_sd = sd(SK_DPD_DEF, na.rm=TRUE),
            SK_DPD_DEF_last_loan_sum = sum(SK_DPD_DEF, na.rm=TRUE))


dim(pc_agg1)
dim(pc_agg2)
dim(pc_agg3)
dim(pc_agg4)

sum_pc_balance <- pc_agg1 %>% left_join(pc_agg2) %>% left_join(pc_agg3) %>% left_join(pc_agg4)
sum_pc_balance <- sum_pc_balance %>% left_join(pc_agg44)

dim(sum_pc_balance)

#########################################################################################################################################

prev$AMT_CREDIT[prev$AMT_CREDIT > 6000000] = NaN
prev$SELLERPLACE_AREA[prev$SELLERPLACE_AREA > 3500000] = NaN
prev$DAYS_TERMINATION = prevz$DAYS_TERMINATION
prev$DAYS_TERMINATION_LESS_500 = prev$DAYS_TERMINATION < -500

prev <- prev %>% 
  mutate(prev_AMT_APPLICATION_AMT_CREDIT = AMT_APPLICATION - AMT_CREDIT,
         prev_DAYS_FIRST_DRAWING_DAYS_FIRST_DUE = DAYS_FIRST_DRAWING - DAYS_FIRST_DUE,
         prev_AMT_APPLICATION_DIV_AMT_CREDIT = AMT_APPLICATION/AMT_CREDIT)

sum_prev <- prev %>%
  select(-SK_ID_PREV) %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
         DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
         DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
         DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
         DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION),
         APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
gc()

prev_agg1 <- prev %>%
  filter(FLAG_LAST_APPL_PER_CONTRACT == 'Y') %>%
  select(-SK_ID_PREV) %>%
  mutate(previous_application_prev_was_approved = (NAME_CONTRACT_STATUS == 'Approved'),
         previous_application_prev_was_refused = (NAME_CONTRACT_STATUS == 'Refused'))  %>%
  select(c(SK_ID_CURR, previous_application_prev_was_approved, previous_application_prev_was_refused, DAYS_DECISION))

prev_agg11 <- prev %>%
  group_by(SK_ID_CURR)  %>%
  summarise(DAYS_DECISION_MAX = max(DAYS_DECISION)) %>%
  select(c(SK_ID_CURR, DAYS_DECISION_MAX))

prev_agg1 <- prev_agg1 %>% left_join(prev_agg11) %>%
  filter(DAYS_DECISION_MAX == DAYS_DECISION) %>%
  select(c(SK_ID_CURR, previous_application_prev_was_approved, previous_application_prev_was_refused))

prev_agg1 <- prev_agg1 %>% group_by(SK_ID_CURR)  %>%
  summarise(prev_app_uscess_mean = mean(previous_application_prev_was_approved))

prev_agg2 <- prev %>%
  select(-SK_ID_PREV) %>%
  mutate(previous_application_prev_was_approved = (NAME_CONTRACT_STATUS == 'Approved'),
         previous_application_prev_was_refused = (NAME_CONTRACT_STATUS == 'Refused')) %>%
  group_by(SK_ID_CURR)  %>%
  summarize(previous_application_fraction_of_refused_applications = mean(previous_application_prev_was_refused)) %>%
  select(c(SK_ID_CURR, previous_application_fraction_of_refused_applications))

prev_agg31 <- prev %>%
  group_by(SK_ID_CURR)  %>%
  summarise(DAYS_DECISION_MAX = max(DAYS_DECISION)) %>%
  select(c(SK_ID_CURR, DAYS_DECISION_MAX))

prev_agg31 <- prev %>% left_join(prev_agg31) %>%
  filter(DAYS_DECISION_MAX == DAYS_DECISION) %>%
  mutate(prev_applications_prev_was_revolving_loan = (NAME_CONTRACT_TYPE ==  'Revolving loans'))  %>%
  select(c(SK_ID_CURR, prev_applications_prev_was_revolving_loan))

prev_agg3 <- prev_agg31 %>%  group_by(SK_ID_CURR)  %>%
  summarise(prev_app_revolving_loan_mean = mean(prev_applications_prev_was_revolving_loan))

sum_prev <- sum_prev %>% left_join(prev_agg1, by='SK_ID_CURR') %>% left_join(prev_agg2) %>% left_join(prev_agg3)

prev$DAYS_FIRST_DRAWING[prev$DAYS_FIRST_DRAWING==365243] = NA

prev_agg31 <- prev %>%
  group_by(SK_ID_CURR)  %>%
  summarise(DAYS_DECISION_MAX = max(DAYS_DECISION)) %>%
  select(c(SK_ID_CURR, DAYS_DECISION_MAX))

prev_agg31 <- prev %>% left_join(prev_agg31) %>%
  filter(!(DAYS_DECISION_MAX == DAYS_DECISION)) 

prev_agg31 <- prev_agg31 %>%
  group_by(SK_ID_CURR)  %>%
  summarise(DAYS_DECISION_MAX_2 = max(DAYS_DECISION)) %>%
  select(c(SK_ID_CURR, DAYS_DECISION_MAX_2))

prev_agg31 <- prev %>% left_join(prev_agg31) %>%
  filter(DAYS_DECISION_MAX_2 == DAYS_DECISION) 

prev_agg31f <- prev_agg31 %>%
  group_by(SK_ID_CURR)  %>%
  summarize(cnt_payment_2_mean = mean(CNT_PAYMENT, na.rm=TRUE),
            days_decision_2_mean = mean(DAYS_DECISION, na.rm=TRUE),
            days_first_drawing_2_mean = mean(DAYS_FIRST_DRAWING, na.rm=TRUE))

sum_prev <- sum_prev %>% left_join(prev_agg31f)

dim(sum_prev)

fns = funs(mean, min, max, .args = list(na.rm = TRUE))
prev_agg4 <- prev %>%
  select(-SK_ID_PREV) %>%
  filter(FLAG_LAST_APPL_PER_CONTRACT == 'Y') %>%
  filter(DAYS_DECISION > -830) %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
         DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
         DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
         DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
         DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION),
         AMT_ANNUITY = ifelse(AMT_ANNUITY == 0, NaN, AMT_ANNUITY),
         AMT_CREDIT = ifelse(AMT_CREDIT == 0, NaN, AMT_CREDIT),
         AMT_GOODS_PRICE = ifelse(AMT_GOODS_PRICE == 0, NaN, AMT_GOODS_PRICE),
         NEW_AMT_DOWN_PAYMENT_DIVIDE_AMT_CREDIT = AMT_DOWN_PAYMENT / AMT_CREDIT,
         NEW_AMT_CREDIT_DIVIDE_AMT_ANNUITY = AMT_CREDIT / AMT_ANNUITY,
         NEW_AMT_CREDIT_DIVIDE_AMT_GOODS_PRICE = AMT_CREDIT / AMT_GOODS_PRICE,
         NEW_AMT_APPLICATION_DIV_CREDIT = AMT_APPLICATION / AMT_CREDIT
  ) %>%          
  select(SK_ID_CURR, NEW_AMT_DOWN_PAYMENT_DIVIDE_AMT_CREDIT, NEW_AMT_CREDIT_DIVIDE_AMT_ANNUITY, 
         NEW_AMT_CREDIT_DIVIDE_AMT_GOODS_PRICE, NEW_AMT_APPLICATION_DIV_CREDIT) %>%
  group_by(SK_ID_CURR) %>% 
  summarise_all(fns)

######################## Reason for previous application rejection ################################

prev_rejected <- prev %>% filter(NAME_CONTRACT_STATUS == 'Refused') %>%
  filter(FLAG_LAST_APPL_PER_CONTRACT == 'Y') %>%
  group_by(SK_ID_CURR) %>%
  summarize(latest_application_rejected_when = max(DAYS_DECISION)) 

prev_rejected_reason_HC <- prev %>% left_join(prev_rejected) %>%
  filter(FLAG_LAST_APPL_PER_CONTRACT == 'Y') %>%
  filter(NAME_CONTRACT_STATUS == 'Refused') %>%
  filter(DAYS_DECISION==latest_application_rejected_when) %>%
  filter(CODE_REJECT_REASON == 'HC') %>%
  mutate(CODE_REJECT_REASON_HC = 1)  %>%
  select(SK_ID_CURR, CODE_REJECT_REASON_HC) %>%
  distinct(SK_ID_CURR, CODE_REJECT_REASON_HC) %>%
  arrange(SK_ID_CURR)

prev_rejected_reason_LIMIT <- prev %>% left_join(prev_rejected) %>%
  filter(FLAG_LAST_APPL_PER_CONTRACT == 'Y') %>%
  filter(NAME_CONTRACT_STATUS == 'Refused') %>%
  filter(DAYS_DECISION==latest_application_rejected_when) %>%
  filter(CODE_REJECT_REASON == 'LIMIT') %>%
  mutate(CODE_REJECT_REASON_LIMIT = 1)  %>%
  select(SK_ID_CURR, CODE_REJECT_REASON_LIMIT) %>%
  distinct(SK_ID_CURR, CODE_REJECT_REASON_LIMIT) %>%
  arrange(SK_ID_CURR)

prev_rejected_reason_SCO <- prev %>% left_join(prev_rejected) %>%
  filter(FLAG_LAST_APPL_PER_CONTRACT == 'Y') %>%
  filter(NAME_CONTRACT_STATUS == 'Refused') %>%
  filter(DAYS_DECISION==latest_application_rejected_when) %>%
  filter(CODE_REJECT_REASON == 'SCO') %>%
  mutate(CODE_REJECT_REASON_SCO = 1)  %>%
  select(SK_ID_CURR, CODE_REJECT_REASON_SCO) %>%
  distinct(SK_ID_CURR, CODE_REJECT_REASON_SCO) %>%
  arrange(SK_ID_CURR)


prev_rejected_reason_VERIF <- prev %>% left_join(prev_rejected) %>%
  filter(FLAG_LAST_APPL_PER_CONTRACT == 'Y') %>%
  filter(NAME_CONTRACT_STATUS == 'Refused') %>%
  filter(DAYS_DECISION==latest_application_rejected_when) %>%
  filter(CODE_REJECT_REASON == 'VERIF') %>%
  mutate(CODE_REJECT_REASON_VERIF = 1)  %>%
  select(SK_ID_CURR, CODE_REJECT_REASON_VERIF) %>%
  distinct(SK_ID_CURR, CODE_REJECT_REASON_VERIF) %>%
  arrange(SK_ID_CURR)

prev_rejected_reason_SCOFR <- prev %>% left_join(prev_rejected) %>%
  filter(FLAG_LAST_APPL_PER_CONTRACT == 'Y') %>%
  filter(NAME_CONTRACT_STATUS == 'Refused') %>%
  filter(DAYS_DECISION==latest_application_rejected_when) %>%
  filter(CODE_REJECT_REASON == 'SCOFR') %>%
  mutate(CODE_REJECT_REASON_SCOFR = 1)  %>%
  select(SK_ID_CURR, CODE_REJECT_REASON_SCOFR) %>%
  distinct(SK_ID_CURR, CODE_REJECT_REASON_SCOFR) %>%
  arrange(SK_ID_CURR)


prev_rejected_reason_HC_count <- prev %>%
  filter(FLAG_LAST_APPL_PER_CONTRACT == 'Y') %>%
  filter(NAME_CONTRACT_STATUS == 'Refused') %>%
  filter(CODE_REJECT_REASON == 'HC') %>%
  group_by(SK_ID_CURR) %>%
  summarise(count_hc = n())

prev_rejected_reason_LIMIT_count <- prev %>%
  filter(FLAG_LAST_APPL_PER_CONTRACT == 'Y') %>%
  filter(NAME_CONTRACT_STATUS == 'Refused') %>%
  filter(CODE_REJECT_REASON == 'LIMIT') %>%
  group_by(SK_ID_CURR) %>%
  summarise(count_limit = n())

prev_rejected_reason_SCO_count <- prev %>%
  filter(FLAG_LAST_APPL_PER_CONTRACT == 'Y') %>%
  filter(NAME_CONTRACT_STATUS == 'Refused') %>%
  filter(CODE_REJECT_REASON == 'SCO') %>%
  group_by(SK_ID_CURR) %>%
  summarise(count_sco = n())

prev_rejected_reason_SCOFR_count <- prev %>%
  filter(FLAG_LAST_APPL_PER_CONTRACT == 'Y') %>%
  filter(NAME_CONTRACT_STATUS == 'Refused') %>%
  filter(CODE_REJECT_REASON == 'SCOFR') %>%
  group_by(SK_ID_CURR) %>%
  summarise(count_scofr = n())


prev_rejected_reason_VERIF_count <- prev %>%
  filter(FLAG_LAST_APPL_PER_CONTRACT == 'Y') %>%
  filter(NAME_CONTRACT_STATUS == 'Refused') %>%
  filter(CODE_REJECT_REASON == 'VERIF') %>%
  group_by(SK_ID_CURR) %>%
  summarise(count_VERIF = n())

sum_prev <- sum_prev %>% left_join(prev_agg4)
gc()

prev_approved <- prev %>%
  filter(NAME_CONTRACT_STATUS == 'Approved') %>%
  select(-SK_ID_PREV) %>%
  group_by(SK_ID_CURR) %>%
  summarise(APPROVED_CNT_PAYMENT_MEAN = mean(CNT_PAYMENT, na.rm=TRUE),
            APPROVED_DAYS_DECISION_MAX = max(DAYS_DECISION, na.rm=TRUE))

agg_payments12_trend_latepay <- payments %>% 
  filter(DAYS_INSTALMENT > -366) %>%
  arrange(SK_ID_CURR, NUM_INSTALMENT_NUMBER)  %>%
  select(-SK_ID_PREV) %>%
  mutate(installment_paid_late_in_days_12_months = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT) %>%
  select(c(SK_ID_CURR, installment_paid_late_in_days_12_months,  NUM_INSTALMENT_NUMBER))

agg_payments12_trend_latepay = data.table(setDT(agg_payments12_trend_latepay))
agg_payments12_trend_latepay[is.na(agg_payments12_trend_latepay)] = 0
resl_late_pay_coef = agg_payments12_trend_latepay[,coef(lm(installment_paid_late_in_days_12_months~NUM_INSTALMENT_NUMBER))[2],by=SK_ID_CURR]
resl_late_pay_intercept = agg_payments12_trend_latepay[,coef(lm(installment_paid_late_in_days_12_months~NUM_INSTALMENT_NUMBER))[1],by=SK_ID_CURR]

colnames(resl_late_pay_coef) = c('SK_ID_CURR', 'trend_coef_paid_late')
colnames(resl_late_pay_intercept) = c('SK_ID_CURR', 'trend_inter_paid_late')

pc_SK_DPD_DEF_60_trend <- pc_balance %>%
  filter(MONTHS_BALANCE > -61) %>%
  arrange(SK_ID_CURR, MONTHS_BALANCE)  %>%
  select(-SK_ID_PREV) %>%
  select(c(SK_ID_CURR, SK_DPD_DEF,MONTHS_BALANCE))

pc_SK_DPD_DEF_60_trend[is.na(pc_SK_DPD_DEF_60_trend)] = 0
pc_SK_DPD_DEF_60_trend = data.table(setDT(pc_SK_DPD_DEF_60_trend))
resl_pc_SK_DPD_DEF_60_trend_coef = pc_SK_DPD_DEF_60_trend[,coef(lm(SK_DPD_DEF~MONTHS_BALANCE))[2],by=SK_ID_CURR]
resl_pc_SK_DPD_DEF_60_trend_intercept = pc_SK_DPD_DEF_60_trend[,coef(lm(SK_DPD_DEF~MONTHS_BALANCE))[1],by=SK_ID_CURR]

colnames(resl_pc_SK_DPD_DEF_60_trend_coef) = c('SK_ID_CURR', 'trend_coef_DPD_DEF')
colnames(resl_pc_SK_DPD_DEF_60_trend_intercept) = c('SK_ID_CURR', 'trend_inter_DPD_DEF')

pc_agg_cnt_installment <- pc_balance %>%
  filter(MONTHS_BALANCE > -19) %>%
  arrange(SK_ID_CURR, MONTHS_BALANCE)  %>%
  select(-SK_ID_PREV) %>%
  select(c(SK_ID_CURR, CNT_INSTALMENT_FUTURE,  MONTHS_BALANCE, pos_cash_paid_late))

pc_agg_cnt_installment[is.na(pc_agg_cnt_installment)] = 0
pc_agg_cnt_installment = data.table(setDT(pc_agg_cnt_installment))
resl_future_install_coef = pc_agg_cnt_installment[,coef(lm(CNT_INSTALMENT_FUTURE~MONTHS_BALANCE))[2],by=SK_ID_CURR]
resl_future_install_intercept = pc_agg_cnt_installment[,coef(lm(CNT_INSTALMENT_FUTURE~MONTHS_BALANCE))[1],by=SK_ID_CURR]

colnames(resl_future_install_coef) = c('SK_ID_CURR', 'trend_coef_future_cnt')
colnames(resl_future_install_intercept) = c('SK_ID_CURR', 'trend_inter_future_cnt')


tri <- 1:nrow(tr)
y <- tr$TARGET

tr_te <- tr %>% 
  select(-TARGET) %>% 
  bind_rows(te) %>%
  left_join(sum_bureau, by = "SK_ID_CURR") %>% 
  left_join(sum_cc_balance, by = "SK_ID_CURR") %>%
  left_join(agg_payments12_months, by = "SK_ID_CURR") %>%
  left_join(agg_payments_last_loan, by = "SK_ID_CURR") %>%
  left_join(sum_payments, by = "SK_ID_CURR") %>%
  left_join(sum_pc_balance, by = "SK_ID_CURR") %>% 
  left_join(sum_prev, by = "SK_ID_CURR") %>% 
  left_join(bureau_active, by = "SK_ID_CURR") %>% 
  left_join(bureau_closed, by = "SK_ID_CURR") %>%
  left_join(prev_approved, by = "SK_ID_CURR") %>%
  left_join(resl_future_install_coef, by = "SK_ID_CURR") %>%
  left_join(resl_future_install_intercept, by = "SK_ID_CURR") %>%
  left_join(resl_pc_SK_DPD_DEF_60_trend_coef, by = "SK_ID_CURR") %>%
  left_join(resl_pc_SK_DPD_DEF_60_trend_intercept, by = "SK_ID_CURR") %>%
  left_join(resl_late_pay_coef, by = "SK_ID_CURR") %>%
  left_join(resl_late_pay_intercept, by = "SK_ID_CURR") %>%
  left_join(prev_rejected) %>%
  left_join(prev_rejected_reason_LIMIT) %>%
  left_join(prev_rejected_reason_HC) %>%
  left_join(prev_rejected_reason_SCO) %>%
  left_join(prev_rejected_reason_SCOFR) %>%
  left_join(prev_rejected_reason_VERIF) %>%
  left_join(prev_rejected_reason_HC_count) %>%
  left_join(prev_rejected_reason_LIMIT_count)  %>%
  left_join(prev_rejected_reason_SCO_count)  %>%
  left_join(prev_rejected_reason_SCOFR_count) %>%
  left_join(prev_rejected_reason_VERIF_count)

tr_te <- tr_te %>% 
  select(-SK_ID_CURR) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(na = apply(., 1, function(x) sum(is.na(x))),
         AMT_CREDIT_DIFF_AMT_GOODS = AMT_CREDIT - AMT_GOODS_PRICE,
         DAYS_EMPLOYED_DIFF_DAYS_BIRTH = DAYS_EMPLOYED - DAYS_BIRTH,
         AMT_INCOME_TOTAL_12_AMT_ANNUITY = (AMT_INCOME_TOTAL/12) - AMT_ANNUITY,
         DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED),
         #days_employed_percentage
         DAYS_EMPLOYED_PERC = sqrt(DAYS_EMPLOYED / DAYS_BIRTH),
         #income_credit_percentage
         INCOME_CREDIT_PERC = AMT_INCOME_TOTAL / AMT_CREDIT,
         #income_per_person
         INCOME_PER_PERSON = log1p(AMT_INCOME_TOTAL / CNT_FAM_MEMBERS),
         # annuity_income_percentage
         ANNUITY_INCOME_PERC = sqrt(AMT_ANNUITY / (1 + AMT_INCOME_TOTAL)), # how much the percentage of annuity compared to total income
         CC_PAYMENT_PLUS_LOAN_ANNUITY = AMT_ANNUITY + AMT_INST_MIN_REGULARITY_mean,
         CC_PAYMENT_PROP = AMT_TOTAL_RECEIVABLE_mean / AMT_PAYMENT_CURRENT_mean,
         CC_AMT_PAYMENT_TOTAL_CURRENT_PROP = AMT_TOTAL_RECEIVABLE_mean / AMT_PAYMENT_TOTAL_CURRENT_mean,
         ANNUITY_INCOME_PERC_CC_PAYMENT_PLUS = sqrt(CC_PAYMENT_PLUS_LOAN_ANNUITY / (1 + AMT_INCOME_TOTAL)),
         #credit_to_income_ratio
         LOAN_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
         #credit to annuity ratio
         PAYMENT_RATE = AMT_ANNUITY / AMT_CREDIT , # in how many installments loan will be repaid
         ANNUITY_LENGTH = AMT_CREDIT / AMT_ANNUITY, # in how many installments loan will be repaid
         #children_ratio
         CHILDREN_RATIO = CNT_CHILDREN / CNT_FAM_MEMBERS,
         #credit_to_goods_ratio
         CREDIT_TO_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
         #income_per_child
         INC_PER_CHLD = AMT_INCOME_TOTAL / (1 + CNT_CHILDREN),
         #income_per_adult
         INC_PER_NON_CHLD = AMT_INCOME_TOTAL / (1 + (CNT_FAM_MEMBERS - CNT_CHILDREN)),
         EXT_1_2_PROD = EXT_SOURCE_1 * EXT_SOURCE_2,
         EXT_1_DIV_BIRTH = EXT_SOURCE_1 / DAYS_BIRTH,
         EXT_2_DIV_BIRTH = EXT_SOURCE_2 / DAYS_BIRTH,
         EXT_3_DIV_BIRTH = EXT_SOURCE_3 / DAYS_BIRTH,
         SOURCES_PROD = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
         SOURCES_SD = sd(c(EXT_SOURCE_1, EXT_SOURCE_2, EXT_SOURCE_3)),
         #external_sources_weighted
         SOURCES_WEIGHTED = EXT_SOURCE_1 * 2 + EXT_SOURCE_2 * 3 + EXT_SOURCE_3 * 4,
         #car_to_birth_ratio
         CAR_TO_BIRTH_RATIO = OWN_CAR_AGE / DAYS_BIRTH,
         #cnt_non_child
         cnt_non_child = CNT_FAM_MEMBERS - CNT_CHILDREN,
         #child_to_non_child_ratio
         child_to_non_child_ratio = CNT_CHILDREN / cnt_non_child,
         #car_to_employ_ratio
         CAR_TO_EMPLOY_RATIO = OWN_CAR_AGE / DAYS_EMPLOYED,
         #phone_to_birth_ratio
         PHONE_TO_BIRTH_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_BIRTH,
         #phone_to_employ_ratio
         PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED,
         credit_per_non_child = AMT_CREDIT / cnt_non_child,
         short_employment = DAYS_EMPLOYED < -2000,
         young_age = DAYS_BIRTH < -14000,
         SOURCES_sum = sum(c(EXT_SOURCE_1, EXT_SOURCE_2,  EXT_SOURCE_3), na.rm=TRUE),
         SOURCES_min = min(c(EXT_SOURCE_1, EXT_SOURCE_2,  EXT_SOURCE_3), na.rm=TRUE),
         SOURCES_max = max(c(EXT_SOURCE_1, EXT_SOURCE_2,  EXT_SOURCE_3), na.rm=TRUE),
         #taken from public script Aug 2nd
         NEW_FLAG_CONTACT_SUM = FLAG_PHONE + FLAG_EMAIL + FLAG_CONT_MOBILE + FLAG_WORK_PHONE + FLAG_EMP_PHONE + FLAG_MOBIL,
         NEW_FLAG_OWN_CAR_REALITY = FLAG_OWN_CAR + FLAG_OWN_REALTY,
         SOURCES_sum_na = is.na(EXT_SOURCE_1) +  is.na(EXT_SOURCE_2) + is.na(EXT_SOURCE_3),
         FLAG_DOC_SUM = tr_te$FLAG_DOCUMENT_2 + tr_te$FLAG_DOCUMENT_3 + tr_te$FLAG_DOCUMENT_4 + tr_te$FLAG_DOCUMENT_7 + tr_te$FLAG_DOCUMENT_10 +  
           tr_te$FLAG_DOCUMENT_11 + tr_te$FLAG_DOCUMENT_12  + tr_te$FLAG_DOCUMENT_14 + tr_te$FLAG_DOCUMENT_15 + tr_te$FLAG_DOCUMENT_17 + 
           tr_te$FLAG_DOCUMENT_18 + tr_te$FLAG_DOCUMENT_19 + tr_te$FLAG_DOCUMENT_20 + tr_te$FLAG_DOCUMENT_16 + tr_te$FLAG_DOCUMENT_6 +
           tr_te$FLAG_DOCUMENT_21,
         DEF_60_CNT_SOCIAL_CIRCLE = ifelse(DEF_60_CNT_SOCIAL_CIRCLE > 4, 4, DEF_60_CNT_SOCIAL_CIRCLE),
         DEF_30_CNT_SOCIAL_CIRCLE = ifelse(DEF_30_CNT_SOCIAL_CIRCLE > 4, 4, DEF_30_CNT_SOCIAL_CIRCLE),
         OBS_60_CNT_SOCIAL_CIRCLE = ifelse(OBS_60_CNT_SOCIAL_CIRCLE > 4, 4, OBS_60_CNT_SOCIAL_CIRCLE),
         OBS_30_CNT_SOCIAL_CIRCLE = ifelse(OBS_60_CNT_SOCIAL_CIRCLE > 4, 4, OBS_60_CNT_SOCIAL_CIRCLE),
         DEF_30_60_SOCIAL_SUM = ifelse(is.na(DEF_30_CNT_SOCIAL_CIRCLE), 0, DEF_30_CNT_SOCIAL_CIRCLE)  + 
           ifelse(is.na(DEF_60_CNT_SOCIAL_CIRCLE), 0, DEF_60_CNT_SOCIAL_CIRCLE),
         OBS_30_60_SOCIAL_SUM = ifelse(is.na(OBS_30_CNT_SOCIAL_CIRCLE), 0, OBS_30_CNT_SOCIAL_CIRCLE)  + 
           ifelse(is.na(OBS_60_CNT_SOCIAL_CIRCLE), 0, OBS_60_CNT_SOCIAL_CIRCLE),
         
         
         TOTAL_CREDIT_CHECKS_MON =  ifelse(is.na(AMT_REQ_CREDIT_BUREAU_HOUR), 0, AMT_REQ_CREDIT_BUREAU_HOUR) + 
           ifelse(is.na(AMT_REQ_CREDIT_BUREAU_DAY), 0, AMT_REQ_CREDIT_BUREAU_DAY) +
           ifelse(is.na(AMT_REQ_CREDIT_BUREAU_WEEK), 0, AMT_REQ_CREDIT_BUREAU_WEEK) +
           ifelse(is.na(AMT_REQ_CREDIT_BUREAU_MON),AMT_REQ_CREDIT_BUREAU_MON ,0),
         
         TOTAL_CREDIT_CHECKS_QRT = TOTAL_CREDIT_CHECKS_MON  + ifelse(is.na(AMT_REQ_CREDIT_BUREAU_QRT),AMT_REQ_CREDIT_BUREAU_QRT ,0),
         
         TOTAL_CREDIT_CHECKS_YEAR = TOTAL_CREDIT_CHECKS_QRT + ifelse(is.na(AMT_REQ_CREDIT_BUREAU_YEAR), 0, AMT_REQ_CREDIT_BUREAU_YEAR),
         
         PER_CREDIT_CHECKS_MON_YEAR =  TOTAL_CREDIT_CHECKS_MON/TOTAL_CREDIT_CHECKS_YEAR,
         
         PER_CREDIT_CHECKS_MON_QRT= PER_CREDIT_CHECKS_MON/TOTAL_CREDIT_CHECKS_QRT,
         
         PER_CREDIT_CHECKS_QRT_YEAR= TOTAL_CREDIT_CHECKS_QRT/TOTAL_CREDIT_CHECKS_YEAR)

# remove useless flags
tr_te$FLAG_DOCUMENT_2 = NULL
tr_te$FLAG_DOCUMENT_4 = NULL
tr_te$FLAG_DOCUMENT_7 = NULL
tr_te$FLAG_DOCUMENT_10 = NULL
tr_te$FLAG_DOCUMENT_12 = NULL
tr_te$FLAG_DOCUMENT_14 = NULL
tr_te$FLAG_DOCUMENT_15 = NULL
tr_te$FLAG_DOCUMENT_17 = NULL
tr_te$FLAG_DOCUMENT_19 = NULL
tr_te$FLAG_DOCUMENT_20 = NULL
tr_te$FLAG_DOCUMENT_21 = NULL

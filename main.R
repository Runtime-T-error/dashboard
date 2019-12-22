# Title     : TODO
# Objective : TODO
# Created by: luka
# Created on: 12/21/19

library(RPostgreSQL)
library(dplyr)
library(magrittr)
library(ggplot2)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="raf-hackaton", host="192.168.11.32", port=5431, user="admin", password="adminRAF")

df_invs <- dbGetQuery(con, "SELECT * from invitations")
df_inv_act <- dbGetQuery(con, "SELECT * from invitation_activities")
df_users <- dbGetQuery(con, "SELECT * from users")
df_users %<>% rename(user_id=id, user_created_at=created_at)
df_invs %<>% rename(inv_id=id, inv_created_at=created_at, inv_status=status)
df_inv_act %<>% rename(act_id=id, act_created_at=created_at)

df_users %<>% inner_join(df_invs, by=c("user_id"="user_id")) %>% inner_join(df_inv_act, by=c("inv_id"="invitation_id"))
df_users$sent_at_weekday <- weekdays(as.Date(df_users$sent_at))
df_users %<>% mutate_at(vars(country, signup_type, gender, inv_status, email_notification_type, type, source_type, sent_at_weekday), factor)

df_users$type_str = df_users$type
df_users$gender_str = df_users$gender
df_users$inv_status_str = df_users$inv_status
df_users$is_phone_number_set <- df_users$phone_number == ""
df_users %<>% mutate_at(vars(type, gender, inv_status), as.numeric)

linear_mod <- lm(inv_status ~ (gender + total_amount + length_of_interview)^2, df_users)
summary(linear_mod)

ggplot(df_users, aes(df_users$total_amount, df_users$length_of_interview, color=df_users$gender_str)) + geom_point(position = "jitter", alpha=0.5) + facet_grid(rows=vars(df_users$inv_status_str))
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
df_invs %<>% rename(inv_id=id, inv_created_at=created_at)
df_inv_act %<>% rename(act_id=id, act_created_at=created_at)

df_users %<>% inner_join(df_invs, by=c("user_id"="user_id")) %>% inner_join(df_inv_act, by=c("inv_id"="invitation_id"))
df_users %<>% mutate_at(vars(country, signup_type, gender, email_notification_type, type, source_type), factor)
df_users$type_str = df_users$type
df_users$gender_str = df_users$gender
df_users %<>% mutate_at(vars(type, gender), as.numeric)

inv_ids <- unique(df_users$inv_id)
inv_status <- character(length(inv_ids))
i <- 1
for(id in inv_ids) {
  invs <- df_users[df_users$inv_id==id,]
  if(nrow(invs) < 2) inv_status[i] <- "incomplete"
  else if(invs[1,]$type_str == "complete" || invs[2,]$type_str == "complete") inv_status[i] <- "complete"
  else inv_status[i] <- "other_fail"
  i<-i+1
}
inv_statuses <- data.frame(inv_ids, inv_status)

print("Done calculating, executing query")

for (row in 1:nrow(inv_statuses)) {
  if(row %% 1000 == 0) {
    print(row)
  }
  dbGetQuery(con, paste0("UPDATE invitations SET status='", inv_statuses[row,]$inv_status, "' where id=", inv_statuses[row,]$inv_ids))
}
# 01_script
library(tidyverse)

## limit before uploading to github to reduce size
a <- read_csv("Data/nihr-summary-view.csv")
 a  <- a %>% 
  filter(Programme == "Health Technology Assessment",
         str_detect(HRCS_RAC_Category, fixed("6. Evaluation of Treatments and Therapeutic Interventions")),
         str_detect(Involvement_Type, "Chief Investigator"))
write_csv(a, "Data/nihr-summary-view.csv.gz")

a <- read_csv("Data/nihr-summary-view.csv.gz")
## 889. when take random samples, all trials

a <- a %>% 
  mutate(yr = year(Start_Date))
a <- a %>% 
  rename(hlth_cat = `HRCS _Health_Category`)

a$hlth_cat_lng <- str_split(a$hlth_cat, pattern = "\\,")
a_lng <- a %>% 
  unnest(hlth_cat_lng)

a_smry <- a %>% 
  group_by(yr) %>% 
  summarise(n = length(Award_Amount),
            cost = sum(Award_Amount)) %>% 
  ungroup() 
plt_n <- ggplot(a_smry, aes(x = yr, y = n)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("Number of trials") +
  scale_x_continuous("Year trial started")
plt_n
plt_cost <- ggplot(a_smry, aes(x = yr, y = cost/1000000)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("Cost in millions (£)") +
  scale_x_continuous("Year trial started")

plt_cost

## breakdown by type
a_lng_smry <- a_lng %>% 
  group_by(hlth_cat_lng) %>% 
  summarise(n = length(Award_Amount),
            cost = sum(Award_Amount)) %>% 
  ungroup()
a_lng_smry <- a_lng_smry %>% 
  arrange(n)%>% 
  mutate(hlth_cat_lng = factor(hlth_cat_lng, levels = hlth_cat_lng))
plt_n_hlth <- ggplot(a_lng_smry, aes(x = hlth_cat_lng, y = n)) +
  geom_point() +
  scale_y_continuous("Number of trials") +
  coord_flip() +
  scale_x_discrete("Health category")
plt_n_hlth
a_lng_smry <- a_lng_smry %>% 
  arrange(cost) %>% 
  mutate(hlth_cat_lng = factor(hlth_cat_lng, levels = hlth_cat_lng))
plt_cost_hlth <- ggplot(a_lng_smry, aes(x = hlth_cat_lng, y = cost/1000000)) +
  geom_point() +
  coord_flip() +
  scale_x_discrete("Health category") +
  scale_y_continuous("Cost in millions (£)")
plt_cost_hlth

tiff("Outputs/plot_n.tiff", res = 300, compression = "lzw", height = 6, width = 6, unit = "in")
plt_n
dev.off()
tiff("Outputs/plot_cost.tiff", res = 300, compression = "lzw", height = 6, width = 6, unit = "in")
plt_cost
dev.off()
tiff("Outputs/plot_n_hlth.tiff", res = 300, compression = "lzw", height = 6, width = 6, unit = "in")
plt_n_hlth
dev.off()
tiff("Outputs/plot_cost_hlth.tiff", res = 300, compression = "lzw", height = 6, width = 6, unit = "in")
plt_cost_hlth
dev.off()



png("Outputs/plot_n.png", res = 300, height = 6, width = 6, unit = "in")
plt_n
dev.off()
png("Outputs/plot_cost.png", res = 300, height = 6, width = 6, unit = "in")
plt_cost
dev.off()
png("Outputs/plot_n_hlth.png", res = 300, height = 6, width = 6, unit = "in")
plt_n_hlth
dev.off()
png("Outputs/plot_cost_hlth.png", res = 300, height = 6, width = 6, unit = "in")
plt_cost_hlth
dev.off()


library(tidyverse)
# library(RPostgreSQL)
## Add AACT CTTI database password. Will create a popout window
# mypassword <- rstudioapi::askForPassword()
## Connect to database
# drv <- dbDriver('PostgreSQL')
# con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org",
#                  port=5432, user="dmcalli",
#                  password=mypassword)
# dbListTables(con)
# ids <- dbGetQuery(con, "SELECT * from ID_information")
# dbDisconnect(con)
# saveRDS(ids, "ids_ctg.Rds")

ids <- readRDS("ids_ctg.Rds")
ids <- as_tibble(ids)
# 2 of 889 on CTG
a %>% semi_join(ids %>% select(Project_ID = id_value))

# install.packages("tidyllm")
library(tidyllm)
schema <- tidyllm_schema(
  list(
    myid = field_chr(""),
    caveats = field_chr("comma-separated caveats")
  )
)
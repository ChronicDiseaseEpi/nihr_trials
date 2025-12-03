# 02_trial_detection

library(tidyverse)
library(europepmc)
## text search of abstract to check if is a trial ----
a <- read_csv("Data/nihr-summary-view.csv.gz")
names(a) <- str_to_lower(names(a))
a <- a %>% 
  rename(hrcs_health_category = "hrcs _health_category")
trial_patterns <- c(
  "randomised",
  "\\btrial\\b",
  "\\brct\\b"
)
trms <- map(trial_patterns, ~ str_detect(a$scientific_abstract %>% str_to_lower(), .x))
names(trms) <- paste0("t", seq_along(trial_patterns))
trms <- bind_cols(trms)
trms$anyhit <- rowMeans(trms) >0
a <- a %>% 
  mutate(trialscientific = trms$anyhit)
a %>% 
  count(trialscientific)
a %>% 
  filter(trialscientific == sample(c(T,F), 1)) %>% 
  mutate(scientific_abstract = paste0(trialscientific, "\n", scientific_abstract)) %>% 
  pull(scientific_abstract) %>% 
  sample(1) %>% 
  write_lines("temp.txt")

## search PMC for papers ----
function(){
allres <- map(a$project_id, ~ {
  print(.x)
  epmc_search(query = paste0('GRANT_ID:"',
                             .x,
  # 16/154/01,
  '"'),
                     limit = 100)
})
saveRDS(allres, "pmc_ids.Rds")
}
allres <- readRDS("pmc_ids.Rds")
names(allres) <- a$project_id
allres <- bind_rows(allres, .id = "project_id")
## review PMC publication type for if it is a trial report ----
str_split(allres$pubType, pattern = ";") %>% unlist() %>%
  str_trim() %>% unique() %>% sort()
# clinical trial ...
# controlled clinical trial
# pragmatic clinical trial
# randomized controlled trial
trialpubtype_vct <- allres %>% 
  filter(str_detect(pubType, "(clinical|controlled) trial")) %>% 
  distinct(project_id) %>% 
  pull()
## 889 projects
length(a$project_id)
## 659 projects with papers in EuropePMC
length(allres$project_id %>% unique())
# 492 projects with trial papers
length(trialpubtype_vct)
# 167 projects with only non-trial papers
setdiff(allres$project_id, trialpubtype_vct) %>% 
  length()

allres_ids <- allres %>% 
  distinct(id, source)

function(){
allres2 <- map2(allres_ids$id,
                allres_ids$source, ~ {
                  print(.x)
                  epmc_details(ext_id = .x, 
                               data_src = .y)
                })
saveRDS(allres2, "pmc_details.Rds")
}
allres2 <- readRDS("pmc_details.Rds")
length(allres2)
## get MeSH
## note that IDs are unique
names(allres2) <- allres_ids$id
allres2$`27884916`$basic
## use trial IDs to see if it is a trial ----
function(){
allidextract <- epmc_annotations_by_id(ids = paste0(allres$source,":", allres$id))
saveRDS(allidextract, "pmc_idextract.Rds")
}
allidextract <- readRDS("pmc_idextract.Rds")
trial_ids <- allidextract %>%
  filter(type == "Accession Numbers",
         subType %in% c("NCT", "EUDRACT", "ISRCTN", "DRKS", "ACTRN", "IRCT", "CTRI", "ChiCTR")) %>%
  transmute(source, ext_id, registry = subType, trial_id = exact) %>%
  distinct()
trial_ids <- trial_ids %>% 
  nest(.by = c(source, ext_id))
# note multiple project IDs for some papers
## no papers lost from linkage
## only 209 projects with one or more papers with a trial ID 
## text-mined by europepmc
trial_ids <- trial_ids %>% 
  inner_join(allres %>% 
               select(project_id, source, ext_id = id) %>% 
               distinct()) %>% 
  unnest(data)
trialidtype_vct <- trial_ids$project_id %>% unique()
# some with trial IDs without trial publication type, etc
setdiff(trialidtype_vct, trialpubtype_vct)
setdiff(trialpubtype_vct, trialidtype_vct)
## 524 have one or other
union(trialpubtype_vct, trialidtype_vct) %>% 
  length()
## review which appear to be trials based on all information ----
a <- a %>% 
  mutate(inpmceurope = project_id %in% allres$project_id,
         trialpubtype = project_id %in% trialpubtype_vct,
         trialidtype = project_id %in% trialidtype_vct)
inpmc <- a %>% 
  filter(inpmceurope)
table(inpmc$trialidtype, inpmc$trialpubtype)
cor(inpmc$trialidtype, inpmc$trialpubtype)
table(inpmc$trialidtype, inpmc$trialpubtype) %>% chisq.test()
a %>% 
  count(trialscientific, inpmceurope, trialpubtype, trialidtype)
a <- a %>% 
  mutate(allfalse = !trialscientific & 
           !trialpubtype &
           !trialidtype)
## 36 all FALSE
a %>% 
  filter(allfalse) %>% 
  View
# 17/82/02 is a trial
a %>% 
  filter(!allfalse == sample(c(T,F), 1)) %>% 
  mutate(scientific_abstract = paste0(!allfalse, "\n", scientific_abstract)) %>% 
  pull(scientific_abstract) %>% 
  sample(1) %>% 
  write_lines("temp.txt")
write_csv(a %>% select(project_id, project_title, 
                       award_amount_m,
                       award_holder_name,
                       hrcs_health_category,
                       inpmceurope, contains("trial")), 
          "Outputs/nihr_hta_trial_yn.csv")

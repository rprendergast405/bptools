# mcd_post_campaign.R
# -------------------------------------------------------------------------
# A script detailing the function used to create a new McDonald's
# Post-Campaign Analysis, with some ready-to-use scripts
# -------------------------------------------------------------------------
# Created by Bert on 11-07-2016
# -------------------------------------------------------------------------
# Last edited by Bert on 12-07-2016
# -------------------------------------------------------------------------



#' Create a new R Project.
#'
#' Create some common sub-directories used in an R project, and import
#' some scripts containing headers for common analysis tasks.
#'
#' @param campaign_name The name that the project will take
#' @param base_dir The base directory for the project
#' @param within Optional path to a subdirectory within base_dir where the project will be located
#' @param sub_dirs Optional names for any additional sub-direcories that the project should contain
#'
#' @return Logical, indicating that the project has been successfully created
#' @export mcd_post_campaign
mcd_post_campaign <- function(campaign_name,
                              base_dir = "M:/clients/mcdonalds/2017/Business Insights/Contract Work/PROMO Windows Post Analyses",
                              within = NULL,
                              sub_dirs = NULL) {

  # The default sub-directories that should be created by the function
  base_sub_dirs <- c("data", "data/processed", "R", "R/rmd", "R/functions", "output", "output/tables", "output/figures", "Report")

  # The root directory for the project takes the project name
  if (!is.null(within)) {
    root_dir <- file.path(base_dir, within, campaign_name)
  } else {
    root_dir <- file.path(base_dir, campaign_name)
  }

  # Add any additional user-specified sub-directories to the defaults
  sub_dirs <- c(base_sub_dirs, sub_dirs)

  # create the directories
  cat("Creating directories\n")
  sapply(sub_dirs, FUN = function(x) {dir.create(file.path(root_dir, x), recursive = TRUE)})

  # create the .Rproj file with default specifications
  rproject_specs <- "Version: 1.0

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: Sweave
LaTeX: pdfLaTeX
"

  cat("Creating .RProj file\n")
  cat(paste(rproject_specs), file = file.path(root_dir, paste(campaign_name, "Rproj", sep = ".")))

  # import the script templates to the root directory
  cat("Creating script templates:\n")
  cat(paste0("  0 ", campaign_name, " initialise.R ..."))
  create_init_script(project_name = campaign_name, root_dir = root_dir)
  cat(" Done\n")

  cat(paste0("  1 ", campaign_name, " processing.R ..."))
  create_mcd_processing(campaign_name = campaign_name, root_dir = root_dir)
  cat(" Done\n")

  cat(paste0("  2 ", campaign_name, " results.R ..."))
  create_mcd_results(campaign_name = campaign_name, root_dir = root_dir)
  cat(" Done\n")

  cat(paste0("  3 ", campaign_name, " forecast.R ..."))
  create_mcd_forecast(campaign_name = campaign_name, root_dir = root_dir)
  cat(" Done\n")

  cat(paste0("  4 ", campaign_name, " report.R ..."))
  create_mcd_report(campaign_name = campaign_name, root_dir = root_dir)
  cat(" Done\n")

  cat(paste0("  ", campaign_name, " RUN SCRIPT.R ..."))
  create_mcd_run(campaign_name = campaign_name, root_dir = root_dir)
  cat(" Done\n")

  cat(paste0("  R/rmd/", campaign_name, " EDA.Rmd ..."))
  create_eda_script(project_name = campaign_name, root_dir = root_dir)
  cat(" Done\n")

  cat(paste0("Copying mvl_template.pptx to ", root_dir, " ..."))
  file.copy(from = system.file("extdata", "mvl_template.pptx", package = "marketview"),  #file.path("M:/R/mvl_template.pptx"),
            to = root_dir)

  file.copy(from = system.file("extdata", "mvl_template_old.pptx", package = "marketview"), #file.path("M:/R/mvl_template_old.pptx"),
            to = root_dir)

  file.copy(from = system.file("extdata", "mcd_template.pptx", package = "marketview"),
            to = root_dir)
  cat(" Done\n")

  cat(paste0("Copying example_items.csv to ", root_dir, "/data ..."))
  file.copy(from = system.file("extdata", "example_items.csv", package = "marketview"),
            to = file.path(root_dir, "data"))
  cat(" Done\n")

  cat("Project created successfully\n")
}



#' Create a processing script.
#'
#' Builds the skeleton of a data processing script in the specified root directory.
#' For internal use in mcd_post_campaign().
#'
#' @param campaign_name The name of the R project.
#' @param root_dir The root directory of the R project.
#'
#' @return Creates a script within the directory specified.
create_mcd_processing <- function(campaign_name, root_dir) {
  procces_text <- c("# 1 ", campaign_name, " processing.R
# Created by ", Sys.info()["user"], ", on ", format(Sys.Date(), "%d %m %Y"), "
# -------------------------------------------------------------------------
# A script designed to process the raw ", campaign_name, " data for analysis
# -------------------------------------------------------------------------
# WORKFLOW
#   0. INITIALISE
#   1. IMPORT DATA
#
# -------------------------------------------------------------------------
# Last edited ", format(Sys.Date(), "%d %m %Y"), " by create_project()
#   - Created
# -------------------------------------------------------------------------


# 0. INITIALISE -----------------------------------------------------------

# Get the time periods from the promo window definition
tpoi_spw_start <- min(c(cpoi_df$spw_start, cpoi_df$spw_end))
tpoi_spw_end <- max(c(cpoi_df$spw_start, cpoi_df$spw_end))

# Define total period of interest
tpoi_start <- seqpromo.df %>% filter(seqpromo_week == tpoi_spw_start) %>% pull(week_start) %>% format(\"%Y%m%d\") %>% as.numeric
tpoi_end <- seqpromo.df %>% filter(seqpromo_week == tpoi_spw_end) %>% pull(week_start) %>% add(days(6)) %>% format(\"%Y%m%d\") %>% as.numeric


# 1. IMPORT DATA ----------------------------------------------------------

#Import overall sales breakdown for generic sections

generic_df <- sqlQuery(DB,
paste(\"select sum(a.spend) as spend, sum(a.trans) as trans,
c.seqpromo_week, substr(c.seqpromo_week,1,4) as year, a.daypart_report_no, a.age_no, a.cust_type,
case when b.merch_tla between 1 and 11 then 'Auckland' when b.merch_tla between 12 and 39 then 'NI Provincial' when b.merch_tla between 40 and 50 then 'Wellington' else 'South Island' end as MCD_REGION,
b.bunch_name, case when b.bunch_id in (19150,19151) then b.bunch_name else b.bunch_namex end as ieo_segment
from MCD_BP_FINAL_CARD_DATASET2 a
inner join MCD_BP_GRP_IDS b on a.merch_id = b.merch_id
inner join mcd.mcd_promo_week_dates c on a.seqday = c.seqday
where a.seqday between\", tpoi_start, \"and\", tpoi_end,
\"group by a.daypart_report_no, c.seqpromo_week, substr(c.seqpromo_week,1,4), a.age_no, a.cust_type,
b.bunch_name, case when b.bunch_id in (19150,19151) then b.bunch_name else b.bunch_namex end,
case when b.merch_tla between 1 and 11 then 'Auckland' when b.merch_tla between 12 and 39 then 'NI Provincial' when b.merch_tla between 40 and 50 then 'Wellington' else 'South Island' end\"))

# Code data
generic_df %<>%
  mutate_if(is.factor,  as.character) %>%
  mcd_process() %>%
  mutate(MCD_REGION = as.character(MCD_REGION)) %>%
  mutate(MCD_REGION = ifelse(is.na(MCD_REGION), \"NI Provincial\", as.character(MCD_REGION))) %>%
  mutate(MCD_REGION = factor(MCD_REGION, levels = c(\"Auckland\", \"NI Provincial\", \"Wellington\", \"South Island\"))) %>%
  mutate(IEO_SEGMENT = fct_recode(IEO_SEGMENT, `QSR - C/P/S` = \"QSR - Chicken/Pizza/Sandwich\")) %>%
  setNames(tolower(names(.))) %>%
  mutate(spend = as.numeric(spend), trans = as.numeric(trans))

generic_df$poi <- \"non-Comparable\"

for (i in 1:dim(cpoi_df)[1]) {

  generic_df[generic_df$seqpromo_week >= cpoi_df[i, 2] & generic_df$seqpromo_week <= cpoi_df[i, 3], \"poi\"] <- cpoi_df[i, 1]

}

generic_df$poi <- factor(generic_df$poi, levels = cpoi_df[,1])


# Get the TLD Specific data using the tld_customer_data function ----
promo_item_data <- tld_customer_data(
  con = DB,
  codes = unique(promo_items$codes),
  start_week = min(promo_items$start_week),
  end_week = max(promo_items$end_week)
) %>%
  left_join(promo_items, by = c(\"MENU_ITEM_NO\" = \"codes\")) %>%
  inner_join(sqlQuery(DB, \"SELECT id AS store_id, merch_name, merch_tla FROM mcd_bp_grp_ids grp INNER JOIN mcd.mcd_store sto ON sto.fozzie_group_id = grp.merch_id\")) %>%
  mcd_process() %>%
  left_join(sqlQuery(DB, \"SELECT id AS pos_type, description AS pos_name FROM mcd.dim_pos_type\")) %>%
  mutate(cust_group = case_when(CUST_TYPE == \"HVC\" ~ as.character(AGEX),
         TRUE ~ \"non-HVC\"),
         POS_NAME = reorder(POS_NAME, POS_TYPE))



# 2. SAVE THE PROCESSED DATA ----------------------------------------------


save(generic_df,
     promo_item_data,
     file = file.path(\"data\", \"processed\", \"", campaign_name, " data.RData\"))
")

  cat(paste(procces_text, collapse = ""), file = file.path(root_dir, "R", paste0("1 ", campaign_name, " processing.R")))

}




#' Create a results script.
#'
#' Builds the skeleton of a MCD Post-Campaign results script in the specified
#' root directory. For internal use in mcd_post_campaign().
#'
#' @param campaign_name The name of the R project.
#' @param root_dir The root directory of the R project.
#'
#' @return Creates a script within the directory specified.
create_mcd_results <- function(campaign_name, root_dir) {
  results_text <- c("# 2 ", campaign_name, " results.R
# Created by ", Sys.info()["user"], ", on ", format(Sys.Date(), "%d %m %Y"), "
# -------------------------------------------------------------------------
# A script designed to render and save the ", campaign_name, " post-campaign
# analysis results
# -------------------------------------------------------------------------
# WORKFLOW
#   0. INITIALISE
#   1. IMPORT DATA
#   2. RESULTS HERE
# -------------------------------------------------------------------------
# Last edited ", format(Sys.Date(), "%d %m %Y"), " by create_project()
#   - Created
# -------------------------------------------------------------------------

# 0. INITIALISE -----------------------------------------------------------

library(data.table)

# initialise the labelling and set the output subdirectory
fig_num <- 1
tab_num <- 1
section_num <- 1

# 2. RESULTS HERE ---------------------------------------------------------

#--------------------------------------------------------------------------
#Generic data sections
#--------------------------------------------------------------------------

#Section 1 - High level overview ------------------------------------------

#Overall change in share, compare between two periods

temp <- generic_df %>%
  group_by(ieo_segment, poi) %>%
  summarise(spend = sum(spend, na.rm = T)) %>%
  group_by(poi) %>%
  mutate(share. = spend/sum(spend)) %>%
  group_by(ieo_segment) %>%
  mutate(Share.Change = share.[1] - share.) %>%
  setDT() %>%
  dcast(ieo_segment ~ poi, value.var = c(\'share.\', \'Share.Change\'))

no_diff <- -1 * (which(apply(temp[, -c(1)], 2, function(x) {all(x == 0)})) + 1)

temp2 <- temp %>%
  select(no_diff) %>%
  as.tbl %>%
  setNames(gsub(\" \", \"_\", names(.))) %>%
  mutate_at(vars(contains(\"change\")), funs(percent_change(., dp = 2))) %>%
  mutate_if(is.numeric, funs(percent(., dp = 2))) %>%
  setNames(gsub(\"_\", \" \", names(.))) %>%
  mutate_all(as.character)

share_vec <- which(grepl(\"share. \", tolower(names(temp2))))
change_vec <- which(grepl(\"share.change \", tolower(names(temp2))))

names(temp2) <- gsub(\"share. \", \"\", names(temp2), fixed = T)
names(temp2) <- gsub(\"Share.Change \", \"\", names(temp2), fixed = T)
names(temp2) <- gsub(\"ieo segment\", \"IEO Category\", names(temp2), fixed = T)


temp2 <- rbind(c(\"\", rep(\"IEO Share\", length(share_vec)), rep(\"Share Change From\", length(change_vec))), names(temp2), temp2)

head_rows <- 1:2

ft <- flextable_leaf_joined(temp2, pr = 1, hr = head_rows) %>%
  flextable_negatives()

ft <- setFlexTableWidths(ft, c(4.5, rep((21.42 - 4.5) / (dim(temp2)[2] - 1), dim(temp2)[2] - 1)) * 0.3937)

assign(envir = .GlobalEnv, paste('section', section_num, 'ft', tab_num, sep = \"_\"), ft)

tab_num <- tab_num + 1

#Change in share by HVC

temp <- generic_df %>%
  group_by(ieo_segment, poi, cust_type) %>%
  summarise(spend = sum(spend, na.rm = T)) %>%
  group_by(poi, cust_type) %>%
  mutate(share. = spend/sum(spend)) %>%
  group_by(ieo_segment, cust_type) %>%
  mutate(Share.Change = share.[1] - share.) %>%
  setDT() %>%
  dcast(cust_type + ieo_segment ~ poi, value.var = c('share.', 'Share.Change'))

no_diff <- -1 * (which(apply(temp[, -c(1:2)], 2, function(x) {all(x == 0)})) + 2)

temp2 <- temp %>%
  select(no_diff) %>%
  as.tbl %>%
  setNames(gsub(\" \", \"_\", names(.))) %>%
  mutate_at(vars(contains(\"change\")), funs(percent_change(., dp = 2))) %>%
  mutate_if(is.numeric, funs(percent(., dp = 2))) %>%
  setNames(gsub(\"_\", \" \", names(.))) %>%
  mutate_all(as.character)

share_vec <- which(grepl(\"share. \", tolower(names(temp2))))
change_vec <- which(grepl(\"share.change \", tolower(names(temp2))))

names(temp2) <- gsub(\"share. \", \"\", names(temp2), fixed = T)
names(temp2) <- gsub(\"Share.Change \", \"\", names(temp2), fixed = T)
names(temp2) <- gsub(\"ieo segment\", \"IEO Category\", names(temp2), fixed = T)
names(temp2) <- gsub(\"cust type\", \"HVC Type\", names(temp2), fixed = T)

temp2 <- rbind(c(rep(\"\", 2), rep(\"IEO Share\", length(share_vec)), rep(\"Share Change From\", length(change_vec))), names(temp2), temp2)

head_rows <- 1:2

ft <- flextable_leaf_joined(temp2, pr = 1, hr = head_rows, size = 9) %>%
  flextable_negatives()


ft <- setFlexTableWidths(ft, c(2.5, 3.5, rep((21.42 - 6) / (dim(temp2)[2] - 2), dim(temp2)[2] - 2)) * 0.3937)

assign(envir = .GlobalEnv, paste('section', section_num, 'ft', tab_num, sep = \"_\"), ft)

tab_num <- tab_num + 1

#Overall spend change

p <- generic_df %>%
  group_by(ieo_segment, poi) %>%
  summarise(spend_week = sum(spend, na.rm = T)/n_distinct(seqpromo_week)) %>%
  group_by(ieo_segment) %>%
  mutate(spend_poi = spend_week[1]) %>%
  mutate(spend_change = spend_poi/(spend_week) - 1) %>%
  filter(poi != poi[1]) %>%
  ggplot +
  geom_col(aes(x = fct_rev(ieo_segment), y = spend_change, fill = poi), position = \"dodge\") +
  scale_y_continuous(\"Spending Change\", labels = percent, breaks = pretty_breaks(10)) +
  scale_fill_mvl(name = \"Comparison Period\", palette = \"mvl\") +
  labs(title = paste(\"IEO Average Weekly Spending Change\"),
       subtitle = paste(\"For\", cpoi_df[1,1], \"as compared to\", paste(cpoi_df[2:dim(cpoi_df)[1],1], collapse = \", \")),
       x = NULL) +
  theme(panel.grid.major.y = element_blank()) +
  coord_flip()

assign(envir = .GlobalEnv, paste('section', section_num, 'gr', fig_num, sep = \"_\"), p)

fig_num <- fig_num + 1

#QSR HVC comparison

temp <- generic_df %>%
  filter(qsr) %>%
  group_by(ieo_segment, poi, cust_type) %>%
  summarise(spend = sum(spend, na.rm = T)) %>%
  group_by(poi, cust_type) %>%
  mutate(share. = spend/sum(spend)) %>%
  group_by(ieo_segment, cust_type) %>%
  mutate(Share.Change = share.[1] - share.) %>%
  setDT() %>%
  dcast(cust_type + ieo_segment ~ poi, value.var = c('share.', 'Share.Change'))

no_diff <- -1 * (which(apply(temp[, -c(1:2)], 2, function(x) {all(x == 0)})) + 2)

temp2 <- temp %>%
  select(no_diff) %>%
  as.tbl %>%
  setNames(gsub(\" \", \"_\", names(.))) %>%
  mutate_at(vars(contains(\"change\")), funs(percent_change(., dp = 2))) %>%
  mutate_if(is.numeric, funs(percent(., dp = 2))) %>%
  setNames(gsub(\"_\", \" \", names(.))) %>%
  mutate_all(as.character)

share_vec <- which(grepl(\"share. \", tolower(names(temp2))))
change_vec <- which(grepl(\"share.change \", tolower(names(temp2))))

names(temp2) <- gsub(\"share. \", \"\", names(temp2), fixed = T)
names(temp2) <- gsub(\"Share.Change \", \"\", names(temp2), fixed = T)
names(temp2) <- gsub(\"ieo segment\", \"IEO Category\", names(temp2), fixed = T)
names(temp2) <- gsub(\"cust type\", \"HVC Type\", names(temp2), fixed = T)

temp2 <- rbind(c(rep(\"\", 2), rep(\"QSR Share\", length(share_vec)), rep(\"Share Change From\", length(change_vec))), names(temp2), temp2)

tempb <- generic_df %>%
  filter(qsr) %>%
  group_by(ieo_segment, poi) %>%
  summarise(spend = sum(spend, na.rm = T)) %>%
  group_by(poi) %>%
  mutate(share. = spend/sum(spend)) %>%
  group_by(ieo_segment) %>%
  mutate(Share.Change = share.[1] - share.) %>%
  setDT() %>%
  dcast(ieo_segment ~ poi, value.var = c('share.', 'Share.Change'))

no_diff <- -1 * (which(apply(tempb[, -c(1)], 2, function(x) {all(x == 0)})) + 1)

temp2b <- tempb %>%
  select(no_diff) %>%
  as.tbl %>%
  setNames(gsub(\" \", \"_\", names(.))) %>%
  mutate_at(vars(contains(\"change\")), funs(percent_change(., dp = 2))) %>%
  mutate_if(is.numeric, funs(percent(., dp = 2))) %>%
  setNames(gsub(\"_\", \" \", names(.))) %>%
  mutate_all(as.character) %>%
  mutate(cust_type = 'Total')

temp2b <- temp2b[,c(1,dim(temp2b)[2], 2:(dim(temp2b)[2] - 1))]

names(temp2b) <- gsub(\"share. \", \"\", names(temp2b), fixed = T)
names(temp2b) <- gsub(\"Share.Change \", \"\", names(temp2b), fixed = T)
names(temp2b) <- gsub(\"ieo segment\", \"IEO Category\", names(temp2b), fixed = T)
names(temp2b) <- gsub(\"cust_type\", \"HVC Type\", names(temp2b), fixed = T)

head_rows <- 1:2

temp <- rbind(temp, tempb %>%  mutate(cust_type = 'total'))

ft <- flextable_leaf_joined(rbind(temp2, temp2b), pr = 1, hr = head_rows) %>%
  flextable_negatives()


assign(envir = .GlobalEnv, paste('section', section_num, 'ft', tab_num, sep = \"_\"), ft)

tab_num <- tab_num + 1

# Average cheque comparison

temp <- generic_df %>%
  group_by(ieo_segment, poi) %>%
  summarise(spend = sum(spend, na.rm = T), trans = sum(trans, na.rm = T)) %>%
  group_by(poi) %>%
  mutate(average.cheque = (spend)/(trans)) %>%
  group_by(ieo_segment) %>%
  mutate(average_cheque_change = average.cheque[1] - average.cheque) %>%
  setDT() %>%
  dcast(ieo_segment ~ poi, value.var = c('average.cheque', 'average_cheque_change'))

no_diff <- -1 * (which(apply(temp[, -c(1)], 2, function(x) {all(x == 0)})) + 1)

temp2 <- temp %>%
  select(no_diff) %>%
  as.tbl %>%
  setNames(gsub(\" \", \"_\", names(.))) %>%
  mutate_at(vars(contains(\"change\")), funs(dollar_change(., dp = 2))) %>%
  mutate_if(is.numeric, funs(dollar(., dp = 2))) %>%
  setNames(gsub(\"_\", \" \", names(.))) %>%
  mutate_all(as.character)

share_vec <- which(grepl(\"average.cheque \", tolower(names(temp2)), fixed = T))
change_vec <- which(grepl(\"average cheque change \", tolower(names(temp2))))

names(temp2) <- gsub(\"average cheque change \", \"\", names(temp2), fixed = T)
names(temp2) <- gsub(\"average.cheque \", \"\", names(temp2), fixed = T)
names(temp2) <- gsub(\"ieo segment\", \"IEO Category\", names(temp2), fixed = T)


temp2 <- rbind(c(\"\", rep(\"Average Cheque\", length(share_vec)), rep(\"Average Cheque Change From\", length(change_vec))), names(temp2), temp2)

head_rows <- 1:2

ft <- flextable_leaf_joined(temp2, pr = 1, hr = head_rows) %>%
  flextable_negatives()


ft <- setFlexTableWidths(ft, c(4.5, rep((21.42 - 4.5) / (dim(temp2)[2] - 1), dim(temp2)[2] - 1)) * 0.3937)

assign(envir = .GlobalEnv, paste('section', section_num, 'ft', tab_num, sep = \"_\"), ft)

tab_num <- tab_num + 1

# Average cheque comparison by HVC

temp <- generic_df %>%
  group_by(ieo_segment, poi, cust_type) %>%
  summarise(spend = sum(spend, na.rm = T), trans = sum(trans, na.rm = T)) %>%
  mutate(average.cheque = (spend)/(trans)) %>%
  group_by(ieo_segment, cust_type) %>%
  mutate(average_cheque_change = average.cheque[1] - average.cheque) %>%
  setDT() %>%
  dcast(cust_type + ieo_segment ~ poi, value.var = c('average.cheque', 'average_cheque_change'))

no_diff <- -1 * (which(apply(temp[, -c(1:2)], 2, function(x) {all(x == 0)})) + 2)

temp %<>%
  select(no_diff) %>%
as.tbl

temp2 <- temp %>%
  setNames(gsub(\" \", \"_\", names(.))) %>%
  mutate_at(vars(contains(\"change\")), funs(dollar_change(., dp = 2))) %>%
  mutate_if(is.numeric, funs(dollar(., dp = 2))) %>%
  setNames(gsub(\"_\", \" \", names(.))) %>%
  mutate_all(as.character)

share_vec <- which(grepl(\"average.cheque \", tolower(names(temp2)), fixed = T))
change_vec <- which(grepl(\"average cheque change \", tolower(names(temp2))))

names(temp2) <- gsub(\"average.cheque\", \"\", names(temp2), fixed = T)
names(temp2) <- gsub(\"average cheque change \", \"\", names(temp2), fixed = T)
names(temp2) <- gsub(\"ieo segment\", \"IEO Category\", names(temp2), fixed = T)
names(temp2) <- gsub(\"cust type\", \"HVC Type\", names(temp2), fixed = T)

temp2 <- rbind(c(rep(\"\", 2), rep(\"Average Cheque\", length(share_vec)), rep(\"Average Cheque Change From\", length(change_vec))), names(temp2), temp2)

head_rows <- 1:2

ft <- flextable_leaf_joined(temp2, pr = 1, hr = head_rows, size = 9) %>%
  flextable_negatives()


ft <- setFlexTableWidths(ft, c(2.5, 3.5, rep((21.42 - 6) / (dim(temp2)[2] - 2), dim(temp2)[2] - 2)) * 0.3937)

assign(envir = .GlobalEnv, paste('section', section_num, 'ft', tab_num, sep = \"_\"), ft)

tab_num <- tab_num + 1

# Section 2 - HVC performance review ---------------------------------------

section_num <- section_num + 1
tab_num <- 1
fig_num <- 1

# HVC age share change

temp <- generic_df %>%
  filter(qsr, cust_type == \"HVC\") %>%
  group_by(age, poi, ieo_segment) %>%
  summarise(spend = sum(spend, na.rm = T)) %>%
  group_by(age, poi) %>%
  mutate(share. = spend/sum(spend)) %>%
  filter(ieo_segment == \"McDonald's\") %>%
  group_by(age) %>%
  mutate(share_change. = share.[1] - share.) %>%
  setDT() %>%
  dcast(age ~ poi, value.var = c('share.', 'share_change.'))

no_diff <- -1 * (which(apply(temp[, -c(1)], 2, function(x) {all(x == 0)})) + 1)

temp %<>%
  select(no_diff)

tempb <- generic_df %>%
  filter(cust_type == \"HVC\") %>%
  group_by(age, poi, ieo_segment) %>%
  summarise(spend = sum(spend, na.rm = T)) %>%
  group_by(age, poi) %>%
  mutate(share. = spend/sum(spend)) %>%
  filter(ieo_segment == \"McDonald's\") %>%
  group_by(age) %>%
  mutate(share_change. = share.[1] - share.) %>%
  setDT() %>%
  dcast(age ~ poi, value.var = c('share.', 'share_change.'))

no_diff <- -1 * (which(apply(tempb[, -c(1)], 2, function(x) {all(x == 0)})) + 1)

tempb %<>%
  select(no_diff)

temp2 <- temp %>%
  setNames(gsub(\" \", \"_\", names(.))) %>%
  mutate_at(vars(contains(\"change\")), funs(percent_change(., dp = 2))) %>%
  mutate_if(is.numeric, percent) %>%
  setNames(gsub(\"_\", \" \", names(.))) %>%
  setNames(gsub(\"age\", \"Age\", names(.))) %>%
  setNames(gsub(\"share. \", \"\", names(.), fixed = T)) %>%
  setNames(gsub(\"share change. \", \"Change - \", names(.), fixed = T)) %>%
  mutate_all(as.character)

temp2b <- tempb %>%
  setNames(gsub(\" \", \"_\", names(.))) %>%
  mutate_at(vars(contains(\"change\")), funs(percent_change(., dp = 2))) %>%
  mutate_if(is.numeric, percent) %>%
  setNames(gsub(\"_\", \" \", names(.))) %>%
  setNames(gsub(\"age\", \"Age\", names(.))) %>%
  setNames(gsub(\"share. \", \"\", names(.), fixed = T)) %>%
  setNames(gsub(\"share change. \", \"Change - \", names(.), fixed = T)) %>%
  mutate_all(as.character)

temp_f <- rbind(rep(\"QSR Market Share\", dim(temp2)[2]),
                names(temp2),
                temp2,
                rep(\"IEO Market Share\", dim(temp2)[2]),
                names(temp2b),
                temp2b)

ft <- flextable_leaf_joined(temp_f, pr = c(1, 3 + dim(temp2)[1]), hr = c(1:2, 3 + dim(temp2)[1], 4 + dim(temp2)[1]), size = 9) %>%
  flextable_negatives()


assign(envir = .GlobalEnv, paste('section', section_num, 'ft', tab_num, sep = \"_\"), ft)


tab_num <- tab_num + 1

# HVC region share change

temp <- generic_df %>%
  filter(qsr, cust_type == \"HVC\") %>%
  group_by(mcd_region, poi, ieo_segment) %>%
  summarise(spend = sum(spend, na.rm = T)) %>%
  group_by(mcd_region, poi) %>%
  mutate(share. = spend/sum(spend)) %>%
  filter(ieo_segment == \"McDonald's\") %>%
  group_by(mcd_region) %>%
  mutate(share_change. = share.[1] - share.) %>%
  setDT() %>%
  dcast(mcd_region ~ poi, value.var = c('share.', 'share_change.'))

no_diff <- -1 * (which(apply(temp[, -c(1)], 2, function(x) {all(x == 0)})) + 1)

temp %<>%
  select(no_diff)

tempb <- generic_df %>%
  filter(cust_type == \"HVC\") %>%
  group_by(mcd_region, poi, ieo_segment) %>%
  summarise(spend = sum(spend, na.rm = T)) %>%
  group_by(mcd_region, poi) %>%
  mutate(share. = spend/sum(spend)) %>%
  filter(ieo_segment == \"McDonald's\") %>%
  group_by(mcd_region) %>%
  mutate(share_change. = share.[1] - share.) %>%
  setDT() %>%
  dcast(mcd_region ~ poi, value.var = c('share.', 'share_change.'))

no_diff <- -1 * (which(apply(tempb[, -c(1)], 2, function(x) {all(x == 0)})) + 1)

tempb %<>%
  select(no_diff)

temp2 <- temp %>%
  setNames(gsub(\" \", \"_\", names(.))) %>%
  mutate_at(vars(contains(\"change\")), funs(percent_change(., dp = 2))) %>%
  mutate_if(is.numeric, percent) %>%
  setNames(gsub(\"_\", \" \", names(.))) %>%
  setNames(gsub(\"mcd region\", \"Region\", names(.))) %>%
  setNames(gsub(\"share. \", \"\", names(.), fixed = T)) %>%
  setNames(gsub(\"share change. \", \"Change - \", names(.), fixed = T)) %>%
  mutate_all(as.character)

temp2b <- tempb %>%
  setNames(gsub(\" \", \"_\", names(.))) %>%
  mutate_at(vars(contains(\"change\")), funs(percent_change(., dp = 2))) %>%
  mutate_if(is.numeric, percent) %>%
  setNames(gsub(\"_\", \" \", names(.))) %>%
  setNames(gsub(\"mcd region\", \"Region\", names(.))) %>%
  setNames(gsub(\"share. \", \"\", names(.), fixed = T)) %>%
  setNames(gsub(\"share change. \", \"Change - \", names(.), fixed = T)) %>%
  mutate_all(as.character)

temp_f <- rbind(rep(\"QSR Market Share\", dim(temp2)[2]),
                names(temp2),
                temp2,
                rep(\"IEO Market Share\", dim(temp2)[2]),
                names(temp2b),
                temp2b)

ft <- flextable_leaf_joined(temp_f, pr = c(1, 3 + dim(temp2)[1]), hr = c(1:2, 3 + dim(temp2)[1], 4 + dim(temp2)[1])) %>%
  flextable_negatives()


assign(envir = .GlobalEnv, paste('section', section_num, 'ft', tab_num, sep = \"_\"), ft)

tab_num <- tab_num + 1

# HVC DP share change

temp <- generic_df %>%
  filter(qsr, cust_type == \"HVC\") %>%
  group_by(daypart, poi, ieo_segment) %>%
  summarise(spend = sum(spend, na.rm = T)) %>%
  group_by(daypart, poi) %>%
  mutate(share. = spend/sum(spend)) %>%
  filter(ieo_segment == \"McDonald's\") %>%
  group_by(daypart) %>%
  mutate(share_change. = share.[1] - share.) %>%
  setDT() %>%
  dcast(daypart ~ poi, value.var = c('share.', 'share_change.'))

no_diff <- -1 * (which(apply(temp[, -c(1)], 2, function(x) {all(x == 0)})) + 1)

temp %<>%
  select(no_diff)

tempb <- generic_df %>%
  filter(cust_type == \"HVC\") %>%
  group_by(daypart, poi, ieo_segment) %>%
  summarise(spend = sum(spend, na.rm = T)) %>%
  group_by(daypart, poi) %>%
  mutate(share. = spend/sum(spend)) %>%
  filter(ieo_segment == \"McDonald's\") %>%
  group_by(daypart) %>%
  mutate(share_change. = share.[1] - share.) %>%
  setDT() %>%
  dcast(daypart ~ poi, value.var = c('share.', 'share_change.'))

no_diff <- -1 * (which(apply(tempb[, -c(1)], 2, function(x) {all(x == 0)})) + 1)

tempb %<>%
  select(no_diff)

temp2 <- temp %>%
  setNames(gsub(\" \", \"_\", names(.))) %>%
  mutate_at(vars(contains(\"change\")), funs(percent_change(., dp = 2))) %>%
  mutate_if(is.numeric, percent) %>%
  setNames(gsub(\"_\", \" \", names(.))) %>%
  setNames(gsub(\"daypart\", \"Daypart\", names(.))) %>%
  setNames(gsub(\"share. \", \"\", names(.), fixed = T)) %>%
  setNames(gsub(\"share change. \", \"Change - \", names(.), fixed = T)) %>%
  mutate_all(as.character)

temp2b <- tempb %>%
  setNames(gsub(\" \", \"_\", names(.))) %>%
  mutate_at(vars(contains(\"change\")), funs(percent_change(., dp = 2))) %>%
  mutate_if(is.numeric, percent) %>%
  setNames(gsub(\"_\", \" \", names(.))) %>%
  setNames(gsub(\"daypart\", \"Daypart\", names(.))) %>%
  setNames(gsub(\"share. \", \"\", names(.), fixed = T)) %>%
  setNames(gsub(\"share change. \", \"Change - \", names(.), fixed = T)) %>%
  mutate_all(as.character)

temp_f <- rbind(rep(\"QSR Market Share\", dim(temp2)[2]),
                names(temp2),
                temp2,
                rep(\"IEO Market Share\", dim(temp2)[2]),
                names(temp2b),
                temp2b)

ft <- flextable_leaf_joined(temp_f, pr = c(1, 3 + dim(temp2)[1]), hr = c(1:2, 3 + dim(temp2)[1], 4 + dim(temp2)[1]), size = 9) %>%
  flextable_negatives()


assign(envir = .GlobalEnv, paste('section', section_num, 'ft', tab_num, sep = \"_\"), ft)

tab_num <- tab_num + 1
")

  cat(paste(results_text, collapse = ""), file = file.path(root_dir, "R", paste0("2 ", campaign_name, " results.R")))

}


#' Create a forecast script.
#'
#' Builds the skeleton of a results script in the specified root directory.
#' For internal use in mcd_post_campaign().
#'
#' @param campaign_name The name of the R project.
#' @param root_dir The root directory of the R project.
#'
#' @return Creates a script within the directory specified.
create_mcd_forecast <- function(campaign_name, root_dir) {
  results_text <- c("# 3 ", campaign_name, " forecast.R
# Created by ", Sys.info()["user"], ", on ", format(Sys.Date(), "%d %m %Y"), "
# -------------------------------------------------------------------------
# A script designed to render and save the ", campaign_name, " results
# -------------------------------------------------------------------------
# WORKFLOW
#   0. INITIALISE
#   1. IMPORT DATA
#   2. RESULTS HERE
# -------------------------------------------------------------------------
# Last edited ", format(Sys.Date(), "%d %m %Y"), " by create_project()
#   - Created
# -------------------------------------------------------------------------

# 0. INITIALISE -----------------------------------------------------------

section_num <- section_num + 1
tab_num <- 1
fig_num <- 1

#Run Forecast for all IEO segments

#Takes an input DF, forecast horizon then performs the forecast
manual_model_forecast <- function(dat, newdat,
                                  model_formula = formula(SPEND ~ REP_DATE + SH + RH + PH + week_indicator + population)){
  forecast_model <- lm(data = dat, formula = model_formula)
  result <- predict.lm(forecast_model, newdata = newdat)
  return(list(result, forecast_model))
}

#Set up forecast data
DataDir <- \"M:/clients/mcdonalds/2016/Business Insights/Regression/Data\"

# Process data
ieo_data %<>%
  mcd_process()

#Tune down ADB over time
ieo_data$ADB <- 0
ieo_data[ieo_data$SEQWEEK > 201619,]$ADB <- 1
ieo_data[ieo_data$SEQWEEK > 201630,]$ADB <- .75
# ieo_data[ieo_data$SEQWEEK > 201701,]$ADB <- .5

# Population
popn_data <- read_excel(file.path(DataDir, \"population.xlsx\"), sheet = 4)
popn_proj <- read_excel(file.path(DataDir, \"population.xlsx\"), sheet = 5)

# Holidays
holiday_dat <- read_excel(file.path(DataDir, \"Holidays Important Dates (processed).xlsx\"),
                          col_types = c(\"date\", \"text\", \"text\", \"text\", rep(\"numeric\", 3)))

holiday_dat$SEQWEEK <- as.numeric(holiday_dat$SEQWEEK)

#McD restaurants
mcd_rest_no <- read_csv(file.path(DataDir, \"McD Restaurant numbers.csv\"))


# POPULATION PROJECTIONS ----
med_popn <- data.frame(population = sapply(2:4, function(abc){
  approx(x = popn_proj[1:2, abc], n = 36)
})[,2]$y, popn_date = seq.Date(from = as.Date(\"2015-06-01\"), to = as.Date(\"2018-05-01\"), by = \"month\"))


temp <- (sapply(7:10, function(a){ approx(popn_data[a:(a+1), 2], n=12)$y}))

popn_data <- data.frame(population =  c(temp[1:12, 1], temp[1:12, 2], temp[1:12,3], temp[1:12, 4]),
                        popn_date =  seq.Date(from = as.Date(\"2011-06-01\"), to = as.Date(\"2015-05-01\"), by = \"month\"))

med_popn <- rbind(popn_data, med_popn)


# PUBLIC HOLIDAY INDICATORS ----
holiday_indicators <- holiday_dat %>%
  mutate(REP_DATE = as.Date(REP_DATE, format = \"%d/%m/%Y\")) %>%
  arrange(REP_DATE) %>%
  mutate(popn_date = as.Date(paste(substr(SEQDAY, 1, 6), 01), format = \"%Y%m%d\")) %>%
  group_by(SEQWEEK) %>%
  summarise(SH = sum(SCHOOL_HOLIDAY),
  PH = sum(PUBLIC_HOLIDAY),
  RH = sum(REGIONAL_HOLIDAY),
  popn_date = min(popn_date),
  REP_DATE = min(REP_DATE)) %>%
  ungroup() %>%
  mutate(week_indicator = as.factor(format(REP_DATE, \"%W\"))) %>%
  left_join(med_popn) %>%
  select(-popn_date) %>%
  left_join(mcd_rest_no) %>%
  filter(!is.na(SEQWEEK))

ieo_data2 <- ieo_data %>%
  filter(SEQWEEK < fc_start_week) %>%
  left_join(holiday_indicators)

# Mcdonald's DF
ieo_week_cat_mcd <- ieo_data2 %>%
  filter(IEO_SEGMENT == \"McDonald's\")

# Burger DF
ieo_week_burger <- ieo_data2 %>%
  filter(IEO_SEGMENT == \"QSR - Burger\")

# CPS DF
ieo_week_cps <- ieo_data2 %>%
  filter(IEO_SEGMENT == \"QSR - Chicken/Pizza/Sandwich\")

# Cafe DF
ieo_week_cafe <-  ieo_data2 %>%
  filter(IEO_SEGMENT == \"Coffee Cafes Bakeries\")

# Takeaway DF
ieo_week_tw <-  ieo_data2 %>%
  filter(IEO_SEGMENT == \"Independent Takeaway\")

# Restaurant DF
ieo_week_rest <-  ieo_data2 %>%
  filter(IEO_SEGMENT == \"Restaurants\")


future_df <- holiday_indicators %>%
  filter(SEQWEEK > max(ieo_data2$SEQWEEK))

# Tune down ADB over time
future_df$ADB <- 0
future_df[ future_df$SEQWEEK > 201619,]$ADB <- 1
future_df[ future_df$SEQWEEK > 201630,]$ADB <- .75
future_df[ future_df$SEQWEEK > 201701,]$ADB <- .5

#----------------------------------------------------------------------------------

# Create DF of forecast values
forecast_predictions.df <- data.frame(SEQWEEK = future_df$SEQWEEK,
                                      REP_DATE = future_df$REP_DATE,
                                      IEO_SEGMENT = \"McDonald's\",
                                      SPEND = manual_model_forecast(ieo_week_cat_mcd, future_df,
                                                                    model_formula = SPEND ~ REP_DATE + SH + RH + PH + week_indicator + population + ADB + RESTAURANT_COUNT)[[1]]) %>%
  rbind(data.frame(SEQWEEK =  future_df$SEQWEEK,
                   REP_DATE =  future_df$REP_DATE,
                   IEO_SEGMENT = \"QSR - Burger\",
                   SPEND = manual_model_forecast(ieo_week_burger,  future_df )[[1]])) %>%
  rbind(data.frame(SEQWEEK =  future_df$SEQWEEK,
                   REP_DATE =  future_df$REP_DATE,
                   IEO_SEGMENT = \"QSR - Chicken/Pizza/Sandwich\",
                   SPEND = manual_model_forecast(ieo_week_cps,  future_df )[[1]])) %>%
  rbind(data.frame(SEQWEEK =  future_df$SEQWEEK,
                   REP_DATE =  future_df$REP_DATE,
                   IEO_SEGMENT = \"Independent Takeaway\",
                   SPEND = manual_model_forecast(ieo_week_tw,  future_df )[[1]])) %>%
  rbind(data.frame(SEQWEEK =  future_df$SEQWEEK,
                   REP_DATE =  future_df$REP_DATE,
                   IEO_SEGMENT = \"Coffee Cafes Bakeries\",
                   SPEND = manual_model_forecast(ieo_week_cafe,  future_df )[[1]])) %>%
  rbind(data.frame(SEQWEEK =  future_df$SEQWEEK,
                   REP_DATE =  future_df$REP_DATE,
                   IEO_SEGMENT = \"Restaurants\",
                   SPEND = manual_model_forecast(ieo_week_rest,  future_df )[[1]]))


# ---------------------------------------------------------------------------------
# Section 3

# Symmertric percentage error
temp1 <- forecast_predictions.df %>%
  filter(SEQWEEK >= fc_start_week) %>%
  mutate(SPEND_pred = SPEND) %>%
  select(-SPEND) %>%
  mcd_process()

temp2 <- ieo_data %>%
  filter(SEQWEEK >= fc_start_week) %>%
  group_by(IEO_SEGMENT, SEQWEEK) %>%
  summarise(SPEND = sum(SPEND, na.rm = T)) %>%
  mutate(SPEND_actual = SPEND) %>%
  select(-SPEND)

temp <- temp1 %>%
  inner_join(temp2) %>%
  mutate(Residual = SPEND_actual - SPEND_pred,
  SAPE = 2 * abs(SPEND_actual - SPEND_pred) / (abs(SPEND_actual) + abs(SPEND_pred)),
  SPE = 2 * (SPEND_actual - SPEND_pred) / (abs(SPEND_actual) + abs(SPEND_pred)))

temp$promo_period <- case_when(temp$SEQWEEK < cpoi_df[1, 2] ~ \"Outside Promo Period\",
                               temp$SEQWEEK > cpoi_df[1, 3] ~ \"Outside Promo Period\",
                               TRUE ~ cpoi_df[1, 1])

temp$promo <- factor(temp$promo_period, levels = c(cpoi_df[1,1]), \"Outside Promo Period\")

p <- temp %>%
  filter(!(IEO_SEGMENT %in% c(\"Coffee Cafes Bakeries\", \"Restaurants\"))) %>%
  ggplot() +
  facet_wrap(~ IEO_SEGMENT) +
  geom_col(aes(x = REP_DATE, y = SPE, fill = promo_period, group = 1)) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(\"Symmetric Percentage Error\", labels = percent, breaks = pretty_breaks(5)) +
  scale_x_date(\"Week Started\", breaks = date_breaks(\"1 week\"), date_labels = \"%d-%b\") +
  scale_fill_manual(values = c(mvl_stone, mvl_plum)) +
  labs(title = paste(\"IEO Performance vs. Forecast for\", cpoi_df[1,1])) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

assign(envir = .GlobalEnv,paste(\"section\", section_num, \"gr\", fig_num, sep = \"_\"), p)

fig_num <- fig_num + 1

# Residuals
p <- temp %>%
  filter(!(IEO_SEGMENT %in% c(\"Coffee Cafes Bakeries\", \"Restaurants\"))) %>%
  ggplot() +
  facet_wrap(~ IEO_SEGMENT) +
  geom_bar(aes(x = REP_DATE, y = Residual, fill = promo_period, group = 1), stat = 'identity') +
  geom_hline(yintercept = 0) +
  scale_y_continuous(\"Residuals\", labels = mdollar, breaks = pretty_breaks(5)) +
  scale_x_date(\"Week Started\", breaks = date_breaks(\"1 week\"), date_labels = \"%d-%b\") +
  scale_fill_manual(values = c(mvl_stone, mvl_plum)) +
  labs(title = paste(\"IEO Performance vs. Forecast for\", cpoi_df[1, 1])) +
  theme_mvl() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))


assign(envir = .GlobalEnv, paste(\"section\", section_num, \"gr\", fig_num, sep = \"_\"), p)

fig_num <- fig_num + 1

# Resisual totals

temp_t <- temp %>%
  group_by(IEO_SEGMENT, promo_period) %>%
  summarise(total_res = sum(Residual), total_spend = sum(SPEND_actual)) %>%
  filter(promo_period == cpoi_df[1, 1]) %>%
  mutate(total_res/total_spend)

assign(envir = .GlobalEnv,paste(\"section\", section_num, \"tab\", tab_num, sep = \"_\"), temp_t)

tab_num <- tab_num + 1


")

  cat(paste(results_text, collapse = ""), file = file.path(root_dir, "R", paste0("3 ", campaign_name, " forecast.R")))

}



#' Create a report script.
#'
#' Builds the skeleton of a script that creates a powerpoint report in the specified root directory.
#' Primarily for internal use in create_project().
#'
#' @param campaign_name The name of the R project.
#' @param root_dir The root directory of the R project.
#'
#' @return Creates a script within the directory specified.
create_mcd_report <- function(campaign_name, root_dir) {
  report_text <- c("# 3 ", campaign_name, " report.R
# Created by ", Sys.info()["user"], ", on ", format(Sys.Date(), "%d %m %Y"), "
# -------------------------------------------------------------------------
# A script designed to compile a powerpoint report containing the
# ", campaign_name, " results.
# -------------------------------------------------------------------------
# WORKFLOW
#   0. INITIALISE
#   1. IMPORT DATA
#   2. COMPILE THE REPORT
# -------------------------------------------------------------------------
# Last edited ", format(Sys.Date(), "%d %m %Y"), " by create_project()
#   - Created
# -------------------------------------------------------------------------

# 0. INITIALISE -----------------------------------------------------------


options(\"ReporteRs-default-font\" = \"Helvetica Neue\",
\"ReporteRs-fontsize\" = 12,
\"ReporteRs-locale.region\" = \"GB\")

# 1. IMPORT DATA ----------------------------------------------------------


data_import()

item_breakdown <- function(df, var) {
  quo_var <- enquo(var)

  df %>%
    filter(!is.na(item),
    !is.na(!!quo_var)) %>%
    group_by(group, !!quo_var) %>%
    summarise(trans = n_distinct(TRANS_ID)) %>%
    mutate(trans = trans / sum(trans),
    trans = percent(trans)) %>%
    spread(!!quo_var, trans, fill = percent(0))
}

# 2. COMPILE THE REPORT ---------------------------------------------------------

# create the report object ----
ppt_report <- pptx(\" \", \"mvl_template.pptx\")


# Add a title slide ----
ppt_report <- ppt_report %>%
  addSlide(slide.layout = \"Title Slide\") %>%
  addTitle(\"McDonald's NZ\") %>%
  addSubtitle(\"", campaign_name, " Campaign Analysis\") %>%
  addParagraph(paste(\"Prepared for: Nik Wong,\", Sys.Date() %>% format(\"%d %B, %Y\") %>% gsub(\"^0\", \"\", .)))

# Add Content -------------------------------------------------------------

# Add definitions
ppt_report %<>%
  addSlide(\"McDefinitions\")

#--------------------------------------------------------------------------
# Generic data sections
#--------------------------------------------------------------------------

# Section 1 ----------------------------------------------------------------

ppt_report %<>%
  addSlide(\"Subtitle\") %>%
  addTitle(\"Section 1\") %>%
  addParagraph(\"High Level Overview of McDonald's Performance\")

ppt_report %<>%
  addSlide(\"Text\") %>%
  addTitle(\"Summary\") %>%
  addParagraph(c(\"\"))

ppt_report %<>%
  addSlide(\"Table\") %>%
  addTitle(toTitleCase(\"\")) %>%
  addParagraph(\"IEO Market Share\") %>%
  addFlexTable(section_1_ft_1) %>%
  addParagraph(c(\"\"))

ppt_report %<>%
  addSlide(\"Table\") %>%
  addTitle(toTitleCase(\"\")) %>%
  addParagraph(\"IEO Market Share\") %>%
  addFlexTable(section_1_ft_2) %>%
  addParagraph(c(\"\"))

ppt_report %<>%
  addSlide(\"Image\") %>%
  addTitle(toTitleCase(\"\")) %>%
  addPlot(function() print(section_1_gr_1), vector.graphic = FALSE)

ppt_report %<>%
  addSlide(\"Table\") %>%
  addParagraph(\"QSR Market Share\") %>%
  addTitle(toTitleCase(\"\")) %>%
  addFlexTable(section_1_ft_3) %>%
  addParagraph(\"\")

ppt_report %<>%
  addSlide(\"Table\") %>%
  addTitle(toTitleCase(\"\")) %>%
  addParagraph(\"Average Cheque\") %>%
  addFlexTable(section_1_ft_4) %>%
  addParagraph(\"\")

ppt_report %<>%
  addSlide(\"Table\") %>%
  addTitle(toTitleCase(\"\")) %>%
  addParagraph(\"Average Cheque\") %>%
  addFlexTable(section_1_ft_5) %>%
  addParagraph(\"\")


# Section 2 ----------------------------------------------------------------

ppt_report %<>%
  addSlide(\"Subtitle\") %>%
  addTitle(\"Section 2\") %>%
  addParagraph(\"McDonald's HVC Performance Review\")

ppt_report %<>%
  addSlide(\"Text\") %>%
  addTitle(\"Summary\") %>%
  addParagraph(\"\")

ppt_report %<>%
  addSlide(\"Table\") %>%
  addTitle(toTitleCase(\"\")) %>%
  addFlexTable(section_2_ft_1)

ppt_report %<>%
  addSlide(\"Table\") %>%
  addTitle(toTitleCase(\"\")) %>%
  addFlexTable(section_2_ft_2)

ppt_report %<>%
  addSlide(\"Table\") %>%
  addTitle(toTitleCase(\"\")) %>%
  addFlexTable(section_2_ft_3)


# Section 3 ----------------------------------------------------------------

#--------------------------------------------------------------------------
# TLD promo specific sections
#--------------------------------------------------------------------------


ppt_report %<>%
  addSlide(\"Subtitle\") %>%
  addTitle(\"Section 3\") %>%
  addParagraph(\"Product-Level Deep Dive\")


ppt_report %<>%
  addSlide(\"Text\") %>%
  addTitle(paste(\"Summary\")) %>%
  addParagraph(c(\"\"))


# Do the item level findings ----
for (item_group in unique(promo_items$item)) {

  # AWU plot ----
  p <- tld_comparison_plot(promo_items, group_name = item_group, con = DB) +
    scale_colour_mvl(palette = \"mcd\")

  ppt_report %<>%
    addSlide(\"Wide Picture Brief\") %>%
    addTitle(paste(item_group, \"AWU Sales\")) %>%
    addPlot(function() print(p), vector.graphic = FALSE)

  # Age & Gender ----
  cust_ftbl <- promo_item_data %>%
    item_breakdown(cust_group) %>%
    left_join(
      promo_item_data %>%
       item_breakdown(GENDER)
    ) %>%
    ft_leaf() %>%
    ft_widths(c(3, rep(1, .$numcol - 1)))

  ppt_report %<>%
    addSlide(\"Table\") %>%
    addTitle(paste(item_group, \"Customers Age & Gender\")) %>%
    addParagraph(paste(item_group, \"Customers Age & Gender\")) %>%
    addFlexTable(cust_ftbl)

  # Daypart ----
  daypart_ftbl <- promo_item_data %>%
    item_breakdown(DAYPART) %>%
    ft_leaf() %>%
    ft_widths(c(2, rep(1, .$numcol - 1)))

  ppt_report %<>%
    addSlide(\"Table\") %>%
    addTitle(paste(item_group, \"Customers by Daypart\")) %>%
    addParagraph(paste(item_group, \"Customers by Daypart\")) %>%
    addFlexTable(daypart_ftbl)

# Region ----
  region_ftbl <- promo_item_data %>%
    item_breakdown(MCD_REGION) %>%
    ft_leaf()

  ppt_report %<>%
    addSlide(\"Table\") %>%
    addTitle(paste(item_group, \"Customers by Region\")) %>%
    addParagraph(paste(item_group, \"Customers by Region\")) %>%
    addFlexTable(region_ftbl)

  # POS ----
  pos_ftbl <- promo_item_data %>%
    item_breakdown(POS_NAME) %>%
    ft_leaf()

  ppt_report %<>%
    addSlide(\"Table\") %>%
    addTitle(paste(item_group, \"POS Types\")) %>%
    addParagraph(paste(item_group, \"POS Types\")) %>%
    addFlexTable(pos_ftbl)

}



# Save the finished report ------------------------------------------------

writeDoc(ppt_report, file = file.path(\"Report/", campaign_name, " Campaign Analysis.pptx\"))
")

  cat(paste(report_text, collapse = ""), file = file.path(root_dir, "R", paste0("3 ", campaign_name, " report.R")))

}



#' Create a run script.
#'
#' Builds the skeleton of a run script in the specified root directory.
#' Primarily for internal use in create_project().
#'
#' @param campaign_name The name of the R project.
#' @param root_dir The root directory of the R project.
#'
#' @return Creates a script within the directory specified.
create_mcd_run <- function(campaign_name, root_dir) {
  run_text <- c("# ", campaign_name, " RUN SCRIPT.R
# Created by ", Sys.info()["user"], ", on ", format(Sys.Date(), "%d %m %Y"), "
# -------------------------------------------------------------------------
# A script designed to run the ", campaign_name, " project from start to
# finish
# -------------------------------------------------------------------------
# WORKFLOW
#   0. INITIALISE
#   1. IMPORT DATA
#   2. PRODUCE THE RESULTS
#   3. COMPILE THE REPORT
# -------------------------------------------------------------------------
# Last edited ", format(Sys.Date(), "%d %m %Y"), " by create_project()
#   - Created
# -------------------------------------------------------------------------


# 0. INITIALISE -----------------------------------------------------------

source(file.path(\"R\", \"0 ", campaign_name, " initialise.R\"))
library(ReporteRs)

# Should the data be refreshed? (generally TRUE)
data_refresh <- TRUE

## Define period of interest attributes ----

# Create a data frame to define comparison periods of interest within the total period of interest
# ** Only contiguous periods are supported **
# Note that promo period of interest should be set as first entry (to become first level of factor)
cpoi_df = data.frame(cpoi_name = c(\"Promo\", \"Rest of Year\", \"Same Promo Last Year\"),
                     spw_start = c(YYYYWW, YYYYWW, YYYYWW),
                     spw_end = c(YYYYWW, YYYYWW, YYYYWW))


## promo item performance -----
promo_items <- read_csv(\"data/example_items.csv\")

# Database connection
DB <- odbcConnect(\"MVIEW\", uid = \"bespoke\", pwd = \"bespoke\")


# 1. IMPORT DATA ----------------------------------------------------------

# Update the processed data if necessary ----

if (data_refresh) {
  source(file.path(\"R\", \"1 ", campaign_name, " processing.R\"))
} else data_import()

# 2. PRODUCE THE RESULTS --------------------------------------------------

# Re-run the results script ----
source(file.path(\"R\", \"2 ", campaign_name, " results.R\"))


# 3. COMPILE THE REPORT ---------------------------------------------------

# Check the spelling ----
file_spellchecker(file.path(\"R\", \"3 ", campaign_name, " report.R\"))

# Re-run the results script ----
source(file.path(\"R\", \"3 ", campaign_name, " report.R\"))
")

  cat(paste(run_text, collapse = ""), file = file.path(root_dir, "R", paste0(campaign_name, " RUN SCRIPT.R")))

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script performs the Amtrak merge verification analysis
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script is inteded to be lightweight
#-------- script intended to be sourced by a markdown file 
#-------- for summary and reporting purposes
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package install and load~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(data.table)
library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(kableExtra)
library(formattable)
library(knitr)
library(ggplot2)
library(forcats)
library(visdat)
library(ggpubr)
library(treemapify)

#global settings~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     ASSETSPEC 2020-04-21
#     ASSETSPEC 2020-03-16

file = "ASSETSPEC 2020-04-21"


#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#path set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/")
rstudioapi::getSourceEditorContext()$path %>% 
  as.character() %>% 
  gsub("(matching).*","\\1", .) %>% 
  path.expand() %>%  
  setwd()

#assetspec import~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#defining asset data columns~~~~~
names_indx = c('assetspecid', 'assetnum', 'assetattrid', 
               'classstructureid', 'displaysequence', 'alnvalue', 
               'changedate', 'changeby',  'Data_Date')

#loading asset data~~~~~
file_path = paste0("./data/", file, ".csv")
asset = fread(file_path) 

num_index = c(which(colnames(asset) %in% names_indx))

asset = asset %>%
  .[, ..num_index]

# asset = asset %>%
#   set_colnames(names_indx)

#munging asset data~~~~~
index = grepl("SEDTRACKSEGMENT", asset$assetattrid) | 
  grepl("SED_CULVERTSPAN_", asset$assetattrid) |
  grepl("AMTMAX", asset$assetattrid) |
  grepl("JCH", asset$assetattrid) == TRUE

asset_records_removed = asset[index,] 

asset_records_removed %>% 
  fwrite(file = "./output/data/asset_records_removed.csv")

asset = asset[!index,]

dupe_checkr = asset %>%  
  .[,.(.N), by = .(assetnum, assetattrid)] %>%  
  .[order(-N)] %>% 
  .[N >1] 

dupe_checkr %>%
  .[, .(.N), by = assetattrid]

dupe_checkr %>%
  .[, .(sum(N))]

dupe_checkr
  fwrite(., file = paste0("./output/data/", 
                          "wtf", ".csv"))

#sed file import~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#loading source file information data~~~~~
master_id = fread("./data/master_id.csv")
master_id_lookup = fread("./data/master_lookup.csv") %>%  
  .[,`:=`(full_src = paste0("./data/", src, ".csv"))]


#removing unnneded source file directories
index = grepl("GEO", master_id_lookup$src) | 
  grepl("META", master_id_lookup$src) | 
  grepl("Speed", master_id_lookup$src) | 
  grepl("Route", master_id_lookup$src) | 
  grepl("Frogs", master_id_lookup$src) | 
  grepl("FROGVERALTVER]", master_id_lookup$src) == TRUE

master_id_lookup = master_id_lookup[!index,]

#merging both files and changing attribute data type
all_source_file_ids = master_id %>%
  merge(master_id_lookup) %>% 
  .[,c(2:3)] %>%  
  unique()

all_source_file_ids$ID = all_source_file_ids$ID %>%  as.integer()



#EDA and munging~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#ASSETSPEC side EDA and munging~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#assetattrid == SED_ID~~~~~
name_sub_index = names_indx[c(1,2,3,6)]

lookup_asset_num_id = asset[assetattrid == "SED_ID", ..name_sub_index] %>% 
  .[order(alnvalue)]
# lookup_asset_num_id = asset[assetattrid == "SED_ID",c(1,2,3,6)] %>% 
#   .[order(alnvalue)]

lookup_asset_num_id %>%  
  fwrite(file = "./output/data/lookup_asset_num_id.csv")

first_num = lookup_asset_num_id %>%  nrow()

lookup_asset_num_id$alnvalue = lookup_asset_num_id$alnvalue %>%  
  as.integer()

#writing out incomplete records
tmp = lookup_asset_num_id[!complete.cases(lookup_asset_num_id)]
bad_num = tmp %>%  nrow()

#complete cases only
lookup_asset_num_id = lookup_asset_num_id[complete.cases(lookup_asset_num_id)]


#aggregating duplicated records per field~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_unique = function(list){
  list %>%  
    unique() %>%  
    length()
}

#summary dupe per column
tmp_check = data.table(complete_records = lookup_asset_num_id %>%  nrow(), 
                       lookup_asset_num_id %>%  
                         purrr::map_df(., get_unique)) %>%  
  melt.data.table(variable.name = "Number of Records per Item", 
                  value.name = "Count") %>%  
  .[,`:=`(non_Distinct_Count = nrow(lookup_asset_num_id)-Count)] %>%  
  .[-4,]


#dupe extract~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
index_dupe_ids = unique(lookup_asset_num_id[duplicated(alnvalue)]$alnvalue)

#get asset_compelete dupe ids
lookup_asset_num_id_duplicated = lookup_asset_num_id[which(alnvalue %in% index_dupe_ids),]

#gets source ID dupes 
sfiles_duplicated_ids = all_source_file_ids[which(ID %in% unique(all_source_file_ids[duplicated(ID)]$ID)),] %>%  
  .[order(ID)]

#sorts all source files by dupe ids
different_source_duplicates = all_source_file_ids[which(ID %in% index_dupe_ids)] %>% 
  .[order(ID)]

#checks to see if there dupes from different source files
different_source_duplicates = setDT(different_source_duplicates)[, if(.N > 1) .SD, by = ID]



#SED extract merge IDs with nonempty asset IDS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

raw_merge = all_source_file_ids %>% 
  merge(., 
        lookup_asset_num_id, 
        by.x = "ID", 
        by.y = "alnvalue", 
        all = TRUE) %>% 
  data.table() %>% 
  unique()

duplicate_id_check = raw_merge %>%  
  semi_join(different_source_duplicates[,1], by = 'ID') %>%  
  data.table() %>% 
  .[order(ID, assetnum)]

#full merge check and identification of NA records
merge_check_NA = raw_merge[!complete.cases(raw_merge)] #all rows w/ NA
merge_check_NA_num_total = merge_check_NA %>%  nrow()

merge_check_NA_src = merge_check_NA[is.na(src)] #records w/ asset data but no s.file date
merge_check_NA_src_num = merge_check_NA_src %>%  nrow()

merge_check_NA_assetnum = merge_check_NA[is.na(assetnum)] #records w/ s.file date but no asset data
merge_check_NA_assetnum_num = merge_check_NA_assetnum %>%  nrow()

#matched IDs b/w SEDEXTARCT and ASSETSPEC 
#complete cases
master_matched_ids = raw_merge %>%  
  na.omit() %>%  
  unique()

duplicated_id_list = master_matched_ids[duplicated(ID),1] %>%  
  unique()

duplicated_record_list = master_matched_ids[duplicated(master_matched_ids),]

master_matched_ids_duplicate_rm = anti_join(master_matched_ids,
                                            duplicated_id_list,
                                            by = "ID") %>%
  data.table() %>%
  .[order(ID), c(1:4)]

#THIS IS IMPORTANT SHOULD THEY BE REMOVED??? THEY ARE CURRENTLY NOT
master_matched_ids_duplicate_only = semi_join(master_matched_ids,
                                              duplicated_id_list,
                                              by = "ID") %>%
  data.table() %>%
  .[order(ID),] 


#section~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DT below has all records ASSETNUMS where their SED_ID values were found
# asset_merged = lookup_asset_num_id %>%  
#   semi_join(asset[,c(1,2,3,6)], ., by = c("assetnum")) %>% 
#   merge(., master_matched_ids[,c(-3,-5)], by = c("assetnum")) %>% 
#   data.table() %>% 
#   unique()

asset_merged = lookup_asset_num_id %>%  
  semi_join(asset[, ..name_sub_index], ., by = c("assetnum")) %>% 
  merge(., master_matched_ids[,c(-3,-5)], by = c("assetnum")) %>% 
  data.table() %>% 
  unique()

asset_merged$ID = as.integer(asset_merged$ID)

#full merge check and identification of NA records
merge_check_NA = asset_merged[!complete.cases(asset_merged)] #all rows w/ NA
merge_check_NA_num_total = merge_check_NA %>%  nrow()


 
# PERFORMS CHECK ON DUPLICATED COLNAMES IN ASSETSPEC~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tmp = asset_merged %>%  
  .[str_detect(assetattrid, "2")] %>%  
  .[,.(assetattrid, src)] %>% 
  unique() 

tmp$assetattrid = str_remove_all(tmp$assetattrid, pattern = "2") %>% 
  gsub('^\\.|\\_$', '', .) %>%  
  gsub('^\\.|\\-$', '', .)

yolo = asset_merged[grepl(paste0(tmp$assetattrid, collapse = "|"), assetattrid)] %>%  
  .[,.(src, assetattrid)] %>% 
  unique()

duplicated_colname_extract_side = yolo[which(src %in% tmp$src)] %>%  
  .[order(src, assetattrid)] 


#intial merge~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#extracting SED file data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#removing SED_TUNNEL, has bad headers
# index = grepl("Tunnel", asset_merged$src)
# asset_merged = asset_merged[!index,]

index = grepl("ALTVER", asset_merged$src)
asset_merged = asset_merged[!index,]

#Extracting all matched ASSETxSEDEXTRACT records' columns and values
suppressMessages({ 
  suppressWarnings({
    
    retreived_source_id_info = data.table()
    
    print("Extracting data from the following source files:")
    for (i in unique(asset_merged$src)){
      
      print(i)
      
      tmp = asset_merged %>% 
        .[src == i,ID] %>%  
        unique()
      
      retreived_source_id_info = master_id_lookup[src == i,full_src] %>%
        fread(., colClasses = 'character') %>% 
        .[ID %in% tmp,] %>% 
        .[,`:=`(id = ID)] %>%  
        purrr::map_df(as.character) %>% 
        reshape2::melt(id.vars = c("ID"), 
                       variable.factor = FALSE, 
                       warning = FALSE) %>% 
        data.table() %>% 
        .[,`:=`(assetattrid = paste0("SED_", str_to_upper(variable)))] %>%  
        bind_rows(retreived_source_id_info, .)
    }
    print("Done")
  })
})

retreived_source_id_info$ID = retreived_source_id_info$ID %>%  
  as.integer()

#full merge check and identification of NA records
merge_check_NA = retreived_source_id_info[!complete.cases(retreived_source_id_info)] #all rows w/ NA
merge_check_NA_num_total = merge_check_NA %>%  nrow()



#sed file and assetspec merge~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#investigating mismatched occurance
colum_order_index = c("src", "assetnum", "ID", "assetspecid", 
                      "assetattrid", "variable", "alnvalue", "value")

attribute_value_master = merge(asset_merged, 
                               retreived_source_id_info, 
                               by.x = c("ID", "assetattrid"), 
                               by.y = c("ID", "assetattrid"), all = TRUE) %>%  
  unique() %>% 
  .[,..colum_order_index] %>%
  .[order(src, ID, assetnum, assetattrid,)] %>% 
  .[,-1] %>%  
  merge(., all_source_file_ids, by = "ID" ) %>% 
  .[,..colum_order_index] %>%  
  .[order(src, ID, assetnum, assetattrid,)]

index = grepl("JCH", attribute_value_master$assetattrid) 
attribute_value_master = attribute_value_master[!index]

attribute_value_master_rows = attribute_value_master %>%  nrow()

attribute_value_master %>% 
  .[duplicated(variable)]

merge_check_NA = attribute_value_master[!complete.cases(attribute_value_master)] #all rows w/ NA
merge_check_NA_num_total = merge_check_NA %>%  nrow()

na_count = is.na(merge_check_NA) %>%  
  data.table() %>% 
  .[,.(.N), by = .(src, ID, assetnum, assetspecid, assetattrid, alnvalue, variable, value)] %>% 
  .[,-c('src', 'ID', 'assetattrid')] 


#incomplete record exploration~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ffmerge_na_no_asset_data =  merge_check_NA[is.na(assetnum) &
                                             is.na(assetspecid) &
                                             is.na(alnvalue) &
                                             !is.na(variable) &
                                             !is.na(value),]


ffmerge_na_no_extract_data = merge_check_NA[!is.na(assetnum) &
                                              !is.na(assetspecid) &
                                              !is.na(alnvalue) &
                                              is.na(variable) &
                                              is.na(value),]


ffmerge_na_no_alnvalue_value = merge_check_NA[!is.na(assetnum) &
                                                !is.na(assetspecid) &
                                                is.na(alnvalue) &
                                                !is.na(variable) &
                                                is.na(value),]


ffmerge_na_no_value = merge_check_NA[!is.na(assetnum) &
                                       !is.na(assetspecid) &
                                       !is.na(alnvalue) &
                                       !is.na(variable) &
                                       is.na(value),]


#splitting on match types~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
attribute_value_master = attribute_value_master %>%  
  na.omit()

attribute_value_master_good_row = attribute_value_master %>%  nrow()

attribute_value_notmatching =  attribute_value_master %>% 
  .[alnvalue != value,] %>% 
  .[,..colum_order_index] %>%  
  .[order(src, ID, assetnum, assetattrid,)]

attribute_value_matching = attribute_value_master %>% 
  .[alnvalue == value,] %>% 
  .[,..colum_order_index] %>%  
  .[order(src, ID, assetnum, assetattrid,)]



#type 1 error identification~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###Case and Whitespace~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#removes all mismatch cases caused by CASE or whitespace issues
index_character_mismatch = str_trim(str_to_lower(attribute_value_notmatching$alnvalue), side = "both") == str_trim(str_to_lower(attribute_value_notmatching$value),side = "both")
tmp = attribute_value_notmatching[!index_character_mismatch]

total_matched_example_case_whitespace_sensitive = attribute_value_notmatching[index_character_mismatch]

### Numeric Padding
#removes all records where mismatch was caused by false padding encoding
index_alnvalue = as.numeric(tmp$alnvalue)
index_alnvalue[is.na(index_alnvalue)] = 0

index_value = as.numeric(tmp$value)
index_value[is.na(index_value)] = -29348576.127

index_PADDING_mismatch = index_alnvalue == index_value

total_matched_example_padding = tmp[index_PADDING_mismatch]
tmp = tmp[!index_PADDING_mismatch]

vairable_type = total_matched_example_padding %>% 
  .[,.(count = .N), by = .(variable)] %>%
  .[order(-count)]


### Rounding~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#removes long/lat columns as proven to be just a rounding error
index = grepl("LONG", tmp$assetattrid) | grepl("LATITUDE", tmp$assetattrid)
rounding = tmp[index]

tmp = tmp[!index]


### Special Characters~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#removes all records where mismatch was caused by false encoding
index_DIMENSIONS = tmp$assetattrid == "SED_DIMENSIONS"

total_matched_bad = tmp[!index_DIMENSIONS] 


bad_special = nrow(tmp[index_DIMENSIONS])

### Date Format Mismatch~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#identify date records 
tmp_time_dt = total_matched_bad[str_detect(total_matched_bad$assetattrid, "DATE") ,] 

date_pos_match_excel = tmp_time_dt[value %>%  
                                     ymd_hms() %>%  
                                     date() == alnvalue %>% 
                                     as.numeric() %>% 
                                     as.Date(origin = "1899-12-30"),]

date_pos_match_format = tmp_time_dt[str_detect(alnvalue, "/")] %>% 
  .[value %>%  
      ymd_hms() %>%  
      date() == alnvalue %>%  
      mdy_hm(format = "%Y-%m-%d") %>%  
      as_date() %>% 
      na.omit(),]

tmp_time_dt_remaining = tmp_time_dt %>%  
  anti_join(date_pos_match_excel, by = c("ID", "assetspecid")) %>%  
  anti_join(date_pos_match_format, by = c("ID", "assetspecid")) %>%  
  data.table()

total_matched_bad = total_matched_bad %>%  
  anti_join(date_pos_match_excel, by = c("ID", "assetspecid")) %>%  
  anti_join(date_pos_match_format, by = c("ID", "assetspecid")) %>%  
  data.table()

tmp_good_date = date_pos_match_excel %>%  
  bind_rows(date_pos_match_format) %>%  
  data.table()


#Final data merge -- sliced on match type~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
total_matched_good = attribute_value_master %>%  
  anti_join(total_matched_bad, by = c('assetnum', 'assetspecid', "assetattrid")) %>%  
  data.table()

total_matched_bad %>%  
  fwrite(file = "./output/data/total_matched_bad.csv")

total_matched_good %>%  
  fwrite(file = "./output/data/total_matched_good.csv")

fail_percent = 100*nrow(total_matched_bad)/(nrow(total_matched_bad)+ nrow(total_matched_good))










#File Correction~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#function creation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
checkr_empty = function(data){
  ifelse(nrow(data) == 0, 
         "All Records Accounted for!", 
         "Records Missing!") %>%  
    message()
}

checkr_compare = function(data_one, data_two){
  ifelse(nrow(data_one) == nrow(data_two), 
         "All Records Accounted for!", 
         "Records Missing!") %>%  
    message()
}

checkr_dupe_diff = function(data, column){
  dupe_num = data %>% 
    .[,.(.N), by = c('assetnum', 'assetattrid', column)] %>% 
    .[,.(.N), by = .(assetnum, assetattrid)] %>% 
    .[N > 1] %>% 
    nrow()
  
  print("Number of duplicates", dupe_num)
  
  ifelse(dupe_num == 0, 
         "All dupe records have same value!", 
         "Different Values!!!!") %>%  
    message()
}



#Date Correction~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Data Set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
remaining_bad_dates = tmp_time_dt_remaining

remaining_bad_dates$value = remaining_bad_dates$value %>%  
  str_trim()

#Date Correction: Negative Matches~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_date_empty_value = remaining_bad_dates %>%  
  .[value == "",] 

remaining_bad_dates = remaining_bad_dates %>%   
  anti_join(bad_date_empty_value, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#FIX:
bad_date_empty_value$nu_date = "NULL"

# bad_date_empty_value %>% 
#   fwrite(., file = paste0("./output/data/outstanding/", 
#                           deparse(substitute(bad_date_NULL)), ".csv"))


#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_date_NULL_value = remaining_bad_dates %>%  
  .[value == "NULL",] 

remaining_bad_dates = remaining_bad_dates %>%   
  anti_join(bad_date_NULL_value, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#FIX:
bad_date_NULL_value$nu_date = "FURTHER WORK REQUIRED"

# bad_date_NULL_value %>% 
#   fwrite(., file = paste0("./output/data/outstanding/", 
#                           deparse(substitute(bad_date_NULL)), ".csv"))


#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_date_NULL_alnvalue_year = remaining_bad_dates %>%  
  .[alnvalue == "NULL",] %>% 
  .[str_count(value) == 4]

remaining_bad_dates = remaining_bad_dates %>%   
  anti_join(bad_date_NULL_alnvalue_year, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#FIX:
bad_date_NULL_alnvalue_year = bad_date_NULL_alnvalue_year %>% 
  .[,`:=`(nu_date =  paste0(value, "-01-01 00:00:00"))] 

# bad_date_NULL_alnvalue_year %>% 
#   fwrite(., file = paste0("./output/data/outstanding/", 
#                           deparse(substitute(bad_date_NULL_alnvalue_year)), ".csv"))


#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_date_NULL_alnvalue_fulldate = remaining_bad_dates %>%  
  .[alnvalue == "NULL",] %>% 
  .[str_count(value) > 10]

remaining_bad_dates = remaining_bad_dates %>%   
  anti_join(bad_date_NULL_alnvalue_fulldate, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#FIX:
bad_date_NULL_alnvalue_fulldate = bad_date_NULL_alnvalue_fulldate %>% 
  .[,`:=`(nu_date = str_trunc(value, 19, "right", ellipsis = ""))]

# bad_date_NULL_alnvalue_fulldate %>% 
#   fwrite(., file = paste0("./output/data/outstanding/", 
#                           deparse(substitute(bad_date_NULL_alnvalue)), ".csv"))


#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_date_NULL_alnvalue_integer = remaining_bad_dates %>%  
  .[alnvalue == "NULL",] 

remaining_bad_dates = remaining_bad_dates %>%   
  anti_join(bad_date_NULL_alnvalue_integer, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#FIX:
bad_date_NULL_alnvalue_integer = bad_date_NULL_alnvalue_integer %>% 
  .[,`:=`(nu_date = as.numeric(value)%/%1 %>%
            as_date(origin = "1899-12-30")),] %>% 
  .[,`:=`(nu_date = nu_date %>%  
            paste("00:00:00") %>%  
            str_trunc(19, "right", ellipsis = "")),] 

# bad_date_NULL_alnvalue_integer %>% 
#   fwrite(., file = paste0("./output/data/outstanding/", 
#                           deparse(substitute(bad_date_NULL_alnvalue)), ".csv"))



#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_date_zero = remaining_bad_dates %>%  
  .[alnvalue == "0",] 

remaining_bad_dates = remaining_bad_dates %>%   
  anti_join(bad_date_zero, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#FIX:
bad_date_zero = bad_date_zero %>%  
  .[,`:=`(nu_date = str_trunc(value, 19, "right", ellipsis = ""))]

# bad_date_zero %>% 
#   fwrite(., file = paste0("./output/data/outstanding/", 
#                           deparse(substitute(bad_date_zero)), ".csv"))



#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_date_custom_format_w_integer = remaining_bad_dates %>% 
  .[str_detect(alnvalue, ":") & 
      str_detect(alnvalue, ".")] %>%  
  .[str_count(value) < 9]

remaining_bad_dates = remaining_bad_dates %>%   
  anti_join(bad_date_custom_format_w_integer, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#FIX:
bad_date_custom_format_w_integer = bad_date_custom_format_w_integer %>% 
  .[,`:=`(nu_date = as.numeric(value) %>% 
           as_date(origin = "1899-12-30") %>% 
           paste0(" 00:00:00")), ]

# bad_date_custom_format_w_integer %>% 
#   fwrite(., file = paste0("./output/data/outstanding/", 
#                           deparse(substitute(bad_date_custom_format_w_integer)), ".csv"))



#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_date_custom_format_w_date = remaining_bad_dates %>% 
  .[str_detect(alnvalue, ":") & 
      str_detect(alnvalue, ".")] %>%  
  .[str_count(value) >= 9]

remaining_bad_dates = remaining_bad_dates %>%   
  anti_join(bad_date_custom_format_w_date, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#FIX:
bad_date_custom_format_w_date$nu_date  = bad_date_custom_format_w_date$value %>%  
  str_trunc(19, "right", ellipsis = "")

# bad_date_custom_format_w_date %>% 
#   fwrite(., file = paste0("./output/data/outstanding/", 
#                           deparse(substitute(bad_date_custom_format_w_date)), ".csv"))



#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_date_decimallong = remaining_bad_dates %>%  
  .[!is.na(as.integer(alnvalue)),] %>% 
  .[str_count(alnvalue)>=8]

remaining_bad_dates = remaining_bad_dates %>%   
  anti_join(bad_date_decimallong, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#FIX
bad_date_decimallong = bad_date_decimallong %>% 
  .[,`:=`(nu_date = as.numeric(alnvalue)%/%1 %>%
            as_date(origin = "1899-12-30")),] %>% 
  .[,`:=`(nu_date = nu_date %>%  
            paste("00:00:00") %>%  
            str_trunc(19, "right", ellipsis = "")),] 

# bad_date_decimallong %>% 
#   fwrite(., file = paste0("./output/data/outstanding/", 
#                           deparse(substitute(bad_date_decimallong)), ".csv"))

#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_date_integers_year = remaining_bad_dates[str_count(value) == 4 ,] %>% 
  .[as.integer(alnvalue) < 2025 &
      as.integer(alnvalue) > 1800] 

remaining_bad_dates = remaining_bad_dates %>%   
  anti_join(bad_date_integers_year, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#FIX
bad_date_integers_year$nu_date = ifelse(abs(as.integer(bad_date_integers_year$alnvalue)-as.integer(bad_date_integers_year$value)) < 10, 
                                        paste0(bad_date_integers_year$value, "-01-01 00:00:00"), 
                                        "FURTHER WORK REQUIRED")

#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_date_remaining = remaining_bad_dates %>% 
  .[,`:=`(value = str_trunc(paste(value, "00:00:00"),19, "right", ellipsis = "" ))] %>% 
  .[,`:=`(nu_date = as.numeric(alnvalue) %>% 
            as_date(origin = "1899-12-30") %>% 
            paste0(" 00:00:00")), ] %>% 
  .[alnvalue == 1 & value == "1900-01-01 00:00:00", `:=`(nu_date = "1900-01-01 00:00:00")]

remaining_bad_dates = remaining_bad_dates %>%   
  anti_join(bad_date_remaining, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()


#final check~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
checkr_empty(remaining_bad_dates)




#Date Correction: Positive Matches~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

remaining_good_dates = total_matched_good[str_detect(assetattrid, "DATE")]
# remaining_good_dates[assetspecid == "8294503"]
# remaining_good_dates$value = remaining_good_dates$value %>%  
#   str_trim()
# remaining_good_dates$alnvalue = remaining_good_dates$alnvalue %>%  
#   str_trim()

#Good Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
good_date_NULL = remaining_good_dates[value == "NULL"] 

remaining_good_dates = remaining_good_dates %>%   
  anti_join(good_date_NULL, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#FIX:
good_date_NULL$nu_date = good_date_NULL$value

#Good Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
good_date_multi_year = remaining_good_dates %>%  
  .[str_detect(alnvalue, ";")]

remaining_good_dates = remaining_good_dates %>%   
  anti_join(good_date_multi_year, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table() 

#FIX
good_date_multi_year$nu_date = good_date_multi_year$value

#Good Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
good_date_X = remaining_good_dates %>%  
  .[str_trim(value) == "X"]

remaining_good_dates = remaining_good_dates %>%   
  anti_join(good_date_X, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table() 

#FIX
good_date_X$nu_date = good_date_X$value

#Good Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# good_date_integers_year = remaining_good_dates %>% 
#   .[str_count(alnvalue) == 4] %>% 
#   .[str_count(value) == 4] 

good_date_integers_year = remaining_good_dates[as.integer(alnvalue) == as.integer(value)] %>% 
  .[as.integer(alnvalue) < 2025 &
      as.integer(alnvalue) > 1800] 

remaining_good_dates = remaining_good_dates %>%   
  anti_join(good_date_integers_year, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#FIX
good_date_integers_year = good_date_integers_year %>% 
  .[,`:=`(nu_date = paste0(value, "-01-01 00:00:00"))] 

#Good Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
good_date_integers = remaining_good_dates[as.integer(alnvalue) == as.integer(value)] 

good_date_integers %>% sample_n(30)
remaining_good_dates = remaining_good_dates %>%   
  anti_join(good_date_integers, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#FIX
good_date_integers = good_date_integers[,`:=`(nu_date = as.numeric(value) %>% 
                                                as_date(origin = "1899-12-30") %>% 
                                                paste0(" 00:00:00")), ]

#Good Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
good_date_SUB = remaining_good_dates[assetattrid == "SED_SUBDATE"]

remaining_good_dates = remaining_good_dates %>%
  anti_join(good_date_SUB, by = c('assetnum', 'ID', 'assetspecid')) %>%
  data.table()

#FIX:
good_date_SUB = good_date_SUB %>%
  .[,`:=`(nu_date = paste0(value, "-01-01 00:00:00"))]

#Good Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
good_date_SUP = remaining_good_dates[assetattrid == "SED_SUPDATE"]

remaining_good_dates = remaining_good_dates %>%
  anti_join(good_date_SUP, by = c('assetnum', 'ID', 'assetspecid')) %>%
  data.table()

#FIX:
good_date_SUP = good_date_SUP %>%
  .[,`:=`(nu_date = paste0(value, "-01-01 00:00:00"))]




#Good Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
good_date_frogpoint = remaining_good_dates[str_detect(assetattrid, "FROG") | 
                                             str_detect(assetattrid, "POINT"),] 

remaining_good_dates = remaining_good_dates %>%   
  anti_join(good_date_frogpoint, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

good_date_frogpoint = good_date_frogpoint %>%  
  .[,`:=`(nu_date = strptime(value, format = "%b %d %Y %H:%M") %>% 
            as.character())]

#Good Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
good_date_formatlong = remaining_good_dates %>%  
  .[str_count(value)>=15]

remaining_good_dates = remaining_good_dates %>%   
  anti_join(good_date_formatlong, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table() 

#FIX
good_date_formatlong = good_date_formatlong %>% 
  .[,`:=`(nu_date = str_trunc(value,19, "right", ellipsis = "" ))] 

#final check~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
checkr_empty(remaining_good_dates)



#Date Aggregation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_bad_date = bind_rows(bad_date_custom_format_w_date,
                         bad_date_custom_format_w_integer,
                         bad_date_decimallong,
                         bad_date_empty_value,
                         bad_date_integers_year,
                         bad_date_NULL_alnvalue_fulldate,
                         bad_date_NULL_alnvalue_integer,
                         bad_date_NULL_value,
                         bad_date_NULL_alnvalue_year,
                         bad_date_remaining,
                         bad_date_zero) %>% 
  .[,`:=`(match_type = "bad")]

all_good_date = bind_rows(good_date_formatlong,
                          good_date_frogpoint, 
                          good_date_integers, 
                          good_date_integers_year, 
                          good_date_multi_year,
                          good_date_NULL,
                          good_date_SUB,
                          good_date_SUP,
                          good_date_X,) %>%
  .[,`:=`(match_type = "good")]

#checks~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cbind(all_bad_date[,.(alnvalue, value, nu_date)] %>% sample_n(30), 
      all_good_date[,.(alnvalue, value, nu_date)] %>% sample_n(30))
checkr_compare(all_bad_date, tmp_time_dt_remaining)
checkr_compare(all_good_date, total_matched_good[str_detect(assetattrid, "DATE")])

total_date = bind_rows(all_bad_date, all_good_date)

checkr_dupe_diff(total_date, "nu_date")

total_date[nu_date != "FURTHER WORK REQUIRED"] %>%
  .[,.(assetnum, assetspecid, assetattrid, nu_date)] %>% 
  fwrite(., file = paste0("./output/data/corrected_records/", 
                          "corrected_dates_",
                          file %>% 
                            str_remove_all(" ") %>% 
                            str_remove_all("-"),
                          ".csv"))

total_date[nu_date == "FURTHER WORK REQUIRED"] %>%
  .[,.(assetnum, assetspecid, assetattrid, nu_date)] %>%
  fwrite(., file = paste0("./output/data/outstanding/", 
                          "outstanding_dates_",
                          file %>% 
                            str_remove_all(" ") %>% 
                            str_remove_all("-"),
                          ".csv"))
  
hdaaatotal_date %>%  
  .[,.(.N), by = .(assetnum, assetspecid, assetattrid, value)] %>% 
  .[order(-N)] %>% 
  .[N > 1] 


column = "nu_date"
dupe_DT = total_date %>% 
  .[,.(.N), by = c('assetnum', 'assetattrid', "assetspecid", column)] %>% 
  .[order(-N)] %>% 
  .[,.(.N), by = .(assetnum, assetattrid, assetspecid)] %>% 
  .[order(-N)] %>% 
  .[N > 1] 
      
tmp = total_date %>%  
  semi_join(dupe_DT, by = c("assetnum", 'assetattrid', 'assetspecid'))

attribute_value_master[assetspecid == "5669921" &assetattrid == "SED_VALIDATIONDATE"]

asset[assetspecid == "5669921" &assetattrid == "SED_VALIDATIONDATE"]

asset[which(assetnum %in% tmp$assetnum[5:6])] %>% 
  .[which(assetspecid %in% tmp$assetspecid[5:6]),]

asset[which(assetnum %in% tmp$assetnum[5:6])] 
# %>% 
#   .[which(assetspecid %in% tmp$assetspecid[5:6]),]

asset %>%  
  .[,.(.N), by = .(assetnum, assetattrid)] %>%  
  .[order(-N)] %>% 
  .[N >1] %>%
  fwrite(., file = paste0("./output/data/", 
                          "wtf", ".csv"))

asset %>%  
  .[,.(Num_repeated_assetattrid_per_assetnum = .N), by = .(assetnum, assetattrid)] %>%  
  # .[order(-N)] %>% 
  .[,.(Count = .N), by = Num_repeated_assetattrid_per_assetnum] %>% 
  .[order(-Count)] 
  .[N >1] %>%  
  .[assetattrid == "SED_ID"]

assetnum[]



asset %>%  
  .[,.(.N), by = .(assetnum)] %>%  
  .[N >1] %>% 
  .

asset[assetnum == "AN LINE UG BRIDGE B ST. - NO 82.82"] %>% 
  .[assetattrid == "SED_ID"]




asset_merged[assetnum == "AN LINE UG BRIDGE B ST. - NO 82.82" & 
               asse]

master_id_lookup
master_id[ID == "155412" | 
            ID == "155659"]







#Non-Date Record Correction~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Data Set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#splitting total bad to non-date
total_matched_bad_cleaned = total_matched_bad %>%   
  anti_join(tmp_time_dt_remaining, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table() 

remaining = total_matched_bad_cleaned
  
total_matched_bad %>%  
  .[str_detect(assetattrid, "DATE")] %>% 
  checkr_compare(., all_bad_date)

#Bad empty~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_empty_character_null = remaining %>%  
  .[value == ""] %>% 
  .[alnvalue == "NULL"]

remaining = remaining %>%  
  anti_join(bad_empty_character_null, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix 
bad_empty_character_null$nu_value = "NULL"

bad_empty_character_null %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_empty_character_null)), ".csv"))

#Bad empty~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_empty_character_alnvalue = remaining %>%  
  .[value == ""] 

remaining = remaining %>%  
  anti_join(bad_empty_character_alnvalue, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix 
bad_empty_character_alnvalue$nu_value = "FURTHER WORK REQUIRED"

bad_empty_character_alnvalue %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_empty_character)), ".csv"))


#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_NA_character = remaining %>%  
  .[value == "N/A"] %>% 
  .[alnvalue == "NULL"]

remaining = remaining %>%  
  anti_join(bad_NA_character, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix 
bad_NA_character$nu_value = "N/A"

bad_NA_character %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_NA_character)), ".csv"))

#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_null_w_value = remaining[alnvalue == "NULL"] %>% 
  .[value != "NULL"] %>% 
  .[value != ""] %>% 
  .[value != "N/A"]

remaining = remaining %>%   
  anti_join(bad_null_value, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix 
bad_null_w_value$nu_value = bad_null_w_value$value

bad_null_value %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_null_value)), ".csv"))


#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_null_alnvalue = remaining[alnvalue != "NULL" &  value == "NULL"] 

remaining = remaining %>%  
  anti_join(bad_null_alnvalue, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix 
bad_null_alnvalue$nu_value = "FURTHER WORK REQUIRED"

bad_null_alnvalue %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_null_alnvalue)), ".csv"))

#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_integer_round = remaining[as.integer(alnvalue) == as.integer(value)]

remaining = remaining %>%  
  anti_join(bad_integer_round, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix 
bad_integer_round$nu_value = bad_integer_round$value

bad_integer_round %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_integer_round)), ".csv"))


#Bad Scientific~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_scientific_notation_2d = remaining %>% 
  data.table() %>% 
  .[alnvalue == value %>%
      as.numeric() %>% 
      formatC(format = "E", digits=2) %>%  
      as.character(),]

remaining = remaining %>%   
  anti_join(bad_scientific_notation_2d, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix 
bad_scientific_notation_2d$nu_value = bad_scientific_notation_2d$value

bad_scientific_notation_2d %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_scientific_notation_2d)), ".csv"))


#Bad Scientific~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_scientific_notation_1d = remaining %>% 
  data.table() %>% 
  .[alnvalue == value %>%
      as.numeric() %>% 
      formatC(format = "E", digits=1) %>%  
      as.character(),]

remaining = remaining %>%   
  anti_join(bad_scientific_notation_1d, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix 
bad_scientific_notation_1d$nu_value = bad_scientific_notation_2d$value

bad_scientific_notation_1d %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_scientific_notation_1d)), ".csv"))


#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_custom_format_timestamp = remaining %>% 
  .[assetattrid == "SED_DBTIMESTAMP"]
  
remaining = remaining %>%   
  anti_join(bad_custom_format_timestamp, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix
bad_custom_format_timestamp = bad_custom_format_timestamp %>% 
  .[,`:=`(nu_value = str_trunc(value,10, "right", ellipsis = "" ) %>%
            paste("00:00:00"))] 

bad_custom_format_timestamp %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_custom_format_timestamp)), ".csv"))

#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_custom_format = remaining %>% 
  .[str_detect(alnvalue, ":") & 
      str_detect(alnvalue, ".") ] 

remaining = remaining %>%   
  anti_join(bad_custom_format, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix
bad_custom_format$nu_value = bad_custom_format$value

bad_custom_format %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_custom_format)), ".csv"))


#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_hastags = remaining %>% 
  .[str_detect(alnvalue,"#"),]

remaining = remaining %>%   
  anti_join(bad_hastags, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix
bad_hastags$nu_value = bad_hastags$value

bad_hastags %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_hastags)), ".csv"))


#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_integers = remaining %>%  
  .[!is.na(as.integer(alnvalue)),] %>% 
  .[!is.na(as.integer(value)),]

remaining = remaining %>%   
  anti_join(bad_integers, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix
bad_integers$nu_value = "FURTHER WORK REQUIRED"

bad_integers %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_integers)), ".csv"))


#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_backslash = remaining %>% 
  .[str_detect(value,"/"),]

remaining = remaining %>%   
  anti_join(bad_backslash, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix
bad_backslash$nu_value = bad_backslash$alnvalue

bad_backslash %>% 
  fwrite(., file = paste0("./output/data/outstanding/", deparse(substitute(bad_backslash)), ".csv"))


#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_numeric_dash = remaining %>% 
  .[str_detect(value,"-"),] %>%  
  .[!is.na(as.integer(alnvalue))]

remaining = remaining %>%   
  anti_join(bad_numeric_dash, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix
bad_numeric_dash$nu_value = "FURTHER WORK REQUIRED"

bad_numeric_dash %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_numeric_dash)), ".csv"))


#Bad Dates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bad_just_wrong = remaining 

remaining = remaining %>%   
  anti_join(bad_just_wrong, by = c('assetnum', 'ID', 'assetspecid')) %>%  
  data.table()

#Fix
bad_just_wrong$nu_value = "FURTHER WORK REQUIRED"

bad_just_wrong %>% 
  fwrite(., file = paste0("./output/data/outstanding/", 
                          deparse(substitute(bad_just_wrong)), ".csv"))

#final check~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
checkr_empty(remaining)


#Date Correction: Positive Matches~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_good = total_matched_good[str_detect(assetattrid, "DATE")] %>%
  .[,`:=`(nu_value = value)] %>% 
  .[,`:=`(match_type = "good")]


#Date Aggregation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_bad = bind_rows(bad_empty_character_null,
                    bad_empty_character_alnvalue,
                    bad_NA_character,
                    bad_null_w_value,
                    bad_null_alnvalue,
                    bad_integer_round,
                    bad_scientific_notation_2d,
                    bad_scientific_notation_1d,
                    bad_custom_format_timestamp,
                    bad_custom_format,
                    bad_hastags,
                    bad_integers,
                    bad_backslash,
                    bad_numeric_dash,
                    bad_just_wrong) %>% 
  .[,`:=`(match_type = "bad")]

checkr_compare(all_bad, total_matched_bad_cleaned)

total_date = bind_rows(all_good, all_bad)

checkr_dupe_diff(total_date, "nu_value")

total_date %>%  unique() %>% 
  .[,.(.N), by = .(assetnum, assetspecid, assetattrid, value)] %>% 
  .[order(-N)]

total_date[,.(assetnum, assetattrid, nu_date)] %>%  
  unique()
fwrite(., file = paste0("./output/data/corrected_records/", "corrected_dates", ".csv"))

column = "nu_value"
total_date[,..column]

total_date %>% 
  .[,.(.N), by = c('assetnum', 'assetattrid', column)]

checkr_dupe_diff = function(data, column){
  dupe_num = data %>% 
    .[,.(.N), by = c('assetnum', 'assetattrid', ..column)] %>% 
    .[,.(.N), by = .(assetnum, assetattrid)] %>% 
    .[N > 1] %>% 
    nrow()
  
  checkr_compare(all_bad_date, tmp_time_dt_remaining)
  checkr_compare(all_good_date, total_matched_good[str_detect(assetattrid, "DATE")])
  
  total_date = bind_rows(all_bad_date, all_good_date)
  
  checkr_dupe_diff(total_date, "nu_date")






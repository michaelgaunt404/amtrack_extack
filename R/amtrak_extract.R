#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script remotely knits RMarkdown files.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script needs to be colocated in same folder as data
#-------- Script writes to same location
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package install and load~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(data.table)
require(magrittr)
require(dplyr)


#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/")
rstudioapi::getSourceEditorContext()$path %>% 
  as.character() %>% 
  gsub("(matching).*","\\1", .) %>% 
  path.expand() %>%  
  setwd()

data_files = dir("./data", pattern = c("csv"), full.names = TRUE) 


#wrangling~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~       
index = grepl("GEO", data_files) | grepl("SED", data_files) == TRUE
data_files = data_files[index]

index = grepl("META", data_files) | grepl("OUTDATED", data_files) == FALSE
data_files = data_files[index]

fix_names = function(data){
  data %>%
    gsub("[./' ]","", .) %>% 
    gsub("csv.*","",.) %>% 
    gsub(".*data","",.) %>%  
    data.table(src = .)
}

master_id_lookup = fix_names(data_files) %>% 
  .[,`:=`(full_src = data_files,
          alias = as.numeric(row.names(.)))] 

master_id = data.table(ID = as.numeric(), 
                    alias = as.numeric())

for (i in 1:nrow(master_id_lookup)){
  df = fread(data_files[i], select = 1) %>% 
    .[,`:=`(alias = master_id_lookup$alias[i])] %>% 
    set_colnames(c("ID", "alias"))
  
  if (is.numeric(df[[1,1]])){
    master_id = bind_rows(master_id, df)} else {
      master_id = master_id
      
    }
}

#output production~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
index_check = unique(master_id$alias) %>%  
  data.table(alias = .)

missing_src = anti_join(master_id_lookup, index_check, by = "alias")

master_duplicated_items =  master_id[duplicated(master_id)] %>%  
  unique()

master_id_lookup %>% 
  .[,`:=`(missing = 0)]

master_id_lookup$missing[which(master_id_lookup$alias %in% missing_src$alias)] = 1

master_id_lookup %>% 
  .[,`:=`(duplications = 0)]

master_id_lookup$duplications[which(master_id_lookup$alias %in% unique(master_duplicated_items$alias))] = 1

#write/save~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fwrite(master_id[,2:1], file = "./data/master_id.csv")
fwrite(master_id_lookup[,c(3,1,2,4,5)], file = "./data/master_lookup.csv")
fwrite(master_duplicated_items[,2:1], file = "./data/master_duplicated_items.csv")
master_id[ID == 1864994]
master_id_lookup[alias == 116]

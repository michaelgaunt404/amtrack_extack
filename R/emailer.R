#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script remotely knits RMarkdown files.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
install.packages("gmailR")
library(gmailr)
library(tidyverse)
library(jsonlite)
read_json("./docs/secret/domorewithr.json")
#getwd()
gm_auth_configure(path = "./docs/secret/domorewithr.json")
use_secret_file("./docs/secret/domorewithr.json")

email_body = "Hello, this is an automated response from your friendly person."

file_directory = paste0(getwd(), "/output") %>% 
  list.files(full.names = T) %>%  
  file.info() %>% 
  mutate(name = rownames(.))

file_attach = file_directory[file_directory$mtime >= max(file_directory$mtime)-1, "name"]

my_email_message = gm_mime() %>% 
  gm_to("mike.gaunt.404@gmail.com") %>% 
  gm_from("mike.gaunt.404@gmail.com") %>% 
  gm_subject(paste("hello",format(Sys.time(), '%Y%m%d'), sep = "_")) %>% 
  gm_text_body(email_body) %>% 
  gm_attach_file(file_attach[2]) %>% 
  gm_attach_file(file_attach[1])

                 
send_message(my_email_message)
  #sets suffix for file name
#generally best if this is 'system date/time'
suffix = format(Sys.time(), '%Y%m%d_%H%M%S')

#section calls script to run, provides filename suffix, and location
#for terminal usage

gmailr::gmai

ezknit(file = "yolo.rmd", 
       out_suffix = suffix,
       out_dir = "../output",
       fig_dir = "../myfigs")

#for internal script usage
#needs to be different than terminal version
# ezknit(file = "r/yolo.rmd", 
#        out_suffix = suffix,
#        out_dir = "output",
#        fig_dir = "myfigs")


print("R script successfully ran from terminal.")

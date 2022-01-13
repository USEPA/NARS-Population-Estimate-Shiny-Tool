list.of.packages <- c("shiny","spsurvey","Hmisc","shinyjs","shinythemes",'shinybusy','shinyhelper',
                      'ggplot2', 'ggridges','stringr', 'shinyBS')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 
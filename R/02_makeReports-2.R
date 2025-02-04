#CREATE REPORTS FOR VINEYARDS
#WRITTEN BY SAMUEL ROBINSON, JAN 2024; USED ON 2018+2019 OBA DATA

library(rmarkdown)
library(knitr)

setwd("~/Projects/2024/OSU Vineyard Project/")

# #All csvs
# csvPaths <- dir('./data/vineyard records 2023/',pattern = '.csv',full.names = FALSE)

attach('./data/allVineyardData2024.Rdata')

# vineyardNames <- gsub('.csv','',gsub('_',' ',csvPaths))
vineyardNames <- names(vyNetworks)
detach('./data/allVineyardData2024.Rdata')

for(i in 1:length(vineyardNames)){
  render('vineyard-report-3.Rmd',
         params = list(set_title=vineyardNames[i],
                       data_path='./data/allVineyardData2024.Rdata'),
         intermediates_dir = './reports',
         output_dir = './reports',knit_root_dir = NULL,
         output_file = paste0(vineyardNames[i],'-report.pdf'),
         envir=new.env(),clean=TRUE
        )
}
## We fix period parameters.


lastupdateperiod <-lp
lastupdateperiod = substr(lastupdateperiod,5,6)


Year <-year 
Week <-weekormonth


  


## We run the others R files  

source("./Scripts/3Libraries_emprunteur.R")
source("./Scripts/4function for reporting_Generic_emprunteur.R") 
source("./Scripts/5Parameters_emprunteur.R") 

if (Report == "Assurland_Loan") {
  source(file = "./Scripts/6Data_process_Emprunteur.R")
} else if (Report == "Lesfurets_Loan") {
  source(file = "./Scripts/6Data_process_Emprunteur_LF.R")
}





## We fix time parameters.

moiactu=strftime(as.Date(Sys.Date(),format="%Y-%m-%d"),format="Y%yM%m")   
moiactu_1=pm(moiactu) 


weekactu=getWeekActu(Sys.Date(),3)



## We save New_Table, which is the cleaning new crawling.

New_Table$Segment <- "Global"
save(New_Table, file = "./Tables/New_Table_Assurland_emp.RData")






## Now we merge the database with the previous crawling.

#crawling_old <- read.csv(file="Crawling data/Assurland_Loan_prices_June.csv", header=TRUE, sep=";")





crawling_old <- crawling_old[!crawling_old$period %in% unique(as.character(New_Table$period)),!colnames(crawling_old) %in% c("type","id","formula","profilid")]


crawling_all <- rbind(crawling_old,New_Table[,colnames(crawling_old)])
crawling_all <- crawling_all[!duplicated(crawling_all[,c("profilID","period","yearmonth","insurer","coverage" )]),]





crawling_all$Segment <- "Global"
save(crawling_all, file = "./Tables/crawling_all.RData")
save(crawling_all, file = "./Tables/data_Assurland_emp.RData")
  
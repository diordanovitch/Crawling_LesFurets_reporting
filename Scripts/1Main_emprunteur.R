#####################################################################################################################################
#   TO PRODUCE THE 4 DESIRED RDATA TABLES, JUST LAUNCH THIS SCRIPT PRECISING HERE THE CRAWLING_FILES AND THE LAST PERIOD,            # 
#   IE THE PERIOD PREVIOUS THE NEW CRAWLING.                                                                                         #
#####################################################################################################################################



new_crawling_file = "./Crawling/lesfurets_prices_june18.csv"  # "./Crawling/lesfurets_prices_june18.csv"
old_crawling_file = "./Crawling/lesfurets_price_march18.csv"   # "./Crawling/lesfurets_price_march18.csv" 

lp = "Y18W10"    # lastperiod, from where we want to update, Y--W-- format.
weekormonth = 10 # to adapt according to lp (W)
year = 2018      # to adapt according to lp


Source = "MONTHLY"  # always for loan process

Report = "Lesfurets_Loan" # loan process, Assurland_Loan or Lesfurets_Loan



source("./Scripts/2updateTable_emprunteur.R")




source("./Scripts/7Cumul_Evolution_emprunteur.R")




source("./Scripts/8Price_Gap-Market_Intensity.R")

source("./Scripts/9MR_global_ranking_Generic_emprunteur.R")


source("./Scripts/10PCA_Crawling.R")

source("./Scripts/11Benchmark_Analysis.R")



source("./Scripts/12Modelisation_Insurers.R")

#####################################################################################################################################
# WHEN IT'S DONE, YOU HAVE THE RDATA TABLES YOU NEED FOR THE PRODUCTION DATABASE IN "./output_MR_all/Assurland_Loan" :              #
#                                                                                                                                   #
# - data_Assurland_emp.RData    ->    crawling_all variable                                                                         #
# - logevolfinal2_emp.RData    ->    logevolfinal2 variable                                                                         #
# - New_Table_Assurland_emp.RData    ->    New_Table variable                                                                       #
# - summaryTab_emp.RData    ->    SummaryTable variable                                                                             #
#####################################################################################################################################

# Dimitri IORDANOVITCH
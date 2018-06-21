## We import the 2 last crawling

crawling_new <- read.csv(file=new_crawling_file, header=TRUE, sep=";")
crawling_old <- read.csv(file=old_crawling_file, header=TRUE, sep=";")



## First small transformations


crawling_new = crawling_new[,c(1,2,4,5,6,7,14,15,16,8)]

crawling_new = subset(crawling_new, crawling_new[,3]!= "NO_INSURER")

crawling_new$fees <- gsub("[\r\n]", "", crawling_new$fees)

crawling_new$fees[as.character(crawling_new$fees)=='Gratuits'] <- 0

names(crawling_new)[names(crawling_new)=="prix"] <- "price"

names(crawling_new)[names(crawling_new)=="date_extraction"] <- "date_aspiration"



crawling_old = crawling_old[,c(1,2,4,5,6,7,14,15,16,8)]

crawling_old = subset(crawling_old, crawling_old[,3]!= "NO_INSURER")

crawling_old$fees <- gsub("[\r\n]", "", crawling_old$fees)

crawling_old$fees[as.character(crawling_old$fees)=='Gratuits'] <- 0

names(crawling_old)[names(crawling_old)=="prix"] <- "price"

names(crawling_old)[names(crawling_old)=="date_extraction"] <- "date_aspiration"




## We create a new column price+fees

crawling_new$price <- as.numeric(crawling_new$price)
crawling_new$fees <- as.numeric(crawling_new$fees)
crawling_new$priceYCfees <- crawling_new$price + crawling_new$fees


crawling_old$price <- as.numeric(crawling_old$price)
crawling_old$fees <- as.numeric(crawling_old$fees)
crawling_old$priceYCfees <- crawling_old$price + crawling_old$fees



## Re-arrangement of columns

crawling_new <- crawling_new[c(1,2,3,4,5,10,6,7,8,9,11)]

crawling_old <- crawling_old[c(1,2,3,4,5,10,6,7,8,9,11)]



## Define period and yearmonth

crawling_new$period <- paste( "Y",substr(crawling_new$year,3,4), "W",formatC(crawling_new$week,width=2, flag="0") , sep = "")
crawling_new$yearmonth <- paste( "Y",substr(crawling_new$year,3,4), "M",formatC(crawling_new$month,width=2, flag="0") , sep = "")
unique(crawling_new$period) <- ap #actual period

crawling_old$period <- paste( "Y",substr(crawling_old$year,3,4), "W",formatC(crawling_old$week,width=2, flag="0") , sep = "")
crawling_old$yearmonth <- paste( "Y",substr(crawling_old$year,3,4), "M",formatC(crawling_old$month,width=2, flag="0") , sep = "")



## Remove useless columns

crawling_new <- subset(crawling_new, select = -c(year,month,week,campaignID,fees))

crawling_new=crawling_new[!duplicated(crawling_new[c("profilID","insurer","coverage","period","yearmonth","price")]),]




crawling_old <- subset(crawling_old, select = -c(year,month,week,campaignID,fees))

crawling_old=crawling_old[!duplicated(crawling_old[c("profilID","insurer","coverage","period","yearmonth","price")]),]



### Define scopes of Insurers and covers

## Covers

levels(crawling_new$coverage) = list("Minimum" = "Minimum", "All" = "Formule Optimum")


crawling_new=crawling_new[crawling_new$coverage%in%covfr,]



levels(crawling_old$coverage) = list("Minimum" = "Minimum", "All" = "Formule Optimum")

crawling_old=crawling_old[crawling_old$coverage%in%covfr,]


## Scopes of insurer: can change according to AL report or LF report.
 

crawling_new <- crawling_new[crawling_new$insurer %in% All,]



New_Table <- crawling_new




crawling_old <- crawling_old[crawling_old$insurer %in% All,]
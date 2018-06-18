## We import the last crawling

crawling_new <- read.csv(file="./Crawling/ASSURLAND_LOAN_prices_March.csv", header=TRUE, sep=";")

crawling_new <- read.csv(file="./Crawling/lesfurets_prices.csv", header=TRUE, sep=";")



#

furets$coverage = ""

furets$coverage = ifelse (furets$authonomyloss == "YES" & furets$disability == "NO", "death+authonomy", furets$coverage)
furets$coverage = ifelse (furets$disability == "YES" & furets$tempincapacity == "NO", "d+a+disability", furets$coverage)
furets$coverage = ifelse (furets$tempincapacity == "YES", "d+a+d+temp_incapacity", furets$coverage)



## First small transformations


crawling_new = crawling_new[,-c(3,11,12,13,14,15,19)]


# crawling_new = subset(crawling_new, crawling_new[,3]!= "NO_INSURER")

# crawling_new$fees <- gsub("[\r\n]", "", crawling_new$fees)

# crawling_new$fees[as.character(crawling_new$fees)=='Gratuits'] <- 0

names(crawling_new)[names(crawling_new)=="prix"] <- "price"

names(crawling_new)[names(crawling_new)=="date_extraction"] <- "date_aspiration"



# ## We create a new column price+fees
# 
# crawling_new$price <- as.numeric(crawling_new$price)
# crawling_new$fees <- as.numeric(crawling_new$fees)
# crawling_new$priceYCfees <- crawling_new$price + crawling_new$fees



## Re-arrangement of columns

crawling_new <- crawling_new[c(1,2,3,4,5,10,6,7,8,9,11)]



## Define period and yearmonth

crawling_new$period <- paste( "Y",substr(crawling_new$year,3,4), "W",formatC(crawling_new$week,width=2, flag="0") , sep = "")
crawling_new$yearmonth <- paste( "Y",substr(crawling_new$year,3,4), "M",formatC(crawling_new$month,width=2, flag="0") , sep = "")



## Remove useless columns

crawling_new <- subset(crawling_new, select = -c(year,month,week,campaignID,fees))

crawling_new=crawling_new[!duplicated(crawling_new[c("profilID","insurer","coverage","period","yearmonth","price")]),]






### Define scopes of Insurers and covers

## Covers

crawling_new=crawling_new[crawling_new$coverage%in%covfr,]




## Scopes of insurer: can change according to AL report or LF report.


crawling_new <- crawling_new[crawling_new$insurer %in% All,]



New_Table <- crawling_new

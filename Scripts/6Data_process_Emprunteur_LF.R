## We import the 2 last crawlings

crawling_new <- read.csv(file=new_crawling_file, header=TRUE, sep=";")
crawling_old <- read.csv(file=old_crawling_file, header=TRUE, sep=";")


# We mapp a coverage column with 3 categories. 

crawling_new$coverage = ""

crawling_new$coverage = ifelse (crawling_new$authonomyloss == "YES" & crawling_new$disability == "NO", "Minimum", crawling_new$coverage)
crawling_new$coverage = ifelse (crawling_new$disability == "YES" & crawling_new$tempincapacity == "NO", "Medium", crawling_new$coverage)
crawling_new$coverage = ifelse (crawling_new$tempincapacity == "YES", "All", crawling_new$coverage)


crawling_old$coverage = ""

crawling_old$coverage = ifelse (crawling_old$authonomyloss == "YES" & crawling_old$disability == "NO", "Minimum", crawling_old$coverage)
crawling_old$coverage = ifelse (crawling_old$disability == "YES" & crawling_old$tempincapacity == "NO", "Medium", crawling_old$coverage)
crawling_old$coverage = ifelse (crawling_old$tempincapacity == "YES", "All", crawling_old$coverage)


## First small transformations


crawling_new = crawling_new[,-c(3,6,7,8,9,11,12,13,14,15,19)]

# crawling_new = subset(crawling_new, crawling_new[,3]!= "NO_INSURER")

# crawling_new$fees <- gsub("[\r\n]", "", crawling_new$fees)

# crawling_new$fees[as.character(crawling_new$fees)=='Gratuits'] <- 0

names(crawling_new)[names(crawling_new)=="prix"] <- "price"

names(crawling_new)[names(crawling_new)=="date_extraction"] <- "date_aspiration"



crawling_old = crawling_old[,-c(3,6,7,8,9,11,12,13,14,15,19)]

# crawling_old = subset(crawling_old, crawling_old[,3]!= "NO_INSURER")

# crawling_old$fees <- gsub("[\r\n]", "", crawling_old$fees)

# crawling_old$fees[as.character(crawling_old$fees)=='Gratuits'] <- 0

names(crawling_old)[names(crawling_old)=="prix"] <- "price"

names(crawling_old)[names(crawling_old)=="date_extraction"] <- "date_aspiration"





# ## We create a new column price+fees
# 
# crawling_new$price <- as.numeric(crawling_new$price)
# crawling_new$fees <- as.numeric(crawling_new$fees)
# crawling_new$priceYCfees <- crawling_new$price + crawling_new$fees



## Re-arrangement of columns

crawling_new <- crawling_new[c(1,2,3,9,4,5,6,7,8)]

crawling_old <- crawling_old[c(1,2,3,9,4,5,6,7,8)]



## Define period and yearmonth

crawling_new$period <- paste( "Y",substr(crawling_new$year,3,4), "W",formatC(crawling_new$week,width=2, flag="0") , sep = "")
crawling_new$yearmonth <- paste( "Y",substr(crawling_new$year,3,4), "M",formatC(crawling_new$month,width=2, flag="0") , sep = "")
ap <- unique(crawling_new$period)  #actual period


crawling_old$period <- paste( "Y",substr(crawling_old$year,3,4), "W",formatC(crawling_old$week,width=2, flag="0") , sep = "")
crawling_old$yearmonth <- paste( "Y",substr(crawling_old$year,3,4), "M",formatC(crawling_old$month,width=2, flag="0") , sep = "")


## Remove useless columns

crawling_new <- subset(crawling_new, select = -c(year,month,week,campaignID))

crawling_new=crawling_new[!duplicated(crawling_new[c("profilID","insurer","coverage","period","yearmonth","price")]),]




crawling_old <- subset(crawling_old, select = -c(year,month,week,campaignID))

crawling_old=crawling_old[!duplicated(crawling_old[c("profilID","insurer","coverage","period","yearmonth","price")]),]




### Define scopes of Insurers and covers

## Covers

crawling_new=crawling_new[crawling_new$coverage%in%covfr,]

crawling_old=crawling_old[crawling_old$coverage%in%covfr,]


## Scopes of insurer.

levels(crawling_new$insurer) = list("Groupe AXA" = "Groupe AXA", "Alptis" = "alptis", "April" = "april", "Cardif" = "bnp", 
                                    "Magnolia" = "magnolia", "Metlife" = "metlife", "Swisslife" = "swisslife")



crawling_new <- crawling_new[crawling_new$insurer %in% All,]



New_Table <- crawling_new




levels(crawling_old$insurer) = list("Groupe AXA" = "Groupe AXA", "Alptis" = "alptis", "April" = "april", "Cardif" = "bnp", 
                                    "Magnolia" = "magnolia", "Metlife" = "metlife", "Swisslife" = "swisslife")



crawling_old <- crawling_old[crawling_old$insurer %in% All,]

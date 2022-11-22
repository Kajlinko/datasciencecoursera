# Week 1 Quiz

x <- c(4, TRUE)
class(x)

x <- c(1, 3, 5)
y <- c(3, 2, 10)      
rbind(x,y)

x <- list(2, "a", "b", TRUE)
class(x[[2]])

x <- c(17, 14, 4, 5, 13, 12, 10)
x[x>=11] <- 4
x

dat <- read.csv("hw1_data.csv")
head(dat)
tail(dat)
dat[47,"Ozone"]
table(is.na(dat$Ozone))
mean(dat$Ozone)
mean(dat$Ozone[!is.na(dat$Ozone)])

dat2 <- dat[dat$Ozone > 31 & dat$Temp > 90, ]
mean(dat2$Solar.R[!is.na(dat2$Solar.R)])

dat3 <- dat[dat$Month == 6,]
mean(dat3$Temp[!is.na(dat3$Temp)])

dat4 <- dat[dat$Month == 5,]
max(dat4$Ozone[!is.na(dat4$Ozone)])

# Week 2 Quiz

cube <- function(x, n) {
  x^3
}
cube(3)

x <- 1:10
if(x > 5) {
  x <- 0
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}
z <- 10
f(3)

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")

source("complete.R")
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))

source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
summary(cr)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

## Getting and Cleaning Data

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv",
              destfile = "./3_DataCleaning/USCommSurvey.csv", method = "curl")

commSurvey <- read.table("./3_DataCleaning/USCommSurvey.csv", sep = ",", header = TRUE)

library(tidyr)
dat <- commSurvey[!is.na(commSurvey$VAL),]
nrow(dat[dat$VAL==24,])

install.packages("xlsx")
library(xlsx)

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx",
              destfile = "./3_DataCleaning/NGAP.xlsx", method = "curl")

dat <- read.xlsx("./3_DataCleaning/NGAP.xlsx", sheetIndex = 1, rowIndex = 18:23, 
          colIndex = 7:15)



if(!file.exists("./3_DataCleaning/baltRestaurants.xml")) {
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml" 
  download.file(fileURL, destfile = "./3_DataCleaning/baltRestaurants.xml")
}

doc <- xmlTreeParse("./3_DataCleaning/baltRestaurants.xml", useInternalNodes = TRUE)
rootNode = xmlRoot(doc)
print(rootNode)
View(doc)


if(!file.exists("./3_DataCleaning/USSurvey.csv")) {
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv" 
  download.file(fileURL, destfile = "./3_DataCleaning/USSurvey.csv")
}

DT <- as.data.table(read.table("./3_DataCleaning/USSurvey.csv", sep = ",", header = TRUE))


system.time(rowMeans(DT)[DT$SEX==1], rowMeans(DT)[DT$SEX==2])
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
system.time(DT[,mean(pwgtp15),by=SEX])
system.time(mean(DT$pwgtp15,by=DT$SEX))
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
system.time(mean(DT[DT$SEX==1,]$pwgtp15), mean(DT[DT$SEX==2,]$pwgtp15))

library(data.table)

## Data Cleaning Week 2

install.packages("httr")
library(httr)

myapp <- oauth_app("github",
                   key = "09d9324b57c2a5b38d0e",
                   secret = "219454bcbc6df2121e24a43cc8fc7a5aae2a4c25"
)

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
2

gtoken <- config(token = github_token)
req <- GET("https://api.github.com/repos/jtleek/datasharing", gtoken)
stop_for_status(req)
content(req)

url <- "https://api.github.com/users/jtleek/repos/datasharing"

acs <- read.csv("american_community_survey.csv")
install.packages("sqldf")
library(sqldf)


sqldf("select pwgtp1 from acs where AGEP < 50")
sqldf("select distinct AGEP from acs")

?readLines

url <- "http://biostat.jhsph.edu/~jleek/contact.html"
text <- readLines(url, n = 100)
for(i in c(10, 20, 30, 100)) {
  print(nchar(text[i]))
}

dat <- read.fwf("fixedwidth.for", skip = 4, 
         widths = c(12, 7, 4, 9, 4, 9, 4, 9, 4))
sum(dat[,4])

idaho <- read.csv("idaho_acs.csv")

agricultureLogical = (idaho$ACR == 3 & idaho$AGS == 6)
head(which(agricultureLogical == TRUE),3)

install.packages("jpeg")
library(jpeg)

jeff <- readJPEG("jeff.jpeg", native = TRUE)
quantile(jeff, 0.3)
quantile(jeff, 0.8)

library(dplyr)
edstats <- read.csv("EDSTATS_Country.csv")
gdp <- read.csv("GDP.csv")

sum(unique(edstats$CountryCode) %in% unique(gdp$X))

head(edstats)
colnames(gdp)

join <- left_join(edstats, gdp, by = c("CountryCode" = "X"))
colnames(join)
join <- arrange(join, desc(as.numeric(Gross.domestic.product.2012)))
join[13, "Long.Name"]
print(join$Gross.domestic.product.2012)

cc<- unique(edstats$CountryCode)[!is.na(unique(edstats$CountryCode))]
cd<- unique(gdp$X)[!is.na(unique(gdp$X))]

vect <- (match(cc,cd))
length(vect[!is.na(vect)])

length(intersect(edstats$CountryCode,gdp$X))

library(tidyr)
colnames(join)
unique(join$Income.Group)

join$Gross.domestic.product.2012 <- as.numeric(join$Gross.domestic.product.2012)
join <- drop_na(join, Gross.domestic.product.2012)
join %>% group_by(Income.Group) %>% summarise(Mean = mean(Gross.domestic.product.2012))
unique(join$Income.Group)
join <- mutate(join, factor(cut(join$Gross.domestic.product.2012, 5)))
unique(join$`factor(cut(join$Gross.domestic.product.2012, 5))`)
tab <- join %>% filter(`factor(cut(join$Gross.domestic.product.2012, 5))` == "(0.811,38.8]") 
colnames(tab)
tab %>% filter(Income.Group == "Lower middle income") %>% nrow()

join <- drop_na(join)
join <- join[((!apply(join == "", 1, all)) | (rowSums(is.na(join)))) != ncol(join),]

strsplit(colnames(idaho), "wgtp")[[123]]

data[rowSums(is.na(data)) != ncol(data),]

vect <- gdp2$X.3
vect <- as.numeric(gsub(",","",vect))
mean(vect, na.rm = T)/4

gdp2 <- read.csv("gdp2.csv")
unique(gdp2$X.2)
countryNames <- unique(gdp2$X.2)
head(gdp2)
grep("^United",countryNames)
print(countryNames[c(3,8,34)])

colnames(join)
lapply(join, unique, simplify = FALSE)

notes <- join$Special.Notes
fisc <- grep("[Ff]iscal", notes)
vect <- grep("[Jj]une", fisc)
vect2 <- fisc[vect]
join[vect2,"Long.Name"]

install.packages("quantmod")
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

library(lubridate)
?year

year <- year(sampleTimes)
vect <- grep(2012, year)
days <- sampleTimes[vect]
length(days[(wday(days, label = T) == "Mon")])


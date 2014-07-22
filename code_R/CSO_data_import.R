#---------------------------------------------------------------------
# Download data from CSO
# Uses function written specifically to download CSO.ie datasets
#
#  function is: getCSO(cso.rss.link)
#    where cso.rss.link is the .xml link you get by clicking the "RSS" 
#         at the top of the StatBank page after selecting your sector
#         and theme on the left banner. Copy and paste the address. 
#
# e.g. Try feeding the following into the function 
# "http://www.cso.ie/px/pxeirestat/Database/eirestat/Livestock%20and%20Farm%20Numbers/RSSLivestock%20and%20Farm%20Numbers.xml"
#---------------------------------------------------------------------

# Record start location, then move to directory containing scripts
startdir <- getwd()
scriptdir <- "D://Data//data_CSO//code_R/"
setwd(scriptdir)


# Make sure the functions are defined
source("functions//convertpxR.R")
source("functions//getCSO.R")



#---------------------------------------------------------------------
# Now define args and call getCSO for each RSS link you want
#  Note: Each RSS link will have multiple series(datasets) each of 
#        which will be downloaded and saved.
#---------------------------------------------------------------------
# root of data.folder's filepath, ensure it exists
origdata <- "D://Data/data_CSO/OrigData/"
setwd(origdata)
try(dir.create("BusinessSectors", showWarnings=F),silent=T) 
try(dir.create("BusinessSectors/AgricultureandFishing", showWarnings=F),silent=T) 
origdata <- "D://Data/data_CSO/OrigData/BusinessSectors/AgricultureandFishing/"
setwd(scriptdir) # go back to script directory


cso.theme    <- "LivestockandFarmNumbers"
cso.rss.link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/Livestock%20and%20Farm%20Numbers/RSSLivestock%20and%20Farm%20Numbers.xml"
data.folder  <- paste(origdata, cso.theme, sep="")
# Ensure folder exists (quietly)
try(dir.create(data.folder, showWarnings=F),silent=T) 
getCSO(cso.rss.link, data.folder)


cso.theme    <- "SupplyBalances"
cso.rss.link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/Supply%20Balances/RSSSupply%20Balances.xml"
data.folder  <- paste(origdata, cso.theme, sep="")
# Ensure folder exists (quietly)
try(dir.create(data.folder, showWarnings=F),silent=T) 
getCSO(cso.rss.link, data.folder)


cso.theme    <- "LivestockSlaughterings" 
cso.rss.link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/Livestock%20Slaughterings/RSSLivestock%20Slaughterings.xml"
data.folder  <- paste(origdata, cso.theme, sep="")
# Ensure folder exists (quietly)
try(dir.create(data.folder, showWarnings=F),silent=T) 
getCSO(cso.rss.link, data.folder)


cso.theme    <- "RegionalAccounts"
cso.rss.link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/Regional%20Accounts/RSSRegional%20Accounts.xml"
data.folder  <- paste(origdata, cso.theme, sep="")
# Ensure folder exists (quietly)
try(dir.create(data.folder, showWarnings=F),silent=T) 
getCSO(cso.rss.link, data.folder)


cso.theme    <- "AgriculturalOutputInputandIncome"
cso.rss.link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/Agricultural%20Output%20Input%20and%20Income/RSSAgricultural%20Output%20Input%20and%20Income.xml"
data.folder  <- paste(origdata, cso.theme, sep="")
# Ensure folder exists (quietly)
try(dir.create(data.folder, showWarnings=F),silent=T) 
getCSO(cso.rss.link, data.folder)


cso.theme    <- "AgriculturalInputandOutputPriceIndices"
cso.rss.link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/Agricultural%20Input%20and%20Output%20Price%20Indices/RSSAgricultural%20Input%20and%20Output%20Price%20Indices.xml"
data.folder  <- paste(origdata, cso.theme, sep="")
# Ensure folder exists (quietly)
try(dir.create(data.folder, showWarnings=F),silent=T) 
getCSO(cso.rss.link, data.folder)


cso.theme    <- "AgriculturalInputandOutputAbsolutePrices"
cso.rss.link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/Agricultural%20Input%20and%20Output%20Absolute%20Prices/RSSAgricultural%20Input%20and%20Output%20Absolute%20Prices.xml"
data.folder  <- paste(origdata, cso.theme, sep="")
# Ensure folder exists (quietly)
try(dir.create(data.folder, showWarnings=F),silent=T) 
getCSO(cso.rss.link, data.folder)


cso.theme    <- "MilkProduction"
cso.rss.link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/Milk%20Production/RSSMilk%20Production.xml"
data.folder  <- paste(origdata, cso.theme, sep="")
# Ensure folder exists (quietly)
try(dir.create(data.folder, showWarnings=F),silent=T) 
getCSO(cso.rss.link, data.folder)


cso.theme    <- "AgricultureAreaUsedandCropProduction"
cso.rss.link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/Agriculture%20Area%20Used%20and%20Crop%20Production/RSSAgriculture%20Area%20Used%20and%20Crop%20Production.xml"
data.folder  <- paste(origdata, cso.theme, sep="")
# Ensure folder exists (quietly)
try(dir.create(data.folder, showWarnings=F),silent=T) 
getCSO(cso.rss.link, data.folder)


cso.theme    <- "LandSales"
cso.rss.link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/Land%20Sales/RSSLand%20Sales.xml"
data.folder  <- paste(origdata, cso.theme, sep="")
# Ensure folder exists (quietly)
try(dir.create(data.folder, showWarnings=F),silent=T) 
getCSO(cso.rss.link, data.folder)


cso.theme    <- "AgricultureLabourandSizeofHoldings"
cso.rss.link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/Agriculture%20Labour%20and%20Size%20of%20Holdings/RSSAgriculture%20Labour%20and%20Size%20of%20Holdings.xml"
data.folder  <- paste(origdata, cso.theme, sep="")
# Ensure folder exists (quietly)
try(dir.create(data.folder, showWarnings=F),silent=T) 
getCSO(cso.rss.link, data.folder)


cso.theme    <- "FishingIndustryProductionandOtherCharacteristics"
cso.rss.link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/Fishing%20Industry%20Production%20and%20Other%20Characteristics/RSSFishing%20Industry%20Production%20and%20Other%20Characteristics.xml"
data.folder  <- paste(origdata, cso.theme, sep="")
# Ensure folder exists (quietly)
try(dir.create(data.folder, showWarnings=F),silent=T)
getCSO(cso.rss.link, data.folder)


cso.theme    <- "ExternalTradeinFishingIndustry"
cso.rss.link <- "http://www.cso.ie/px/pxeirestat/Database/eirestat/External%20Trade%20in%20Fishing%20Industry/RSSExternal%20Trade%20in%20Fishing%20Industry.xml"
data.folder  <- paste(origdata, cso.theme, sep="")
# Ensure folder exists (quietly)
try(dir.create(data.folder, showWarnings=F),silent=T) 
getCSO(cso.rss.link, data.folder)

#---------------------------------------------------------------------



# Bring R back to the starting location
setwd(startdir)

# clean up after yourself
 rm(list=c("cso.rss.link",
           "cso.theme",
           "data.folder",
           "scriptdir",
           "convert.pxR",
           "getCSO"
           )
     )
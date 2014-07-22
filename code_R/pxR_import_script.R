#---------------------------------------------------------------------
# Download data from CSO
# Try feeding the following into the function when you call it.
# "http://www.cso.ie/px/pxeirestat/Database/eirestat/Livestock%20and%20Farm%20Numbers/RSSLivestock%20and%20Farm%20Numbers.xml"
#---------------------------------------------------------------------


getCSO <- function(cso.rss.link){

#----------------------------------------
# XML package to read the RSS feed to get 
# the appropriate link urls
#----------------------------------------
library(XML)     # for xmlParse, xpathSApply
library(stringr) # for str_length
library(pxR)     # for read.px

# Save the URL of the xml file in a variable
xml.url <- cso.rss.link


# Parse XML structure from the RSS xml document (the xml.url)
doc <- xmlParse(xml.url)


# Can now access the links attribute under each "item" node
#  You're left with a list for which you can use an lapply
links   <- xpathSApply(doc,'//item/link',xmlValue)

links.vector <- as.vector(links)
#---------------------------------------------------------------------


#---------------------------------------------------------------------
# Convert to csv's using my convert function (wrapper for pxR)
#---------------------------------------------------------------------

# Quick function to extract the series code from the link url
get.code <- function(link) {
  
  start <- str_length(link) - 7
  stop  <- str_length(link) - 3
  code <- substr(link, start, stop)
  return(code)
}

# Now apply to the vector of links 
dataname <- lapply(links, function(x) get.code(x)) 



# Function to read .px file and write out .csv
convert.pxR <- function(link, dataname){
  origdata  <- "D:/Data/data_CSO/OrigData/"  
  data.pxR  <- as.data.frame(read.px(link))
  write.csv(data.pxR,
            paste(origdata,
                  dataname,
                  ".csv",
                  sep=""
                  )
            )
  
  return(print(paste(dataname,".csv written.",sep="")
               )
         )
  
}

# The above is a scalar function, but it's easily vecorized
convert.pxR <- Vectorize(convert.pxR) # will now work for all links


# Run the function to create the datasets
convert.pxR(links, dataname)

}
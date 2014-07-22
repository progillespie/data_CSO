# Bulk download CSO data using PC-Axis plugin for R. Writes out csv.
# Requires you to define url and dataname in your main script.

convert.pxR <- function(url, dataname){

data.pxR <- as.data.frame(read.px(url))
write.csv(data.pxR,paste(dataname,".csv"))

return(print(paste("Check that ",dataname,".csv ", "is in the ",
                   "current working directory.")
            )
      )


}

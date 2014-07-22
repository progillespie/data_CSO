# What happened around 1984? Uses CSO stats.






#====================================================================
# Head
#====================================================================


# Load required packages
library(data.table)


# Save starting location 
startdir <- getwd()

# Specify where original (unedited) data is
origdata <- "D://Data//data_CSO//OrigData//BusinessSectors//AgricultureandFishing///" 

# Set outdata on the basis of origdata filepath above
regex.match    <- regexpr("D:.+data_[[:alnum:]]+//", origdata)
datasource     <- regmatches(origdata, regex.match)
outdata        <- paste(datasource, "OutData//",sep="")
outdata.graphs <- paste(outdata,"graphs//",sep="")

# Create needed outdata subdirectories
suppressWarnings(
  try(dir.create(outdata.graphs), silent=T)
)

# Code runs from origdata
setwd(origdata)

# Run the next line to pull down the latest CSO data (back up first!)
#source("../../../code_R/CSO_data_import.R", echo=F)


# Print some messages
writeLines("\n\nChanged to data directory...")
print(getwd())
writeLines("\n\n...Script running...\n\n")



# Save default plot parameters, as function will alter these
orig.plot.par <- par(no.readonly=T)

# Make sure the graphics device has been turned off from last time
suppressWarnings(
  try(dev.off(), silent=T)
)

#-----------------------------------------------------------
# Define function to import, manipulate, and plot data
#-----------------------------------------------------------

plotCSO <- function(data.path, 
                    keycols, 
                    cat1,
                    cat2,
                    plot.title,
                    units,
                    stat,
                    lt.year)      {


  
  
  # Import the data
  data <- data.table(read.csv(data.path))
  data <- data[,always.true:=T]
  
  
  # Make sure that there is a year variable (for monthly series)
  suppressWarnings(
    try(data[,Year := as.numeric(substr(data$Month,1,4))] , silent=T)
  )
  
  # Subset to Total for Receiving.Benefits.or.Not if using worker data  
  suppressWarnings(
    try(setkey(data,"Receiving.Benefits.or.Not"), silent=T)
  )
  suppressWarnings(
    try(data <- copy(data["Total"]), silent=T)
  )
  
  
  # Set key variables (sorts and allows subsetting)
  setkeyv(data, keycols)
  
  

  
  # Make annual data if specified
  data_monthly <- copy(data)
  data <- copy(data_monthly[,mean(value), by=keycols])
  #colnames(data)[4] <- "value" # change back to correct colname  
  setnames(data,"V1","value")
  
  # Create indicator vars for selecting rows
  data[,lt95:= Year < lt.year]
  data[,of.interest := data[[keycols[1]]]==cat1 |
                       data[[keycols[1]]]==cat2   ]
  
  
  # Use those as keys
  altkeycols <- c("of.interest",keycols[2] ,"lt95")
  setkeyv(data, altkeycols)



  #  Plot both together (as a single series first to get scale of axes 
  #  correct, then put in our vertical line. Points should be same 
  #  color as background to make them invisible.
  
  
  time.var <- keycols[3]
  
  
  # Use WMF graphics driver for saving the plot as a .wmf file
  #  This is the best choice for inserting in Word docs, but there
  #  are plenty of other options
  #  for more info see: https://www.stat.berkeley.edu/classes/s133/saving.html
  

  graph.filename <- paste(outdata.graphs,plot.title,".wmf",sep="") 
  win.metafile(graph.filename,
               family="serif",
               width=5,
               height=3.5,
               res=120
               )
  
  
  # Alter default plotting parameters as desired
  par(family="serif", bty="l",lwd=1.75, xpd=T)
  
  
  # Scatterplot of the two series 
  plot(data[J(T,stat, T), c(time.var, "value"),with=F],
       col="white",
       ylab=units,
       main=plot.title) 
  data[J(T,stat, T), abline(v=1984, xpd=F)] 
  
  
  # Now draw the lines using two calls to points() 
  altkeycols <- c(keycols[1:2],"lt95") # have to reset keys first
  setkeyv(data, altkeycols)

  points(data[J(cat1,stat, T), c(time.var,"value"),with=F],
         type="l", 
         lty=1
         )
  points(data[J(cat2,stat, T), c(time.var,"value"),with=F],
       type="l", 
       lty=2
         )
  mtext("Source: CSO",
        side=1,
        adj=0,
        line=3.5,
        font=3,
        cex=0.75
        )
}

#-----------------------------------------------------------
# End of function definition
#-----------------------------------------------------------

#====================================================================
# End of Head
#====================================================================





#====================================================================
# Plotting
#====================================================================

# Options common to all plots
lt.year    <- 1000000 # Set ridiculously if you want all available


# Herd populations **** Definitively shift from Dairy to Beef head
#---------------------

# Set function arguments specific to this plot
data.path  <- "LivestockandFarmNumbers//AAA02.csv"
keycols    <- c("Type.of.Animal","always.true", "Year")
cat1       <- "Dairy cows"
cat2       <- "Other cows"
plot.title <- "Herd populations"
units      <- "1,000 head"
stat       <- T

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
  legend("bottomright",
         c(cat1,cat2),
         lty=c(1,2),
         lwd=c(1.75,1.75),
         bty="n"
         )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Slaughterings      ? Possible lead and lag (announcement lead to 
#                            cull, which ended when new size reached)
#---------------------

# Set function arguments specific to this plot
data.path  <- "LivestockSlaughterings//ADA01.csv"
keycols    <- c("Type.of.Animal","Statistic", "Year")
cat1       <-  "Cows"
cat2       <-  "Heifers"
plot.title <- "Slaughterings (number)"
units      <- "1,000 head"
stat       <- "Number of Animals Slaughtered (000 Head)"

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

# Add a legend and we're done
legend(1995,325,
       c(cat1,cat2),
       lty=c(1,2),
       lwd=c(1.75,1.75),
       bty="n"
       )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Cattle Prices-- Cows for slaughter  (absolute prices) 
#  XX - Nothing showing up here
#---------------------

# Set function arguments specific to this plot
data.path  <- "AgriculturalInputandOutputAbsolutePrices//AJA01.csv"
keycols    <- c("Type.of.Cattle", "Statistic", "Year")
cat1       <- "Cows for slaughter"
cat2       <- "Cows for slaughter"
plot.title <- "Cows for slaughter (absolute prices)"
units      <- "Euro per 100 Kg"
stat       <- "Cattle Price per 100 Kg (Euro)"

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
#   legend(,,
#          c(cat1,cat2),
#          lty=c(1,2),
#          lwd=c(1.75,1.75),
#          bty="n"
#          )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Cattle Prices-- Heifers (250 - 349 Kg)  (absolute prices)
#  XX - Nothing showing up here
#---------------------

# Set function arguments specific to this plot
data.path  <- "AgriculturalInputandOutputAbsolutePrices//AJA01.csv"
keycols    <- c("Type.of.Cattle", "Statistic", "Year")
cat1       <- "Heifers 250-299kg"
cat2       <- "Heifers 300-349kg"
plot.title <- "Heifers (absolute prices)"
units      <- "Euro per 100 Kg"
stat       <- "Cattle Price per 100 Kg (Euro)"

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
  legend(1990,260,
         c(cat1,cat2),
         lty=c(1,2),
         lwd=c(1.75,1.75),
         bty="n"
         )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Cattle Prices-- Bullocks (400 - 499 Kg)  (absolute prices)
#  XX - Nothing showing up here (slight dip in price only)
#---------------------

# Set function arguments specific to this plot
data.path  <- "AgriculturalInputandOutputAbsolutePrices//AJA01.csv"
keycols    <- c("Type.of.Cattle", "Statistic", "Year")
cat1       <- "Bullocks 400-449kg"
cat2       <- "Bullocks 450-499kg"
plot.title <- "Bullocks (absolute prices)"
units      <- "Euro per 100 Kg"
stat       <- "Cattle Price per 100 Kg (Euro)"

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
  legend(1990,140,
         c(cat1,cat2),
         lty=c(1,2),
         lwd=c(1.75,1.75),
         bty="n"
         )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Dairy feed (nuts and cubes) (absolute prices)
#   **** - Definitive drop in price of feed (slack demand)
#---------------------

# Set function arguments specific to this plot
data.path  <- "AgriculturalInputandOutputAbsolutePrices//AJA04.csv"
keycols    <- c("Type.of.Feedstuff","always.true","Year")
cat1       <- "Dairy nuts and cubes (13-15% protein)"
cat2       <- "Dairy nuts and cubes (16-18% protein)"
plot.title <- "Dairy feed (nuts and cubes) (absolute prices)"
units      <- "Euro per tonne"
stat       <- T

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Shorten category names for legend
  cat1 <- gsub("Dairy nuts and cubes ", "", cat1)
  cat2 <- gsub("Dairy nuts and cubes ", "", cat2)

  # Add a legend and we're done
  legend("top",
         c(cat1,cat2),
         lty=c(1,2),
         lwd=c(1.75,1.75),
         bty="n"
         )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Fertiliser (absolute prices)
#  ? - Possible lagged effect (initial shift from feed, followed by 
#                  reduced forage demand due to reduced numbers)
#---------------------

# Set function arguments specific to this plot
data.path  <- "AgriculturalInputandOutputAbsolutePrices//AJA05.csv"
keycols    <- c("Type.of.Fertiliser","always.true","Year")
cat1       <- "Calcium Ammonium Nitrate (27.5% N)"  
cat2       <- "Compound 10-10-20"
plot.title <- "Fertiliser (absolute prices)"
units      <- "Euro per tonne"
stat       <- T

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
  legend(1983.5,475,
         c(cat1,cat2),
         lty=c(1,2),
         lwd=c(1.75,1.75),
         bty="n"
         )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Manufacturing milk prices
#  XX - Nothing showing up here. Prices up, but following trend
#---------------------

# Set function arguments specific to this plot
data.path  <- "AgriculturalInputandOutputAbsolutePrices//AJA06.csv"
keycols    <- c("Product","always.true","Year")
cat1       <- "Milk (per litre), actual butterfat"
cat2       <- "Milk (per litre), 3.7% butterfat"
plot.title <- "Manufacturing Milk (absolute prices)"
units      <- "Euro per litre"
stat       <- T

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
  legend(1987,0.22,
         c(cat1,cat2),
         lty=c(1,2),
         lwd=c(1.75,1.75),
         bty="n"
         )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Fat Content of Creamery Milk
#  ? - Spike in Fat Content. Possibly response to litre based quota.
#---------------------

# Set function arguments specific to this plot
data.path  <- "MilkProduction//AKM01.csv" 
keycols    <- c("Statistic","Domestic.or.Import.Source","Year")
cat1       <- "Fat Content (%)"
cat2       <- "Fat Content (%)"
plot.title <- "Fat Content of Creamery Milk"
units      <- "%"
stat       <- "Domestic"

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
#   legend("topleft",
#          c(cat1,cat2),
#          lty=c(1,2),
#          lwd=c(1.75,1.75),
#          bty="n"
#          )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Protein Content of Creamery Milk
#  **** - Definite change in Protein content (reduced feed suppl.?)
#---------------------

# Set function arguments specific to this plot
data.path  <- "MilkProduction//AKM01.csv" 
keycols    <- c("Statistic","Domestic.or.Import.Source","Year")
cat1       <- "Protein Content (%)"
cat2       <- "Protein Content (%)"
plot.title <- "Protein Content of Creamery Milk"
units      <- "%"
stat       <- "Domestic"

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
#   legend("topleft",
#          c(cat1,cat2),
#          lty=c(1,2),
#          lwd=c(1.75,1.75),
#          bty="n"
#          )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Intake of Cows Milk by Creameries and Pasteurisers (Million Litres)
#  **** - Definite flattening of Creamery Milk intake (albeit with a lag)
#---------------------

# Set function arguments specific to this plot
data.path  <- "MilkProduction//AKM01.csv" 
keycols    <- c("Statistic","Domestic.or.Import.Source","Year")
cat1       <- "Intake of Cows Milk by Creameries and Pasteurisers (Million Litres)"
cat2       <- "Intake of Cows Milk by Creameries and Pasteurisers (Million Litres)"
plot.title <- "Creamery Milk Intake"
units      <- "Million Litres"
stat       <- "Domestic"

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
#   legend("topleft",
#          c(cat1,cat2),
#          lty=c(1,2),
#          lwd=c(1.75,1.75),
#          bty="n"
#          )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Butter and Cheese production
#  **** - Butter flattened, cheese did not.
#---------------------

# Set function arguments specific to this plot
data.path  <- "MilkProduction//AKM03.csv" 
keycols    <- c("Product","always.true","Year")
cat1       <- "Butter"
cat2       <- "Cheese"
plot.title <- "Butter and Cheese production"
units      <- "1000 tonnes"
stat       <- T

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
  legend("bottomright",
         c(cat1,cat2),
         lty=c(1,2),
         lwd=c(1.75,1.75),
         bty="n"
         )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Skim Milk Powder Production
#  **** - Substantial fall immediately, then recovery, then falling trend.
#---------------------


# Set function arguments specific to this plot
data.path  <- "MilkProduction//AKM03.csv" 
keycols    <- c("Product","always.true","Year")
cat1       <- "Skimmed Milk Powder"
cat2       <- "Skimmed Milk Powder"
plot.title <- "Skimmed Milk Powder production"
units      <- "1000 tonnes"
stat       <- T

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
#   legend("topleft",
#          c(cat1,cat2),
#          lty=c(1,2),
#          lwd=c(1.75,1.75),
#          bty="n"
#          )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Liquid milk
#  **** - Liquid skim begins from 1984. Increases ever since.
#---------------------

# Set function arguments specific to this plot
data.path  <- "MilkProduction//AKM02.csv" 
keycols    <- c("Type.of.Milk","always.true","Year")
cat1       <- "All Milk"
cat2       <- "Whole Milk"
plot.title <- "Liquid Milk production"
units      <- "Million Litres"
stat       <- T

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
  legend("bottom",
         c(cat1,cat2),
         lty=c(1,2),
         lwd=c(1.75,1.75),
         bty="n"
         )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Hay and silage area
#  XX - Nothing here. Series incomplete too.
#---------------------

# Set function arguments specific to this plot
data.path  <- "AgricultureAreaUsedandCropProduction//AQA02.csv"
keycols    <- c("Type.of.Land.Use", "Region", "Year")
cat1       <- "Hay"
cat2       <- "Grass silage"
plot.title <- "Hay and silage area"
units      <- "1000 hectares"
stat       <- "State"

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
  legend("bottomleft",
         c(cat1,cat2),
         lty=c(1,2),
         lwd=c(1.75,1.75),
         bty="n"
         )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Pasture and Rough forage
#  XX - Nothing here. Series incomplete too.
#---------------------

# Set function arguments specific to this plot
data.path  <- "AgricultureAreaUsedandCropProduction//AQA02.csv"
keycols    <- c("Type.of.Land.Use", "Region", "Year")
cat1       <- "Pasture"
cat2       <- "Rough grazing in use"
plot.title <- "Pasture and Rough forage area"
units      <- "1000 hectares"
stat       <- "State"

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
  legend("bottomleft",
         c(cat1,cat2),
         lty=c(1,2),
         lwd=c(1.75,1.75),
         bty="n"
         )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Workers by type (weekly hours)
#  **** - Definite fall in hours for Livestock and Livestock special.
#---------------------

# Set function arguments specific to this plot
data.path  <- "AgricultureLabourandSizeofHoldings//ASKL1.csv"
keycols    <- c("Type.of.Work", "Statistic", "Year")
cat1       <- "All Livestock Workers"
cat2       <- "Livestock Specialised Workers"
plot.title <- "Worker hours by Worker Type"
units      <- "Weekly hours"
stat       <- "Average Number of Paid Hours per week for Permanent Agricultural Workers (Number)"


# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Shorten category names for legend
  cat1 <- gsub("Workers", "", cat1)
  cat2 <- gsub("Workers", "", cat2)

  # Add a legend and we're done
  legend("topright",
         c(cat1,cat2),
         lty=c(1,2),
         lwd=c(1.75,1.75),
         bty="n"
         )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



#  Workers by Age (weekly hours)
#   XX - Nothing showing up here. 
#---------------------

# Set function arguments specific to this plot
data.path  <- "AgricultureLabourandSizeofHoldings//ASKL2.csv"
keycols    <- c("Age.Group", "Statistic", "Year")
cat1       <- "From 30 to 44"
cat2       <- "55 and Over"
plot.title <- "Worker hours by Age Group"
units      <- "Weekly hours"
stat       <- "Average Number of Paid Hours per week for Permanent Agricultural Workers (Number)"

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
  legend("bottomleft",
         c(cat1,cat2),
         lty=c(1,2),
         lwd=c(1.75,1.75),
         bty="n"
         )
dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# Workers by Workforce (weekly hours)
#  ? - Workers on farms with larger workforce saw hours increase.
#---------------------

# Set function arguments specific to this plot
data.path  <- "AgricultureLabourandSizeofHoldings//ASKL3.csv"
keycols    <- c("Size.of.Farm.Workforce", "Statistic", "Year")
cat1       <- "1 to 2 Workers"
cat2       <- "10 or More Workers"
plot.title <- "Worker hours by Size of Workforce on Farm"
units      <- "Weekly hours"
stat       <- "Average Number of Paid Hours per week for Permanent Agricultural Workers (Number)"

# Stats available for chosen stats variable:
levels(paste("data$",keycols[2],sep="")) 

# Generate plot
plotCSO(data.path,
        keycols,
        cat1,
        cat2,
        plot.title,
        units,
        stat,
        lt.year
        )

  # Add a legend and we're done
  legend(1990, 45,
         c(cat1,cat2),
         lty=c(1,2),
         lwd=c(1.75,1.75),
         bty="n"
         )

dev.off()
writeLines(paste("...Created plot: \t",plot.title,sep=""))



# # 
# #---------------------
# 
# # Set function arguments specific to this plot
# data.path  <- 
# keycols    <- 
# cat1       <- 
# cat2       <- 
# plot.title <- 
# units      <- 
# stat       <- 
# 
# # Stats available for chosen stats variable:
# levels(paste("data$",keycols[2],sep="")) 
# 
# # Generate plot
# plotCSO(data.path,
#         keycols,
#         cat1,
#         cat2,
#         plot.title,
#         units,
#         stat,
#         lt.year
#         )
# 
#   # Add a legend and we're done
#   legend(,,
#          c(cat1,cat2),
#          lty=c(1,2),
#          lwd=c(1.75,1.75),
#          bty="n"
#          )
# dev.off()
# writeLines(paste("...Created plot: \t",plot.title,sep=""))

#====================================================================
# End of Plotting
#====================================================================





#====================================================================
# Clean up 
#====================================================================
#par(orig.plot.par) # If not writing to file, reset to defaults
setwd(startdir)
rm(list=c("cat1",
          "cat2",
          "data.path",
          "keycols",
          "orig.plot.par",
          "origdata",
          "plot.title",
          "startdir",
          "stat",
          "units",
          "lt.year",
          "plotCSO"
          )
   )
writeLines("\n\nScript complete. Returning you to starting directory...")
print(getwd())

#====================================================================
# End of Clean up -- End of File
#====================================================================
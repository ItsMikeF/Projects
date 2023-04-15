#write an excel file with the sap codes as the worksheet names
#code produced by ChatGPT

# Load the openxlsx package
library(tidyverse)
library(openxlsx)

#unused libraries
#library(readxl)
#library(glue)
#library(gdata)

#read the xlsx file into a dataframe
pails <- read.xlsx("./01_data/inventory/pails/pail codes.xlsx")

# Create a new workbook object
wb <- createWorkbook()

sap <- sort(unique(pails$PAILS),decreasing = F)

#add the SAP codes as worksheet names
for (i in 1:79) {
  #create a new worksheet with the sap code as the name
  addWorksheet(wb, sap[i])
  
  #read in the csv file with the same code as the file name
  #df <- read.xls(glue("./01_data/inventory/pails/sap_data/{sap[1]}.xls"))
  
  #write the data to the worksheet
  #writeData(wb, sheet = sap[i])
}

# Save the workbook to a file
saveWorkbook(wb, file = "./01_data/inventory/pail workbook.xlsx", overwrite = T)


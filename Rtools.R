# R tools
# Alex Looi

graphics.off();
rm(list=ls());
############################################################################
# using the library RCUrl
install.packages("RCurl")
library(RCurl)

web = "https://looixiv.github.io/Bio381/Fish_data.txt"

getData = function(web, delimiter){

  output = getURL(web) # gets the data
  X = read.table(textConnection(output), sep = delimiter, header = T, fill = T)
  # return the output
  return(X)
}

getData(web,delimiter="\t") # tab delimited

############################################################################
# read user input
read_dw = function(){
  w = readline("Enter Web Address: ") # input of the user URL (where to DL data)
  d = readline("Enter Delimiter: ")
  if(d == "\\t" || d == "\"\\t\"" || d == "tab") d = "\t"
  output = c(w,d)
  return(output) # return the web URL
}

inputs = read_dw() # getting user input
data = getData(inputs[1], inputs[2])

write.csv(data, "Test Data", row.names = F) # output the data to your directory


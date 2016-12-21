install.packages("jsonlite", dependencies=TRUE)
require(jsonlite)

# get metadata from API
url="http://dw.euro.who.int/api/v3/measures/"
rd <- readLines(url, warn="F") 
metadata_from_api <- fromJSON(rd, simplifyDataFrame = TRUE)
data <- data.frame()

download_measure <- function(indic_code) {

  index <- which(metadata_from_api$code == indic_code)  
  cat(paste("\n-================ indicator code: ", indic_code ," ================================================- ", sep=""))
  cat(paste("\n-================ indicator title: ", metadata_from_api$full_name[index] ," ===============- ", sep="")) 
  
  # get data from API
  url=paste("http://dw.euro.who.int/api/v3/measures/", indic_code, sep="")
  rd <- readLines(url, warn="F") 
  temp <- fromJSON(rd, simplifyDataFrame = TRUE)
  data <<- temp[["data"]]
  
  cat("\n Downloaded! \n") 
  
}

download_measure("HFA_1")
index <- which(metadata_from_api$code == "HFA_1")
assign(metadata_from_api$full_name[index], data)
array_indic_code <- c("HFA_1")

for(i in 832:850) {
  download_measure(paste("HFAMDB",i,sep="_"))
  index <- which(metadata_from_api$code == paste("HFAMDB",i,sep="_"))
  array_indic_code <- c(array_indic_code, paste("HFAMDB",i,sep="_"))
  assign(metadata_from_api$full_name[index], data)
}

metadata_from_api <- metadata_from_api[metadata_from_api$code %in% array_indic_code,]

rm(i)
rm(index)
rm(rd)
rm(url)
rm(download_measure)
rm(data)
rm(array_indic_code)
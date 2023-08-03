#
dataInputClass <- setClass("dataInputClass", 
         slots = list(blastDuration="numeric", blastCount="numeric", 
                      replicateNum="numeric", checkData="data.frame", 
                      inputDataFrame="data.frame", valid_input="logical",
                      mainData="data.frame",id ="character"))
         
setGeneric("printDataMessage", function(x) standardGeneric("printDataMessage"))
setMethod(f = "printDataMessage", 
                   signature="dataInputClass",
                   function(x){
                     if(x@valid_input){
                       val <- "Data added"
                     }else{
                       val <- ("Please check your data")
                     }
                     return(val)
                   }
         )
setGeneric("checkMetaDataValid", function(x) standardGeneric("checkMetaDataValid"))
setMethod(f = "checkMetaDataValid", 
          signature = "dataInputClass", 
          function(x) {
            x@valid_input <- T
            x@id = paste0(x@blastDuration, x@blastCount, x@replicateNum)
            if(x@id %in% x@checkData$id) {
              x@valid_input <- F
            } 
            if(is.na(x@blastCount)|| is.na(x@blastDuration) || is.na(x@replicateNum)) {
              x@valid_input <- F
            }
           return(x) 
          })

setGeneric("saveData", function(x) standardGeneric("saveData"))
setMethod(f = "saveData", 
          signature = "dataInputClass",
          function(x) {
            if(x@valid_input){
              x@mainData <- rbind(x@mainData, x@inputDataFrame)
              blastData <- x@mainData
              save(blastData, file = "tmp/mainData.Rda")
              #drive_upload(media = "tmp/mainData.Rda", path = "beads/data/mainData.Rda")
              
            }
            return(x)
          })
setGeneric("saveMetaData", function(x) standardGeneric("saveMetaData"))
setMethod(f = "saveMetaData", 
          signature = "dataInputClass",
          function(x) {
            if(x@valid_input){
              df <- data.frame(id = x@id)
              x@checkData <- rbind(x@checkData, df)
              metaData <- x@checkData
              save(metaData, file = "tmp/metaData.Rda")
              #drive_upload(media = "tmp/metaData.Rda", path = "beads/data/metaData.Rda")
              
            }
            return(x)
          })

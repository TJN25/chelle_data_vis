allDataClass <- setClass("allDataClass", 
                           slots = list(blast_duration="numeric", blast_count="numeric", 
                                        run="numeric", blastData="data.frame"))







setGeneric("getExperimentVariables", function(x) standardGeneric("getExperimentVariables"))
setMethod(f = "getExperimentVariables", 
          signature="allDataClass",
          function(x){
            x@blast_duration <-  sort(unique(x@blastData$blast_duration))
            x@blast_count <-  sort(unique(x@blastData$blast_count))
            x@run <-  sort(unique(x@blastData$run))
            return(x)
          }
)

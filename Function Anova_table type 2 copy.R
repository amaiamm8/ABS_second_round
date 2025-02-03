# Advanced statistics CP 5 thru 12
# This function extends the anova table that is produced
# by Anova() from the car library
# It is set to produce type II sums of squares.
# argument model: an object as produced by the lm() function

library(car)
library(tibble)
Anova_table <- function(model) {
  
  #get the TypeII Anova table 
  At2<-Anova(model, Type =2)
  
  #construct SStot and dftot from a TypeI Anova table
  At1<-anova(model)
  SStotal <- sum(At1[,2])
  dftotal <- sum(At1[,1])
  Total<-as.data.frame(cbind(SStotal, dftotal,"",""))
  rownames(Total)<-"Total"
  names(Total) <- names(At2)
  
  Avtable <- rbind(At2,Total)  
  
  for (c in 1:4) {
    Avtable[,c]<-as.numeric(Avtable[,c])
  }  
  rows<-dim(Avtable)[1] 
  MS<-vector(length=rows)
  MS<- as.numeric(Avtable[,1])/as.numeric(Avtable[,2])
  
  Avtable <- add_column(Avtable,MS,.after=2)
  
  return(Avtable) 
}


#~~~~~~Notes Start~~~~~~#
# Make sure pricing Convertable and Detachables are 10% higher than their counterparts
#~~~~~~Notes End~~~~~~#


#~~~~~~Test Values Start~~~~~~#
# df <- Combined_Shipment_ASP
# percentage_more_expensive <- 1.1 #10% more expensive
# upper_limit <- 1500
# lower_limit <- 200
# ceiling <- 150
# floor <- 50
#~~~~~~Test Values End~~~~~~#
  


  
Pricing.Adjustment <- function(df, percentage_more_expensive, lower_limit, upper_limit,floor, ceiling){

  
  name_flag <- 0 
  if(!is.na(match("Form.Factor", names(df)))){
    names(df)[match("Form.Factor", names(df))] <- "Form.factor"
    name_flag <- 1
  }

  df_slate <- df[df$Form.factor=="Slate" | df$Form.factor=="Clamshell",]
   
  df_slate <- df_slate[,-grep("Units",names(df_slate))]
  df_slate <- df_slate[,-grep("Revenue",names(df_slate))]
  #df_slate <- df_slate[,-match("ASP",names(df_slate))]
  df_slate <- df_slate[,-match("Form.factor",names(df_slate))]
  

  # temp <- df[,-grep("Units",names(df))]
  # temp <- temp[,-grep("Revenue",names(temp))]
  # temp <- temp[,-match("ASP",names(temp))]
  # temp <- temp[,-match("Form.factor",names(temp))]
  # temp <- unique(temp) 
  # temp <- left_join(temp, df_slate)
  # df_slate <- temp
  
  
  
  # df_slate$ASP[as.numeric(as.character(df_slate$ASP)) <= lower_limit] <- df_slate$ASP[as.numeric(as.character(df_slate$ASP)) <= lower_limit] + floor #add a floor on minimum more expensive
  # df_slate$ASP[as.numeric(as.character(df_slate$ASP))>upper_limit] <- df_slate$ASP[as.numeric(as.character(df_slate$ASP))>upper_limit] + ceiling #add a cap on maximum more expensive
  # df_slate$ASP[as.numeric(as.character(df_slate$ASP))>lower_limit & as.numeric(as.character(df_slate$ASP)) <= upper_limit] <- 
  #   df_slate$ASP[as.numeric(as.character(df_slate$ASP))>lower_limit & as.numeric(as.character(df_slate$ASP)) <= upper_limit] * percentage_more_expensive

  df_slate$Form.factor[df_slate$Product=="Tablet"] <- "Detachable"
  df_slate$Form.factor[df_slate$Product=="Notebook PC"] <- "Convertible"
  names(df_slate)[match("ASP", names(df_slate))] <- "Slate_ASP"
  df <- left_join(df, df_slate, by = intersect(names(df),names(df_slate)))
  
  x <- df[is.na(df$Slate_ASP),] # spec without both 2-in-1 and non-2-in-1 equiv
  y <- df[!is.na(df$Slate_ASP),]# spec with both 2-in-1 and non-2-in-1 equiv
  z <- y[y$OS=="iOS" | y$OS=="Mac",]
  y <- y[!y$OS=="iOS",]
  y <- y[!y$OS=="Mac",]
  
  
  y$ASP[y$ASP < percentage_more_expensive * y$Slate_ASP] <- 
    y$ASP[y$ASP < percentage_more_expensive * y$Slate_ASP] * percentage_more_expensive  #minimum guarantee
  
  y$ASP[ (y$ASP-y$Slate_ASP) < floor] <- 
    y$Slate_ASP[(y$ASP-y$Slate_ASP) < floor] + floor  #lower limit
  
  
  y$ASP[(y$ASP-y$Slate_ASP) >ceiling] <- 
    y$Slate_ASP[(y$ASP-y$Slate_ASP) >ceiling] + ceiling  #upper limit
  
  y <- rbind.data.frame(y, z)
  df <- rbind.data.frame(x,y)
  df <- df[,-match(c("Slate_ASP"),names(df))]
  
  
  if(name_flag==1){
    names(df)[match("Form.factor", names(df))] <- "Form.Factor"
  }
  return(df)
}
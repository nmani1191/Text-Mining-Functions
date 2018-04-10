forward_pattern_match <- function(input,lookup,content)
{
  # Assign the input data to df variable & add row number
  df1 <- input
  df1$SNo<-1:nrow(df1)
  
  # Pre-Process the Text Column Name of Input Data
  content <- as.character(content)
  
  # Extract Desc column & SNo column as seperate DataFrame
  df <- df1[,c("SNo",content)]
  
  # This step is to preprocess the input data
  df$modDesc <- df[,which(colnames(df)==content)]
  df$modDesc <- as.character(df$modDesc)
  Encoding(df$modDesc) <- "UTF-8"
  df$modDesc <- iconv(df$modDesc, "UTF-8", "ASCII", sub="")
  df$modDesc <-tolower(df$modDesc)
  df$modDesc <- textp(df$modDesc)
  
  # Add required additional column to Input Dataset
  df$MatchedPatterns<-0
  
  #Assign the SupervisedSet pattern to lookup.pattern variable & Pre-process it
  lookup.pattern <- lookup$Patterns
  lookup.pattern <- as.character(lookup.pattern)
  
  # Append Regex pattern to the search pattern from Supervised Set and store the output in list variable
  pattern_df <- lapply(lookup.pattern, function(x) {
    words <- strsplit(x," ")
    pat <- lapply(words, function(y) {pattern <- paste(".*?\\b",y,"\\b.*?",sep = "")})
  })
  
  # Initialise blank Dataframe
  final_df <- data.frame()
  
  # Actual code to search the word/words forward in a sentence and map respective supporting fileds
  tryCatch(
    { print(Sys.time())
      for(j in 1:length(pattern_df)) # To loop through each Pattern
      {
        #print(j)  
        df <- subset(df,df$MatchedPatterns==0) # Subsetting the dataframe which is not mapped
        
        pattern_length <- length(unlist(pattern_df[j]))
        check <- data.frame(rep(FALSE,nrow(df)),rep(FALSE,nrow(df)))
        names(check) <- c("Pattern_Present","Forward_Pattern")
        check$Pattern_Present[rowSums(sapply(unlist(pattern_df[j]), grepl, df$modDesc))==pattern_length] <- TRUE
        position_matrix <- sapply(unlist(strsplit(lookup.pattern[j]," ")[[1]]), regexpr, df$modDesc)
        
        if(pattern_length==1)
        {
          check$Forward_Pattern <- check$Pattern_Present
        } else if(pattern_length==2)
        {
          check$Forward_Pattern[position_matrix[,1] < position_matrix[,2]] <- TRUE
        } else if(pattern_length==3)
        {
          check$Forward_Pattern[position_matrix[,1] < position_matrix[,2] & position_matrix[,2] < position_matrix[,3]] <- TRUE
        } else if (pattern_length==4)
        {
          check$Forward_Pattern[(position_matrix[,1] < position_matrix[,2]) & (position_matrix[,2] < position_matrix[,3]) &
                                  (position_matrix[,3] < position_matrix[,4]) ] <- TRUE
        } else if (pattern_length==5)
        {
          check$Forward_Pattern[(position_matrix[,1] < position_matrix[,2]) & (position_matrix[,2] < position_matrix[,3]) &
                                  (position_matrix[,3] < position_matrix[,4]) & (position_matrix[,4] < position_matrix[,5]) ] <- TRUE
        } else if (pattern_length==6)
        {
          check$Forward_Pattern[(position_matrix[,1] < position_matrix[,2]) & (position_matrix[,2] < position_matrix[,3]) &
                                  (position_matrix[,3] < position_matrix[,4]) & (position_matrix[,4] < position_matrix[,5]) &
                                  (position_matrix[,5] < position_matrix[,6])] <- TRUE
        }
        
        check$match[check$Pattern_Present==TRUE & check$Forward_Pattern==TRUE] <- TRUE
        matched <- check$match
        
        df$MatchedPatterns[matched] <- as.character(lookup$Patterns[j])
        
        final_df <- rbind(final_df,df[df$MatchedPatterns!=0,])
      }
      print(Sys.time())  },
    warning = function(w) {print(paste("Excecution has some problem",w))},
    error = function(e) {print(paste("Excecution Failed",e))})
  
  # Merging the whole dataset
  df <- subset(df,df$MatchedPatterns==0)
  final_df <- rbind(final_df,df)
  
  last_df <- merge(df1,final_df,by=c("SNo",content))
  last_df<- last_df[order(last_df$SNo),]
  
  return(last_df)
}

random_pattern_match<- function(input,lookup,content)
{
  # Assign the input data to df variable & add row number
  df1 <- input
  df1$SNo<-1:nrow(df1)
  
  # Pre-Process the Text Column Name of Input Data
  content <- as.character(content)
  
  # Extract Desc column & SNo column as seperate DataFrame
  df <- df1[,c("SNo",content)]
  
  # This step is to preprocess the input data
  df$modDesc <- df[,which(colnames(df)==content)]
  df$modDesc <- as.character(df$modDesc)
  Encoding(df$modDesc) <- "UTF-8"
  df$modDesc <- iconv(df$modDesc, "UTF-8", "ASCII", sub="")
  df$modDesc <-tolower(df$modDesc)
  df$modDesc <- textp(df$modDesc)
  
  # Add required additional column to Input Dataset
  df$MatchedPatterns<-0
  
  #Assign the SupervisedSet pattern to lookup.pattern variable & Pre-process it
  lookup.pattern <- lookup$Patterns
  lookup.pattern <- as.character(lookup.pattern)
  
  # Append Regex pattern to the search pattern from Supervised Set and store the output in list variable
  pattern_df <- lapply(lookup.pattern, function(x) {
    words <- strsplit(x," ")
    pat <- lapply(words, function(y) {pattern <- paste(".*?\\b",y,"\\b.*?",sep = "")})
  })
  
  # Initialise blank Dataframe
  final_df <- data.frame()
  
  # Actual code to search the word/words anywhere in a sentence and map respective supporting fileds
  tryCatch(
    { print(Sys.time())
      for(j in 1:length(pattern_df)) # To loop through each Pattern
      {
        #print(j)  
        df <- subset(df,df$MatchedPatterns==0) # Subsetting the dataframe which is not mapped
        
        pattern_length <- length(unlist(pattern_df[j]))
        matched <- rep(FALSE,length(df))
        matched[rowSums(sapply(unlist(pattern_df[j]), grepl, df$modDesc))==pattern_length] <- TRUE
        
        df$MatchedPatterns[matched] <- as.character(lookup$Patterns[j])
        
        final_df <- rbind(final_df,df[df$MatchedPatterns!=0,])
      }
      print(Sys.time())  },
    warning = function(w) {print(paste("Excecution has some problem"))},
    error = function(e) {print(paste("Excecution Failed"))})
  
  # Merging the whole dataset
  df <- subset(df,df$MatchedPatterns==0)
  final_df <- rbind(final_df,df)
  
  last_df <- merge(df1,final_df,by=c("SNo",content))
  last_df<- last_df[order(last_df$SNo),]
  
  return(last_df)
}

textp <- function (FreeText)
{
  
  FreeText <- gsub("[[:punct:]]", "", FreeText) # Remove punctuation
  FreeText <- gsub("[[:blank:]]"," ", FreeText) # Remove blank space
  FreeText <- gsub("[[:space:]]"," ", FreeText) # Remove space
  FreeText <- tolower(FreeText)                 # convert to lower case
  FreeText <- gsub("\\s+", " ", FreeText)       # Convert to lower case
  
  FreeText <- gsub("[^ -z]", "", FreeText)      # Remove Numeric
  FreeText <- trimws(FreeText)                  # Remove white spaces
  return(FreeText) 
}

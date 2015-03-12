xse II: Pilot Study on Training           ##
## Classifier to Detect fuzzy matching         ##
## name pairs                                  ##
##                                             ##
## Author: Yi Zhang <yi.zhang@huntington.com   ##
## Date: Feb/19/2015                           ##
## ########################################### ##
library(dplyr)
library(ggplot2)
library(reshape2)

library(stringdist)
library(stringr)
library(caret)
library(ROCR)

library(xlsx)
library(rattle)
library(pmml)
## ########################################### ##
## ENVIRONMENT SETTING                         ##
## ########################################### ##
dir        <- list()
dir$root   <- getwd()
dir$data   <- paste(dir$root, "/data/",     sep = "")
dir$output <- paste(dir$root, "/output/",   sep = "")
dir$img    <- paste(dir$root, "/images/",   sep = "")

## ############################## #
## FUNCTION DEFINITION ------------
## ############################## #
getDataPath <- function(filename, dir = dir$data) {
  ## ************************************ ##
  ## Return full path of file             ##
  ## ************************************ ##
  res <- paste(dir, filename, sep="")
  return(res)
}

structName <- function(x.str, format = c('firstname', 'middlename', 'lastname'), str_process = FALSE ){
  # ********************************************** #
  # Structure name string by following             #
  # the formate defined after cleaning string      #
  #                                                #
  # Parameters:                                    #
  # -----------                                    #
  # x.str: (string)                                #
  # format: (vector), define the role of words     # 
  #          in string                             #
  #                                                #
  # Returns:                                       #
  # res: (named vector), c("firstname", "lastname" #
  #                       , "middlename")          #
  # ********************************************** #
  ## remove spaces
  x.str <- str_trim(string = x.str)
  if(str_process){
    ## retain alphabetic only characters
    x.str <- gsub(pattern = "[^[:alpha:] ]", replacement = "", x = x.str )
    ## capitalize all letters
    x.str <- toupper(x = x.str)
  }
  ## split to last, first
  x.str <- strsplit(x = x.str, split = " ")[[1]]
  res   <- c("firstname"=NA, "lastname"=NA, "middlename"=NA)
  if(length(x.str) > 2){
    if(length(format) > 0) res[format[1]] <- x.str[1]
    if(length(format) > 1) res[format[2]] <- x.str[2]
    if(length(format) > 2) res[format[3]] <- x.str[3]
  }else{
    res[]
  }
  
  return(res)
}

acc_name_struct <- function(x_str){
  # ************************************* #
  # Conver string into a named list       #
  # assuming x is consisted of            #
  # "firstname", "middlename", "lastname" #
  #                                       #
  # Parameter:                            # 
  # ---------                             #
  # x_str: {string} account ownername     # 
  #                                       #
  # Returns:                              #
  # --------                              # 
  # res: {vector-lie, list}               #
  # ************************************* #
  x_str     <- str_trim(string = x_str)
  has_comma <- grepl(pattern = ",", x_str)
  
  if(has_comma) x_str <- gsub(pattern = ",", replacement = " ", x = x_str)
  
  x_str     <- gsub(pattern = "[^[:alpha:] ]", replacement = "", x = x_str )
  x_str     <- toupper(x = x_str)
  x_str     <- strsplit(x = x_str, split = " ")[[1]]
  x_str     <- x_str[!(x_str == "")]
  res       <- c("firstname"=NA, "middlename"=NA, "lastname"=NA)
  if(has_comma){
    if(length(x_str) > 2){
      res["firstname"]  <- x_str[2]
      res["middlename"] <- x_str[3]
      res["lastname"]   <- x_str[1]
    }else{
      res["firstname"]  <- x_str[2]
      res["lastname"]   <- x_str[1]
    }
  }else{
    if(length(x_str) > 2){
      # *------- "FOR" in name string ------ * 
      if(x_str[length(x_str)] == "FOR"){
        if(length(x_str) > 2){
          res["firstname"]  <- x_str[1]
          res["lastname"]   <- x_str[2]
        }else{
          res["firstname"]  <- x_str[1]
          res["middlename"] <- x_str[2]
          res["lastname"]   <- x_str[3]
        }
      }else{
        res["firstname"]  <- x_str[1]
        res["middlename"] <- x_str[2]
        res["lastname"]   <- x_str[3]
      }
    }else{
      res["firstname"]  <- x_str[1]
      res["lastname"]   <- x_str[2]
    }
  }
  return(res)
}


clnt_name_struct <- function(x_str){
  # ************************************* #
  # Conver string into a named list       #
  # assuming x is consisted of            #
  # "firstname", "middlename", "lastname" #
  #                                       #
  # Parameter:                            # 
  # ---------                             #
  # x_str: {string} account ownername     # 
  #                                       #
  # Returns:                              #
  # --------                              # 
  # res: {vector-lie, list}               #
  # ************************************* #
  x_str <- str_trim(string = x_str)
  x_str <- gsub(pattern = "[^[:alpha:] ]", replacement = " ", x = x_str )
  x_str <- toupper(x = x_str)
  x_str <- strsplit(x = x_str, split = " ")[[1]]
  res   <- c("firstname"=NA, "middlename"=NA, "lastname"=NA)
  if(length(x_str) > 2){
    res["firstname"]  <- x_str[2]
    res["middlename"] <- x_str[3]
    res["lastname"]   <- x_str[1]
  }else{
    res["firstname"]  <- x_str[2]
    res["lastname"]   <- x_str[1]
  }
  return(res)
}

str_insertion <- function(str, pos, chars){
  # ************************************** #
  # Insert characters into string at       #
  # specified position                     #
  #                                        #
  # Paramsters:                            #
  # -----------                            #
  # str: {string}, string for injection    #
  # pos: {vector, integer}, positions      #
  # char: {vector, character}, characters  #
  #                                        #
  # Returns:                               #
  # -------                                #
  # res: {character}                       # 
  # ************************************** #
  in_str  <- str_split(string = str, pattern = "")[[1]][-1]
  out_str <- rep(NA, times = length(in_str) + length(pos))
  
  if( max(pos) > length(in_str) + 1 ) stop("Error: defined position out of meaningful range !")
  if( length(pos) != length(chars) )   stop("Error: positions (pos) is not compatible with characters (chars) !")
  
  counter_str <- 1
  counter_pos <- 1
  
  for(i in 1:length(out_str)){
    if( i %in% pos ){
      out_str[i]  <- chars[ counter_pos ]
      counter_pos <- counter_pos + 1
    }else{
      out_str[i]  <- in_str[ counter_str ]
      counter_str <- counter_str + 1
    }
  }
  
  res <- paste(out_str, collapse = "")
  return(res)
}

str_replacement <- function(str, pos, chars){
  # ************************************** #
  # Replace elements of string at position #
  # -s (pos) with characters (chars)       #
  #                                        #
  # Paramsters:                            #
  # -----------                            #
  # str: {string}, string for injection    #
  # pos: {vector, integer}, positions      #
  # char: {vector, character}, characters  #
  #                                        #
  # Returns:                               #
  # -------                                #
  # res: {character}                       # 
  # ************************************** #
  in_str  <- str_split(string = str, pattern = "")[[1]][-1]
  out_str <- in_str
  
  if( max(pos) > length(in_str) | min(pos) < 1 ) stop("Error: positions (pos) fall out of legitimate range !")
  if( length(pos) != length(chars) )   stop("Error: positions (pos) is not compatible with characters (chars) !")
  
  counter_pos <- 1
  for(i in pos){
    out_str[i]  <- chars[ counter_pos ]
    counter_pos <- counter_pos + 1 
  }
  
  res     <- paste(out_str, collapse = "")
  return(res)
}

str_deletion <- function(str, pos){
  # ********************************* #
  # Remove characters from string at  #
  # positions (pos)                   #
  #                                   #
  # Parameters:                       #
  # ----------                        #
  # str: {string}                     #
  # pos: {vector, integer} positions  #
  #                                   #
  # Return:                           #
  # ------                            #
  # res: {string}                     #
  # ********************************* #
  in_str  <- str_split(string = str, pattern = "")[[1]][-1]
  
  if( max(pos) > length(in_str) | min(pos) < 1 ) stop("Error: positions (pos) fall out of legitimate range !")
  
  out_str <- in_str[ -pos ]
  res     <- paste(out_str, collapse = "")
  return(res)
}

str_muter <- function(str, degree = 1, type = NA){
  # ************************************** #
  # Mute the string with random noise      #
  #                                        #
  # Parameters:                            #
  # ----------                             #
  # str: {string}, string for mutation     #
  # degress: {numeric, [0 - 1]}            #
  # type: {integer}, optional mutation     #
  #             type                       #
  #                                        #
  # Return:                                #
  # ------                                 #
  # out: {string}, muted string            #
  # ************************************** # 
  # str_muter
  # s1: string --> vector
  # s2: random mutation type: {"insertion": 1}, {"replace": 2}, {"deletion": 3} 
  # s3: random mutation parameter:  ["num_char", "position", ("characters")]
  # s4: apply changes 
  # s5: concatenate vector of characters into a single string
  if( degree > 1 | degree < 0 ) degree <- 1
  if( is.na(type) )             type   <- sample(x = c(1, 2, 3), size = 1)
  num_range <- ceiling(x = nchar(str) * degree)
  
  if( type == 1 ){
    # inerstion
    #num_char <- sample( 1:(num_range + 1), size = 1)
    num_char <- sample( 1:num_range , size = 1)
    pos      <- sort(sample(1:nchar(str), size = num_char, replace = FALSE))
    chars    <- sample(c(toupper(letters), "*", "+", "~", "+", "!"), size = length(pos), replace = TRUE)
    out      <- str_insertion(str = str, pos = pos, chars = chars)
  }else{
    num_char <- sample( 1:num_range, size = 1)
    pos      <- sort( sample(1:nchar(str), size = num_char, replace = FALSE) )
    if( type == 2 ){
      chars  <- sample(c(toupper(letters), "*", "+", "~", "+", "!"), size = length(pos), replace = TRUE)
      out    <- str_replacement(str = str, pos = pos, chars = chars)
    }else{
      out    <- str_deletion(str = str, pos = pos)
    }
  }
  return(out)
}

## ##################################### ##
## LOAD DATA                             ##
## ##################################### ##
trans_scored <- read.csv(file = getDataPath(filename = "SCORED_ACH_CREDIT_TRANS.csv", dir = dir$output)
                         , header = TRUE, stringsAsFactors = FALSE)
trans_scored <- select(trans_scored, NAME, CLNT_SHRT_NAME, FN, MN, LN, ALL_SAME
                       , FN_SAME, LN_SAME, dist_score) %>% 
                    filter(ALL_SAME == 1)

## #################### ##
## SAMPLE               ##
## #################### ##
set.seed(seed = 123456)
config           <- list()
config$samp_size <- 1000
sample_names     <- sample(trans_scored$CLNT_SHRT_NAME, config$samp_size)
sample_names     <- data.frame( t( sapply(sample_names, clnt_name_struct) ) )
## DROP MIDDLE NAME
sample_names    <- select(sample_names, -(middlename))


## ###################### ##
## CREATE SYNTHETIC DATA  ##
## ###################### ##
synth_name_db <- list()
## GROUP 01: EXACTLY SAME
synth_name_db$g1 <- apply(sample_names
                          , MARGIN = 1
                          , function(x){
                            true_fn  <- x[1]
                            true_ln  <- x[2]
                            synth_fn <- x[1]
                            synth_ln <- x[2]
                            label    <- 1
                            param    <- 0
                            compound_label <- paste(label, param, collapse = "_")
                            res <- c("true_fn" = true_fn, "true_ln" = true_ln
                                     , "synth_fn" = synth_fn, "synth_ln" = synth_ln
                                     , "label" = label, "param" = param, "compound_label" = compound_label)
                            return(res)
                          })
synth_name_db$g1 <- as.data.frame(t(synth_name_db$g1), stringsAsFactors = FALSE)

## GROUP 02-1: MUTE_PATIAL IN FIRST NAME (INERTION)
synth_name_db$g2_1 <- apply(sample_names
                           , MARGIN = 1
                           , function(x) {
                             label    <- 2
                             param    <- 1
                            
                             true_fn  <- x[1]
                             true_ln  <- x[2]
                            
                             synth_fn <- str_muter(str = x[1], type = param)
                             synth_ln <- x[2]
                           
                             compound_label <- paste(label, param, collapse = "_")
                             res <- c("true_fn" = true_fn, "true_ln" = true_ln
                                      , "synth_fn" = synth_fn, "synth_ln" = synth_ln
                                      , "label" = label, "param" = param, "compound_label" = compound_label)
                             return(res)
                           })

synth_name_db$g2_1 <- as.data.frame(t(synth_name_db$g2_1), stringsAsFactors = FALSE)
## GROUP 02-2: MUTE_PATIAL IN FIRST NAME (REPLACEMENT)
synth_name_db$g2_2 <- apply(sample_names
                            , MARGIN = 1
                            , function(x) {
                              label    <- 2
                              param    <- 2
                              
                              true_fn  <- x[1]
                              true_ln  <- x[2]
                              
                              synth_fn <- str_muter(str = x[1], type = param)
                              synth_ln <- x[2]
                              
                              compound_label <- paste(label, param, collapse = "_")
                              res <- c("true_fn" = true_fn, "true_ln" = true_ln
                                       , "synth_fn" = synth_fn, "synth_ln" = synth_ln
                                       , "label" = label, "param" = param, "compound_label" = compound_label)
                              return(res)
                            })

synth_name_db$g2_2 <- as.data.frame(t(synth_name_db$g2_2), stringsAsFactors = FALSE)
## GROUP 02-3: MUTE_PATIAL IN FIRST NAME (DELTION)
synth_name_db$g2_3 <- apply(sample_names
                            , MARGIN = 1
                            , function(x) {
                              label    <- 2
                              param    <- 3
                              
                              true_fn  <- x[1]
                              true_ln  <- x[2]
                              
                              synth_fn <- str_muter(str = x[1], type = param)
                              synth_ln <- x[2]
                              
                              compound_label <- paste(label, param, collapse = "_")
                              res <- c("true_fn" = true_fn, "true_ln" = true_ln
                                       , "synth_fn" = synth_fn, "synth_ln" = synth_ln
                                       , "label" = label, "param" = param, "compound_label" = compound_label)
                              return(res)
                            })

synth_name_db$g2_3 <- as.data.frame(t(synth_name_db$g2_3), stringsAsFactors = FALSE)


## GROUP 03-1: MUTE_PATIAL IN LAST NAME (INERTION)
synth_name_db$g3_1 <- apply(sample_names
                            , MARGIN = 1
                            , function(x) {
                              label    <- 3
                              param    <- 1
                              
                              true_fn  <- x[1]
                              true_ln  <- x[2]
                              
                              synth_fn <- x[1]
                              synth_ln <- str_muter(str = x[2], type = param)
                              
                              compound_label <- paste(label, param, collapse = "_")
                              res <- c("true_fn" = true_fn, "true_ln" = true_ln
                                       , "synth_fn" = synth_fn, "synth_ln" = synth_ln
                                       , "label" = label, "param" = param, "compound_label" = compound_label)
                              return(res)
                            })

synth_name_db$g3_1 <- as.data.frame(t(synth_name_db$g3_1), stringsAsFactors = FALSE)
## GROUP 03-2: MUTE_PATIAL IN LAST NAME (REPLACEMENT)
synth_name_db$g3_2 <- apply(sample_names
                            , MARGIN = 1
                            , function(x) {
                              label    <- 3
                              param    <- 2
                              
                              true_fn  <- x[1]
                              true_ln  <- x[2]
                              
                              synth_fn <- x[1]
                              synth_ln <- str_muter(str = x[2], type = param)
                              
                              compound_label <- paste(label, param, collapse = "_")
                              res <- c("true_fn" = true_fn, "true_ln" = true_ln
                                       , "synth_fn" = synth_fn, "synth_ln" = synth_ln
                                       , "label" = label, "param" = param, "compound_label" = compound_label)
                              return(res)
                            })

synth_name_db$g3_2 <- as.data.frame(t(synth_name_db$g3_2), stringsAsFactors = FALSE)
## GROUP 03-3: MUTE_PATIAL IN LAST NAME (DELTION)
synth_name_db$g3_3 <- apply(sample_names
                            , MARGIN = 1
                            , function(x) {
                              label    <- 3
                              param    <- 3
                              
                              true_fn  <- x[1]
                              true_ln  <- x[2]
                              
                              synth_fn <- x[1]
                              synth_ln <- str_muter(str = x[2], type = param)
                              
                              compound_label <- paste(label, param, collapse = "_")
                              res <- c("true_fn" = true_fn, "true_ln" = true_ln
                                       , "synth_fn" = synth_fn, "synth_ln" = synth_ln
                                       , "label" = label, "param" = param, "compound_label" = compound_label)
                              return(res)
                            })

synth_name_db$g3_3 <- as.data.frame(t(synth_name_db$g3_3), stringsAsFactors = FALSE)

## GROUP03: MUTATE BOTH FIRST NAME AND LAST NAME
synth_name_db$g4 <- apply(sample_names
                          , MARGIN = 1
                          , function(x) {
                            label    <- 4
                            param    <- NA
                            
                            true_fn  <- x[1]
                            true_ln  <- x[2]
                            
                            synth_fn <- str_muter(str = x[1], type = param)
                            synth_ln <- str_muter(str = x[2], type = param)
                            
                            compound_label <- paste(label, param, collapse = "_")
                            res <- c("true_fn" = true_fn, "true_ln" = true_ln
                                     , "synth_fn" = synth_fn, "synth_ln" = synth_ln
                                     , "label" = label, "param" = param, "compound_label" = compound_label)
                            return(res)
                          })
synth_name_db$g4 <- as.data.frame(t(synth_name_db$g4), stringsAsFactors = FALSE)

## GROUP04: SHUFFLE THE FIRSTNAME AND LAST NAME TO CONSTRUCT DIFFERENT NAME
synth_name_db$g5               <- data.frame(matrix(NA, nrow = nrow(sample_names), ncol = ncol(synth_name_db$g1)))
colnames(synth_name_db$g5)      <- c("true_fn", "true_ln", "synth_fn", "synth_ln", "label", "param", "compound_label")
synth_name_db$g5[, 1:2]         <- sample_names
synth_name_db$g5$synth_fn       <- sample_names$firstname[sample(x = 1:length(sample_names$firstname), size = nrow(synth_name_db$g5), replace = TRUE)]
synth_name_db$g5$synth_ln       <- sample_names$lastname[sample(x = 1:length(sample_names$firstname), size = nrow(synth_name_db$g5), replace = TRUE)]
synth_name_db$g5$label          <- 5
synth_name_db$g5$compound_label <- 5
## RENAME
colnames(synth_name_db$g1)   <- c("true_fn", "true_ln", "synth_fn", "synth_ln", "label", "param", "compound_label")
colnames(synth_name_db$g2_1) <- c("true_fn", "true_ln", "synth_fn", "synth_ln", "label", "param", "compound_label")
colnames(synth_name_db$g2_2) <- c("true_fn", "true_ln", "synth_fn", "synth_ln", "label", "param", "compound_label")
colnames(synth_name_db$g2_3) <- c("true_fn", "true_ln", "synth_fn", "synth_ln", "label", "param", "compound_label")
colnames(synth_name_db$g3_1) <- c("true_fn", "true_ln", "synth_fn", "synth_ln", "label", "param", "compound_label")
colnames(synth_name_db$g3_2) <- c("true_fn", "true_ln", "synth_fn", "synth_ln", "label", "param", "compound_label")
colnames(synth_name_db$g3_3) <- c("true_fn", "true_ln", "synth_fn", "synth_ln", "label", "param", "compound_label")
colnames(synth_name_db$g4)   <- c("true_fn", "true_ln", "synth_fn", "synth_ln", "label", "param", "compound_label")
colnames(synth_name_db$g5)   <- c("true_fn", "true_ln", "synth_fn", "synth_ln", "label", "param", "compound_label")
## RECODE LABEL
synth_name_db$g1$compound_label   <- 1
synth_name_db$g2_1$compound_label <- 2
synth_name_db$g2_2$compound_label <- 3
synth_name_db$g2_3$compound_label <- 4
synth_name_db$g3_1$compound_label <- 5
synth_name_db$g3_2$compound_label <- 6
synth_name_db$g3_3$compound_label <- 7
synth_name_db$g4$compound_label   <- 8
synth_name_db$g5$compound_label   <- 9

## Consolidate all rows
synth_name_db$all <- as.data.frame(rbind(synth_name_db$g1, synth_name_db$g2_1, synth_name_db$g2_2, synth_name_db$g2_3
                                         , synth_name_db$g3_1, synth_name_db$g3_2, synth_name_db$g3_3, synth_name_db$g4
                                         , synth_name_db$g5)
                                   , stringsAsFactors = FALSE)

write.table(x = synth_name_db$all, file = getDataPath("SYNTH_DATA.csv", dir = dir$data)
            , col.names = TRUE, row.names = FALSE, sep = ",")

## ######################## ##
## CLASSIFIER TRAINING      ##
## ######################## ##
## CALCULATE FIRST_NAME distance metrics
#proc_df          <- data.frame()
#proc_df[, c("")] <- apply()

feature_extractor <- function(s1, s2, prefix = NA){
  ## **************************** ##
  ## Feature Extractor (s1, s2)   ##
  ## **************************** ##
  lv  <- stringdist(a = s1, b = s2, method = "lv")
  #osa <- stringdist(a = s1, b = s2, method = "osa")
  #dl  <- stringdist(a = s1, b = s2, method = "dl")
  hamming  <- stringdist(a = s1, b = s2, method = "hamming") 
  #qgram    <- stringdist(a = s1, b = s2, method = "qgram")
  cosine   <- stringdist(a = s1, b = s2, method = "cosine")
  jw       <- stringdist(a = s1, b = s2, method = "jw")
  soundex  <- stringdist(a = s1, b = s2, method = "soundex")
  jaccard  <- stringdist(a = s1, b = s2, method = "jaccard")
  lcs      <- stringdist(a = s1, b = s2, method = "lcs") # lcs correlated with qgram and more stable (less Inf yields)
  res <- c("lv" = lv #,"osa" = osa, "dl" = dl
           #, "hamming" = hamming # too easy to yield Inf
           # , "qgram" = qgram
           , "jw" = jw, "cosine" = cosine
           , "soundex" = soundex, "jaccard" = jaccard, "lcs" = lcs)
  if(!is.na(prefix)) names(res) <- paste(prefix, names(res), sep = "_")
  return(res)
}

## ######################## ##
## FILTER                   ##
## ######################## ##
proc_df          <- synth_name_db$all[sample(1:nrow(synth_name_db$all)), ]

## synth_fn == "" | synth_ln == ""
proc_df <- filter(proc_df, !(synth_fn == "") & !(synth_ln == "") )

fn_dist_mat <- t( apply( proc_df[, c("true_fn", "synth_fn")], MARGIN = 1, function(x) feature_extractor(x[1], x[2], prefix = "fn") ) ) 
ln_dist_mat <- t( apply( proc_df[, c("true_ln", "synth_ln")], MARGIN = 1, function(x) feature_extractor(x[1], x[2], prefix = "ln") ) )
proc_df     <- data.frame(cbind(proc_df, fn_dist_mat, ln_dist_mat))

## ######################################## ##
## EXPLORE THE RELASTIONHIP AMONG VARIABLES ##
## ######################################## ##
fn_df           <- select(proc_df, true_fn, synth_fn, compound_label, fn_lv, fn_jw, fn_cosine, fn_soundex, fn_jaccard, fn_lcs)
ln_df           <- select(proc_df, true_fn, synth_fn, compound_label, fn_lv, fn_jw, fn_cosine, fn_soundex, fn_jaccard, fn_lcs)
colnames(fn_df) <- c('true', 'synth', 'compound_label', 'fn_lv', 'fn_jw', 'fn_cosine', 'fn_soundex', 'fn_jaccard', 'fn_lcs')
colnames(ln_df) <- c('true', 'synth', 'compound_label', 'fn_lv', 'fn_jw', 'fn_cosine', 'fn_soundex', 'fn_jaccard', 'fn_lcs')
concat_fl       <- bind_rows(fn_df, ln_df)

cor_mtx     <- list()
cor_mtx$all <- cor(filter(concat_fl) %>% select(-(true), -(synth), -(compound_label)) )
cor_mtx$g2  <- cor(filter(concat_fl, compound_label %in% c(2, 3, 4)) %>% select(-(true), -(synth), -(compound_label)) )
cor_mtx$g3  <- cor(filter(concat_fl, compound_label %in% c(5, 6, 7)) %>% select(-(true), -(synth), -(compound_label)) )
cor_mtx$g4  <- cor(filter(concat_fl, compound_label == 8) %>% select(-(true), -(synth), -(compound_label)) )
cor_mtx$g5  <- cor(filter(concat_fl, compound_label == 9) %>% select(-(true), -(synth), -(compound_label)) )

imgs <- list()
imgs$cor_heatmap_g1 <- ggplot(data = melt(cor_mtx$all, ), aes(x = Var1, y = Var2)) + 
                       geom_tile(aes(fill = value)) + 
                       geom_text(aes(label = round(value, 2))) + 
                       ggtitle(label = "Correlation Matrix Heatmap (All Observations)") + 
                       theme(legend.position = "none")

imgs$cor_heatmap_g2 <- ggplot(data = melt(cor_mtx$g2, ), aes(x = Var1, y = Var2)) + 
                       geom_tile(aes(fill = value)) + 
                       geom_text(aes(label = round(value, 2))) + 
                       ggtitle(label = "Correlation Matrix Heatmap (Group 2: Imuted Frist Name)") + 
                       theme(legend.position = "none")

imgs$cor_heatmap_g3 <-ggplot(data = melt(cor_mtx$g3, ), aes(x = Var1, y = Var2)) + 
                      geom_tile(aes(fill = value)) + 
                      geom_text(aes(label = round(value, 2))) + 
                      ggtitle(label = "Correlation Matrix Heatmap (Group 3: Imuted Last Name)") + 
                      theme(legend.position = "none")

imgs$cor_heatmap_g4 <- ggplot(data = melt(cor_mtx$g4, ), aes(x = Var1, y = Var2)) + 
                       geom_tile(aes(fill = value)) + 
                       geom_text(aes(label = round(value, 2))) + 
                       ggtitle(label = "Correlation Matrix Heatmap (Group 4: Imuted First and Last Name)") + 
                       theme(legend.position = "none")

imgs$cor_heatmap_g5 <- imgs$cor_heatmap_g5 <-ggplot(data = melt(cor_mtx$g5, ), aes(x = Var1, y = Var2)) + 
                       geom_tile(aes(fill = value)) + 
                       geom_text(aes(label = round(value, 2))) + 
                       ggtitle(label = "Correlation Matrix Heatmap (Group 5: Shuffled Name)") + 
                       theme(legend.position = "none")

ggsave(plot = imgs$cor_heatmap_g1, filename = getDataPath("cor_heatmap_g1.png", dir = dir$img))
ggsave(plot = imgs$cor_heatmap_g2, filename = getDataPath("cor_heatmap_g2.png", dir = dir$img))
ggsave(plot = imgs$cor_heatmap_g3, filename = getDataPath("cor_heatmap_g3.png", dir = dir$img))
ggsave(plot = imgs$cor_heatmap_g4, filename = getDataPath("cor_heatmap_g4.png", dir = dir$img))
ggsave(plot = imgs$cor_heatmap_g5, filename = getDataPath("cor_heatmap_g5.png", dir = dir$img))

imgs$var_distr_bygroup <- ggplot(melt(proc_df[, -c(1:6)], id.vars = "compound_label"), aes(x = log(value + 1), fill = variable)) + 
                          geom_density(alpha = .3) + 
                          facet_wrap(~ compound_label) + 
                          ggtitle(label = "Variable (dist metrics) Density Distribution Grouped by Groups") + 
                          ylim(c(0, 10))
ggsave(plot = imgs$var_distr_bygroup, filename = getDataPath("var_distribution_bygroup.png", dir = dir$img))

## ############### ##
## TRAINING MODEL  ##
## ############### ##
## Categorize groups into (0, 1)
## 0 for matched
## 1 for un-matched
config <- list()
config$matched_groups   <- c(1, 2, 3, 4, 5, 6, 7)
config$unmatched_groups <- c(8, 9) 

proc_df$class <- 0
proc_df$id    <- 1:nrow(proc_df)
proc_df$class[proc_df$compound_label %in% config$matched_groups]   <- 0 
proc_df$class[proc_df$compound_label %in% config$unmatched_groups] <- 1

proc_df <- select(proc_df, -(label), -(param))
proc_df <- proc_df[, c(19, 1:17, 18)]
## #################################
## filter 

## ################################# 
## Create Partitioning
proc_df$class <- as.factor(proc_df$class)
inTrain       <- createDataPartition(y = proc_df$class, p = .75, list = FALSE)
training      <- proc_df[inTrain,  -c(2:6)]
testing       <- proc_df[-inTrain, -c(2:6)]

var_selection      <- c("fn_lv", "fn_jw", "fn_cosine", "fn_soundex", "fn_jaccard"
                        , "fn_lcs", "ln_lv", "ln_jw", "ln_cosine", "ln_soundex"
                        , "ln_jaccard", "ln_lcs", "class")

rpart_control_grid <- expand.grid(cp = c(2, 1, 0.9, 0.8, .05, 0.4, 0.3, 0.2, 0.1, 0.05, 0.01))

train_ctrl <- trainControl(method = "repeatedcv"
                           , repeats = 10
                           , classProbs = TRUE
                           , summaryFunction = twoClassSummary
                           , allowParallel = TRUE)

tuning_rpart  <- train(x = training[, var_selection]
                       , y = training$class
                       , method = "rpart"
                       , trControl = train_ctrl
                       , metric = "ROC"
                       , tuneGrid = rpart_control_grid)




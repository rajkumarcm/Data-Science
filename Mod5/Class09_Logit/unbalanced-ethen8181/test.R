
# environment setting
library(ROCR)
library(grid)
library(broom)
library(caret)
library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)
# ggthemr is not on CRAN. Need to install from console using
# devtools::install_github('cttobin/ggthemr')
# Might need to install devtools first if not already on system
# Follow prompt. Use 1 or 2 to install/update other dependencies.
# Choose "no" to install those without needing to compile
# Nevermind, still failed. Not working for R 4.1.1 (202111 - EL)
# library(ggthemr)
library(ggthemes)
library(gridExtra)
library(data.table)
# setwd("/Users/ethen/machine-learning/unbalanced")

# read in the dataset ("HR.csv")
data <- fread( list.files( "data", full.names = TRUE )[2] )
str(data)

library(ezids) # EL
# using summary
# summary(data)
xkablesummary(data) # EL
xkabledply(cor(data)) # EL
# find correlations to exclude from the model
findCorrelation( cor(data), cutoff = .75, names = TRUE )

# prop.table( table(data$left) )
xkabledply( prop.table( table(data$left) ), wide = TRUE, title = "prop table: left 0/1" ) # EL

# prediction
data_train$prediction <- predict( model_glm, newdata = data_train, type = "response" )
data_test$prediction  <- predict( model_glm, newdata = data_test , type = "response" )

# distribution of the prediction score grouped by known outcome
ggplot( data_train, aes( prediction, color = as.factor(left) ) ) +
geom_density( size = 1 ) +
ggtitle( "Training Set's Predicted Score" ) +
scale_color_economist( name = "data", labels = c( "negative", "positive" ) ) +
theme_economist()

# functions are sourced in, to reduce document's length
source("unbalanced_code/unbalanced_functions.R")

data_train$predictp <- data_train$prediction # EL
#' Classifier cutoff transform function
#' Convert probability to binary output 0/1
#' ELo 202111 GWU DATS
#' @param df The dataframe.
#' @param model The model
#' @param cutoff The cutoff value between 0 and 1
#' @param name_p The column name of the predicted probability if already calculated from model. Leave blank otherwise.
#' @param name_01 The column name of the calculated vector to attached to original df. If blank, it will not be saved.
#' @return The df with the new calculated column, or the vector of 0 and 1, if not saved to the original df.
#' @examples
#' @export
cutoffXform <- function(df, model, cutoff = 0.5, name_p = "", name_01 = "") {
  colmatch <- match( name_p , colnames(df), nomatch = 0 )
  probv <- ifelse( colmatch > 0 , df[,colmatch] , predict( model, newdata = df, type = "response" ) )
  prob01 <- ifelse( probv < cutoff, 0, 1)
  if (name_01 == "") { return(prob01) } # simply return the vector of 0 and 1 here
  # otherwise, append to dataframe then return
  # first, if new name already exist, replace
  colmatch <- match( name_01 , colnames(df), nomatch = 0 )
  if ( colmatch > 0) {
    df[,colmatch] <- prob01
    return(df)
  }
  # finally, add new name
  nmax <- length(df)
  df[,nmax+1] <- prob01
  colnames(df) <- c(colnames(df), name_01)
  return(df)
}
# sample

# To fix the accuracy_info  Error: `data` and `reference` should be factors with the same levels.
t<-cutoffXform(data_train,model_glm,.5)

#accuracy_info <- AccuracyCutoffInfo( train = data_train, test = data_test, predict = "prediction", actual = "left" )
# define the theme for the next plot
# ggthemr("light")
# accuracy_info$plot

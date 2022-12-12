

#--------------------------------------------------------------------------------
# Unbit:7
# Abgabe: 7
# Verfasser:Jonas Benischek
# Beschreibung: Vorwärts-Feature-Auswahl in mehreren linearen Regressionsmodellen
#--------------------------------------------------------------------------------

# MANDANTORY: Definieren des Stammordners Ändern Sie diese Zeile NICHT
rootDIR = "C:/Datenanalyse/UsingR/Unit7/"

##############################################################################
# Weizen vs. alles andere
# Link: https://geomoer.github.io/moer-mpg-data-analysis/unit07/unit07-03_assignment.html
#
##########################################################################################
# Schreiben Sie eine R-Funktion, die einen Forward-Feature-Selektionsansatz für ein 
# multiples lineares Regressionsmodell implementiert. Die Funktion sollte zwei Optionen für 
# die Leistungsmessung haben: (i) die Akaike-Informationskriterien (AIC) und (ii) eine 
# Leave-many-out-Kreuzvalidierungsstrategie, die vier Falten verwendet.
#
# Testen Sie die Funktion, indem Sie den Winterweizenertrag auf der Grundlage anderer 
# Erträge vorhersagen, die in Ihrem Erntedataset enthalten sind.
#
# Schreiben Sie eine RMD-Datei mit HTML-Ausgabe, die die Funktionalität der oben genannten 
# Funktionen testet und die Leistung der einzelnen Modelle, die während der Feature-Auswahl 
# berechnet werden, mit einer zunehmenden Reihenfolge der für die Vorhersage verwendeten 
# Variablen grafisch darstellt. Die RMD-Datei sollte am Ende auch die Leistung und die im 
# endgültigen Modell verwendeten Variablen ausgeben.    
#
##########################################################################################

#Öffnen über Notepad++

df <- read.table("/Users/Jonas/Documents/Datenanalyse/UsingR/Unit4/feldfruechte.txt",
                 skip = 1, header = TRUE, sep = ";", dec = ",",encoding = "latin1", 
                 nrows = 8930) #Zeile 8930 löschen um die Fehlermelung zu umgehen

str(df) #Ansicht in der Console


###########################################
# Das umbennen der Spalten 

names(df) <- c("Jahr", "Plz", "Region", "Winterweizen", "Roggen und Wintermenggetreide", 
               "Wintergerste", "Sommergerste","Hafer", "Triticale", "Kartoffeln", 
               "Zuckerrüben", "Winterraps", "Silomais")
str(df)

########################################################
# Das Konvertieren von Datentypen von chr zu numeric

for(c in colnames(df)[4:7]){
  df[, c][df[, c] == "."] <- NA  #jede Zelle, die nur ein "." enthält, auf NA abändern
  df[, c] <- as.numeric(sub(",", ".", as.character(df[, c])))
}

str(df)

################################
# Na Werte Löschen


data_clean <- na.omit(df) 

str(data_clean)


############################
# Mit data_clean arbeiten

y <- anscombe$y1
x1 <- anscombe$x1
lmod <- lm(y~x1)

set.seed(5)
x2 <- lmod$model$y - lmod$fitted.values * sample(seq(-1, 0, 0.1), nrow(anscombe))
set.seed(2)
x3 <- anscombe$x3 + sample(seq(-1, 1, 0.1), nrow(anscombe))
set.seed(3)
x4 <- anscombe$x4 + sample(seq(-1, 1, 0.1), nrow(anscombe))

df <- data.frame(y = y,
                 x1 = x1,
                 x2 = x2,
                 x3 = x3,
                 x4 = x4)

#############################################################
# Multiples lineares Regressionsmodell

lmod <- lm(y ~ x2 + x1, data = df)

ss_obsrv <- sum((lmod$model$y - mean(lmod$model$y))**2)
ss_model <- sum((lmod$fitted.values - mean(lmod$model$y))**2)
ss_resid <- sum((lmod$model$y - lmod$fitted.values)**2)

mss_obsrv <- ss_obsrv / (length(lmod$model$y) - 1)
mss_model <- ss_model / 2
mss_resid <- ss_resid / (length(lmod$model$y) - 2 -1)

r_square <- round(1 - ss_resid/ss_obsrv, 5)
r_square_adjusted <- round(1 - 
                             (ss_resid / 
                                (length(lmod$model$y) - (2+1))) / 
                             (ss_obsrv / 
                                (length(lmod$model$y) - 1)), 5)

f_value <- round(mss_model / mss_resid, 2)

print(data.frame(Name = c("r square", "adj. r square", "f value"), 
                 Value = c(r_square, r_square_adjusted, f_value)))

##########################
#        Name    Value   #
#      r square  0.75584 #
# adj. r square  0.69480 #
#       f value 12.38000 #
##########################

anova(lmod) # Analysis of Variance Table

#################################################################
#  Response: y
#  Df Sum Sq Mean Sq F value   Pr(>F)   
#  x2         1 19.036 19.0358 15.1123 0.004626 **
#  x1         1 12.160 12.1599  9.6536 0.014511 * 
#  Residuals  8 10.077  1.2596                    
#----------------------------------------------------------------
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#################################################################

summary(lmod) # Ergebnis der t-Statistik

#########################################
# Modell mit einer geänderten Reihenfolge

lmod <- lmod <- lm(y ~ x1 + x2, data = df)
anova(lmod)

summary(lmod) # Ergebnis der t-Statistik


##########################
# Vorwärts-Feature-Auswahl

forward_feature_selection <- function(data, dep, vars, selected_vars = NULL){
  fwd_fs <- lapply(seq(length(vars)), function(v){
    if(is.null(selected_vars)){
      formula <- paste(dep, " ~ ", paste(vars[v], collapse=" + "))
    } else {
      formula <- paste(dep, " ~ ", paste(c(selected_vars, vars[v]), collapse=" + "))
    }
    
    lmod <- lm(formula, data = data)
    results <- data.frame(Variable = vars[v],
                          Adj_R_sqrd = round(summary(lmod)$adj.r.squared, 4),
                          AIC = round(AIC(lmod), 4))
    return(results)
  })
  fwd_fs <- do.call("rbind", fwd_fs)
  
  if(!is.null(selected_vars)){
    formula <- paste(dep, " ~ ", paste(selected_vars, collapse=" + "))
    lmod <- lm(formula, data = data)
    results_selected <- data.frame(Variable = paste0("all: ", paste(selected_vars, collapse=", ")),
                                   Adj_R_sqrd = round(summary(lmod)$adj.r.squared, 4),
                                   AIC = round(AIC(lmod), 4))
    fwd_fs <- rbind(results_selected, fwd_fs)
  }
  
  print(fwd_fs)
  
  best_var <- as.character(fwd_fs$Variable[which(fwd_fs$AIC == min(fwd_fs$AIC))])
  return(best_var)
}

###################################################
# x1 bis x4 werden an die obrige Funktion übergeben:

next_vars <- c("x1", "x2", "x3", "x4")
act_var <- forward_feature_selection(data = df, dep = "y", vars = next_vars)


# Variable Adj_R_sqrd     AIC
#1       x1     0.6295 39.6814
#2       x2     0.4014 44.9591
#3       x3     0.6666 38.5214
#4       x4     0.2021 48.1199


next_vars <- next_vars[-which(next_vars == act_var)]
selected_vars = act_var
act_var <- forward_feature_selection(data = df, dep = "y", vars = next_vars,
                                     selected_vars = selected_vars)

# Variable Adj_R_sqrd     AIC
#1  all: x1     0.6295 39.6814
#2       x2     0.6826 38.6849
#3       x3     0.5939 41.3942
#4       x4     0.6067 41.0438


ext_vars <- next_vars[-which(next_vars == act_var)]
selected_vars = c(selected_vars, act_var)
act_var <- forward_feature_selection(data = df, dep = "y", vars = next_vars,
                                     selected_vars = selected_vars)

# Variable Adj_R_sqrd     AIC
#1 all: x3, x2     0.6929 38.3205
#2          x1     0.6552 40.1258
#3          x2     0.6929 38.3205
#4          x4     0.6512 40.2534


#################################
# Leave-many-out-Kreuzvalidierung

range <- nrow(anscombe)
nbr <- nrow(anscombe) * 0.8

cv_sample <- lapply(seq(4), function(i){
  set.seed(i)
  smpl <- sample(range, nbr)
  train <- anscombe[smpl,]
  test <- anscombe[-smpl,]
  lmod <- lm(y1 ~ x1, data = train)
  pred <- predict(lmod, newdata = test)
  obsv <- test$y1
  resid <- obsv - pred
  ss_obsrv <- sum((obsv - mean(obsv))**2)
  ss_model <- sum((pred - mean(obsv))**2)
  ss_resid <- sum((obsv - pred)**2)
  mss_obsrv <- ss_obsrv / (length(obsv) - 1)
  mss_model <- ss_model / 1
  mss_resid <- ss_resid / (length(obsv) - 2)
  data.frame(pred = pred,
             obsv = obsv,
             resid = resid,
             ss_obsrv = ss_obsrv,
             ss_model = ss_model,
             ss_resid = ss_resid,
             mss_obsrv = mss_obsrv,
             mss_model = mss_model,
             mss_resid = mss_resid,
             r_squared = ss_model / ss_obsrv
  )
})
cv_sample <- do.call("rbind", cv_sample)

ss_obsrv <- sum((cv_sample$obsv - mean(cv_sample$obsv))**2)
ss_model <- sum((cv_sample$pred - mean(cv_sample$obsv))**2)
ss_resid <- sum((cv_sample$obsv - cv_sample$pred)**2)

mss_obsrv <- ss_obsrv / (length(cv_sample$obsv) - 1)
mss_model <- ss_model / 1
mss_resid <- ss_resid / (length(cv_sample$obsv) - 2)

#################################################

data.frame(NAME = c("cross validation F value",
                    "linear model F value", 
                    "cross validation r squared",
                    "linear model r squared"),
           VALUE = c(round(mss_model / mss_resid, 2),
                     round(anova(lmod)$'F value'[1], 2),
                     round(1 - ss_resid / ss_obsrv, 2),
                     round(summary(lmod)$r.squared, 2)))

#                        NAME VALUE
#1   cross validation F value 22.27
#2       linear model F value 21.84
#3 cross validation r squared  0.71
#4     linear model r squared  0.76


se <- function(x) sd(x, na.rm = TRUE)/sqrt(length(na.exclude(x)))

me <- round(mean(cv_sample$pred - cv_sample$obs, na.rm = TRUE), 2)
me_sd <- round(se(cv_sample$pred - cv_sample$obs), 2)
mae <- round(mean(abs(cv_sample$pred - cv_sample$obs), na.rm = TRUE), 2)
mae_sd <- round(se(abs(cv_sample$pred - cv_sample$obs)), 2)
rmse <- round(sqrt(mean((cv_sample$pred - cv_sample$obs)^2, na.rm = TRUE)), 2)
rmse_sd <- round(se((cv_sample$pred - cv_sample$obs)^2), 2)

data.frame(NAME = c("Mean error (ME)", "Std. error of ME", 
                    "Mean absolute error (MAE)", "Std. error of MAE", 
                    "Root mean square error (RMSE)", "Std. error of RMSE"),
           VALUE = c(me, me_sd,
                     mae, mae_sd,
                     rmse, rmse_sd))

#                            NAME VALUE
#1               Mean error (ME)  0.31
#2              Std. error of ME  0.33
#3     Mean absolute error (MAE)  0.75
#4             Std. error of MAE  0.26
#5 Root mean square error (RMSE)  1.15
#6            Std. error of RMSE  0.57


###########################################################

print(cv_sample) 

summary(cv_sample)

#########################################################
# load required package for model training ...
install.packages("caret")
library(caret)

#########################################

install.packages("ggplot2")
library(ggplot2)

##########################################

cp <- readRDS("C:/Users/Jonas/Documents/Datenanalyse/UsingR/Unit7/data/cp_clean.rds")
head(cp)

str(cp)

###########################
# clean data from NA values
cp <- na.omit(cp[,6:15])

str(cp) # 5378 rows left


############################################
# define a forward feature selction function

forward_feature_selection <- function(data, dep, vars, mode){
  
  set.seed(42)  # set seed for selections
  n <- 1 # define first value for number of runs
  
  if (mode == 'aic') # processing for AIC method 
    
  {
    
    # define the variables for the first run
    selected_vars = NULL
    
    # define a forward feature selection loop for the first run
    fwd_fs <- lapply(seq(length(vars)), function(v){
      
      formula <- paste(dep, " ~ ", paste(vars[v], collapse=" + ")) # calculate formula
      
      lmod <- lm(formula, data = data) # calculate a linear regression model
      
      results <- data.frame(Variable = vars[v],
                            Adj_R_sqrd = round(summary(lmod)$adj.r.squared, 4),
                            AIC = round(AIC(lmod), 4)
      ) # define an output data frame for the required model statistics
      return(results)
    })
    
    fwd_fs <- do.call("rbind", fwd_fs) # write the returned results to a variable
    
    # select the relevant statistics and write them to a data frame
    AIC_add <- min(fwd_fs$AIC) # the minimum AIC value
    Adj_R_sq_add <- fwd_fs$Adj_R_sqrd[which(fwd_fs$AIC == min(fwd_fs$AIC))] # the respective Adj. R Squared value
    best_var <- as.character(fwd_fs$Variable[which(fwd_fs$AIC == min(fwd_fs$AIC))]) # and the name of the variable
    
    ffs_df <- data.frame(n = n, # assign respective number of run
                         best_var = best_var, # add the name of best variable
                         AIC = AIC_add, # add the best AIC value...
                         Adj_R_sqrd = Adj_R_sq_add # and the respective Adj. R Squared value as well
    )
    
    print(paste(best_var, "has been identified as best variable."))
    
    selected_vars = best_var # define the first entry for appending the selected variables
    vars = vars[-which(vars == best_var)] # remove the selected variable from the variables
    
    # define a while loop for the further variable selection
    
    forward = TRUE
    
    while(forward == TRUE){
      
      # define a forward feature selection loop for the further runs
      fwd_fs <- lapply(seq(length(vars)), function(v){
        
        formula <- paste(dep, " ~ ", paste(paste(selected_vars, collapse = " + "), vars[v], sep = "+")) # calculate formula
        
        lmod <- lm(formula, data = data) # calculate a linear regression model
        
        results <- data.frame(Variable = vars[v],
                              Adj_R_sqrd = round(summary(lmod)$adj.r.squared, 4),
                              AIC = round(AIC(lmod), 4)
        ) # define an output data frame for the required model statistics
        
        return(results)
      })
      
      fwd_fs <- do.call("rbind", fwd_fs) # write the returned results to a variable
      
      AIC_sel <- min(fwd_fs$AIC)
      
      # define an if-else-condition
      if (AIC_sel < AIC_add){
        
        # count one up
        n <- n + 1
        
        # isolate the statistics
        Adj_R_sq_add <- fwd_fs$Adj_R_sqrd[which(fwd_fs$AIC == min(fwd_fs$AIC))] # the respective Adj. R Squared value
        best_var <- as.character(fwd_fs$Variable[which(fwd_fs$AIC == min(fwd_fs$AIC))]) # and the name of the variable
        
        print(paste(best_var, "has been identified as best variable."))
        
        # write the information into a data frame
        ffs_df_app <- data.frame(n = n, # assign respective number of run
                                 best_var = best_var, # add the name of best variable
                                 AIC = AIC_add, # add the best AIC value...
                                 Adj_R_sqrd = round(Adj_R_sq_add, 4) # and the respective Adj. R Squared value as well
        )
        
        # append the data frame
        ffs_df <- rbind(ffs_df, ffs_df_app)
        
        selected_vars <- c(selected_vars, best_var) # define the first entry for appending the selected variables
        vars <- vars[-which(vars == best_var)] # remove the selected variable from the variables 
        
        AIC_add = AIC_sel
        
        # keep the loop running
        forward = TRUE
        
      } else {
        
        print(paste("the following variables have been identified for optimal model design:", 
                    paste(selected_vars, collapse = ",")))
        
        # stop condition for while-loop
        forward = FALSE
      }
    }
  }
  else if (mode == 'lmo') { # processing for leave-many-out cross calidation method
    
    selected_vars = NULL # define the variables for the first run
    
    # define a forward feature selection loop for the first run
    fwd_fs <- lapply(seq(length(vars)), function(v){
      
      formula <- paste(dep, " ~ ", vars[v]) # calculate formula
      formula <- as.formula(formula)
      
      ctrl <- trainControl(method = 'cv', number = 4)
      
      lmod <- train(form = formula, data = data, method = 'lm', trControl = ctrl) # calculate a linear regression model
      
      results <- data.frame(Variable = vars[v],
                            R_Sqrd = round(summary(lmod)$r.squared, 4)
      ) # define an output data frame for the required model statistics
      
      return(results) # return the results
    })
    
    fwd_fs <- do.call("rbind", fwd_fs) # write the returned results to a variable
    
    # select the relevant statistics and write them to a data frame
    R_Sqrd_add = min(fwd_fs$R_Sqrd)
    best_var <- as.character(fwd_fs$Variable[which(fwd_fs$R_Sqrd == min(fwd_fs$R_Sqrd))]) # and the name of the variable
    
    ffs_df <- data.frame(n = n, # assign respective number of run
                         best_var = best_var, # add the name of best variable
                         R_Sqrd = R_Sqrd_add # add the respective R-Squared value
    )
    
    print(paste(best_var, "has been identified as best variable."))
    
    #n = n + 1 # count one up for the loop
    
    selected_vars = best_var # define the first entry for appending the selected variables
    vars = vars[-which(vars == best_var)] # remove the selected variable from the variables
    
    # define a while loop for the further variable selection
    
    forward = TRUE
    
    while(forward == TRUE){
      
      # define a forward feature selection loop for the further runs
      fwd_fs <- lapply(seq(length(vars)), function(v){
        
        formula <- paste(dep, " ~ ", paste(paste(selected_vars, collapse = " + "), vars[v], sep = "+")) # calculate formula
        formula <- as.formula(formula)
        
        ctrl <- trainControl(method = 'cv', number = 4)
        
        lmod <- train(form = formula, data = data, method = 'lm', trControl = ctrl) # calculate a linear regression model
        
        results <- data.frame(Variable = vars[v],
                              R_Sqrd = round(summary(lmod)$r.squared, 4)
        ) # define an output data frame for the required model statistics
        
        return(results) # return the results
      })
      
      fwd_fs <- do.call("rbind", fwd_fs) # write the returned results to a variable
      
      R_Sqrd_sel <- min(fwd_fs$R_Sqrd)
      
      # define an if-else-condition
      if (R_Sqrd_sel < R_Sqrd_add){
        
        # count one up
        n <- n + 1
        
        # isolate the statistics
        best_var <- as.character(fwd_fs$Variable[which(fwd_fs$R_Sqrd == min(fwd_fs$R_Sqrd))]) # and the name of the variable
        
        print(paste(best_var, "has been identified as best variable."))
        
        # write the information into a data frame
        ffs_df_app <- data.frame(n = n, # assign respective number of run
                                 best_var = best_var, # add the name of best variable
                                 R_Sqrd = round(R_Sqrd_sel, 4)
        )
        
        # append the data frame
        ffs_df <- rbind(ffs_df, ffs_df_app)
        
        selected_vars <- c(selected_vars, best_var) # define the first entry for appending the selected variables
        vars <- vars[-which(vars == best_var)] # remove the selected variable from the variables 
        
        R_Sqrd_add = R_Sqrd_sel
        
        # keep the loop running
        forward = TRUE
        
      } else {
        
        print(paste("the following variables have been identified for optimal model design:", 
                    paste(selected_vars, collapse = ",")))
        
        # stop condition for while-loop
        forward = FALSE
      }
    }
  } else
    print("Error: The argument 'mode' has to be either 'aic' or 'lmo'.")
  
  return(ffs_df)
}


# define the independent input variables 
next_vars <- colnames(cp[,2:10])

# apply the FFS function with 'aic' mode
aic_test <- forward_feature_selection(data = cp, 
                                      dep = 'Winter_wheat',
                                      vars = next_vars,
                                      mode = 'aic') 



# apply the FFS function with 'lmo' mode
lmo_test <- forward_feature_selection(data = cp, 
                                      dep = 'Winter_wheat',
                                      vars = next_vars,
                                      mode = 'lmo') 

install.packages("ggplot")


# AIC
ggplot(aes(x = n, y = AIC, label = best_var), data = aic_test) + 
  geom_line(colour = "blue", cex = 2) +
  geom_point(colour = "green", cex = 5) + 
  ggtitle("Influence of variables selected by AIC on model performance.") + 
  geom_text(hjust = 0, nudge_x = 0.2, nudge_y = 30, angle = 20, size = 3) + 
  theme_classic() +
  expand_limits(y = c(35900,37200), x = c(1,9))


print(aic_test)










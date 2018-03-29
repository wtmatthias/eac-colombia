#-----------------------------------------------------------------------------#
#  File Name:    02_analysis_eac.R
#  Author:       Billy Matthias 
#  Email:        bmatthias88@gmail.com
#  Purpose:      (1) exploratory data analysis
#                (2) stats analysis
#  Data Output:    
#-----------------------------------------------------------------------------#


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####    SETUP    #### 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
rm(list=ls())

library(sandwich)
library(RCurl)
library(interplot)
library(tidyverse)
library(ggplot2)
library(modelr)
library(haven)
library(magrittr)
options(na.action = na.warn)
library(xtable)
library(knitr)
library(texreg)
library(broom)
library(htmltools)
library(stringr)



## Set file paths --------------------------------------------------------------
dir.outdata <- file.path("./01_outputdata")

dir.figures.tables <- file.path("./02_tables_and_figures")



## Load Data -------------------------------------------------------------------
load(file = file.path(dir.outdata,
                      "eac_vio_muni_cross-section.RData")
)



## Functions -------------------------------------------------------------------


## ROBUST SEs
robust <- function(model.object){
  require(sandwich)
  
  sandwich_se <- sqrt(diag(vcovHC(model.object, type = "HC1")))
  
  z_stat <- coef(model.object)/sandwich_se
  p_values <- pchisq(z_stat^2, 1, lower.tail=FALSE)
  
  robust.mod <- summary(model.object)
  
  robust.mod$coefficients[, 2] <- sandwich_se
  robust.mod$coefficients[, 3] <- z_stat
  robust.mod$coefficients[, 4] <- p_values
  corrected <- robust.mod
  se.corrections <- list("robust.mod" = corrected, "sandwich_se" = sandwich_se,
                         "z_stat" = z_stat, "p_values" = p_values)
  se.corrections
}


## REG TABLE W/ ROBUST SEs TO .doc TABLE
# needs function "robust". also created in this script
robust_table <- function(linear.model, filename, title, coefnames, note,
                         latex = FALSE){
  require(texreg)
  sandwich_se <- linear.model %>% map(robust) %>% map(2)
  p_values <- linear.model %>% map(robust) %>% map(4)
  
  if (latex == TRUE && is.null(filename)){
    message("Printing code for latex table...")
    # Latex reg table
    latex_table <- texreg(linear.model,
                          file = NULL,
                          booktabs = TRUE,
                          dcolumn = TRUE,
                          table = TRUE,
                          sideways = TRUE,
                          stars = c(0.01, 0.05, 0.1),
                          custom.note = note,
                          digits = 2,
                          leading.zero = TRUE,
                          custom.model.names = str_c("(", seq_along(linear.model), ")"),
                          custom.coef.map = coefnames,
                          caption.above = TRUE,
                          caption = title,
                          override.se = sandwich_se,
                          override.pvalues = p_values,
                          # passed to extract() method for "lm"
                          include.adjr = TRUE,
                          include.rsquared = FALSE,
                          include.rmse = FALSE,
                          include.nobs = TRUE
    )
    return(latex_table)
  } else if (latex == FALSE && !is.null(filename)){
    # output table to .doc
    htmlreg(linear.model,
            file = filename,
            custom.note = note,
            digits = 2,
            stars = c(0.01, 0.05, 0.1),
            leading.zero = TRUE,
            custom.model.names = str_c("(", seq_along(linear.model), ")"),
            custom.coef.map = coefnames,
            caption.above = TRUE,
            caption = title,
            override.se = sandwich_se,
            override.pvalues = p_values,
            inline.css = FALSE,
            # better for Word doc
            doctype = TRUE,
            html.tag = TRUE,
            head.tag = TRUE,
            body.tag = TRUE,
            # passed to extract() method for "lm"
            include.adjr = TRUE,
            include.rsquared = FALSE,
            include.rmse = FALSE,
            include.nobs = TRUE
    )
    
  } else{
    message("Printing code for markdown table...")
    # html markdown in browser
    view_markdown <- htmlreg(linear.model,
                             file = NULL,
                             custom.note = note,
                             digits = 2,
                             stars = c(0.01, 0.05, 0.1),
                             leading.zero = TRUE,
                             custom.model.names = str_c("(", seq_along(linear.model), ")"),
                             custom.coef.map = coefnames,
                             caption.above = TRUE,
                             caption = title,
                             override.se = sandwich_se,
                             override.pvalues = p_values,
                             star.symbol = "*",
                             # better for markdown
                             doctype = FALSE,
                             html.tag = FALSE,
                             head.tag = FALSE,
                             body.tag = FALSE,
                             # passed to extract() method for "lm"
                             include.adjr = TRUE,
                             include.rsquared = FALSE,
                             include.rmse = FALSE,
                             include.nobs = TRUE
    )
    return(view_markdown)
  }
}

## robust SEs through map and purrr:
# sandwich_se <- map(cropveg.aid.coca1, ~ sqrt(diag(vcovHC(., type = "HC1")
# )
# )
# )
# z_stat <- map2(cropveg.aid.coca1, sandwich_se, ~ coef(.x)/.y)
# 
# p_values <- map(z_stat, ~ pchisq(.^2, 1, lower.tail=FALSE))


## EXAMPLE CODE FOR robust_table:

## table to .doc
# robust_table(linear.model, filename = filename, title, coefnames, note, latex = FALSE)
## table to html code
# html_table <- robust_table(linear.model, filename = NULL, title, coefnames, note, latex = FALSE)
# html_table
# html_table %>% HTML() %>% browsable() # view the html in R's viewer
## table to latex
# robust_table(linear.model, filename = NULL, title, coefnames, note, latex = TRUE)



## THREE WAY MARGINAL EFFECTS FUNCTION
ThreewayME.f <- function(M,X,Z,W,xlab,ylab,lloc,Min,Q1,Mean,Q3,Max,level,rob,hist)  
{ 
  ## A function to generate 3-way marginal effects plots in R with stars for set confidence levels. 
  ## Written by Joshua Gubler ~  http://joshuagubler.com
  ## Last updated: 26 April 2017
  ## Variables must be in the following order: y = x z w (control variables here) xz xw zw xzw .  Of course, the model can include as many control variables as you need; the following code specifically uses just the variables we want for the interaction.  As long as you get the first three variables in correct order, R will correctly order the interaction terms.
  ## M = an object of type "lm," "glm," or other estimation -- i.e. the object that contains the regression estimation you seek to plot
  ## X = the variable whose effect on Y you seek to plot
  ## Z = the first moderating variable (will be positioned on the X-axis of the plot)
  ## W = the second moderating variable (the lines on the plot)
  ## xlab = Label for x-axis (in quotes)
  ## ylab = label for y-axis (in quotes)
  ## lloc = location of the legend for the plot, use values like "bottomleft"
  ## Min, Q1, Mean, Q3, Max = titles for each of these quartiles to be put in the legend -- i.e. "Min Q88" (titles must be in quotes)
  ## level = to set the confidence level.  Two options (don't put these in quotes): 95, 90.  Stars will show on lines that are significant at the level you set.  If you do not put in either option, stars will show on all lines.
  ## rob = if you desire Huber-White robust standard errors
  ## hist = if hist, then you will get a density histogram of your variable on the X-axis.  If not, then you will get a rug plot of your variable on the X-axis
  
  ## Example: ThreewayME.f(estimation.lm,ses,edu,pop,"Education levels","Effect of SES on Civil War","bottomleft","Min Pop","1Q Pop","Mean Pop","3Q Pop","Max Pop",90,rob,hist)
  
  ##Note: this function can be altered easily to change the number of lines in "W", depending on how many one prefers (W could be made dichtomous, etc.)
  
  S <- summary(M)
  N <- c(1:20)
  
  # Create 20 equally spaced values in a vector between min and max on the Z variable            
  zmin <- rep(min(Z,na.rm=TRUE), 20) 
  zmax <- rep(max(Z, na.rm=TRUE), 20) 
  Znew <- (((N-1)/(20-1))*(zmax-zmin))+zmin
  
  # Generate the values of W for which you want to calculate the marginal effect (and standard errors) of X on Y.  Note: the default is the following percentiles, but these can be changed.  If one deletes lines here, the corresponding lines will need to be deleted later in the function.
  W0 <- quantile(W,   0, na.rm=TRUE) 
  W1 <- quantile(W, .25, na.rm=TRUE)
  W2 <- quantile(W, .50, na.rm=TRUE)
  W3 <- quantile(W, .75, na.rm=TRUE)
  W4 <- quantile(W,   1, na.rm=TRUE)
  
  # Grab elements of coefficient and vcov matrix.                                               
  require(sandwich)
  H <- head(S$coefficients,4)
  T <- tail(S$coefficients,4)
  b <- rbind(H,T)
  if(missing(rob)){Vcov <- vcov(M)}
  else{
    Vcov <- vcovHC(M,"HC1")
  }
  Vcov <- as.data.frame(Vcov)
  Vcov1 <- Vcov[,c(1:4)]
  Vcov2 <- Vcov[,-c(3:0-length(Vcov))]
  Vcov <- cbind(Vcov1,Vcov2)
  Vh <- head(Vcov,4)
  Vt <- tail(Vcov,4)
  V <- rbind(Vh,Vt)
  
  b1 <- b[2,1]
  b4 <- b[5,1]
  b5 <- b[6,1]
  b7 <- b[8,1]
  
  varb1 <- V[2,2]
  varb4 <- V[5,5]
  varb5 <- V[6,6]
  varb7 <- V[8,8]
  
  covb1b4 <- V[5,2]
  covb1b5 <- V[6,2]
  covb1b7 <- V[8,2]
  covb4b5 <- V[6,5]
  covb4b7 <- V[8,5]
  covb5b7 <- V[8,6]
  
  # Calculate ME values for all levels of W
  conb0 <- b1+b4*Znew+b5*W0+b7*(Znew*W0)
  conb1 <- b1+b4*Znew+b5*W1+b7*(Znew*W1)
  conb2 <- b1+b4*Znew+b5*W2+b7*(Znew*W2)
  conb3 <- b1+b4*Znew+b5*W3+b7*(Znew*W3)
  conb4 <- b1+b4*Znew+b5*W4+b7*(Znew*W4)
  
  # Calculate standard errors for all levels of W
  if(missing(rob)){
    conse0 <- sqrt(varb1         
                   + varb4*(Znew^2) + varb5*(W0^2) + varb7*(Znew^2)*(W0^2)
                   + 2*Znew*covb1b4 + 2*W0*covb1b5 + 2*Znew*W0*covb1b7 + 2*Znew*W0*covb4b5
                   + 2*W0*(Znew^2)*covb4b7 + 2*(W0^2)*Znew*covb5b7)
    
    conse1 <- sqrt(varb1         
                   + varb4*(Znew^2) + varb5*(W1^2) + varb7*(Znew^2)*(W1^2)      
                   + 2*Znew*covb1b4 + 2*W1*covb1b5 + 2*Znew*W1*covb1b7 + 2*Znew*W1*covb4b5
                   + 2*W1*(Znew^2)*covb4b7 + 2*(W1^2)*Znew*covb5b7)
    
    conse2 <- sqrt(varb1       
                   + varb4*(Znew^2) + varb5*(W2^2) + varb7*(Znew^2)*(W2^2) 
                   + 2*Znew*covb1b4 + 2*W2*covb1b5 + 2*Znew*W2*covb1b7 + 2*Znew*W2*covb4b5
                   + 2*W2*(Znew^2)*covb4b7 + 2*(W2^2)*Znew*covb5b7)
    
    conse3 <- sqrt(varb1
                   + varb4*(Znew^2) + varb5*(W3^2) + varb7*(Znew^2)*(W3^2)
                   + 2*Znew*covb1b4 + 2*W3*covb1b5 + 2*Znew*W3*covb1b7 + 2*Znew*W3*covb4b5
                   + 2*W3*(Znew^2)*covb4b7 + 2*(W3^2)*Znew*covb5b7)      
    
    conse4 <- sqrt(varb1
                   + varb4*(Znew^2) + varb5*(W4^2) + varb7*(Znew^2)*(W4^2)
                   + 2*Znew*covb1b4 + 2*W4*covb1b5 + 2*Znew*W4*covb1b7 + 2*Znew*W4*covb4b5
                   + 2*W4*(Znew^2)*covb4b7 + 2*(W4^2)*Znew*covb5b7)    
  }else{
    conse0 <- sqrt(varb1         
                   + varb4*(Znew^2) + varb5*(W0^2) + varb7*(Znew^2)*(W0^2)
                   + 2*Znew*covb1b4 + 2*W0*covb1b5 + 2*Znew*W0*covb1b7 + 2*Znew*W0*covb4b5
                   + 2*W0*(Znew^2)*covb4b7 + 2*(W0^2)*Znew*covb5b7)
    
    conse1 <- sqrt(varb1         
                   + varb4*(Znew^2) + varb5*(W1^2) + varb7*(Znew^2)*(W1^2)      
                   + 2*Znew*covb1b4 + 2*W1*covb1b5 + 2*Znew*W1*covb1b7 + 2*Znew*W1*covb4b5
                   + 2*W1*(Znew^2)*covb4b7 + 2*(W1^2)*Znew*covb5b7)
    
    conse2 <- sqrt(varb1       
                   + varb4*(Znew^2) + varb5*(W2^2) + varb7*(Znew^2)*(W2^2) 
                   + 2*Znew*covb1b4 + 2*W2*covb1b5 + 2*Znew*W2*covb1b7 + 2*Znew*W2*covb4b5
                   + 2*W2*(Znew^2)*covb4b7 + 2*(W2^2)*Znew*covb5b7)
    
    conse3 <- sqrt(varb1
                   + varb4*(Znew^2) + varb5*(W3^2) + varb7*(Znew^2)*(W3^2)
                   + 2*Znew*covb1b4 + 2*W3*covb1b5 + 2*Znew*W3*covb1b7 + 2*Znew*W3*covb4b5
                   + 2*W3*(Znew^2)*covb4b7 + 2*(W3^2)*Znew*covb5b7)      
    
    conse4 <- sqrt(varb1
                   + varb4*(Znew^2) + varb5*(W4^2) + varb7*(Znew^2)*(W4^2)
                   + 2*Znew*covb1b4 + 2*W4*covb1b5 + 2*Znew*W4*covb1b7 + 2*Znew*W4*covb4b5
                   + 2*W4*(Znew^2)*covb4b7 + 2*(W4^2)*Znew*covb5b7)    
  }
  
  # Create t statistics
  t0 <- conb0/conse0
  t1 <- conb1/conse1
  t2 <- conb2/conse2
  t3 <- conb3/conse3
  t4 <- conb4/conse4
  
  # Make a `shadow' variable that is missing if the t score is not larger than the critical level of significance you want.
  ci <- NA
  ci[level == 95] <- qt(.975,M$df.residual)
  ci[level == 90] <- qt(.95,M$df.residual)
  
  stars.df <- data.frame(consb0=conb0,consb1=conb1,consb2=conb2,consb3=conb3,consb4=conb4,t0=t0,t1=t1,t2=t2,t3=t3,t4=t4)
  
  stars.df$consb0[abs(stars.df$t0)<ci] <- NA
  stars.df$consb1[abs(stars.df$t1)<ci] <- NA
  stars.df$consb2[abs(stars.df$t2)<ci] <- NA
  stars.df$consb3[abs(stars.df$t3)<ci] <- NA
  stars.df$consb4[abs(stars.df$t4)<ci] <- NA
  
  # Generate a string variable called txt that is designated with a star.
  txt <- c("*")
  
  # Graph the marginal effect of X on Y across the desired range of the modifying variable Z. Do this for when W=0, when W=1, when W=2, when W=3, and when W=4.                                   
  
  if(missing(hist)){
    plot(c(Znew,Znew,Znew,Znew,Znew,Znew,Znew,Znew,Znew,Znew), c(conb0,stars.df$consb0,conb1,stars.df$consb1,conb2,stars.df$consb2,conb3,stars.df$consb3,conb4,stars.df$consb4), type="n",xlab=xlab,ylab=ylab)+rug(jitter(Z))
    
    lines(Znew,conb0,col="blue")
    text(x=Znew,y=stars.df$consb0,labels=txt)
    lines(Znew,conb1,col="red")
    text(x=Znew,y=stars.df$consb1,labels=txt)
    lines(Znew,conb2,col="forest green")
    text(x=Znew,y=stars.df$consb2,labels=txt)
    lines(Znew,conb3,col="yellow")
    text(x=Znew,y=stars.df$consb3,labels=txt)
    lines(Znew,conb4,col="brown")
    text(x=Znew,y=stars.df$consb4,labels=txt)
    legend(lloc,legend=c(Min,Q1,Mean,Q3,Max),col=c("blue","red","forest green","yellow","brown"),lty = c("solid"))
    abline(h=0)
  }else{
    hist(Z, axes=F, xlab="", ylab="",main="", col="light gray")
    par(new=TRUE)
    plot(c(Znew,Znew,Znew,Znew,Znew,Znew,Znew,Znew,Znew,Znew), c(conb0,stars.df$consb0,conb1,stars.df$consb1,conb2,stars.df$consb2,conb3,stars.df$consb3,conb4,stars.df$consb4), type="n",xlab=xlab,ylab=ylab)
    
    lines(Znew,conb0,col="blue")
    text(x=Znew,y=stars.df$consb0,labels=txt)
    lines(Znew,conb1,col="red")
    text(x=Znew,y=stars.df$consb1,labels=txt)
    lines(Znew,conb2,col="forest green")
    text(x=Znew,y=stars.df$consb2,labels=txt)
    lines(Znew,conb3,col="yellow")
    text(x=Znew,y=stars.df$consb3,labels=txt)
    lines(Znew,conb4,col="brown")
    text(x=Znew,y=stars.df$consb4,labels=txt)
    legend(lloc,legend=c(Min,Q1,Mean,Q3,Max),col=c("blue","red","forest green","yellow","brown"),lty = c("solid"), cex = 0.5)
    abline(h=0)
  }
}


## Change df name --------------------------------------------------------------
eac <- aid_merged
rm(aid_merged)



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####  EXPLORATORY DATA ANALYSIS  ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 

eac_names <- as.data.frame(variable.names(eac))
View(eac_names)


## Summary/Descriptive Stats Table -----------------------------------------

# select() vars to use in "" function below
descriptive_vars <- eac %>%
  ungroup() %>% 
  select(trendcrop, trendc3, trendcropveg, trendgrass,
         muni_locd, cropsha_pct01, SPI12m_mean, SPI12m_sd,
         upstream, altitude, road0,
         illegal_crops, vio_13,
         farc_zone, vio_15,
         starts_with("pres_"),
         num_range("pol_", 9:18),
         starts_with("c_"),
         starts_with("s_"),
         num_range("vio_", c(1:3, 9:11, 14, 26:28, 31:36, 46:47, 87)),
         vio_31_mean, vio_32_mean, vio_33_mean, vio_34_mean, vio_35_mean,
         vio_36_mean
  )

##  WITH COFFEE VARS
# descriptive_vars <- eac %>%
#   ungroup() %>% 
#   select(trendcrop, trendcropveg, muni_locd, SPI12m_sd,
#          upstream, altitude,
#          illegal_crops, vio_13,
#          pres_pidmuni,
#          starts_with("c_match"),
#          starts_with("s_match"),
#          vio_31_mean, vio_32_mean, vio_34_mean,
#          p_cafe, r_cafe, coffee_2014, coffee_avg
#   )



# "Inf" values in cropsha_pct01 compute descriptive stats properly
descriptive_vars %<>%
  mutate(cropsha_pct01 = if_else(is.infinite(cropsha_pct01),
                                 NA_real_,
                                 cropsha_pct01)
  )


# "overview" = as in overview or summary of vars. can't use summarize
overview <- function(descriptive_vars, stat){
 stat <- enquo(stat)
  
 overview_df <- descriptive_vars %>%
    summarise_all(stat, na.rm = TRUE) %>% 
    gather(key = "name_var", value = !!stat)
}

# df for each descriptive stat
summarized_mean <- overview(descriptive_vars, mean)
summarized_med <- overview(descriptive_vars, median)
summarized_sd <- overview(descriptive_vars, sd)
summarized_min <- overview(descriptive_vars, min)
summarized_max <- overview(descriptive_vars, max)


# "count # of non-NA observations" is not it's own summary function
summarized_n <- descriptive_vars %>%
  map_df(function(x) sum(!is.na(x))) %>%
  gather(key = "name_var", value = "n")


# summary stats table w/ var names and values across the row
summarized_vars <- bind_cols(summarized_n, summarized_mean, summarized_sd,
                             summarized_med, summarized_max, summarized_min)

# create a tidy df w/ no duplicate cols
summarized_vars %<>% select(-num_range("name_var", c(1:5)))

summarized_vars %<>% rename(Variable = name_var, Mean = mean, `Std Dev` = sd,
                            Median = median, Max = max, Min = min)

# require("xtable")
summarized_vars_html <- xtable(summarized_vars, digits = 0) 
# %>%
#   print(type = "html",
#         html.table.attributes = "",
#         include.rownames = FALSE,
#         format.args = list(big.mark = ","))

# require(knitr)
kable(summarized_vars)

## Visualize Key Variables ------------------------------------------------

# eac %>%
#   select(codmuni, starts_with("s_seatpro_")
#   ) %>%
#   View()


## coca presence ----
# replace NA in coca_km2 w/ 0 only for illegal crop == 0 munis
eac %>% count(illegal_crops)
eac %>% count(is.na(coca_km2))
# eac %>% select(codmuni, illegal_crops, coca_km2) %>% filter(illegal_crops == 0 & is.na(coca_km2)) %>% View()

eac %<>%
  mutate(
    coca_km2 = if_else(illegal_crops == 0 & is.na(coca_km2),
                       0,
                       coca_km2
    )
  ) 

class(eac$illegal_crops)
eac %>% select(illegal_crops) %>% count()
eac$illegal_crops <- as.integer(eac$illegal_crops)

# illegal crops (dummy variable)
ggplot(eac) + geom_bar(aes(x = illegal_crops))

# vio_13 (coca presence dummy)
ggplot(eac) + geom_freqpoly(aes(x = vio_13), binwidth = 0.05) # count
ggplot(eac) + geom_histogram(aes(x = vio_13), binwidth = 0.05) # 

eac %>% select(codmuni, vio_13) %>% View()

## AID (muni_locd)
eac$muni_locd <- as.integer(eac$muni_locd)
ggplot(data = eac) + geom_bar(mapping = aes(x = muni_locd))
eac %>% select(muni_locd) %>% count()



## trendcrop ----
ggplot(eac) + geom_freqpoly(aes(x = trendcrop), binwidth = 20)

ggplot(eac) +
  geom_histogram(aes(x = trendcrop), binwidth = 20) +
  xlim(-500, 500)

# outliers
eac %>% select(codmuni, trendcrop) %>% filter(trendcrop > 500)
eac %>% select(codmuni, trendcrop) %>% filter(trendcrop < -500)
eac %>% select(codmuni, department, municipality, trendcrop) %>% filter(codmuni == 44847)

# data w/out the huge outlier (NAs also dropped)
crop_out <- eac %>% filter(trendcrop < 10000)

# w/out outlier
ggplot(data = crop_out, aes(x = trendcrop)) +
  geom_freqpoly(binwidth = 20)

ggplot(crop_out) +
  geom_histogram(aes(x = trendcrop), binwidth = 20) +
  xlim(-500, 500)



## trendcropveg ----
# outliers
eac %>%
  select(codmuni, department, municipality, trendcropveg) %>%
  filter(trendcropveg < -2000)
eac %>%
  select(codmuni, department, municipality, trendcropveg) %>%
  filter(trendcropveg > 4000)

# distribution w/out some outliers
ggplot(eac) +
  geom_histogram(aes(x = trendcropveg), binwidth = 20) +
  xlim(-2000, 4000)

# data w/out cropveg outlier
cropveg_out <- eac %>% filter(trendcropveg < 8000)



## road0 ----



## upstream ----



## match vars: pres_pidmuni ----
eac %>% ungroup() %>% select(pres_pidmuni) %>% count()
ggplot(eac) + geom_histogram(aes(x = pres_pidmuni), bins = 6)


## camara match vars ----
# c_match_munidept
eac %>% select(c_match_munidept) %>% count()
ggplot(eac) + geom_histogram(aes(x = c_match_munidept), bins = 7)

# c_match_presdept
eac %>% select(c_match_presdept) %>% count()
ggplot(eac) + geom_histogram(aes(x = c_match_presdept), bins = 7)

# c_match_munipresdept
eac %>% select(c_match_munideptpres) %>% count()
ggplot(eac) + geom_histogram(aes(x = c_match_munideptpres), bins = 7)

# c_match_none
eac %>% select(c_match_none) %>% count()
ggplot(eac) + geom_histogram(aes(x = c_match_none), bins = 6)



## senado match vars ----
# s_match_muniseat
eac %>% select(s_match_muniseat) %>% count()
ggplot(eac) + geom_histogram(aes(x = s_match_muniseat), bins = 7)

# s_match_presseat
eac %>% select(s_match_presseat) %>% count()
ggplot(eac) + geom_histogram(aes(x = s_match_presseat), bins = 7)

# s_match_munipresseat
eac %>% select(s_match_muniseatpres) %>% count()
ggplot(eac) + geom_histogram(aes(x = s_match_muniseatpres), bins = 7)

# s_match_none
eac %>% select(s_match_none) %>% count()
ggplot(eac) + geom_histogram(aes(x = s_match_none), bins = 6)


## vio_2 ----



## farc_zone & vio_15 ----



## vio_31-33: population exposure ----
# vio_31: exposure to state
ggplot(eac) + geom_histogram(aes(x = vio_31_mean), binwidth = .1)
# vio_32: exposure to guer
ggplot(eac) + geom_histogram(aes(x = vio_32_mean), binwidth = .1)
# vio_33: exposure to guer
ggplot(eac) + geom_histogram(aes(x = vio_33_mean), binwidth = .1)


## vio_34-36: dispute ----
# vio_34: state vs. guerilla conflict
ggplot(eac) + geom_histogram(aes(x = vio_34_mean), binwidth = .1)

# vio_35: state vs. paramilitary/neoparamilitary
ggplot(eac) + geom_histogram(aes(x = vio_35_mean), binwidth = .1)

# vio_36: guer vs. paramilitary/neoparamilitary
ggplot(eac) + geom_histogram(aes(x = vio_36_mean), binwidth = .1)


## vio_46: incidence of conflict ----
ggplot(eac) + geom_bar(aes(x = vio_46))



## p_cafe: coffee production (tons) ----

p_cafe_outlier <- eac %>%
  filter(p_cafe < 10000)

eac %>%
  filter(p_cafe < 10000) %>% 
  ggplot() +
  geom_histogram(aes(x = p_cafe), binwidth = 100)

## r_cafe: coffee production (tons) ----

## coffee_avg: coffee production (tons) ----

## coffee_2014: coffee production (tons) ----




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
    ####   I. CROPSHARE (cropveg & crop): AID + COCA (avg.) +  POL   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 

## Does aid have an effect on cropveg/crop trend?

## CROPVEG
# box plot
ggplot(cropveg_out, aes(muni_locd, trendcropveg, group = muni_locd)) +
  geom_boxplot()

# distribution of receiving aid/not receiving aid
# 1 = No aid, 2 = aid only for the purpose of this graph.
#   - 'muni_locd' usually 0 and 1 (dummy)
#   - need muni_locd as factor to see 2 distributions on same plot
cropveg_out$muni_locd_cat <- as.factor(cropveg_out$muni_locd)

ggplot(cropveg_out) +
  geom_freqpoly(aes(x = trendcropveg, color = muni_locd_cat), binwidth = 20)

ggplot(cropveg_out) +
  geom_freqpoly(aes(x = trendcropveg, y = ..density.., color = muni_locd_cat), binwidth = 20)


## CROP
# boxplot
ggplot(crop_out, aes(muni_locd, trendcrop, group = muni_locd)) +
  geom_boxplot()

crop_out %>% filter(trendcrop < 2500) %>%
  ggplot(., aes(muni_locd, trendcrop, group = muni_locd)) +
  geom_boxplot()

crop_out$muni_locd_cat <- as.factor(crop_out$muni_locd)

ggplot(crop_out) +
  geom_freqpoly(aes(x = trendcrop, color = muni_locd_cat), binwidth = 30)

ggplot(crop_out) +
  geom_freqpoly(aes(x = trendcrop, y = ..density.., color = muni_locd_cat), binwidth = 30)



## models ----
## VARS TO INCLUDE IN MODEL
dep_var <- ~trendcropveg
dep_var2 <- ~trendcrop
base_vars <- ~muni_locd + SPI12m_sd + geo_3 + upstream + altitude + vio_13
interact_vars <- ~muni_locd:vio_13
# political/election vars (camara, senate, president)
pol_vars <- ~c_match_presdept + c_match_munidept + c_match_munideptpres +
  pres_pidmuni + s_match_presseat + s_match_muniseatpres

# set models
# cropveg dv model
linear.model.form <- formulas(.response = dep_var,
                              base = base_vars,
                              interact =  add_predictors(base, interact_vars)
)

##  FOR APPENDIX - W/ POLITICAL VARS
# linear.model.form <- formulas(.response = dep_var,
#                               base = base_vars,
#                               interact =  add_predictors(base, interact_vars),
#                               politics =  add_predictors(base, pol_vars),
#                               full = add_predictors(politics, interact_vars)
# )

# crop dv model
linear.model.form2 <- formulas(.response = dep_var2,
                              base = base_vars,
                              interact =  add_predictors(base, interact_vars)
)

## FOR APPENDIX - W/ POLITICAL VARS
# linear.model.form2 <- formulas(.response = dep_var2,
#                               base = base_vars,
#                               interact =  add_predictors(base, interact_vars),
#                               politics =  add_predictors(base, pol_vars),
#                               full = add_predictors(politics, interact_vars)
# )


## LINEAR MODEL
linear.model <- map(linear.model.form, ~ lm(.x, data = eac, model = TRUE))
# w/out outlier (Uribia in La Guajira)
linear.model.out <- map(linear.model.form, ~ lm(.x, data = cropveg_out, model = TRUE))

linear.model2 <- map(linear.model.form2, ~ lm(.x, data = eac, model = TRUE))
# w/out outlier (Uribia in La Guajira)
linear.model.out2 <- map(linear.model.form2, ~ lm(.x, data = crop_out, model = TRUE))

## REGRESSION OUTPUT
# linear.model %>% map(summary)
# linear.model %>% map(robust)
# linear.model.out %>% map(summary)
# linear.model.out %>% map(robust)
# linear.model2 %>% map(summary)
# linear.model2 %>% map(robust)
# linear.model.out2 %>% map(summary)
# linear.model.out2 %>% map(robust)



## output regression tables ----------------------------------------------------

## FILENAME STRING - CROPVEG 
filename <- "cropveg_aid_coca1_table.doc"
# string for the outlier reg table
filename_out <- "cropveg_aid_coca1_table_out.doc"

## FILENAME STRING - CROP
filename2 <- "crop_aid_coca1_table.doc"
# string for the outlier reg table
filename_out2 <- "crop_aid_coca1_table_out.doc"

## TABLE TITLE - CROPVEG
title <- "Aid's Effect on MODIS Crop Veg Cover"
# title for the outlier table
title_out <- "Aid's Effect on MODIS Crop Veg Cover (without Uribia, La Guajira)"

## TABLE TITLE - CROP
title2 <- "Aid's Effect on MODIS Crop Cover"
title_out2 <- "Aid's Effect on MODIS Crop Cover (without Uribia, La Guajira)"

## RENAME VARS IN TABLE
coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
                  "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
                  "upstream" = "Upstream", "altitude" = "Altitude",
                  "vio_13" = "Coca presence avg",
                  "muni_locd:vio_13" = "Aid * Coca")

## FOR APPENDIX TABLE (W/ POLITICAL VARS)
# coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
#                   "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
#                   "upstream" = "Upstream", "altitude" = "Altitude",
#                   "vio_13" = "Coca presence avg", "c_match_presdept" = NA,
#                   "c_match_munidept" = NA, "c_match_munideptpres" = NA,
#                   "pres_pidmuni" = NA, "s_match_presseat" = NA,
#                   "s_match_muniseatpres" = NA,
#                   "muni_locd:vio_13" = "Aid * Coca")

## NOTE FOR BOTTOM OF TABLE
# %stars puts p-value definitions in Note
note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
variable. Table shows robust standard errors."

note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
variable. Table shows robust standard errors."

## FOR APPENDIX TABLES (W/ POL VARS)
# note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
# variable. 'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
# exclusive categories compared against a category where there is no match
# in ideology. Table shows robust standard errors."
# 
# note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
# variable. 'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
# exclusive categories compared against a category where there is no match
# in ideology. Table shows robust standard errors."


## SAVE REG OUTPUT TABLE W/ ROBUST SEs
setwd(dir.figures.tables)
# table to .doc
robust_table(linear.model, filename = filename, title, coefnames, note,
             latex = FALSE)
# robust_table(linear.model.out, filename = filename_out, title_out, coefnames,
#              note, latex = FALSE)

robust_table(linear.model2, filename = filename2, title2, coefnames, note2,
             latex = FALSE)
# robust_table(linear.model.out2, filename = filename_out2, title_out2, coefnames,
#              note2, latex = FALSE)


## SHOW REG OUTPUT TABLE W/ ROBUST SEs
## table to html code

# cropveg tables
# html_table <-  robust_table(linear.model, filename = NULL, title, coefnames, note,
#                             latex = FALSE)
# html_table
# html_table %>% HTML() %>% browsable() # view the html in R's viewer

# w/out outlier
# html_table_out <- robust_table(linear.model.out, filename = NULL, title_out, coefnames,
#                                note, latex = FALSE)
# html_table_out
# html_table_out %>% HTML() %>% browsable()

# crop tables
# html_table2 <- robust_table(linear.model2, filename = NULL, title2, coefnames, note2,
#                             latex = FALSE)
# html_table2
# html_table2 %>% HTML() %>% browsable()
# 
# html_table2_out <- robust_table(linear.model.out2, filename = NULL, title_out2,
#                                 coefnames, note2, latex = FALSE)
# html_table2_out
# html_table2_out %>% HTML() %>% browsable()


# # TIDY, BROOM, GLANCE
# linear.model
# glance(linear.model[[4]])
# tidy(linear.model[[4]])


# interaction plots ----

## CROPVEG PLOTS
# aid*coca
interplot(m = linear.model[[2]], var1 = "muni_locd", var2 = "vio_13",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover (in Ha)") + xlab("Avg. Coca Presence") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (1993-2015)")

setwd(dir.figures.tables)
ggsave("cropveg_aid-coca1.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = linear.model[[4]], var1 = "muni_locd", var2 = "vio_13",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover (in Ha)") + xlab("Avg. Coca Presence") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (1993-2015)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("cropveg_aid-coca1_elecs.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)


## CROP PLOTS
title_ylab_crop <- "Aid's Effect on Crop Cover (in Ha)"

# aid*coca
interplot(m = linear.model2[[2]], var1 = "muni_locd", var2 = "vio_13",
          hist = TRUE) +
  ylab(title_ylab_crop) + xlab("Avg. Coca Presence") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (1993-2015)")

setwd(dir.figures.tables)
ggsave("crop_aid-coca1.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = linear.model2[[4]], var1 = "muni_locd", var2 = "vio_13",
          hist = TRUE) +
  ylab(title_ylab_crop) + xlab("Avg. Coca Presence") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (1993-2015)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("crop_aid-coca1_elecs.pdf", plot = last_plot(), device = "pdf",
       width = 7, height = 7, units = "in", dpi = 300)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   II. CROPSHARE: AID + COCA (dummy) + POL   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
# replicate above but w/ illegal crop dummy instead of avg.

## models ---------------------------------------------------------------------
# 1) base model: cropshare = aid + coca
# 2) base model + aid*coca
# 3) base model + pol vars (_match_none dropped out!)
# 4) base model + aid*coca + pol vars

## VARS TO INCLUDE IN MODEL
base_vars <- ~muni_locd + SPI12m_sd + geo_3 + upstream + altitude + illegal_crops
interact_vars <- ~muni_locd:illegal_crops

# set models
linear.model.form <- formulas(.response = dep_var,
                              base = base_vars,
                              interact =  add_predictors(base, interact_vars)
)

## FOR APPENDIX (W/ POL VARS)
# linear.model.form <- formulas(.response = dep_var,
#                               base = base_vars,
#                               interact =  add_predictors(base, interact_vars),
#                               politics =  add_predictors(base, pol_vars),
#                               full = add_predictors(politics, interact_vars)
# )

linear.model.form2 <- formulas(.response = dep_var2,
                              base = base_vars,
                              interact =  add_predictors(base, interact_vars)
)

## FOR APPENDIX (W/ POL VARS)
# linear.model.form2 <- formulas(.response = dep_var2,
#                               base = base_vars,
#                               interact =  add_predictors(base, interact_vars),
#                               politics =  add_predictors(base, pol_vars),
#                               full = add_predictors(politics, interact_vars)
# )


## LINEAR MODEL
linear.model <- map(linear.model.form, ~ lm(.x, data = eac, model = TRUE))
# w/out outlier (Uribia in La Guajira)
# linear.model.out <- map(linear.model.form, ~ lm(.x, data = cropveg_out, model = TRUE))


linear.model2 <- map(linear.model.form2, ~ lm(.x, data = eac, model = TRUE))
# w/out outlier (Uribia in La Guajira)
# linear.model.out2 <- map(linear.model.form2, ~ lm(.x, data = crop_out, model = TRUE))

## REGRESSION OUTPUT
# linear.model %>% map(summary)
# linear.model %>% map(robust)
# linear.model.out %>% map(summary)
# linear.model.out %>% map(robust)
# linear.model2 %>% map(summary)
# linear.model2 %>% map(robust)
# linear.model.out2 %>% map(summary)
# linear.model.out2 %>% map(robust)



## output regression tables ----------------------------------------------------

## FILENAME STRING
filename <- "cropveg_aid_coca2_table.doc"
# string for the outlier reg table
filename_out <- "cropveg_aid_coca2_table_out.doc"

## FILENAME STRING
filename2 <- "crop_aid_coca2_table.doc"
# string for the outlier reg table
filename_out2 <- "crop_aid_coca2_table_out.doc"

## RENAME VARS IN TABLE
coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
                  "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
                  "upstream" = "Upstream", "altitude" = "Altitude",
                  "illegal_crops" = "Coca Presence (1 = Yes)",
                  "muni_locd:illegal_crops" = "Aid * Coca")

## FOR APPENDIX (W/ POL VARS)
# coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
#                   "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
#                   "upstream" = "Upstream", "altitude" = "Altitude",
#                   "illegal_crops" = "Coca Presence (1 = Yes)",
#                   "c_match_presdept" = NA, "c_match_munidept" = NA,
#                   "c_match_munideptpres" = NA, "pres_pidmuni" = NA,
#                   "s_match_presseat" = NA, "s_match_muniseatpres" = NA,
#                   "muni_locd:illegal_crops" = "Aid * Coca")


## NOTE FOR BOTTOM OF TABLE
# %stars puts p-value definitions in Note
note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
variable. 'Coca Presence' is a dummy variable counting measured coca growth
in 2013. Table shows robust standard errors."

note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
variable. 'Coca Presence' is a dummy variable counting measured coca growth
in 2013. Table shows robust standard errors."

## FOR APPENDIX (W/ POL VARS):
# note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
# variable. 'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
# exclusive categories compared against a category where there is no match
# in ideology. 'Coca Presence' is a dummy variable counting measured coca growth
# in 2013. Table shows robust standard errors."
# 
# note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
# variable. 'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
# exclusive categories compared against a category where there is no match
# in ideology. 'Coca Presence' is a dummy variable counting measured coca growth
# in 2013. Table shows robust standard errors."


## SAVE REG OUTPUT TABLE W/ ROBUST SEs
setwd(dir.figures.tables)
## table to .doc

# cropveg tables
robust_table(linear.model, filename = filename, title, coefnames, note,
             latex = FALSE)
robust_table(linear.model.out, filename = filename_out, title_out, coefnames,
             note, latex = FALSE)

# crop tables
robust_table(linear.model2, filename = filename2, title2, coefnames, note2,
             latex = FALSE)
robust_table(linear.model.out2, filename = filename_out2, title_out2, coefnames,
             note2, latex = FALSE)


# table html code
# testhtml <- robust_table(linear.model, filename = NULL, title, coefnames, note, latex = FALSE)
# testhtml %>% HTML() %>% browsable() # view the html in R's viewer
# table to latex
# robust_table(linear.model, filename = NULL, title, coefnames, note, latex = TRUE)


## interaction plots ------------------------------------------------------------

## CROPVEG PLOTS
# aid*coca
interplot(m = linear.model[[2]], var1 = "muni_locd", var2 = "illegal_crops",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover (in Ha)") + xlab("Coca Presence (1 = yes)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (2013)")

setwd(dir.figures.tables)
ggsave("cropveg_aid-coca2.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = linear.model[[4]], var1 = "muni_locd", var2 = "illegal_crops",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover") + xlab("Coca Presence (1 = yes)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (2013)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("cropveg_aid-coca2_elecs.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)


## CROP PLOTS

# aid*coca
interplot(m = linear.model2[[2]], var1 = "muni_locd", var2 = "illegal_crops",
          hist = TRUE) +
  ylab(title_ylab_crop) + xlab("Coca Presence (1 = yes)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (2013)")

setwd(dir.figures.tables)
ggsave("crop_aid-coca2.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = linear.model2[[4]], var1 = "muni_locd", var2 = "illegal_crops",
          hist = TRUE) +
  ylab(title_ylab_crop) + xlab("Coca Presence (1 = yes)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (2013)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("crop_aid-coca2_elecs.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   III. CROPSHARE: AID + STATE-GUER CONF + POL   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 

# 1) base model: cropshare = aid + state_guer conflict
# 2) base model + aid*state-guer
# 3) base model + pol vars
# 4) base model + pol vars + aid*state_guer

## models ----------------------------------------------------------------------

## VARS TO INCLUDE IN MODEL
base_vars <- ~muni_locd + SPI12m_sd + geo_3 + upstream + altitude +
              vio_34_mean + vio_31_mean + vio_32_mean
interact_vars <- ~muni_locd:vio_34_mean

# set models
linear.model.form <- formulas(.response = dep_var,
                              base = base_vars,
                              interact =  add_predictors(base, interact_vars)
)

## FOR APPENDIX (W/ POL VARS):
# linear.model.form <- formulas(.response = dep_var,
#                               base = base_vars,
#                               interact =  add_predictors(base, interact_vars),
#                               politics =  add_predictors(base, pol_vars),
#                               full = add_predictors(politics, interact_vars)
# )

linear.model.form2 <- formulas(.response = dep_var2,
                              base = base_vars,
                              interact =  add_predictors(base, interact_vars)
)

## FOR APPENDIX (W/ POL VARS):
# linear.model.form2 <- formulas(.response = dep_var2,
#                               base = base_vars,
#                               interact =  add_predictors(base, interact_vars),
#                               politics =  add_predictors(base, pol_vars),
#                               full = add_predictors(politics, interact_vars)
# )

## LINEAR MODEL
linear.model <- map(linear.model.form, ~ lm(.x, data = eac, model = TRUE))
# w/out outlier (Uribia in La Guajira)
linear.model.out <- map(linear.model.form, ~ lm(.x, data = cropveg_out, model = TRUE))
linear.model2 <- map(linear.model.form2, ~ lm(.x, data = eac, model = TRUE))
linear.model.out2 <- map(linear.model.form2, ~ lm(.x, data = crop_out, model = TRUE))

## REGRESSION OUTPUT
# linear.model %>% map(summary)
# linear.model %>% map(robust)
# linear.model.out %>% map(summary)
# linear.model.out %>% map(robust)
# linear.model2 %>% map(summary)
# linear.model2 %>% map(robust)
# linear.model.out2 %>% map(summary)
# linear.model.out2 %>% map(robust)



## output regression tables ----------------------------------------------------

## FILENAME STRING
filename <- "cropveg_aid_dispute_table.doc"
# string for the outlier reg table
filename_out <- "cropveg_aid_dispute_table_out.doc"

filename2 <- "crop_aid_dispute_table.doc"
# string for the outlier reg table
filename_out2 <- "crop_aid_dispute_table_out.doc"

## RENAME VARS IN TABLE
coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
                  "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
                  "upstream" = "Upstream", "altitude" = "Altitude",
                  "vio_34_mean" = "State-Guerilla Dispute",
                  "vio_31_mean" = "Exposure to State",
                  "vio_32_mean" = "Exposure to Guerillas",
                  "muni_locd:vio_34_mean" = "Aid * State-Guer Dispute")

## FOR APPENDIX (W/ POL VARS):
# coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
#                   "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
#                   "upstream" = "Upstream", "altitude" = "Altitude",
#                   "vio_34_mean" = "State-Guerilla Dispute",
#                   "vio_31_mean" = "Exposure to State",
#                   "vio_32_mean" = "Exposure to Guerillas",
#                   "c_match_presdept" = NA, "c_match_munidept" = NA,
#                   "c_match_munideptpres" = NA, "pres_pidmuni" = NA,
#                   "s_match_presseat" = NA, "s_match_muniseatpres" = NA,
#                   "muni_locd:vio_34_mean" = "Aid * State-Guer Dispute")


## NOTE FOR BOTTOM OF TABLE
# %stars puts p-value definitions in Note
note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
variable. 'State-Guerilla Dispute' = the proportion of years (1998-2009) that 
government and guerilla forces have a dispute in a given municipality. 
'Exposure to State' = proportion of years (1998-2009) that the non-combatant 
population comes in contact with government forces. 'Exposure to Guerilla' = 
the same definition, but for guerilla forces. Table shows robust standard 
errors."

note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
variable. 'State-Guerilla Dispute' = the
proportion of years (1998-2009) that government and guerilla forces have a
dispute in a given municipality. 'Exposure to State' = proportion of years
(1998-2009) that the non-combatant population comes in contact with government
forces. 'Exposure to Guerilla' = the same definition, but for guerilla forces.
Table shows robust standard errors."

## FOR APPENDIX (W/ POL VARS):
# note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
# variable. 'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
# exclusive categories compared against a category where there is no match
# in ideology. 'State-Guerilla Dispute' = the
# proportion of years (1998-2009) that government and guerilla forces have a
# dispute in a given municipality. 'Exposure to State' = proportion of years
# (1998-2009) that the non-combatant population comes in contact with government
# forces. 'Exposure to Guerilla' = the same definition, but for guerilla forces.
# Table shows robust standard errors."
# 
# note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
# variable. 'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
# exclusive categories compared against a category where there is no match
# in ideology. 'State-Guerilla Dispute' = the
# proportion of years (1998-2009) that government and guerilla forces have a
# dispute in a given municipality. 'Exposure to State' = proportion of years
# (1998-2009) that the non-combatant population comes in contact with government
# forces. 'Exposure to Guerilla' = the same definition, but for guerilla forces.
# Table shows robust standard errors."

## SAVE REG OUTPUT TABLE W/ ROBUST SEs
setwd(dir.figures.tables)

## table to .doc

# cropveg tables
robust_table(linear.model, filename = filename, title, coefnames, note,
             latex = FALSE)
robust_table(linear.model.out, filename = filename_out, title_out, coefnames,
             note, latex = FALSE)

# crop tables
robust_table(linear.model2, filename = filename2, title2, coefnames, note2,
             latex = FALSE)
robust_table(linear.model.out2, filename = filename_out2, title_out2, coefnames,
             note2, latex = FALSE)



## interaction plots -----------------------------------------------------------

## CROPVEG PLOTS
# aid*coca
interplot(m = linear.model[[2]], var1 = "muni_locd", var2 = "vio_34_mean",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover (in Ha)") +
  xlab("Proportion of State-Guerilla Disputes (1998-2009)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by the Proportion of Disputes (1998-2009)")

setwd(dir.figures.tables)
ggsave("cropveg_aid-dispute.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = linear.model[[4]], var1 = "muni_locd", var2 = "vio_34_mean",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover") +
  xlab("Proportion of State-Guerilla Disputes (1998-2009)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by the Proportion of Disputes (1998-2009)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("cropveg_aid-dispute_elecs.pdf", plot = last_plot(), device = "pdf",
       width = 7, height = 7, units = "in", dpi = 300)


## CROP PLOTS
# aid*coca
interplot(m = linear.model2[[2]], var1 = "muni_locd", var2 = "vio_34_mean",
          hist = TRUE) +
  ylab(title_ylab_crop) +
  xlab("Proportion of State-Guerilla Disputes (1998-2009)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by the Proportion of Disputes (1998-2009)")

setwd(dir.figures.tables)
ggsave("crop_aid-dispute.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = linear.model2[[4]], var1 = "muni_locd", var2 = "vio_34_mean",
          hist = TRUE) +
  ylab(title_ylab_crop) +
  xlab("Proportion of State-Guerilla Disputes (1998-2009)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by the Proportion of Disputes (1998-2009)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("crop_aid-dispute_elecs.pdf", plot = last_plot(), device = "pdf",
       width = 7, height = 7, units = "in", dpi = 300)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   IV. CROPSHARE: AID + CONF INCIDENCE/INTENSITY + POL   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
# 1) base model: cropshare = aid + conflict incidence
# 2) base model + aid*conflict-incidence
# 3) base model + pol vars
# 4) base model + pol vars + aid*conflict-incidence

## models ----------------------------------------------------------------------

## VARS TO INCLUDE IN MODEL
base_vars <- ~muni_locd + SPI12m_sd + geo_3 + upstream + altitude +
  vio_46 + vio_31_mean + vio_32_mean
interact_vars <- ~muni_locd:vio_46

# set models
linear.model.form <- formulas(.response = dep_var,
                              base = base_vars,
                              interact =  add_predictors(base, interact_vars)
)

## FOR APPENDIX (W/ POL VARS):
# linear.model.form <- formulas(.response = dep_var,
#                               base = base_vars,
#                               interact =  add_predictors(base, interact_vars),
#                               politics =  add_predictors(base, pol_vars),
#                               full = add_predictors(politics, interact_vars)
# )


linear.model.form2 <- formulas(.response = dep_var2,
                              base = base_vars,
                              interact =  add_predictors(base, interact_vars)
)

## FOR APPENDIX (W/ POL VARS):
# linear.model.form2 <- formulas(.response = dep_var2,
#                               base = base_vars,
#                               interact =  add_predictors(base, interact_vars),
#                               politics =  add_predictors(base, pol_vars),
#                               full = add_predictors(politics, interact_vars)
# )


## LINEAR MODEL
# cropveg
linear.model <- map(linear.model.form, ~ lm(.x, data = eac, model = TRUE))
# w/out outlier (Uribia in La Guajira)
linear.model.out <- map(linear.model.form, ~ lm(.x, data = cropveg_out, model = TRUE))

# crop
linear.model2 <- map(linear.model.form2, ~ lm(.x, data = eac, model = TRUE))
linear.model.out2 <- map(linear.model.form2, ~ lm(.x, data = crop_out, model = TRUE))

## REGRESSION OUTPUT
# linear.model %>% map(summary)
# linear.model %>% map(robust)
# linear.model.out %>% map(summary)
# linear.model.out %>% map(robust)
# linear.model2 %>% map(summary)
# linear.model2 %>% map(robust)
# linear.model.out2 %>% map(summary)
# linear.model.out2 %>% map(robust)



## output regression tables ----------------------------------------------------

## FILENAME STRING
filename <- "cropveg_aid_incidence_table.doc"
# string for the outlier reg table
filename_out <- "cropveg_aid_incidence_table_out.doc"

filename2 <- "crop_aid_incidence_table.doc"
# string for the outlier reg table
filename_out2 <- "crop_aid_incidence_table_out.doc"

## RENAME VARS IN TABLE
coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
                  "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
                  "upstream" = "Upstream", "altitude" = "Altitude",
                  "vio_46" = "Conflict Incidence",
                  "vio_31_mean" = "Exposure to State",
                  "vio_32_mean" = "Exposure to Guerillas",
                  "muni_locd:vio_46" = "Aid * Conflict Incidence")

## FOR APPENDIX (W/ POL VARS):
# coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
#                   "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
#                   "upstream" = "Upstream", "altitude" = "Altitude",
#                   "vio_46" = "Conflict Incidence",
#                   "vio_31_mean" = "Exposure to State",
#                   "vio_32_mean" = "Exposure to Guerillas",
#                   "c_match_presdept" = NA, "c_match_munidept" = NA,
#                   "c_match_munideptpres" = NA, "pres_pidmuni" = NA,
#                   "s_match_presseat" = NA, "s_match_muniseatpres" = NA,
#                   "muni_locd:vio_46" = "Aid * Conflict Incidence")


## NOTE FOR BOTTOM OF TABLE
# %stars puts p-value definitions in Note
note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
variable. 'Conflict Incidence' takes on values 1-5 moving from low levels of
conflict occurence to high levels of conflict occurence. 'Exposure to State' = 
proportion of years (1998-2009) that the non-combatant population comes in 
contact with government forces. 'Exposure to Guerilla' = the same definition, 
but for guerilla forces. Table shows robust SEs."

note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
variable. 'Conflict Incidence' takes on values 1-5 moving from low levels of
conflict occurence to high levels of conflict occurence. 'Exposure to State' = 
proportion of years (1998-2009) that the non-combatant population comes in
contact with government forces. 'Exposure to Guerilla' = the same definition,
but for guerilla forces. Table shows robust SEs."

## FOR APPENDIX (W/ POL VARS):
# note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
# variable. 'Conflict Incidence' takes on values 1-5 moving from low levels of
# conflict occurence to high levels of conflict occurence. 'c_match', 's_match',
# and 'pres_pidmuni' variables indicate mutually exclusive categories compared
# against a category where there is no match in ideology. 'Exposure to State' = 
# proportion of years (1998-2009) that the non-combatant population comes in
# contact with government forces. 'Exposure to Guerilla' = the same definition,
# but for guerilla forces. Table shows robust SEs."
# 
# note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
# variable. 'Conflict Incidence' takes on values 1-5 moving from low levels of
# conflict occurence to high levels of conflict occurence. 'c_match', 's_match',
# and 'pres_pidmuni' variables indicate mutually exclusive categories compared
# against a category where there is no match in ideology. 'Exposure to State' = 
# proportion of years (1998-2009) that the non-combatant population comes in
# contact with government forces. 'Exposure to Guerilla' = the same definition,
# but for guerilla forces. Table shows robust SEs."

## SAVE REG OUTPUT TABLE W/ ROBUST SEs
setwd(dir.figures.tables)

## table to .doc
# cropveg tables
robust_table(linear.model, filename = filename, title, coefnames, note,
             latex = FALSE)
robust_table(linear.model.out, filename = filename_out, title_out, coefnames,
             note, latex = FALSE)

# crop tables
robust_table(linear.model2, filename = filename2, title2, coefnames, note2,
             latex = FALSE)
robust_table(linear.model.out2, filename = filename_out2, title_out2, coefnames,
             note2, latex = FALSE)




## interaction plots -----------------------------------------------------------


## CROPVEG PLOTS
# aid*coca
interplot(m = linear.model[[2]], var1 = "muni_locd", var2 = "vio_46",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover (in Ha)") + 
  xlab("Conflict Incidence Index (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Conflict Incidence (2002-2013)")

setwd(dir.figures.tables)
ggsave("cropveg_aid-incidence.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = linear.model[[4]], var1 = "muni_locd", var2 = "vio_46",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover (in Ha)") + 
  xlab("Conflict Incidence Index (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Conflict Incidence (2002-2013)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("cropveg_aid-incidence_elecs.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)


## CROP PLOTS
interplot(m = linear.model2[[2]], var1 = "muni_locd", var2 = "vio_46",
          hist = TRUE) +
  ylab(title_ylab_crop) + xlab("Conflict Incidence Index (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Conflict Incidence (2002-2013)")

setwd(dir.figures.tables)
ggsave("crop_aid-incidence.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = linear.model2[[4]], var1 = "muni_locd", var2 = "vio_46",
          hist = TRUE) +
  ylab(title_ylab_crop) + xlab("Conflict Incidence Index (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Conflict Incidence (2002-2013)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("crop_aid-incidence_elecs.pdf", plot = last_plot(), device = "pdf",
       width = 7, height = 7, units = "in", dpi = 300)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   V. CROPSHARE: AID + STATE-GUER CONF + POL + COCA    ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 

## model -----------------------------------------------------------------------

## VARS TO INCLUDE IN MODEL
base_vars <- ~muni_locd + SPI12m_sd + geo_3 + upstream + altitude +
  vio_34_mean + vio_31_mean + vio_32_mean + vio_13
interact_vars <- ~muni_locd + vio_34_mean + vio_13 + SPI12m_sd + geo_3 +
  upstream + altitude + vio_31_mean + vio_32_mean +
  muni_locd:vio_34_mean + muni_locd:vio_13 + vio_34_mean:vio_13 +
  muni_locd:vio_34_mean:vio_13

# set models
linear.model.form <- formulas(.response = dep_var,
                              base = base_vars,
                              interact =  interact_vars
)

## FOR APPENDIX (W/ POL VARS):
# linear.model.form <- formulas(.response = dep_var,
#                               base = base_vars,
#                               interact =  add_predictors(base, interact_vars),
#                               politics =  add_predictors(base, pol_vars),
#                               full = add_predictors(politics, interact_vars)
# )

linear.model.form2 <- formulas(.response = dep_var2,
                              base = base_vars,
                              interact =  interact_vars
)

## FOR APPENDIX (W/ POL VARS):
# linear.model.form2 <- formulas(.response = dep_var2,
#                               base = base_vars,
#                               interact =  add_predictors(base, interact_vars),
#                               politics =  add_predictors(base, pol_vars),
#                               full = add_predictors(politics, interact_vars)
# )


## LINEAR MODEL
# cropveg
linear.model <- map(linear.model.form, ~ lm(.x, data = eac, model = TRUE))
# w/out outlier (Uribia in La Guajira)
linear.model.out <- map(linear.model.form, ~ lm(.x, data = cropveg_out, model = TRUE))

# crop
linear.model2 <- map(linear.model.form2, ~ lm(.x, data = eac, model = TRUE))
linear.model.out2 <- map(linear.model.form2, ~ lm(.x, data = crop_out, model = TRUE))

## REGRESSION OUTPUT
# linear.model %>% map(summary)
# linear.model %>% map(robust)
# linear.model.out %>% map(summary)
# linear.model.out %>% map(robust)
# linear.model2 %>% map(summary)
# linear.model2 %>% map(robust)
# linear.model.out2 %>% map(summary)
# linear.model.out2 %>% map(robust)


## ONLY THE VARS AND THEIR INTERACTIONS
triple_effect <- lm(trendcropveg ~ muni_locd + vio_34_mean + vio_13 +
                      muni_locd:vio_34_mean + muni_locd:vio_13 +
                      vio_34_mean:vio_13 + muni_locd:vio_34_mean:vio_13,
                      data = eac)

triple_effect2 <- lm(trendcrop ~ muni_locd + vio_34_mean + vio_13 +
                      muni_locd:vio_34_mean + muni_locd:vio_13 +
                      vio_34_mean:vio_13 + muni_locd:vio_34_mean:vio_13,
                      data = eac)



## output regression tables ----------------------------------------------------

## FILENAME STRING
filename <- "cropveg_aid-coca-dispute_table.doc"
# string for the outlier reg table
filename_out <- "cropveg_aid-coca-dispute_table_out.doc"

filename2 <- "crop_aid-coca-dispute_table.doc"
# string for the outlier reg table
filename_out2 <- "crop_aid-coca-dispute_table_out.doc"

## RENAME VARS IN TABLE
coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
                  "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
                  "upstream" = "Upstream", "altitude" = "Altitude",
                  "vio_46" = "Conflict Incidence",
                  "vio_31_mean" = "Exposure to State",
                  "vio_32_mean" = "Exposure to Guerillas",
                  "vio_13" = "Coca Presence Avg",
                  "muni_locd:vio_34_mean" = "Aid * Dispute",
                  "muni_locd:vio_13" = "Aid * Coca",
                  "vio_34_mean:vio_13" = "Dispute * Coca",
                  "muni_locd:vio_34_mean:vio_13" = "Aid * Dispute * Coca")

## FOR APPENDIX (W/ POL VARS):
# coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
#                   "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
#                   "upstream" = "Upstream", "altitude" = "Altitude",
#                   "vio_46" = "Conflict Incidence",
#                   "vio_31_mean" = "Exposure to State",
#                   "vio_32_mean" = "Exposure to Guerillas",
#                   "vio_13" = "Coca Presence Avg",
#                   "c_match_presdept" = NA, "c_match_munidept" = NA,
#                   "c_match_munideptpres" = NA, "pres_pidmuni" = NA,
#                   "s_match_presseat" = NA, "s_match_muniseatpres" = NA,
#                   "muni_locd:vio_34_mean:vio_13" = "Aid * Dispute * Coca")


## NOTE FOR BOTTOM OF TABLE
# %stars puts p-value definitions in Note
note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
variable. 'State-Guerilla Dispute' = the proportion of years (1998-2009) that
government and guerilla forces have a dispute in a given municipality. 
'Exposure to State' = proportion of years (1998-2009) that the non-combatant
population comes in contact with government forces. 'Exposure to Guerilla' = the 
same definition, but for guerilla forces. Table shows robust SEs."

note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
variable. 'State-Guerilla Dispute' = the proportion of years (1998-2009) that
government and guerilla forces have a dispute in a given municipality. 
'Exposure to State' = proportion of years (1998-2009) that the non-combatant
population comes in contact with government forces. 'Exposure to Guerilla' = the 
same definition, but for guerilla forces. Table shows robust SEs."


## FOR APPENDIX (W/ POL VARS):
# note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
# variable. 'State-Guerilla Dispute' = the proportion of years (1998-2009) that
# government and guerilla forces have a dispute in a given municipality. 
# 'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually exclusive
# categories compared against a category where there is no match in ideology. 
# 'Exposure to State' = proportion of years (1998-2009) that the non-combatant
# population comes in contact with government forces. 'Exposure to Guerilla' = the 
# same definition, but for guerilla forces. Table shows robust SEs."
# 
# note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
# variable. 'State-Guerilla Dispute' = the proportion of years (1998-2009) that
# government and guerilla forces have a dispute in a given municipality. 
# 'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually exclusive
# categories compared against a category where there is no match in ideology. 
# 'Exposure to State' = proportion of years (1998-2009) that the non-combatant
# population comes in contact with government forces. 'Exposure to Guerilla' = the 
# same definition, but for guerilla forces. Table shows robust SEs."


## SAVE REG OUTPUT TABLE W/ ROBUST SEs
setwd(dir.figures.tables)

## table to .doc
# cropveg tables
robust_table(linear.model, filename = filename, title, coefnames,
             note, latex = FALSE)
robust_table(linear.model.out, filename = filename_out, title_out, coefnames,
             note, latex = FALSE)

# crop tables
robust_table(linear.model2, filename = filename2, title2, coefnames, note2,
             latex = FALSE)
robust_table(linear.model.out2, filename = filename_out2, title_out2, coefnames,
             note2, latex = FALSE)



## interaction plots -----------------------------------------------------------

## HOLDING DISPUTE AT DIFFERENT LEVELS + CHANGING COCA
# only key vars + interactions

# CROPVEG PLOTS
ThreewayME.f(M = triple_effect,
             X = eac$muni_locd,
             Z = eac$vio_13,
             W = eac$vio_34_mean,
             xlab = "Proportion of Years w/ Coca Presence (1993-2015)",
             ylab = "Aid's Effect on Crop Veg Cover (in Ha)",
             lloc = "bottomright",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob,
             hist
            )

# base model + interactions
ThreewayME.f(M = linear.model[[2]],
             X = eac$muni_locd,
             Z = eac$vio_13,
             W = eac$vio_34_mean,
             xlab = "Proportion of Years w/ Coca Presence (1993-2015)",
             ylab = "Aid's Effect on Crop Veg Cover (in Ha)",
             lloc = "topleft",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob,
             hist
            )
# setwd(dir.figures.tables)
# ggsave("cropveg_aid-coca_dispute-levs.pdf", plot = last_plot(), device = "pdf",
#        width = 7, height = 7, units = "in", dpi = 300)


# full model
ThreewayME.f(M = linear.model[[4]],
             X = eac$muni_locd,
             Z = eac$vio_13,
             W = eac$vio_34_mean,
             xlab = "Proportion of Years w/ Coca Presence (1993-2015)",
             ylab = "Aid's Effect on Crop Veg Cover",
             lloc = "bottomright",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob,
             hist
            )
# setwd(dir.figures.tables)
# ggsave("cropveg_aid-coca_dispute-levs_elecs.pdf", plot = last_plot(),
#        device = "pdf", width = 7, height = 7, units = "in", dpi = 300)


# CROP PLOTS
ThreewayME.f(M = triple_effect2,
             X = eac$muni_locd,
             Z = eac$vio_13,
             W = eac$vio_34_mean,
             xlab = "Proportion of Years w/ Coca Presence (1993-2015)",
             ylab = title_ylab_crop,
             lloc = "bottomright",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob,
             hist
            )
# setwd(dir.figures.tables)
# ggsave("crop_aid-coca_dispute-levs_simple.pdf", plot = last_plot(),
#        device = "pdf", width = 7, height = 7, units = "in", dpi = 300)


# base model + interactions
ThreewayME.f(M = linear.model2[[2]],
             X = eac$muni_locd,
             Z = eac$vio_13,
             W = eac$vio_34_mean,
             xlab = "Proportion of Years w/ Coca Presence (1993-2015)",
             ylab = title_ylab_crop,
             lloc = "topleft",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob,
             hist
            )
# setwd(dir.figures.tables)
# ggsave("crop_aid-coca_dispute-levs.pdf", plot = last_plot(), device = "pdf",
#        width = 7, height = 7, units = "in", dpi = 300)


# full model
ThreewayME.f(M = linear.model2[[4]],
             X = eac$muni_locd,
             Z = eac$vio_13,
             W = eac$vio_34_mean,
             xlab = "Proportion of Years w/ Coca Presence (1993-2015)",
             ylab = title_ylab_crop,
             lloc = "bottomright",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob,
             hist
            )
# setwd(dir.figures.tables)
# ggsave("crop_aid-coca_dispute-levs_elecs.pdf", plot = last_plot(),
#        device = "pdf", width = 7, height = 7, units = "in", dpi = 300)



## HOLDING COCA PROPORTION @ DIFFERENT LEVELS + CHANGING DISPUTE

# CROPVEG PLOTS
# only key vars + interactions
ThreewayME.f(M = triple_effect,
             X = eac$muni_locd,
             Z = eac$vio_34_mean,
             W = eac$vio_13,
             xlab = "Proportion of Years w/ State-Guerilla Conflict (1998-2012)",
             ylab = "Aid's Effect on Crop Veg Cover (in Ha)",
             lloc = "topleft",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob,
             hist
            )
# setwd(dir.figures.tables)
# ggsave("cropveg_aid-dispute_coca-levs_simple.pdf", plot = last_plot(),
#        device = "pdf", width = 7, height = 7, units = "in", dpi = 300)


# base model + interactions
ThreewayME.f(M = linear.model[[2]],
             X = eac$muni_locd,
             Z = eac$vio_34_mean,
             W = eac$vio_13,
             xlab = "Proportion of Years w/ State-Guerilla Conflict (1998-2012)",
             ylab = "Aid's Effect on Crop Veg Cover (in Ha)",
             lloc = "topleft",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob,
             hist
            )
# setwd(dir.figures.tables)
# ggsave("cropveg_aid-dispute_coca-levs.pdf", plot = last_plot(),
#        device = "pdf", width = 7, height = 7, units = "in", dpi = 300)

# full model
ThreewayME.f(M = linear.model[[4]],
             X = eac$muni_locd,
             Z = eac$vio_34_mean,
             W = eac$vio_13,
             xlab = "Proportion of Years w/ State-Guerilla Conflict (1998-2012)",
             ylab = "Aid's Effect on Crop Veg Cover",
             lloc = "topleft",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob,
             hist
            )
setwd(dir.figures.tables)
ggsave("cropveg_aid-dispute_coca-levs_elecs.pdf", plot = last_plot(),
       device = "pdf", width = 7, height = 7, units = "in", dpi = 300)


# CROP PLOTS
# only key vars + interactions
ThreewayME.f(M = triple_effect2,
             X = eac$muni_locd,
             Z = eac$vio_34_mean,
             W = eac$vio_13,
             xlab = "Proportion of Years w/ State-Guerilla Conflict (1998-2012)",
             ylab = title_ylab_crop,
             lloc = "topleft",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob,
             hist
            )
# setwd(dir.figures.tables)
# ggsave("crop_aid-dispute_coca-levs_simple.pdf", plot = last_plot(),
#        device = "pdf", width = 7, height = 7, units = "in", dpi = 300)


# base model + interactions
ThreewayME.f(M = linear.model2[[2]],
             X = eac$muni_locd,
             Z = eac$vio_34_mean,
             W = eac$vio_13,
             xlab = "Proportion of Years w/ State-Guerilla Conflict (1998-2012)",
             ylab = title_ylab_crop,
             lloc = "topleft",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob,
             hist
            )
# setwd(dir.figures.tables)
# ggsave("crop_aid-dispute_coca-levs.pdf", plot = last_plot(), device = "pdf",
#        width = 7, height = 7, units = "in", dpi = 300)

# full model
ThreewayME.f(M = linear.model2[[4]],
             X = eac$muni_locd,
             Z = eac$vio_34_mean,
             W = eac$vio_13,
             xlab = "Proportion of Years w/ State-Guerilla Conflict (1998-2012)",
             ylab = title_ylab_crop,
             lloc = "topleft",
             Min = "Min Value",
             Q1 = "First Quantile",
             Mean = "Mean",
             Q3 = "Third Quantile",
             Max = "Max Value",
             level = 95,
             rob,
             hist
            )
# setwd(dir.figures.tables)
# ggsave("crop_aid-dispute_coca-levs_elecs.pdf", plot = last_plot(),
#        device = "pdf", width = 7, height = 7, units = "in", dpi = 300)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   VI. CROPSHARE: AID + POP EXPOSURE + POL + COCA    ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 

## model -----------------------------------------------------------------------

## VARS TO INCLUDE IN MODEL
base_vars <- ~muni_locd + SPI12m_sd + geo_3 + upstream + altitude +
  vio_31_mean + vio_32_mean + vio_13
interact_vars <- ~muni_locd:vio_31_mean
interact_vars2 <- ~muni_locd:vio_32_mean


# set models
linear.model.form <- formulas(.response = dep_var,
                              base = base_vars,
                              interact =  add_predictors(base, interact_vars),
                              interact2 = add_predictors(base, interact_vars2)
                      )

## FOR APPENDIX (W/ POL VARS):
# linear.model.form <- formulas(.response = dep_var,
#                               base = base_vars,
#                               interact =  add_predictors(base, interact_vars),
#                               interact2 = add_predictors(base, interact_vars2),
#                               politics =  add_predictors(base, pol_vars),
#                               full = add_predictors(politics, interact_vars),
#                               full2 = add_predictors(politics, interact_vars2)
#                       )

linear.model.form2 <- formulas(.response = dep_var2,
                              base = base_vars,
                              interact =  add_predictors(base, interact_vars),
                              interact2 = add_predictors(base, interact_vars2)
                      )


## FOR APPENDIX (W/ POL VARS):
# linear.model.form2 <- formulas(.response = dep_var2,
#                               base = base_vars,
#                               interact =  add_predictors(base, interact_vars),
#                               interact2 = add_predictors(base, interact_vars2),
#                               politics =  add_predictors(base, pol_vars),
#                               full = add_predictors(politics, interact_vars),
#                               full2 = add_predictors(politics, interact_vars2)
#                       )


## LINEAR MODEL
# cropveg
linear.model <- map(linear.model.form, ~ lm(.x, data = eac, model = TRUE))
# w/out outlier (Uribia in La Guajira)
linear.model.out <- map(linear.model.form, ~ lm(.x, data = cropveg_out, model = TRUE))

# crop
linear.model2 <- map(linear.model.form2, ~ lm(.x, data = eac, model = TRUE))
linear.model.out2 <- map(linear.model.form2, ~ lm(.x, data = crop_out, model = TRUE))

## REGRESSION OUTPUT
# linear.model %>% map(summary)
# linear.model %>% map(robust)
# linear.model.out %>% map(summary)
# linear.model.out %>% map(robust)
# linear.model2 %>% map(summary)
# linear.model2 %>% map(robust)
# linear.model.out2 %>% map(summary)
# linear.model.out2 %>% map(robust)



## output regression tables ----------------------------------------------------

## FILENAME STRING
filename <- "cropveg_aid_popexpo_table.doc"
# string for the outlier reg table
filename_out <- "cropveg_aid_popexpo_table_out.doc"

filename2 <- "crop_aid_popexpo_table.doc"
# string for the outlier reg table
filename_out2 <- "crop_aid_popexpo_table_out.doc"

## RENAME VARS IN TABLE
coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
                  "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
                  "upstream" = "Upstream", "altitude" = "Altitude",
                  "vio_31_mean" = "Exposure to State",
                  "vio_32_mean" = "Exposure to Guerillas",
                  "vio_13" = "Coca Presence Avg",
                  "muni_locd:vio_31_mean" = "Aid * Exposure to State",
                  "muni_locd:vio_32_mean" = "Aid * Exposure to Guer")

## FOR APPENDIX (W/ POL VARS):
# coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
#                   "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
#                   "upstream" = "Upstream", "altitude" = "Altitude",
#                   "vio_31_mean" = "Exposure to State",
#                   "vio_32_mean" = "Exposure to Guerillas",
#                   "vio_13" = "Coca Presence Avg",
#                   "c_match_presdept" = NA, "c_match_munidept" = NA,
#                   "c_match_munideptpres" = NA, "pres_pidmuni" = NA,
#                   "s_match_presseat" = NA, "s_match_muniseatpres" = NA,
#                   "muni_locd:vio_31_mean" = "Aid * Exposure to State",
#                   "muni_locd:vio_32_mean" = "Aid * Exposure to Guer")


## NOTE FOR BOTTOM OF TABLE
# %stars puts p-value definitions in Note
note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
variable. 'Exposure to State' = proportion of years (1998-2009) that the 
non-combatant population comes in contact with government forces. 'Exposure to 
Guerilla' = the same definition, but for guerilla forces. Table shows robust 
SEs."

note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
variable. 'Exposure to State' = proportion of years (1998-2009) that the 
non-combatant population comes in contact with government forces. 'Exposure to 
Guerilla' = the same definition, but for guerilla forces. Table shows robust 
SEs."


## FOR APPENDIX (W/ POL VARS):
# note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
# variable. 'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually 
# exclusive categories compared against a category where there is no match in 
# ideology. 'Exposure to State' = proportion of years (1998-2009) that the 
# non-combatant population comes in contact with government forces. 'Exposure to 
# Guerilla' = the same definition, but for guerilla forces. Table shows robust 
# SEs."
# 
# note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
# variable. 'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually 
# exclusive categories compared against a category where there is no match in 
# ideology. 'Exposure to State' = proportion of years (1998-2009) that the 
# non-combatant population comes in contact with government forces. 'Exposure to 
# Guerilla' = the same definition, but for guerilla forces. Table shows robust 
# SEs."


## SAVE REG OUTPUT TABLE W/ ROBUST SEs
setwd(dir.figures.tables)

## table to .doc
# cropveg tables
robust_table(linear.model, filename = filename, title, coefnames, note, latex = FALSE)
robust_table(linear.model.out, filename = filename_out, title_out, coefnames,
             note, latex = FALSE)

# crop tables
robust_table(linear.model2, filename = filename2, title2, coefnames, note2,
             latex = FALSE)
robust_table(linear.model.out2, filename = filename_out2, title_out2, coefnames,
             note2, latex = FALSE)




## interaction plots -----------------------------------------------------------

## CROPVEG PLOTS
# aid*vio_31_mean
interplot(m = linear.model[[2]], var1 = "muni_locd", var2 = "vio_31_mean",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover (in Ha)") +
  xlab("Muni Population's Exposure to State Conflict (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Exposure to Conflict (2002-2013)")
setwd(dir.figures.tables)
ggsave("cropveg_aid-popexpo_state.pdf", plot = last_plot(),
       device = "pdf", width = 7, height = 7, units = "in", dpi = 300)

## FOR APPENDIX (W/ POL VARS):
# aid*vio_31_mean
interplot(m = linear.model[[5]], var1 = "muni_locd", var2 = "vio_31_mean",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover (in Ha)") +
  xlab("Muni Population's Exposure to State Conflict (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Exposure to Conflict (2002-2013)")
setwd(dir.figures.tables)
ggsave("cropveg_aid-popexpo_state_elecs.pdf", plot = last_plot(),
       device = "pdf", width = 7, height = 7, units = "in", dpi = 300)

# aid*vio_32_mean
interplot(m = linear.model[[3]], var1 = "muni_locd", var2 = "vio_32_mean",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover (in Ha)") +
  xlab("Muni Population's Exposure to Guer Conflict (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Exposure to Conflict (2002-2013)")
setwd(dir.figures.tables)
ggsave("cropveg_aid-popexpo_guer.pdf", plot = last_plot(),
       device = "pdf", width = 7, height = 7, units = "in", dpi = 300)

## FOR APPENDIX (W/ POL VARS):
# aid*vio_32_mean
interplot(m = linear.model[[6]], var1 = "muni_locd", var2 = "vio_32_mean",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover") +
  xlab("Muni Population's Exposure to Guer Conflict (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Exposure to Conflict (2002-2013)")
setwd(dir.figures.tables)
ggsave("cropveg_aid-popexpo_guer_elecs.pdf", plot = last_plot(),
       device = "pdf", width = 7, height = 7, units = "in", dpi = 300)


##  CROP PLOTS
# aid*vio_31_mean
interplot(m = linear.model2[[2]], var1 = "muni_locd", var2 = "vio_31_mean",
          hist = TRUE) +
  ylab(title_ylab_crop) +
  xlab("Muni Population's Exposure to State Conflict (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Exposure to Conflict (2002-2013)")
setwd(dir.figures.tables)
ggsave("crop_aid-popexpo_state.pdf", plot = last_plot(),
       device = "pdf", width = 7, height = 7, units = "in", dpi = 300)

## FOR APPENDIX (W/ POL VARS):
# aid*vio_31_mean
interplot(m = linear.model2[[5]], var1 = "muni_locd", var2 = "vio_31_mean",
          hist = TRUE) +
  ylab(title_ylab_crop) +
  xlab("Muni Population's Exposure to State Conflict (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Exposure to Conflict (2002-2013)")
setwd(dir.figures.tables)
ggsave("crop_aid-popexpo_state_elecs.pdf", plot = last_plot(),
       device = "pdf", width = 7, height = 7, units = "in", dpi = 300)

# aid*vio_32_mean
interplot(m = linear.model2[[3]], var1 = "muni_locd", var2 = "vio_32_mean",
          hist = TRUE) +
  ylab(title_ylab_crop) +
  xlab("Muni Population's Exposure to Guer Conflict (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Exposure to Conflict (2002-2013)")
setwd(dir.figures.tables)
ggsave("crop_aid-popexpo_guer.pdf", plot = last_plot(),
       device = "pdf", width = 7, height = 7, units = "in", dpi = 300)


## FOR APPENDIX (W/ POL VARS):
# aid*vio_32_mean
interplot(m = linear.model2[[6]], var1 = "muni_locd", var2 = "vio_32_mean",
          hist = TRUE) +
  ylab(title_ylab_crop) +
  xlab("Muni Population's Exposure to Guer Conflict (2002-2013)") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Exposure to Conflict (2002-2013)")
setwd(dir.figures.tables)
ggsave("crop_aid-popexpo_guer_elecs.pdf", plot = last_plot(),
       device = "pdf", width = 7, height = 7, units = "in", dpi = 300)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   MISSINGNESS: COCA + STATE-GUER DISPUTE   ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 

## MISSING VALUES?
eac %>% count(is.na(vio_34_mean)) # 137 missing values
eac %>% count(is.na(vio_13)) # no missingness
eac %>% count(is.na(illegal_crops))  # only 2 missing values

eac %<>%
  mutate(
    missing_stateguer = if_else(is.na(vio_34_mean),
                                1,
                                0,
                                NA_real_)
    )

eac %>% count(is.na(missing_stateguer))


## model -----------------------------------------------------------------------

## PREDICT MISSINGNESS
dv_missing <- ~missing_stateguer
missing1 <- ~geo_3 + altitude + demo_3_mean + vio_13 + vio_46 + vio_29
missing2 <- ~geo_3 + demo_3_mean + vio_13 + vio_46 + vio_29
missing3 <- ~geo_3 + altitude + demo_3_mean + vio_13 + vio_46 + vio_30
missing4 <- ~geo_3 + demo_3_mean + vio_13 + vio_46 + vio_30



logit.missing.form <- formulas(.response = dv_missing,
                    missing1 = missing1,
                    missing2 = missing2,
                    missing3 = missing3,
                    missing4 = missing4
                    )

logit.missing <- map(logit.missing.form, ~ glm(.x,
                                                family = binomial(link='logit'),
                                                data = eac, model = TRUE))

logit.missing %>% map(summary)



## output regression tables ----------------------------------------------------

## TITLE
title_missing <- "Predicting Missing Values in Key Violence Measures"

## FILENAME STRING
filename <- "missing_state-guerrilla-dispute_table.doc"

## RENAME VARS IN TABLE
coefnames <- list("(Intercept)" = NA,
                  "geo_3" = "Muni area",
                  "altitude" = "Altitude",
                  "vio_46" = "Conflict Incidence",
                  "vio_13" = "Coca Presence Avg",
                  "vio_29" = "People Displaced ('Chapman')",
                  "vio_30" = "People Displaced ('Lincoln-Petersen')"
                  )


## NOTE FOR BOTTOM OF TABLE
# %stars puts p-value definitions in Note
note <- "%stars. Logit regression estimating the likelihood of a missing value 
for 'State-Guerilla Dispute' and the 'Population Exposure' variables (missing 
values are consistent for the same municipalities in the CERAC database). There 
are 137 missing values, i.e. municipalities not represented in the dispute and 
exposure variables."


## SAVE REG OUTPUT TABLE W/ ROBUST SEs
setwd(dir.figures.tables)
## table to .doc
# 'predicting missing values' table
robust_table(logit.missing, filename = filename, title_missing, coefnames,
             note, latex = FALSE)




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   MISSINGNESS: POPULATION EXPOSURE TO CONFLICT    ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
eac %>% count(is.na(vio_31_mean)) # 137 missing 
eac %>% count(is.na(vio_32_mean)) # 137 missing
eac %>%
  select(codmuni, municipality, department, vio_34_mean,
         vio_31_mean, vio_32_mean) %>%
  filter(is.na(vio_34_mean)) %>% 
  View() # same 137 values missing as vio_34_mean

## Note:
#       - b/c the same 137 munis are missing data, the "missingness" prediction 
#         is not just for State-Guerr. It's predicting "missingness" for these
#         vars in general.




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   AID + COCA (ADDING COFFEE CONTROL)    ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 

## models ----------------------------------------------------------------------

## VARS TO INCLUDE IN MODEL
dep_var <- ~trendcropveg
dep_var2 <- ~trendcrop
base_vars <- ~muni_locd + SPI12m_sd + geo_3 + upstream + altitude + vio_13
coffee_dummy <- ~coffee_2014
coffee_prod <- ~p_cafe
interact_vars <- ~muni_locd:vio_13
# political/election vars (camara, senate, president)
pol_vars <- ~c_match_presdept + c_match_munidept + c_match_munideptpres +
  pres_pidmuni + s_match_presseat + s_match_muniseatpres

# set models
# cropveg dv model
linear.model.form <- formulas(.response = dep_var,
                              base = base_vars,
                              coffee = add_predictors(base, coffee_dummy),
                              coffee2 = add_predictors(base, coffee_prod),
                              interact1 =  add_predictors(base, coffee_dummy,
                                                         interact_vars),
                              interact2 = add_predictors(base, coffee_prod,
                                                         interact_vars)
)

# crop dv model
linear.model.form2 <- formulas(.response = dep_var2,
                               base = base_vars,
                               coffee = add_predictors(base, coffee_dummy),
                               coffee2 = add_predictors(base, coffee_prod),
                               interact1 =  add_predictors(base, coffee_dummy,
                                                          interact_vars),
                               interact2 = add_predictors(base, coffee_prod,
                                                          interact_vars)
)

## LINEAR MODEL
linear.model <- map(linear.model.form, ~ lm(.x, data = eac, model = TRUE))

linear.model2 <- map(linear.model.form2, ~ lm(.x, data = eac, model = TRUE))

## REGRESSION OUTPUT
linear.model %>% map(summary)
linear.model %>% map(robust)
linear.model2 %>% map(summary)
linear.model2 %>% map(robust)



## output regression tables ----------------------------------------------------

## FILENAME STRING - CROPVEG 
filename <- "cropveg_aid_coca1_table.doc"

## FILENAME STRING - CROP
filename2 <- "crop_aid_coca1_table.doc"

## TABLE TITLE - CROPVEG
title <- "Aid's Effect on MODIS Crop Veg Cover"

## TABLE TITLE - CROP
title2 <- "Aid's Effect on MODIS Crop Cover"

## RENAME VARS IN TABLE
coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
                  "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
                  "upstream" = "Upstream", "altitude" = "Altitude",
                  "vio_13" = "Coca presence avg",
                  "muni_locd:vio_13" = "Aid * Coca")

## FOR APPENDIX TABLE (W/ POLITICAL VARS)
# coefnames <- list("(Intercept)" = NA, "muni_locd" = "Aid given",
#                   "SPI12m_sd" = "SPI (std dev)", "geo_3" = "Muni area",
#                   "upstream" = "Upstream", "altitude" = "Altitude",
#                   "vio_13" = "Coca presence avg", "c_match_presdept" = NA,
#                   "c_match_munidept" = NA, "c_match_munideptpres" = NA,
#                   "pres_pidmuni" = NA, "s_match_presseat" = NA,
#                   "s_match_muniseatpres" = NA,
#                   "muni_locd:vio_13" = "Aid * Coca")

## NOTE FOR BOTTOM OF TABLE
# %stars puts p-value definitions in Note
note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
variable. Table shows robust standard errors."

note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
variable. Table shows robust standard errors."

## FOR APPENDIX TABLES (W/ POL VARS)
# note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
# variable. 'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
# exclusive categories compared against a category where there is no match
# in ideology. Table shows robust standard errors."
# 
# note2 <- "%stars. OLS regressions with MODIS crop trend as the dependent
# variable. 'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
# exclusive categories compared against a category where there is no match
# in ideology. Table shows robust standard errors."


## SAVE REG OUTPUT TABLE W/ ROBUST SEs
setwd(dir.figures.tables)
# table to .doc
robust_table(linear.model, filename = filename, title, coefnames, note,
             latex = FALSE)
# robust_table(linear.model.out, filename = filename_out, title_out, coefnames,
#              note, latex = FALSE)

robust_table(linear.model2, filename = filename2, title2, coefnames, note2,
             latex = FALSE)
# robust_table(linear.model.out2, filename = filename_out2, title_out2, coefnames,
#              note2, latex = FALSE)


## SHOW REG OUTPUT TABLE W/ ROBUST SEs
## table to html code

# cropveg tables
# html_table <-  robust_table(linear.model, filename = NULL, title, coefnames, note,
#                             latex = FALSE)
# html_table
# html_table %>% HTML() %>% browsable() # view the html in R's viewer

# w/out outlier
# html_table_out <- robust_table(linear.model.out, filename = NULL, title_out, coefnames,
#                                note, latex = FALSE)
# html_table_out
# html_table_out %>% HTML() %>% browsable()

# crop tables
# html_table2 <- robust_table(linear.model2, filename = NULL, title2, coefnames, note2,
#                             latex = FALSE)
# html_table2
# html_table2 %>% HTML() %>% browsable()
# 
# html_table2_out <- robust_table(linear.model.out2, filename = NULL, title_out2,
#                                 coefnames, note2, latex = FALSE)
# html_table2_out
# html_table2_out %>% HTML() %>% browsable()


# # TIDY, BROOM, GLANCE
# linear.model
# glance(linear.model[[4]])
# tidy(linear.model[[4]])


# interaction plots ----

## CROPVEG PLOTS
# aid*coca
interplot(m = linear.model[[2]], var1 = "muni_locd", var2 = "vio_13",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover (in Ha)") + xlab("Avg. Coca Presence") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (1993-2015)")

setwd(dir.figures.tables)
ggsave("cropveg_aid-coca1.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = linear.model[[4]], var1 = "muni_locd", var2 = "vio_13",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover (in Ha)") + xlab("Avg. Coca Presence") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (1993-2015)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("cropveg_aid-coca1_elecs.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)


## CROP PLOTS
title_ylab_crop <- "Aid's Effect on Crop Cover (in Ha)"

# aid*coca
interplot(m = linear.model2[[2]], var1 = "muni_locd", var2 = "vio_13",
          hist = TRUE) +
  ylab(title_ylab_crop) + xlab("Avg. Coca Presence") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (1993-2015)")

setwd(dir.figures.tables)
ggsave("crop_aid-coca1.pdf", plot = last_plot(), device = "pdf", width = 7,
       height = 7, units = "in", dpi = 300)

# aid*coca + pol
interplot(m = linear.model2[[4]], var1 = "muni_locd", var2 = "vio_13",
          hist = TRUE) +
  ylab(title_ylab_crop) + xlab("Avg. Coca Presence") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coca Presence (1993-2015)",
          subtitle = "controlling for election effects")

setwd(dir.figures.tables)
ggsave("crop_aid-coca1_elecs.pdf", plot = last_plot(), device = "pdf",
       width = 7, height = 7, units = "in", dpi = 300)





# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
####   AID + COFFEE    ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 

## models ----------------------------------------------------------------------

## VARS TO INCLUDE IN MODEL
base_vars <- ~muni_locd + SPI12m_sd + geo_3 + upstream + altitude +
                vio_13 + p_cafe
interact_vars <- ~muni_locd:p_cafe

# set models
# cropveg dv model
linear.model.form <- formulas(.response = dep_var,
                              base = base_vars,
                              interact =  add_predictors(base,
                                                          interact_vars)
)

# crop dv model
linear.model.form2 <- formulas(.response = dep_var2,
                               base = base_vars,
                               interact = add_predictors(base,
                                                          interact_vars)
)

## LINEAR MODEL
linear.model <- map(linear.model.form, ~ lm(.x, data = eac, model = TRUE))
linear.model.out <- map(linear.model.form, ~ lm(.x, data = p_cafe_outlier, model = TRUE))

linear.model2 <- map(linear.model.form2, ~ lm(.x, data = eac, model = TRUE))
linear.model2.out <- map(linear.model.form2, ~ lm(.x, data = p_cafe_outlier, model = TRUE))

## REGRESSION OUTPUT
linear.model %>% map(summary)
linear.model %>% map(robust)
linear.model.out %>% map(summary)
linear.model.out %>% map(robust)

linear.model2 %>% map(summary)
linear.model2 %>% map(robust)


interplot(m = linear.model[[2]], var1 = "muni_locd", var2 = "p_cafe",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover (in Ha)") +
  xlab("Avg. Coffee Production") + 
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coffee Production (1993-2015)")

interplot(m = linear.model.out[[2]], var1 = "muni_locd", var2 = "p_cafe",
          hist = TRUE) +
  ylab("Aid's Effect on Crop Veg Cover (in Ha)") +
  xlab("Avg. Coffee Production") +
  geom_hline(yintercept = 0, color="grey35", size = .3) +
  ggtitle("Estimated Coefficient of Aid by Coffee Production (1993-2015)")




# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 
    ####   AID: POL MATCH    ####
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= # 

# pres
# senado
# cam


## TARGETED FOR AID: 201705 analysis
# logit.aid <- glm(muni_locd ~ cropsha_pct2001 + spistd + sq_km_mun + upstream1 + altitude1 +
#                    road_zero + illegal_crops + mean_turnout + mean_mov + mean_muni_dept_match +
#                    local_parliament_match_mean, family = binomial(link='logit'), data = agaid)








#-------------------------------------------------------------------------------
## TABLE HELP
## LOGIT RESULTS TO EXCEL
# setwd("/Users/wtmatthias/Google Drive/Research/colombia_pb_pleb/data/03_data_viz/")
# results_df <-summary.glm(logit11)$coefficients
# results_df <- as.data.frame(results_df)
# write_csv(results_df, "general_effects.csv")
# summary(logit11)
# 
# ## ODDS RATIOS TO EXCEL
# #  change "logit[]" to fit the logit model you need
# odds_ratios <- as.data.frame(exp(cbind(coef(logit11), confint(logit11))))
# write_csv(odds_ratios, "odds_ratios.csv")

#-------------------------------------------------------------------------------
# #### Marginal effect of income with respect to age
# # Prediction
# all.pairs <- expand.grid(seq(min(eac$vio_13), max(eac$vio_13), by=0.05),
#                          range(eac$muni_locd))
# colnames(all.pairs) <- c('vio_13', 'muni_locd')
# pred.data.me <- data.frame(muni_locd = all.pairs[2],
#                            vio_13 = all.pairs[1]
#                            )
# pred.data.me$id <- row.names(pred.data.me)
# 
# 
# # Calculate Marginal effect of Coca Presence
# pred.data.me$me <- cropveg.aid.coca1[[4]]$coefficients[['vio_13']] + pred.data.me$muni_locd * cropveg.aid.coca1[[4]]$coefficients[['muni_locd:vio_13']]

# # Calculate CI of the marginal effect
# var.coca <- vcovHC(cropveg.aid.coca1[[4]], type = "HC1")['vio_13','vio_13']
# var.inter <- vcovHC(cropveg.aid.coca1[[4]], type = "HC1")['muni_locd:vio_13','muni_locd:vio_13']
# cov <- vcovHC(cropveg.aid.coca1[[4]], type = "HC1")['vio_13','muni_locd:vio_13']
# muni_locd <- pred.data.me$muni_locd
# pred.data.me$me.se <- sqrt(var.coca + var.inter*(muni_locd^2) + 2*muni_locd*cov)
# pred.data.me$me.upper <- pred.data.me$me + 1.96*pred.data.me$me.se
# pred.data.me$me.lower <- pred.data.me$me - 1.96*pred.data.me$me.se
# 
# # Reformat the data again
# pred.melt.me <- melt(pred.data.me, id='muni_locd',
#                      measure = c('me', 'me.upper', 'me.lower'))
# pred.melt.me$stat <- ifelse(pred.melt.me$variable == 'me',
#                             'P', 'CI')
# 
# # Plot
# ggplot(data = pred.melt.me, aes(y=muni_locd, x=vio_13, group=variable,
#                                 linetype=factor(stat))) +
#   geom_line(color='orange') +
#   scale_linetype_manual(breaks=c("CI","P"), values=c(2,1)) +
#   ylab('Marginal effect of  on Aid') +
#   guides(linetype=F)

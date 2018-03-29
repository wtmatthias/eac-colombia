## VARS TO INCLUDE IN MODEL
dep_var <- ~trendcropveg
base_vars <- ~muni_locd + SPI12m_sd + geo_3 + upstream + altitude + vio_13
interact_vars <- ~muni_locd:vio_13
# political/election vars (camara, senate, president)
pol_vars <- ~c_match_presdept + c_match_munidept + c_match_munideptpres +
  pres_pidmuni + s_match_presseat + s_match_muniseatpres


linear.model.form <- formulas(.response = dep_var,
                           base = base_vars,
                           interact =  add_predictors(base, interact_vars),
                           politics =  add_predictors(base, pol_vars),
                           full = add_predictors(politics, interact_vars)
)


## LINEAR MODEL
linear.model <- map(linear.model.form, ~ lm(.x, data = eac, model = TRUE))
# w/out outlier (Uribia in La Guajira)
linear.model.out <- map(linear.model.form, ~ lm(.x, data = cropveg_out, model = TRUE))

## REGRESSION OUTPUT
# linear.model %>% map(summary)
# linear.model %>% map(robust)
# linear.model.out %>% map(summary)
# linear.model.out %>% map(robust)

## Set Options For robust.table ------------------------------------------------

## FILENAME STRING
filename <- "TEST.doc"
# string for the outlier reg table
filename_out <- "cropveg_aid_coca1_outlier.doc"

## TABLE TITLE
title <- "Aid's Effect on MODIS Crop Veg Cover"
# title for the outlier table
title_out <- "Aid's Effect on MODIS Crop Veg Cover (without Uribia, La Guajira)"

## RENAME VARS IN TABLE
coefnames <- c("(Intercept)", "Aid given", "SPI (std dev)", "Muni area",
               "Upstream", "Altitude", "Coca presence avg",
               "c_match_presdept", "c_match_munidept", "c_match_munideptpres",
               "pres_pidmuni", "s_match_presseat", "s_match_muniseatpres",
               "Aid * Coca")

## NOTE FOR BOTTOM OF TABLE
# %stars puts p-value definitions in Note
note <- "%stars. OLS regressions with MODIS cropveg trend as the dependent
variable. 'c_match', 's_match', and 'pres_pidmuni' variables indicate mutually
exclusive categories compared against a category where there is no match
in ideology. Table shows robust standard errors."


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
           custom.coef.names = coefnames,
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
            custom.coef.names = coefnames,
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
                             custom.coef.names = coefnames,
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


## SAVE REG OUTPUT TABLE W/ ROBUST SEs
# setwd(dir.figures.tables)
# testhtml <- robust_table(linear.model, filename = NULL, title, coefnames, note, latex = FALSE)
# testhtml %>% HTML() %>% browsable()
# robust_table(linear.model, filename = NULL, title, coefnames, note, latex = TRUE)
# robust_table(linear.model, filename = filename, title, coefnames, note, latex = FALSE)



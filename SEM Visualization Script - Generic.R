##############################################################
# SEM Visualization Code - Dot Plots and Correlations        #
# First Written Date: 07/16/24                               #
# Last Edited: 07/16/24                                      #
# Author: Michael Becker                                     #
##############################################################

# Data Prep ####

## Predict values for each of the latent concepts via lavPredict [for Correlation figure]
data_pred_vals<-as_tibble(lavPredict(fit_alt_spec_final2, ## This is the model object, NOT the summary object
                                     append.data = TRUE)) ## This adds the scores as columns for EACH row

## Extract Coefficients from Summarized Model Fit Object
dot_viz_data<-fit_summary[["pe"]] ## This pulls out the coefficients, standard errors, 'operations', and std levels
dot_viz_data<-dot_viz_data%>% #create an upper CI and lower CI value for each of the coefficient estimates. 
  mutate(upper_ci=est+(1.96*se))%>%
  mutate(lower_ci=est-(1.96*se))

## Adding scale names for each of the scales.
dot_viz_data<-dot_viz_data%>% 
  mutate(scale = case_when( #'scale' will be the factor/character variable for when we visualize it 
    grepl("INDICATOR STEM 1",rhs) ~"SCALE NAME 1", # using grepl here to find the 'common stem' for each of the scale indicators
    grepl("INDICATOR STEM 2",rhs) ~"SCALE NAME 2",
    grepl("INDICATOR STEM 3",rhs) ~"SCALE NAME 3",
    grepl("INDICATOR STEM 4",rhs) ~"SCALE NAME 4"))

# Factor Loadings Figure ####
factor_loadings<-dot_viz_data%>%
  filter(op=="=~")%>% # Since =~ is the measurement model operator, subset to these rows
  ggplot(aes(y=est, x=rhs, color = scale))+ # y is estimate, x is 'rhs variable' which is to say, the individual indicators for common LHS latent vars
  geom_point(na.rm = TRUE)+ # points so we have point estimates
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci))+ # error bars so we have whiskers
  coord_flip(ylim = c(0,2))+ # coordinate flip so these go left/right rather than up/down (aesthetic)
  geom_hline(yintercept = 0, linetype = "dashed")+ # vertical dashed line at zero 
  theme_bw(base_size = 10) #theme to make it pretty (shrink text a bit)
factor_loadings


# Regression Coefficients Figure ####
dot_viz_data%>%
  filter(op=="~")%>% # Since ~ is the regression operator, subset to these rows
  ggplot(aes(y=est, x=rhs, color = lhs))+ #y is the estimate, x is the rhs variable (predictors), color by lhs (response) variable
  geom_point(na.rm = TRUE, # points so we have point estimates 
             position = position_dodge2(width = 0.5, padding = 0.5))+ #since you may have sommon predictors for outcomes, dodge so you can see them 
  geom_linerange(aes(ymin = lower_ci, ymax = upper_ci), # error bars
                 position = position_dodge2(width = 0.5, padding = 0.5))+ # dodge the error bars
  coord_flip(ylim = c(-1.0,1.5))+ # make it horizontal
  geom_hline(yintercept = 0, linetype = "dashed")+ # veritcal dashed line at zero
  theme_bw()+ # make it pretty
  labs(title = "Stuctural Equation Model Results", #title
       subtitle = "Regression Coefficients by Outcome")+ #subtitle
  xlab("Predictor Variable")+ #x axis label
  ylab("Standardized Coefficient Estimate")+ #x axis label
  scale_color_discrete(name = "Outcome Variable", #You can change the text for the legend items (cuz varnames are often weird)
                       labels = c("OUTCOME LABEL 1","OUTCOME LABEL 2","OUTCOME LABEL 3","OUTCOME LABEL 4"))+
  scale_x_discrete(limits = c("PREDICTOR4","PREDICTOR3","PREDICTOR2","PREDICTOR1"), # You can reorder the predictors (but since its coord flip, it is reversed sequence)
                   labels = c("PREDICTOR4"="PRETTY NAME 4","PREDICTOR3"="PRETTY NAME 3","PREDICTOR2"="PRETTY NAME 2", "PREDICTOR1"="PRETTY NAME 1")) # You can also change the text for them


# Variances ####
dot_viz_data%>%
  filter(op=="~~")%>% # Since ~~ is the variance operator, subset to these rows
  ggplot(aes(y=est, x=rhs, color = scale))+ # y is estimate, x is rhs - these don't matter since it is variance... not cov. Scale is to color coordinate items
  geom_point(na.rm = TRUE)+ # point estimates
  geom_linerange(aes(ymin = lower_ci, ymax = upper_ci))+ # standard errors
  coord_flip(ylim = c(0,4))+ # flip coords (you shouldn't have negative variances)
  geom_hline(yintercept = 0, linetype = "dashed") # vertical dashed line at zero


# Correlation figure for concepts ####
library(GGally)

## Select relevant latent concepts (or as needed, indicators)
data_viz<-data_pred_vals%>% ## start with predicted values
  select(LV1,   ## subset to only relevant items
         LV2,
         LV3,
         LV4)%>%
  lapply(as.numeric)%>% ## convert to numeric in case it wasn't before
  as.data.frame() ## make it a data frame cuz it may not be

## Generate a correlation matrix for the subset 
data_viz_cor<-cor(data_viz,use = "complete.obs")
data_viz_cor%>% ## Start with the correlation matrix
  ggpairs(aes(alpha = 0.5), ## ggpairs is from the GGally package
          #lower = list(continuous = "smooth"),
          upper = list(continuous = wrap("cor",size = 2)), ## This is the upper triangle of the matrix
          columnLabels = c("LABEL 1","LABEL 2","LABEL 3","LABEL 4"))+ ##labeling columns 
  labs(title = "Correlations Among SEM Latent Variables", size = 12)+ ## Changing title and text size
  theme_bw(base_size = 6) ## Theme so it is pretty
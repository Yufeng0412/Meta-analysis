library(metaforest)
library(ggplot2)
library(dmetar)
library(glmulti)
library(caret)


set.seed(50)

# read data and factorize categorical variables
data_dir <- 'D:/carbon credit/manuscript/nature communications/revision/all data/synthetic data collections/model inputs_2405/' 
plot_dir <- 'D:/carbon credit/metaforest/county_project/model_plots_2405/'
data_soc <- read.csv(paste0(data_dir,  "SOC_model_inputs.csv"))
data_n2o <- read.csv(paste0(data_dir,  "N2O_model_inputs.csv"))
data_lch <- read.csv(paste0(data_dir,  "Nleach_model_inputs.csv"))
data_yld <- read.csv(paste0(data_dir,  "yld_model_inputs.csv"))

# observed variables
obs_vars = c('delta_SOC_sameinit_30', 'delta_SOC_sameinit_NTeffect_60', 
             'delta_SOC_filled_30', 'delta_SOC_filled_NTeffect_60', 
             'delta_N2O', 'delta_N2O', 'delta_Nleach', 'delta_Nleach', 'delta_yield', 'delta_yield')
# explanatory variables
soil_vars = c('pH', 'BD', 'Clay', 'Silt', 'Sand', 'Ks', 'FC', 'gSOC', 'CEC')
weather_vars = c('MAP', 'SPP', 'GSP', 'MAT', 'MCGDD', 'CCGDD', 'NSR', 'VPD')
manage_vars_cc = c('State','Study_ID', 'vi', 'Rotation', 'Crop', 'Legume', 'Nrate', 'No_year') # 'Lat', 'Lon', 
manage_vars_nt = c('State','Study_ID', 'vi', 'Rotation', 'Crop', 'Tillage', 'Nrate', 'No_year')
cc_vars = c(manage_vars_cc, soil_vars, weather_vars)
nt_vars = c(manage_vars_nt, soil_vars, weather_vars)

# extract dataframes for cover crops and no-till.
# for SOC, 'Crop' is not a predictor so excluded.
# Nrate was excluded for CC_SOC for missing in some studies and tested not important in the rest
data_cc_soc_cpl <- data_soc[data_soc$Experiment == 'Cover crops', c(cc_vars[-c(5,7)], obs_vars[1])]
data_nt_soc_cpl <- data_soc[data_soc$Experiment == 'Tillage', c(nt_vars[-c(5)], obs_vars[2])]
data_cc_soc_fil <- data_soc[data_soc$Experiment == 'Cover crops', c(cc_vars[-c(5,7)], obs_vars[3])]
data_nt_soc_fil <- data_soc[data_soc$Experiment == 'Tillage', c(nt_vars[-c(5)], obs_vars[4])]
data_cc_n2o <- data_n2o[data_n2o$Experiment == 'Cover crops', c(cc_vars, obs_vars[5])]
data_nt_n2o <- data_n2o[data_n2o$Experiment == 'Tillage', c(nt_vars, obs_vars[6])]
data_cc_lch <- data_lch[data_lch$Experiment == 'Cover crops', c(cc_vars, obs_vars[7])]
data_nt_lch <- data_lch[data_lch$Experiment == 'Tillage', c(nt_vars, obs_vars[8])]
data_cc_yld <- data_yld[data_yld$Experiment == 'Cover crops', c(cc_vars, obs_vars[9])]
data_nt_yld <- data_yld[data_yld$Experiment == 'Tillage', c(nt_vars, obs_vars[10])]


# exclude short-term SOC data from modeling, duration < 3 years for NT and < 7 years for CC
data_cc_soc_cpl <- data_cc_soc_cpl[data_nt_soc_cpl['No_year'] >= 4, ]
data_cc_soc_fil <- data_cc_soc_fil[data_nt_soc_fil['No_year'] >= 4, ]
data_nt_soc_cpl <- data_nt_soc_cpl[data_nt_soc_cpl['No_year'] >= 8, ]
data_nt_soc_fil <- data_nt_soc_fil[data_nt_soc_fil['No_year'] >= 8, ]


data_list <- list(data_cc_soc_cpl,data_nt_soc_cpl,data_cc_soc_fil, data_nt_soc_fil,
                  data_cc_n2o,data_nt_n2o,data_cc_lch,data_nt_lch,data_cc_yld,data_nt_yld)
data_clean <- list()
for (i in seq_along(data_list)) {
    data = na.omit(data_list[[i]])
    print(dim(data))
    data$Rotation <- factor(data$Rotation)
    if (i %in% c(2,4,6,8,10)) {
        data$Tillage <- factor(data$Tillage)
    }
    if (i %in% c(1,3,5,7,9,11)) {
        data$Legume <- factor(data$Legume)
    }
    if (i > 4) {
        data$Crop <- factor(data$Crop)
    }
    data_clean[[i]] <- data
}


# Model with 1000 trees for replications
mf_cc_soc_cpl <- MetaForest(as.formula(paste0("delta_SOC_sameinit_30~", paste(cc_vars[-c(1,2,3,5,7)], collapse = "+"))),
                        data = data_clean[[1]],
                        whichweights = "random",
                        method = 'DL',
                        num.trees = 1000)
mf_nt_soc_cpl <- MetaForest(as.formula(paste0("delta_SOC_sameinit_NTeffect_60~", paste(nt_vars[-c(1,2,3,5)], collapse = "+"))),
                        data = data_clean[[2]],
                        whichweights = "random",
                        method = 'DL',
                        num.trees = 1000)
mf_cc_soc_fil <- MetaForest(as.formula(paste0("delta_SOC_filled_30~", paste(cc_vars[-c(1,2,3,5,7)], collapse = "+"))),
                            data = data_clean[[3]],
                            whichweights = "random",
                            method = 'DL',
                            num.trees = 1000)
mf_nt_soc_fil <- MetaForest(as.formula(paste0("delta_SOC_filled_NTeffect_60~", paste(nt_vars[-c(1,2,3,5)], collapse = "+"))),
                            data = data_clean[[4]],
                            whichweights = "random",
                            method = 'DL',
                            num.trees = 1000)
mf_cc_n2o <- MetaForest(as.formula(paste0("delta_N2O~", paste(cc_vars[-c(1,2,3)], collapse = "+"))),
                        data = data_clean[[5]],
                        whichweights = "random",
                        method = 'DL',
                        num.trees = 1000)
mf_nt_n2o <- MetaForest(as.formula(paste0("delta_N2O~", paste(nt_vars[-c(1,2,3)], collapse = "+"))),
                        data = data_clean[[6]],
                        whichweights = "random",
                        method = 'DL',
                        num.trees = 1000)
mf_cc_lch <- MetaForest(as.formula(paste0("delta_Nleach~", paste(cc_vars[-c(1,2,3)], collapse = "+"))),
                        data = data_clean[[7]],
                        whichweights = "random",
                        method = 'DL',
                        num.trees = 1000)
mf_nt_lch <- MetaForest(as.formula(paste0("delta_Nleach~", paste(nt_vars[-c(1,2,3)], collapse = "+"))),
                        data = data_clean[[8]],
                        whichweights = "random",
                        method = 'DL',
                        num.trees = 1000)
mf_cc_yld <- MetaForest(as.formula(paste0("delta_yield~", paste(cc_vars[-c(1,2,3)], collapse = "+"))),
                        data = data_clean[[9]],
                        whichweights = "random",
                        method = 'DL',
                        num.trees = 1000)
mf_nt_yld <- MetaForest(as.formula(paste0("delta_yield~", paste(nt_vars[-c(1,2,3)], collapse = "+"))),
                        data = data_clean[[10]],
                        whichweights = "random",
                        method = 'DL',
                        num.trees = 1000)


plot(mf_nt_yld)
# summary(mf_nt_lch)
# pred1 <- predict(mf_nt_lch, data_clean[[8]])$predictions
# sim_obs1 <- as.data.frame(cbind(as.numeric(data_clean[[8]]$delta_Nleach), as.numeric(pred1)))
# colnames(sim_obs1) <- c('obs_', 'prd_')
# # Create a formula dynamically, and fit to linear regression
# formula <- reformulate('obs_', response = 'prd_')
# lm1 = lm(formula, data=sim_obs1)
# plot(sim_obs1$obs_, sim_obs1$prd_, xlab="observation", ylab="prediction")
# abline(lm1, col="red")
# print(summary(lm1))



model_list <- list(mf_cc_soc_cpl, mf_nt_soc_cpl, mf_cc_soc_fil, mf_nt_soc_fil, 
                   mf_cc_n2o, mf_nt_n2o, mf_cc_lch, mf_nt_lch, mf_cc_yld, mf_nt_yld)
model_names <- c('mf_cc_soc_cpl', 'mf_nt_soc_cpl', 'mf_cc_soc_fil', 'mf_nt_soc_fil', 'mf_cc_n2o',
                 'mf_nt_n2o', 'mf_cc_lch', 'mf_nt_lch', 'mf_cc_yld', 'mf_nt_yld')
mf_models <- setNames(model_list, model_names)
mf_data <- setNames(data_clean, model_names)

# view variable importance and partial dependence
ord_vars <- list()
for (i in seq_along(model_names)) {
  model_name = model_names[i]
  model = mf_models[[model_name]]
  # VarImpPlot(model)
  ord_vars[[model_name]] <- names(model$forest$variable.importance)[order(model$forest$variable.importance, decreasing = TRUE)]
  # PartialDependence(mf_cc_soc_cpl)
  }


# # Run recursive preselection
# preselects <- list()
# for (i in seq_along(model_names)) {
#   model_name = model_names[i]
#   model = mf_models[[model_name]]
#   preselects[[model_name]] <- preselect(model,
#                            replications = 50,
#                            algorithm = "recursive")
# }


# # Retain predictors with positive variable importance in more than 50% of replications
# retain_vars <- list()
# for (i in seq_along(model_names)) {
#   model_name = model_names[i]
#   model = mf_models[[model_name]]
#   selected_vars <- preselect_vars(preselects[[model_name]], cutoff=0.01)
#   # selected_vars <- sort(selected_vars, decreasing = TRUE)
#   retain_vars[[model_name]] <- selected_vars
#   # VarImpPlot(model)
# }

# Set up 10-fold cross validation
grp_cvs <- list()
for (i in seq_along(model_names)) {
  model_name = model_names[i]
  model = mf_models[[model_name]]
  print(model_name)
  grp_cvs[[model_name]] <- trainControl(method = "cv",
                                        index = groupKFold(mf_data[[model_name]]$Study_ID, k = 10))
}


# Set up a tuning grid for the tuning parameters of MetaForest
# use function ModelInfo_mf from caret to tune parameters: 
# 1) mtry: the number of candidate variables considered at each split of each tree; 
# 2) min.node.size: the minimum number of cases that must remain in a post-split group within each tree; 
tuning_grid <- expand.grid(whichweights = c("random"),
                           mtry = 2:5,
                           min.node.size = 2:5)


# create X that contains only retained variables, clustering variable, and vi
X_list <- list()
for (i in seq_along(model_names)) {
  model_name = model_names[i]
  # print(retain_vars[[model_name]])
  # X_list[[model_name]] <- mf_data[[model_name]][, c("vi", retain_vars[[model_name]])]
  X_list[[model_name]] <- mf_data[[model_name]][, c("vi", ord_vars[[model_name]])]
}


# Train the model
mf_cv_models <- list()
for (i in seq_along(model_names)) {

    model_name = model_names[i]
    model_data = mf_data[[model_name]]
    grp_cv = grp_cvs[[model_name]]
    obs_var = obs_vars[i]
    print(paste0('training', obs_var))
    X_ = X_list[[model_name]]
    mf_cv_models[[model_name]] <- train(y = model_data[[obs_var]],
                      x = X_,
                      # study = "Study_ID",
                      method = ModelInfo_mf(),
                      trControl = grp_cv,
                      tuneGrid = tuning_grid,
                      keep.inbag=TRUE,
                      num.trees = 1000)
}


# extract the final models
final_models <- list()
for (i in seq_along(model_names)) {
    model_name = model_names[i]
    final_models[[model_name]] <- mf_cv_models[[model_name]]$finalModel
}

# Plot variable importance
ord_cv_vars <- list()
for (i in seq_along(model_names)) {
    model_name = model_names[i]
    final_model = final_models[[model_name]]
    ImpPlot = VarImpPlot(final_model)
    png(paste0(model_name, ".png"), width=1200, height=1500, res = 300)
    print(ImpPlot)
    # dev.off()
    ord_cv_vars[[model_name]] <- names(final_model$forest$variable.importance)[order(final_model$forest$variable.importance, decreasing = TRUE)]
    print(ord_cv_vars[[model_name]][1:10])
    # PartialDependence(final_model, vars = ord_cv_vars[[model_name]])
}


# # keep the most important 10 predictors to prevent overfitting
# final_vars <- list(c("MAT","MCGDD", "CCGDD", "NSR", "Silt", "CEC", "GSP", "gSOC","Clay","VPD"),
#                    c("Lon", "Tillage", "GSP", "No_year","NSR", "MAP", "SPP",  "Ks", "FC", "Silt"),
#                    c("Lon", "CCGDD","NSR","Ks", "BD", "VPD",  "MAT", "Lat", "MAP", "MCGDD"),
#                    c("Tillage","Lon", "No_year", "GSP",  "pH", "MAP","FC", "Lat", "Clay", "Sand"),
#                    c("CCGDD","BD","VPD","Silt","Nrate","Sand","No_year","Clay", "FC","gSOC"),
#                    c("Nrate","Rotation", "Lon","MAP","pH","Sand", "Silt", "Lat", "MAT", "FC"),
#                    c("Silt", "Nrate", "Ks", "pH", "MAP", "FC", "BD","Lon","Sand", "CEC"),
#                    c("Rotation","Nrate","No_year","MAT","CCGDD","NSR","BD","SPP"),
#                    c("MAT", "MCGDD", "CCGDD", "Lat", "VPD", "Nrate","Clay","BD","Sand",'Crop'),
#                    c("MAP", "CCGDD", "Lat","gSOC", "CEC","MAT","NSR","MCGDD", "FC","Crop"))
# keep the most important 10 predictors to prevent overfitting
final_vars <- list(c('gSOC', 'VPD', 'MCGDD', 'BD', 'pH', 'SPP','GSP', 'CEC', 'MAP', 'MAT'),
                   c('Ks', 'GSP', 'No_year', 'Tillage', 'MAP', 'pH', 'Sand', 'SPP', 'Clay', 'FC'),
                   c("Lon", "CCGDD","NSR","Ks", "BD", "VPD",  "MAT", "Lat", "MAP", "MCGDD"),
                   c("Tillage","Lon", "No_year", "GSP",  "pH", "MAP","FC", "Lat", "Clay", "Sand"),
                   c('CCGDD', 'VPD', 'BD', 'Sand', 'Clay', 'Silt', 'CEC', 'Nrate', 'gSOC', 'FC'),
                   c("Nrate","Rotation", "MAP","pH","Sand", "Silt", "MAT", "FC", "MCGDD", "NSR"),
                   c('Silt', 'Ks', 'Nrate', 'FC', 'MAP', 'pH', 'Sand', 'BD', 'MAT', 'MCGDD'),
                   c("Rotation","Nrate","No_year", "FC", "BD", "NSR", "Silt","CEC", "MCGDD", "MAT"),
                   c('MAT', 'Nrate', 'MCGDD', 'CCGDD', 'VPD', 'BD', 'Clay', 'Crop', 'Sand', 'FC'),
                   c("MAP", "gSOC", "Crop", "CCGDD", "CEC","MAT","NSR","MCGDD", "GSP", "Silt"))


X_list_final <- list()
for (i in seq_along(model_names)) {
  model_name = model_names[i]
  # X_list_final[[model_name]] <- mf_data[[model_name]][, c("vi", ord_cv_vars[[model_name]][1:10])]
  X_list_final[[model_name]] <- mf_data[[model_name]][, c("vi", final_vars[[i]])]
  # print(ord_cv_vars[[model_name]][1:10])
}


# retrain the model
mf_final_models <- list()
for (i in seq_along(model_names)) {
  
  model_name = model_names[i]
  model_data = mf_data[[model_name]]
  grp_cv = grp_cvs[[model_name]]
  obs_var = obs_vars[i]
  print(paste0('training ', obs_var))
  X_ = X_list_final[[model_name]]
  mf_final_models[[model_name]] <- train(y = model_data[[obs_var]],
                                      x = X_,
                                      # study = "Study_ID",
                                      method = ModelInfo_mf(),
                                      trControl = grp_cv,
                                      tuneGrid = tuning_grid,
                                      keep.inbag=TRUE,
                                      num.trees = 1000)
}


# extract the final models
final_models <- list()
for (i in seq_along(model_names)) {
  model_name = model_names[i]
  final_models[[model_name]] <- mf_final_models[[model_name]]$finalModel
}

# plot variable partial dependence
ord_fn_vars <- list()
for (i in seq_along(model_names)) {
  model_name = model_names[i]
  final_model = final_models[[model_name]]
  # VarImpPlot(final_model)
  ord_fn_vars[[model_name]] <- names(final_model$forest$variable.importance)[order(final_model$forest$variable.importance, decreasing = TRUE)]
  PartialDependence(final_model, vars = ord_fn_vars[[model_name]][order(final_model$forest$variable.importance, decreasing = TRUE)],pi=0.95)
  png(paste0('PD1_', model_name, ".png"), width=1500, height=1500, res = 300)
  # dev.off()
}

final_model = final_models[['mf_nt_yld']]
ord_cv_vars[[model_name]] <- names(final_model$forest$variable.importance)[order(final_model$forest$variable.importance, decreasing = TRUE)]
PartialDependence(final_model, vars = ord_cv_vars[[model_name]][order(final_model$forest$variable.importance, decreasing = TRUE)],pi=0.95)
png(paste0('PD1_', model_name, ".png"), width=1500, height=1500, res = 300)


# compare model prediction with observations
model_dir <- 'D:/carbon credit/metaforest/county_project/model_2405_lt/'
for (i in seq_along(final_model)) {
    model_name = model_names[i]
    mf_model = final_models[[model_name]]
    model_data = mf_data[[model_name]]
    obs_var = obs_vars[i]
    
    model_pred <- predict(mf_model, model_data)$predictions
    sim_obs <- as.data.frame(cbind(as.numeric(model_data[[obs_var]]), as.numeric(model_pred)))
    colnames(sim_obs) <- c(paste0('obs_',obs_var), paste0('prd_', obs_var))
    # Create a formula dynamically, and fit to linear regression
    formula <- reformulate(paste0('obs_', obs_var), response = paste0('prd_', obs_var))
    lm_regress = lm(formula, data=sim_obs)
    print(obs_var)
    print(summary(lm_regress)$r.squared)
    # plot the data and regression line
    plot(model_data[[obs_var]], model_pred, xlab="observation", ylab="prediction")
    abline(lm_regress, col="red")
    # print(sqrt(mean(lm_regress$residuals^2)))
    
    # save model and predictions
    # sim_obs <- cbind(sim_obs, model_data['State'], model_data['Lat'], model_data['Lon'])
    sim_obs <- cbind(sim_obs, model_data['State'])
    if (i %% 2 == 1) {
      write.csv(sim_obs, paste0(model_dir, 'CC_', obs_var, ".csv"))
      saveRDS(mf_model, file = paste0(model_dir, 'CC_', obs_var, ".rds"))
    }
    else {
      write.csv(sim_obs, paste0(model_dir, 'NT_', obs_var, ".csv"))
      saveRDS(mf_model, file = paste0(model_dir, 'NT_', obs_var, ".rds"))
    }
}



# predict at county level
library(base)
library(zoo)
Nrate <- read.csv('inputs/county_N_rate_kgN_ha_2012.csv')
inputs <- read.csv('inputs/County_inputs.csv')

county_n <- dim(inputs)[1]
inputs$gSOC = inputs$gSOC_gkg
inputs$Nrate <- rep(Nrate$X2012, each=5)
inputs$Tillage <- rep('MP', county_n)
inputs$Legume <- rep('Mixed', county_n)
inputs$Tillage <- factor(inputs$Tillage, levels=c('CP','CT','DP','MP'))
inputs$Legume <- factor(inputs$Legume, levels=c('Mixed','No','Yes'))
inputs <- na.locf(inputs)
# create copies of the inputs for rotation (yes/no) and crop (corn/soy)
inputs_Rot_corn <- copy(inputs)
inputs_Rot_corn$Rotation <- rep('Yes', county_n)
inputs_Rot_corn$Crop <- rep('Corn', county_n)
inputs_Rot_corn$Crop <- factor(inputs_Rot_corn$Crop,levels=c('Corn', 'Soybean'))

inputs_Rot_soy <- copy(inputs)
inputs_Rot_soy$Rotation <- rep('Yes', county_n)

inputs_Rot_soy$Crop <- rep('Soybean', county_n)
inputs_Rot_soy$Crop <- factor(inputs_Rot_soy$Crop,levels=c('Corn', 'Soybean'))

inputs_Con_corn <- copy(inputs)
inputs_Con_corn$Rotation <- rep('No', county_n)
inputs_Con_corn$Crop <- rep('Corn', county_n)
inputs_Con_corn$Crop <- factor(inputs_Con_corn$Crop,levels=c('Corn', 'Soybean'))

# prediction group 1: rotation
# set prediction time for CC as 10 years, and NT as 20 years
pred_vars = c('delta_SOC_30', 'delta_SOC_60', 'delta_SOC_30_filled', 'delta_SOC_60_filled', 
              'delta_N2O_cc', 'delta_N2O_nt', 'delta_lch_cc', 'delta_lch_nt', 'delta_yld_cc', 'delta_yld_nt')
model_prediction = data.frame(State=inputs$State, FIPS=inputs$FIPS)
model_prediction_se = data.frame(State=inputs$State, FIPS=inputs$FIPS)

for (i in seq_along(final_models)) {
  model_name = model_names[i]
  model = final_models[[model_name]]
  var = paste0('Rot_', pred_vars[i])
  print(paste('predicting', var))
  if (i <= 4  | i== 6) {
      inputs_Rot_corn$Rotation <- factor(inputs_Rot_corn$Rotation, levels=c('Mixed', 'No', 'Yes'))
      inputs_Rot_soy$Rotation <- factor(inputs_Rot_corn$Rotation, levels=c('Mixed', 'No', 'Yes'))
  }
  else {
      inputs_Rot_corn$Rotation <- factor(inputs_Rot_corn$Rotation, levels=c('No', 'Yes'))
      inputs_Rot_soy$Rotation <- factor(inputs_Rot_corn$Rotation, levels=c('No', 'Yes'))
  }
  
  if (i%%2 == 1) {
    inputs_Rot_corn$No_year <- rep(10, county_n)
    inputs_Rot_soy$No_year <- rep(10, county_n)
  }
  else {
    inputs_Rot_corn$No_year <- rep(20, county_n)
    inputs_Rot_soy$No_year <- rep(20, county_n)
  }
  
  if (i <= 4) {
      model_prediction[, var] <- predict(model, inputs_Rot_corn)$predictions
      model_prediction_se[, var] <- predict(model, inputs_Rot_corn, type='se')$se
      }
  else {
      var_corn = paste0(var,'_corn')
      var_soy = paste0(var,'_soy')
      model_prediction[, var_corn] <- predict(model, inputs_Rot_corn)$predictions
      model_prediction[, var_soy] <- predict(model, inputs_Rot_soy)$predictions
      model_prediction_se[, var_corn] <- predict(model, inputs_Rot_corn, type='se')$se
      model_prediction_se[, var_soy] <- predict(model, inputs_Rot_soy, type='se')$se
      }
}
print(mean(model_prediction[['Rot_delta_SOC_60']]))


# prediction group 2: continuous corn
# set prediction time for CC as 10 years, and NT as 20 years
for (i in seq_along(final_models)) {
  model_name = model_names[i]
  model = final_models[[model_name]]
  var = paste0('Con_', pred_vars[i])
  print(paste('predicting', var))
  if (i <= 4  | i== 6) {
    inputs_Rot_corn$Rotation <- factor(inputs_Rot_corn$Rotation, levels=c('Mixed', 'No', 'Yes'))
  }
  else {
    inputs_Rot_corn$Rotation <- factor(inputs_Rot_corn$Rotation, levels=c('No', 'Yes'))
  }
  if (i%%2 == 1) {
      inputs_Con_corn$No_year <- rep(10, county_n)
      model_prediction[, var] <- predict(model, inputs_Con_corn)$predictions
  }
  else {
      inputs_Con_corn$No_year <- rep(20, county_n)
      model_prediction[, var] <- predict(model, inputs_Con_corn)$predictions
  }

  model_prediction_se[, var] <- predict(model, inputs_Con_corn, type='se')$se
}


# save model predictions
write.csv(model_prediction, file=paste0(model_dir,'metaforest_county_prediction_new.csv'))
write.csv(model_prediction_se, file=paste0(model_dir, 'metaforest_county_prediction_se_new.csv'))


#----- ShapforXgboost - Reproducible Example with H2O.

library(data.table)
library(h2o)

h2o.init()
   prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
   prostate <- h2o.uploadFile(path = prostate_path)
   prostate_gbm <- h2o.gbm(3:9, "AGE", prostate)
   h2o.predict(prostate_gbm, prostate)
   
   # Get Shap values with h2o.predict_contributions
   contri_dt <- as.data.table(h2o.predict_contributions(prostate_gbm, prostate))
   # Prepare Shap outputs 
   contri_gd <- contri_dt[ , BiasTerm := NULL]
   dattrain_gd <- prostate[, c(4:ncol(prostate)) ]
   
   library("SHAPforxgboost")
   shap_long <- shap.prep(shap_contrib = contri_gd, X_train = dattrain_gd)
   # (Notice that there will be a data.table warning from `melt.data.table` due to `dayint`    coerced from integer to double)
   
   # **SHAP summary plot**
   shap.plot.summary(shap_long)
   
   # sometimes for a preview, you want to plot less data to make it faster using `dilute`
   shap.plot.summary(shap_long, x_bound  = 1.2, dilute = 10)
   
   # option 2: supply a self-made SHAP values dataset (e.g. sometimes as output from cross   -validation)
   shap.plot.summary.wrap2(contri_dt, as.matrix(dattrain_gd))
   
   #------------- Change axis values ---------------------------
   # **SHAP dependence plot**
   # if without y, will just plot SHAP values of x vs. x
   shap.plot.dependence(data_long = shap_long, x = "PSA")
   
   # optional to color the plot by assigning `color_feature` (Fig.A)
   shap.plot.dependence(data_long = shap_long, x= "PSA",
                        color_feature = "GLEASON")
   
   # optional to put a different SHAP values on the y axis to view some interaction (Fig.B)         
   shap.plot.dependence(data_long = shap_long, x= "PSA",
                        y = "GLEASON", color_feature = "GLEASON")     
   
   
   #-------------- Force Plot
   # choose to show top 4 features by setting `top_n = 4`, set 6 clustering groups.  
   plot_data <- shap.prep.stack.data(shap_contrib = contri_gd, top_n = 4, n_groups = 6)
   
   # choose to zoom in at location 500, set y-axis limit using `y_parent_limit`  
   # it is also possible to set y-axis limit for zoom-in part alone using `y_zoomin_limit`  
   shap.plot.force_plot(plot_data, zoom_in_location = 100, y_parent_limit = c(-1,1))
   
   # plot by each cluster
   shap.plot.force_plot_bygroup(plot_data)

h2o.shutdown()


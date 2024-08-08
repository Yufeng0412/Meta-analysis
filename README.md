# Meta-analysis
Develop meta-random-forest models to quantify the enviromental-agronomic performances (SOC, N2O emissions, N leaching, crop yield) of two climate-smart practices promoted in the US based on field observations. Use these model predictions to drive a customized cost-benefit analysis framework that will guide the performance-based allocation of public incentives.

# Steps
1. Data processing (Jupiter Notebook):
2. 
   ● Create unique study ID and site ID by mapping on hash tables.

   ● Calculate SOC stock in different layers using BD and SOC concentration.
 
   ● Sum up to get the total SOC stock at each site.
 
   ● Fit a qaudratic curve using SOC stock data at depth >= 100 cm.
 
   ● Convert SOC at various depths to a uniformed depth, i.e. 30 cm or 60 cm using the fitted equation.
 
3. Meta-random-forest model development (R):

   ● Feature selection based on recursive elimination.
 
   ● Set up 10-fold cross-validation.
 
   ● Train random-effect models on n.mtry and n.nodesize.
 
   ● Build models seperately for SOC, N2O, N leaching, and crop yield.
 
   ● Make prediction using county-level soil and weather inputs.
 
4. Cost-benefit analysis (Jupiter Notebook):

   ● Calculate on-farm costs and carbon benefits associated with SOC, N2O, N leaching, and crop yield.
 
   ● Aggregate the density of cost-benefit ($/ha) by cropland area at county-level.
 
   ● Prioritize the use of incentives (to offset the on-farm costs) based on performances on two dimensions: mitigation intensity (t CO2/ha) and mitigation cost ($ /t CO2).
 
   ● Compare the Prioritized incentive-mitigation curve to the pathway without priority information.
 
5. Visualzation of results (Jupiter Notebook)

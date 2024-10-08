# reject-inference-methods-in-credit-scoring
Statistics for data science final project in R


Repository contains 4 different folders:
	
	-Simulated_Data_scripts:
				 - sim_data_reclassification.R
				 - sim_data_augmentation.R
				 - sim_data_parcelling.R
				 - sim_data_plot_ALL.R
				 - functions.R
				 - Figure_2_simulated_data.png
				 - reclassified_models_list.rds  (just in drive)
				 - financed_models_list.rds      (drive)
				 - augmentation_models_list.rds  (drive)
				 - parcelling_models_list.rds    (drive


Every list of models was saved as rds using saveRDS() in order to facilitate final plot in sim_data_plot_ALL.R reading them by readRDS().

As these files have more than 6GB all together we have uploaded them to a google drive where they can be downloaded to further manipulation and full run with models already calculated.

Script function.R contains all functions which were repeated for all models. The script is imported for every models script (source) 


	-Real_Data_scripts:
			    - UCI_CC_augmentation.R
		            - UCI_CC_parcelling.R
			    - UCI_CC_complete.R
			    - UCI_Credit_Card.csv
		      	    - UCI_CC_all_plot.png
			    - parcelling_models_list_real.rds   (drive)
			    - augmentation_models_list_real.rds (drive)

Parcelling and augmentation models are saved as rds. by saveRDS ().

UCI_CC_complete.R script contains reclassified model. Importing parcelling_models_list_real and augmentation_models_list_real can be runned to obtain a final comparation plot.


	-Strategy_3_single_script:
				   -strategy_3_reclassification.R
				   -Figure_1_Reclassification.png

This file contains just a single strategy for the initial plot of comparing financed and reclassification logistics regression.


	-Statistical_test_script:
				   -statistical_tests.R

This file contains a script of Friedmman, Nemenyi and Shapiro normality test. As previous scripts models have to be uploaded by readRDS().


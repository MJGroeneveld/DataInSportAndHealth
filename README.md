# DataInSportAndHealth

The main file (main.R) in our project contains all the necessary steps for data preparation, visualization, feature engineering, data exploration, correlation plot, etc., and creating the "data_running" file for modeling.

The xgboost.R file contains our machine learning model, which was developed in two steps. Firstly, we trained the default xgboost model, and then we optimized its hyperparameters using grid search. We divided this optimization process into several steps to manage the computation time.

The baseline.R file contains the baseline model, which includes creating a dummy variable and testing it against the target variable. 

The functions.R file contains all the necessary functions used in our main file, which we source on line "source(functions.R)" in main.R. 

Finally, the parameters.R file contains some parameters. 
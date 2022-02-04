import pandas as pd
import re
import xgboost as xgb
import numpy as np
from skopt import BayesSearchCV
from sklearn.cross_decomposition import PLSRegression
from sklearn.model_selection import StratifiedGroupKFold, GroupKFold, KFold

ITERATION = 10
TRAINING_SIZE = 10000
TEST_SIZE = 2500

training_data = pd.read_feather(path = "data/final_dataset_python.feather")
training_data.shape

# Extract the column names (dpin):
p = re.compile("^dpin")
dpin = [s for s in training_data.columns if p.match(s)]

# Extract the pls scores (pls):
p = re.compile("^pls")
pls_col = [s for s in training_data.columns if p.match(s)]

base_variables = ["dim", "milk_yield", "parity"] + dpin
pls_variables =  ["dim", "milk_yield", "parity"] + pls_col


X = training_data[pls_variables]
y = training_data["bodyweight"].values

X.shape
y.shape


bayes_cv_tuner = BayesSearchCV(
    estimator=xgb.XGBRegressor(
        verbosity=0, # Silent [0, 3] Bug
        objective='reg:squarederror', # (objective)
        booster='gbtree', # (booster): gblinear, gbtree, dart.
        tree_method = 'approx', # Could be turned to 'exact' for the final selected model
        n_jobs=1, # Parallel threads used to run xgboost
        eval_metric=['rmse'],
    ),
    search_spaces={
        'n_estimators': (1, 5), # n of gradient boosted trees (num_round)
        'max_depth': (1, 10), # Maximum tree depht for base learner. (max_depth)
        'learning_rate': (.01, 1.0, 'log-uniform'), # Boosting learning rate (eta)
        'gamma': (1, 2, 'log-uniform'), # (gamma) minimum loss-reduction required to partition
        # 'min_child_weight': (10, 40), # minimum of weight (or samples, if all weighed to one) to allow leaf split
        'subsample': (.2, .4, 'uniform'),
        'colsample_bytree': (.01, 1.0, 'uniform'),
        'reg_lambda': (1, 10, 'log-uniform'),
        'reg_alpha': (1, 10, 'log-uniform'),
        'min_child_weight': (0, 5),
        # 'scale_pos_weight': (1, 2, 'log_uniform'),
    },
    #scoring='rmse',
    #cv=StratifiedGroupKFold(n_splits=3, shuffle=True, random_state=42),
    cv=KFold(n_splits=3, shuffle=True, random_state=42),
    n_jobs=3,
    n_iter=2,
    verbose=1,
    refit=True,
    random_state=42
)


def status_print(optim_result):
    """Status call back"""
    # print(optim_result)
    # all_models = pd.DataFrame(bayes_cv_tuner.cv_results_)
    # best_params = pd.Series(bayes_cv_tuner.best_params_)
    # print(f'Model {len(all_models)}\n'
    #       f'RMSE: {np.round(bayes_cv_tuner.best_score_)}')
    # clf_name=bayes_cv_tuner.estimator.__class__.__name__
    #all_models.to_csv(clf_name+"_cv_result.csv")


result = bayes_cv_tuner.fit(X, y, callback=status_print)
#result = bayes_cv_tuner.fit(X, y)

result.cv_results_

result.best_score_
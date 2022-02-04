# Bandit like approach
# In every iteration, the score for each terminal implementation of top-level signature is caculated
# Score of a terminal implementation := the mean of the ELPD values of the models that contain the implementation
# For each model in the model space, the score of the model is the sum of score of its terminal implementations

import numpy as np
import itertools
import random

# Top_level_signature_implementations: 2-dimensional list
# the ith element is a list that contains all possible terminal implementation of the ith top-level signature

# Each model can be represented as an index vector
# e.g. model m = [2,3,0,1,3] the terminal implementations of model m are the 3rd, 4th, 1st, 2nd, 4th implementations of the top-level signatures


def ELPD_compute(model):
    return 0

def Bandit_approach_Score_based_search(Top_level_signature_implementations,k):
    # n: the number of top-level signatures
    n = len(Top_level_signature_implementations)
    # Create model list
    Model_list = [model for model in itertools.product([range(len(ith_signature)) for ith_signature in Top_level_signature_implementations])]
    # create lists to save the models whose ELPD values are evaluated and their respective ELPD values
    Models_evaluated = []
    ELPD_vals = []
    sum_ELPDs = 0
    # create lists to save the following two values for each terminal implementation
    # (1) the sum of ELPD values of the models that contain the terminal implementation (2) the number of models (whose ELPD values are evaluated) that contain the terminal implementation
    sum_ELPD_values_for_each_implementation =[[0 for terminal_implementation in ith_signature] for ith_signature in Top_level_signature_implementations]
    total_num_of_occurences_for_each_implementation = [[0 for terminal_implementation in ith_signature] for ith_signature in Top_level_signature_implementations]
    for iteration_ind in range(k):
        # calculate the average ELPD value for a terminal implementation
        avg_ELPD = sum_ELPDs/(iteration_ind) if iteration_ind >0 else 1
        # compute the score of each terminal implementation
        Score_cur_iteration = [[
            sum_ELPD_values_for_each_implementation[i][j]/total_num_of_occurences_for_each_implementation[i][j] if total_num_of_occurences_for_each_implementation[i][j] > 0 else avg_ELPD
            for j in range(Top_level_signature_implementations[i])]
            for i in range(len(Top_level_signature_implementations))
        ]
        # Compute the score of each model by adding up the score of its terminal implementations
        Score_each_model = [ sum([Score_cur_iteration[i][model[i]] for i in range(n)]) for model in Model_list]
        Model_score_sum = sum(Score_each_model)
        # Compute the selection probability of each model by normalizing the scores
        Selection_prob_each_model = [model_score/Model_score_sum for model_score in Score_each_model]
        # Randomly draw one model based on the selection probabilities
        cur_iteration_model_ind = random.choices(range(len(Model_list)), weights=Selection_prob_each_model, cum_weights=None, k=1)[0]
        # update Models_evaluated, ELPD_vals, sum_ELPDs
        Models_evaluated.append(cur_iteration_model_ind)
        cur_iter_model_ELPD = ELPD_compute(Model_list[cur_iteration_model_ind])
        ELPD_vals.append(cur_iter_model_ELPD)
        sum_ELPDs += cur_iter_model_ELPD
        # update sum_ELPD_values_for_each_implementation, total_num_of_occurences_for_each_implementation
        for i in range(n):
            implementation_ind = Model_list[cur_iteration_model_ind][i]
            sum_ELPD_values_for_each_implementation[i][implementation_ind] = sum_ELPD_values_for_each_implementation[i][implementation_ind] + cur_iter_model_ELPD
            total_num_of_occurences_for_each_implementation[i][implementation_ind] = total_num_of_occurences_for_each_implementation[i][implementation_ind]+1
    # Return the model selected at the final iteration
    return Model_list[Models_evaluated[-1]]
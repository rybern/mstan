import numpy as np
import random
import subprocess
import sys

# Generate a chain given the Top_level_Signature_Hierarchy information
# The following function returns a chain of models where for any i, the ith model's model complexity is strictly higher than that of the (i+1)th model
# In other words, the models are sorted in a decreasing order of model complexity along the chain
# (1) Chain is a list (2) Each element of the Chain is a model (type: list) (3) The ith element of a model is the 'implementation' of the model for the ith top-level signature
def Chain_Generation(Top_level_Signature_Hierarchy):
    Chain = []
    # m: nubmer of top-level signatures
    m = len(Top_level_Signature_Hierarchy)
    # Create the apex predator model of the chain by choosing the implementations with highest hierarchies for all the top level signatures
    # Apex_predator = [top_level_signature_implementations[0] for top_level_signature_implementations in Top_level_Signature_Hierarchy]
    # Add the apex predator model to the Chain
    # Chain.append(Apex_predator)
    # create a list that checks the indices of the implementations of the most recent module (added to the chain) in the top-level signature implementation lists
    Cur_indices = [0 for i in range(m)]
    Cur_Indices_sum = sum(Cur_indices)
    # The following list represents the possible increments for the indices
    Possible_increments_for_indices = [ len(Top_level_Signature_Hierarchy[i]) - 1 - Cur_indices[i] for i in range(m)]
    # Candidates for increments (the indices of top-level signatures whose implementations' indices can be increased)
    Candidates_for_increment = []
    for i in range(m):
        if Possible_increments_for_indices[i] > 0:
            Candidates_for_increment.append(i)
    # The maximum value for the sum of indices
    Indices_sum_UB =  np.sum([len(implementations)-1 for implementations in Top_level_Signature_Hierarchy])
    # Create the next model for the chain by randomly increasing the index of exactly one implementation of the previous model wherever possible
    # If the Chain contains the model with the lowest possible complexity (in which case the Cur_Indices_sum equals the Indices_sum_UB, terminate the Chain generation algorithm)
    while Cur_Indices_sum < Indices_sum_UB:
        # Get the current iteration's model based on the Current indices & Top-level signature hierarchy
        cur_iter_model = [Top_level_Signature_Hierarchy[i][Cur_indices[i]] for i in range(m)]
        Chain.append(cur_iter_model)
        # Randomly increase the index of a particular implementation
        increment_ind = random.choice(Candidates_for_increment)
        # Increment the index of the implementation and update Cur_Indices_sum, Cur_indices, Possible_increments_for_indices, Candidates_for_increment
        Cur_Indices_sum +=1
        Cur_indices[increment_ind] = Cur_indices[increment_ind]+1
        Possible_increments_for_indices[increment_ind] = Possible_increments_for_indices[increment_ind]-1
        if Possible_increments_for_indices[increment_ind] == 0:
            Candidates_for_increment.remove(increment_ind)
    return Chain



def text_command(args):
    """Run a shell command, return its stdout as a String or throw an exception if it fails."""

    try:
        result = subprocess.run(args, text=True, check=True,
                                stderr=subprocess.STDOUT, stdout=subprocess.PIPE)

        stdout = result.stdout.strip()
        return stdout
    except subprocess.CalledProcessError as exc:
        sys.exit("Error in `mstan`: \"" + exc.output.strip() + "\"")


class ModelEvaluator:
    def __init__(self, dataFile):
        self.dataFile = dataFile

    def score(self, modelPath):
        """Return the numerical score for the Stan program at the given filepath"""
        stdout_result = text_command(["Rscript", "elpd.R", modelPath, self.dataFile])
        return float(stdout_result.split('\n')[-1].strip())


# Compute the ELPD value of a model
# model is a list where its ith element is a string that represents the implementation for the ith top-level signature
def ELPD(model, data_file):
    
    # use the elements of 'model' (type: list) to obtain the full 'name' of the model
    # Then use 'STAN' to compute ELPD of the model (based on the 'model name' obtained above)
    model_code_args = ["mstan", "-f", "birthday.m.stan", "concrete-model", "-s", ",".join(model) + ",Regression:glm",]
    model_code = text_command(model_code_args)
    with open("temp_stanmodel.stan", "w") as f:
        f.write(model_code)
    result = ModelEvaluator(data_file).score("temp_stanmodel.stan")
    print(f"model: {','.join(model)} ELPD:{result}")
    return result


# Chain is a list whose elements are individual models. Each model is a list that consists of the implementations of the top-level signatures.
# K: a parameter that represents the maximum number of models that their ELPD values can be computed (the value of the parameter is dependent on the computational resource, total number of chains to be searched, etc.)
# alpha: parameter for controlling the range of the stepsize in each iteration
# The following function conducts a dynamic search along the chain
# and finally returns the model with the highest ELPD value and its ELPD value (among those that the ELPD values were computed)
# Suppose that the chain is given as an input and the models are sorted in a decreasing order of model complexity.
# i.e. the 1st model in the chain has the highest model complexity and the last model has the lowest complexity.
def Chain_Search(Chain,K,data_file_dir, alpha=0.5):
    n = len(Chain)
    # if number of models in the chain is smaller than or equal to K, we can compute ELPD values of each model and choose the one with the highest value
    if n <=K:
        ELPD_values = [ELPD(model, data_file_dir) for model in Chain]
        highest_ELPD_model_ind = np.argmax(ELPD_values)
        best_model, best_ELPD_val = Chain[highest_ELPD_model_ind], ELPD_values[highest_ELPD_model_ind]
        return best_model, best_ELPD_val
    # if number of models in the chain is strictly larger than K, then there is no choice but to conduct a search
    else:
        cur_ind = 0
        num_ELPD_computed = 0
        ELPD_computed_model_indices = []
        ELPD_values_obtained = []
        while num_ELPD_computed < K and cur_ind < n:
            # compute the ELPD value of the current iteration's model
            cur_iter_ELPD = ELPD(Chain[cur_ind], data_file_dir)
            # update the ELPD compute model indices, ELPD values, the nubmer of ELPD values computed obtained respectively
            ELPD_computed_model_indices.append(cur_ind)
            ELPD_values_obtained.append(cur_iter_ELPD)
            num_ELPD_computed+=1
            step_size = 1 # set default step size as 1
            # if it is neither the 1st nor the 2nd iteration, the step size should be modified.
            if not (num_ELPD_computed == 1 or num_ELPD_computed==2):
                step_size_Uniform = (n-1-cur_ind)/(K-num_ELPD_computed)
                step_size_LB = step_size_Uniform*(1-alpha)
                step_size_UB = step_size_Uniform*(1+alpha)
                ELPD_slope_cur_iter = abs((ELPD_values_obtained[-1]-ELPD_values_obtained[-2]))/(ELPD_computed_model_indices[-1]-ELPD_computed_model_indices[-2])
                ELPD_slope_previous_iter = abs((ELPD_values_obtained[-2]-ELPD_values_obtained[-3]))/(ELPD_computed_model_indices[-2]-ELPD_computed_model_indices[-3])
                step_size_candidate = step_size_Uniform*(ELPD_slope_previous_iter/ELPD_slope_cur_iter)
                step_size = min(step_size_UB,min(step_size_LB,step_size_candidate))
            # update the current index (which would the index of the model in the next iteration)
            cur_ind += step_size
        # find the best model (the model with the highest ELPD value) among the models whose ELPD values were computed.
        highest_ELPD_value_model_chain_search_ind = np.argmax(ELPD_values_obtained)
        Final_best_ELPD_model_ind, Final_best_ELPD_val = ELPD_computed_model_indices[highest_ELPD_value_model_chain_search_ind], ELPD_values_obtained[highest_ELPD_value_model_chain_search_ind]
        Final_best_model = Chain[Final_best_ELPD_model_ind]
        return Final_best_model, Final_best_ELPD_val


# Top level Signature Hiearchy is a list
# The ith element of a Hiearchy is a list which contains different implementations that are sorted in a decreasing order of implementation hierarchy
Top_level_Signature_Hierarchy = [
    ["DayOfWeekTrend:yes,DayOfWeekWeights:weighted","DayOfWeekTrend:yes,DayOfWeekWeights:uniform","DayOfWeekTrend:no"],
    ["DayOfYearTrend:yes,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:yes","DayOfYearTrend:yes,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:no","DayOfYearTrend:yes,DayOfYearHeirarchicalVariance:no,DayOfYearNormalVariance:no","DayOfYearTrend:no"],
    ["HolidayTrend:yes","HolidayTrend:yes"],
    ["LongTermTrend:yes","LongTermTrend:no"],
    ["SeasonalTrend:yes","SeasonalTrend:no"]
]

chain = Chain_Generation(Top_level_Signature_Hierarchy)

data_file_dir = "examples/birthday/births_usa_1969.json"

K = 3

best_model, best_elpd = Chain_Search(Chain=chain, K=K, data_file_dir=data_file_dir)
print(best_model, best_elpd)

# Original Chain Generation Algorithm (Proposed earlier)

# Simple example
# Model_collection =[
#    'Mean:normal,Stddev:lognormal,StddevInformative:no',
#    'Mean:normal,Stddev:lognormal,StddevInformative:yes',
#    'Mean:normal,Stddev:standard',
#    'Mean:standard,Stddev:lognormal,StddevInformative:no',
#    'Mean:standard,Stddev:lognormal,StddevInformative:yes',
#    'Mean:standard,Stddev:standard'
# ]


# # Simple example
# Current_Node = ['normal','lognormal,StddevInformative:yes']
# Module_names = ['Mean','Stddev']
# Hierarchy = [{'normal':['standard']},
#                 {'lognormal,StddevInformative:yes':['standard'],'lognormal,StddevInformative:no':['standard']}
#            ]

# # Birthday Case study
# Current_Node = ['yes,DayOfWeekWeights:weighted','yes,DayOfYearHierarchicalVariance:yes,DayOfYearNormalVariance:yes','yes','yes','yes']
# Module_names = ['DayOfWeekTrend','DayofYearTrend','HolidayTrend','LongTermTrend','SeasonalTrend']
# Hierarchy = [{'yes,DayOfWeekWeights:weighted':['no'],'yes,DayOfWeekWeights:uniform':['no']},
#             {'yes,DayOfYearHierarchicalVariance:yes,DayOfYearNormalVariance:yes':['no'],
#              'yes,DayOfYearHierarchicalVariance:yes,DayOfYearNormalVariance:no':['no'],
#              'yes,DayOfYearHierarchicalVariance:no,DayOfYearNormalVariance:yes':['no'],
#              'yes,DayOfYearHierarchicalVariance:no,DayOfYearNormalVariance:no':['no']},
#             {'yes':['no']},
#             {'yes': ['no']},
#             {'yes': ['no']}
# ]

# def Generate_Maximal_Chain(Current_Node,Module_names,Hierarchy):
#    num_Modules = len(Current_Node)
#    Chain = [Current_Node]
#    Chain_length = 1
#    Longest_Chain = [Current_Node]
#    Longest_Chain_length = 1
#    for i in range(num_Modules):
#        if Current_Node[i] in Hierarchy[i]:
#            for alternative in Hierarchy[i][Current_Node[i]]:
#                Next_node = [Current_Node[j] if i !=j else alternative for j in range(num_Modules)]
#                Additional_Chain,Additional_Chain_length = Generate_Maximal_Chain(Next_node,Module_names, Hierarchy)
#                if Chain_length+Additional_Chain_length > Longest_Chain_length:
#                    Longest_Chain = Chain + Additional_Chain
#                    Longest_Chain_length = Chain_length+Additional_Chain_length
#    return Longest_Chain, Longest_Chain_length

# Longest_Chain, Longest_Chain_length = Generate_Maximal_Chain(Current_Node,Module_names,Hierarchy)
# print("Longest Chain: ",[[Module_names[k]+":"+node[k] for k in range(len(node))]  for node in Longest_Chain])
# print("Longest Chain Length: ",Longest_Chain_length)


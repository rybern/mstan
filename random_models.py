from json.tool import main
from sys import implementation
import pandas as pd
import numpy as np
from scipy.stats import linregress
from elpd_df import *

model_df = read_csv("birthday_df.csv")


def row_to_string(row):
    dict_form = row.to_dict()
    del dict_form["elpd"]
    return ",".join(list(map(lambda x: f"{x[0]}:{list(x[1].values())[0]}", dict_form.items())))


def generate_chain(length):
    filtered_df = model_df
    model_chain = []
    yes_set = set()
    selected_model_strs = []
    for _ in range(length):
        temp_df = filtered_df
        for col in yes_set:
            temp_df = temp_df[(temp_df[col] != "no") & (temp_df[col] != None)]
        selected_model = temp_df.sample()
        string_repr = row_to_string(selected_model)
        if string_repr in selected_model_strs:
            if temp_df.shape[0] == 1:
                break
            continue
        
        selected_model_strs.append(string_repr)
        filtered_df = temp_df
        model_chain.append([string_repr, selected_model["elpd"].item()])
        yes_set.update(list(selected_model.columns[(selected_model == "yes").iloc[0]]))

    return model_chain

if __name__ == "__main__":
    N = 200
    n_positive = 0
    slope_sum = 0
    failed = 0
    for _ in range(N):
        x = []
        y = []
        chain = generate_chain(5)
        #print(len(chain))
        for indx, val in enumerate(chain):
            x.append(indx)
            y.append(val[1])
        
        slope = linregress(x, y).slope
        if np.isnan(slope):
            failed += 1
        else:
            slope_sum += slope
        print(slope)
        if slope > 0:
            n_positive += 1
    
    print("-" * 10)
    print(slope_sum)
    print(slope_sum / (N - failed))
    print(n_positive)
        



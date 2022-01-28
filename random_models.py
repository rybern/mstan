from json.tool import main
from sys import implementation
import pandas as pd
import numpy as np
from scipy.stats import linregress


elpd_results = {
    "DayOfWeekTrend:no,DayOfYearTrend:no,HolidayTrend:no,LongTermTrend:no,Regression:glm,SeasonalTrend:no": 5232.03,
    "DayOfWeekTrend:no,DayOfYearHeirarchicalVariance:no,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:no,Regression:glm,SeasonalTrend:no": 4578.51,
    "DayOfWeekTrend:no,DayOfYearTrend:no,HolidayTrend:no,LongTermTrend:no,Regression:glm,SeasonalTrend:yes": 5585.64,
    "DayOfWeekTrend:no,DayOfYearTrend:no,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 6717.52,
    "DayOfWeekTrend:no,DayOfYearTrend:no,HolidayTrend:yes,LongTermTrend:no,Regression:glm,SeasonalTrend:no": 5258.05,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearTrend:no,HolidayTrend:no,LongTermTrend:no,Regression:glm,SeasonalTrend:no": 6910.55,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearTrend:no,HolidayTrend:no,LongTermTrend:no,Regression:glm,SeasonalTrend:no": 2534.39,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearHeirarchicalVariance:no,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:no,Regression:glm,SeasonalTrend:no": 6540.03,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearHeirarchicalVariance:no,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:no,Regression:glm,SeasonalTrend:no": 7457.22,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:no,Regression:glm,SeasonalTrend:no": 7812.98,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:no,Regression:glm,SeasonalTrend:no": 6935.09,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearTrend:no,HolidayTrend:no,LongTermTrend:no,Regression:glm,SeasonalTrend:yes": 7522.39,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearTrend:no,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 10947.29,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearTrend:no,HolidayTrend:yes,LongTermTrend:no,Regression:glm,SeasonalTrend:no": 6818.07,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearHeirarchicalVariance:no,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 13949.47,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearHeirarchicalVariance:no,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 13956.11,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 13979.34,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 13959.7,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearTrend:no,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:yes": 13127.09,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearTrend:no,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 11204.72,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearTrend:no,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 9539.07,
    "DayOfWeekTrend:no,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 7179.98,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:yes": 14076.06,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 14526.21,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 14620.97,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:no,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 14599.08,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:no,Regression:glm,SeasonalTrend:no": 4232.97,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:yes": 8271.31,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 15299.22,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 14598.53,
    "DayOfWeekTrend:no,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 7228.88,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:no,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 14755.47,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:no,Regression:glm,SeasonalTrend:no": 3283.42,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:no,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:yes": 15071.18,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 15313.68,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearTrend:no,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 11466.83,
    "DayOfWeekTrend:no,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 5882.44,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 12213.75,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:no,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:no": 14929.74,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:no,Regression:glm,SeasonalTrend:no": 1606.94,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:yes": 15384.86,
    "DayOfWeekTrend:no,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:yes": 5888.01,
    "DayOfWeekTrend:yes,DayOfWeekWeights:uniform,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:yes": 14608.01,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:no,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:yes": 15301.54,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:no,LongTermTrend:yes,Regression:glm,SeasonalTrend:yes": 11698.06,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:yes,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:no,Regression:glm,SeasonalTrend:yes": 2161.10,
    "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearTrend:no,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:yes": 14109.15
}

columns = ['DayOfWeekWeights', 'HolidayTrend', 'DayOfYearNormalVariance', 'DayOfYearHeirarchicalVariance', 'Regression', 'DayOfWeekTrend', 'SeasonalTrend', 'DayOfYearTrend', 'LongTermTrend', 'elpd']

model_list = []

for model_string, elpd in elpd_results.items():
    values = [None] * len(columns)
    for substr in model_string.split(","):
        signature, implementation = substr.split(":")
        values[columns.index(signature)] = implementation
    values[-1] = elpd
    model_list.append(values)

model_df = pd.DataFrame(model_list, columns=columns)



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
        



from elpd_df import *
import pandas as pd

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
    model_dict = model_string_to_dict(model_string)
    model_dict["elpd"] = elpd
    model_list.append(model_dict)

df = pd.DataFrame(model_list)

save_csv(df, "birthday_df.csv")

# 15301.54
test_str = "DayOfWeekTrend:yes,DayOfWeekWeights:weighted,DayOfYearHeirarchicalVariance:no,DayOfYearNormalVariance:yes,DayOfYearTrend:yes,HolidayTrend:yes,LongTermTrend:yes,Regression:glm,SeasonalTrend:yes"

print(model_string_to_dict(test_str))

# lookup a model
result = search_df(df, model_string_to_dict(test_str))
print(result)

print("-" * 10)
# update/add a model and elpd value
df = upsert_model(df, model_string=test_str, elpd=9999999)
print(df)


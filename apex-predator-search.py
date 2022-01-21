import subprocess

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

model_file_name = "examples/birthday/birthday.m.stan"

args = ["mstan", "-f", model_file_name, "get-highest-models"]

result = subprocess.run(args, text=True, check=True,
                        stderr=subprocess.STDOUT, stdout=subprocess.PIPE)

stdout = result.stdout.strip().split("\n")

results = {}

for model in stdout:
    model_code_args = ["mstan", "-f", model_file_name, "concrete-model", "-s", model,]
    print(model_code_args)
    model_code = subprocess.run(model_code_args, text=True, check=True, stderr=subprocess.STDOUT, stdout=subprocess.PIPE).stdout.strip()
    with open("temp_stanmodel.stan", "w") as f:
        f.write(model_code)
    
    result = ModelEvaluator("examples/birthday/births_usa_1969.json").score("temp_stanmodel.stan")
    results[model] = result

print(results)




hierarchy_info = [
    ["DayofWeekTrend:Yes,DayofWeekWeights:weighted", "DayofWeekTrend:Yes,DayofWeekWeights:uniform", "DayofWeekTrend:no"],
    ["DayofYearTrend:yes,DayofHierarchicalVariance:yes,DayofYearNormalVariance:yes","DayofYearTrend:yes,DayofHierarchicalVariance:yes,DayofYearNormalVariance:no","DayofYearTrend:yes,DayofHierarchicalVariance:no,DayofYearNormalVariance:yes", "DayofYearTrend:no"]
    ["HolidayTrend:Yes", "HolidayTrend:No"],
    #...
]  # n, n-1, ... 1

current_model = ["DayofWeek:Yes", "HolidayTrend:Yes"]

chain = []
chain.append(",".join(current_model))
current_model[0] = hierarchy_info[0][1]
chain.append(",".join(current_model))
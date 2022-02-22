import subprocess
import pathlib
import elpd_df

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
    def __init__(self, df_path):
        self.df_path = df_path
        self.df = elpd_df.read_csv(df_path)

    def score(self, model_string):
        """Return the numerical score for the Stan program at the given filepath"""
        elpd = elpd_df.search_df(self.df, model_string=model_string).elpd.values[0]
        return elpd


example_dir = pathlib.Path(__file__).resolve().parents[1].absolute().joinpath("examples")
model_file_path = example_dir.joinpath("birthday/birthday.m.stan")
model_df_path = "birthday_df.csv"

model_file_path = example_dir.joinpath("roach/roach.m.stan")
model_df_path = "roach_df.csv"


args = ["mstan", "-f", model_file_path, "get-highest-models"]

result = subprocess.run(args, text=True, check=True,
                        stderr=subprocess.STDOUT, stdout=subprocess.PIPE)

stdout = result.stdout.strip().split("\n")

results = {}

for model in stdout:
    result = ModelEvaluator(model_df_path).score(model)
    results[model] = result


for key, val in results.items():
    print(f"{key} : {val}")


# hierarchy_info = [
#     ["DayofWeekTrend:yes,DayofWeekWeights:weighted", "DayofWeekTrend:yes,DayofWeekWeights:uniform", "DayofWeekTrend:no"],
#     ["DayofYearTrend:yes,DayofHierarchicalVariance:yes,DayofYearNormalVariance:yes","DayofYearTrend:yes,DayofHierarchicalVariance:yes,DayofYearNormalVariance:no","DayofYearTrend:yes,DayofHierarchicalVariance:no,DayofYearNormalVariance:yes", "DayofYearTrend:no"]
#     ["HolidayTrend:yes", "HolidayTrend:no"],
#     ["LongTermTrend:yes", "LongTermTrend:no"]
#     ["SeasonTrend:yes", "SeasonTrend:no"]
#     #...
# ]  # n, n-1, ... 1

# current_model = ["DayofWeek:Yes", "HolidayTrend:Yes"]

# chain = []
# chain.append(",".join(current_model))
# current_model[0] = hierarchy_info[0][1]
# chain.append(",".join(current_model))
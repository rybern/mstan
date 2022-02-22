import subprocess
import sys
import tempfile


def text_command(args):
    """Run a shell command, return its stdout as a String or throw an exception if it fails."""

    try:
        result = subprocess.run(args, text=True, check=True,
                                stderr=subprocess.STDOUT, stdout=subprocess.PIPE)

        stdout = result.stdout.strip()
        return stdout
    except subprocess.CalledProcessError as exc:
        sys.exit("Error running shell command: \"" + exc.output.strip() + "\"")


class ModelEvaluator:
    def __init__(self, dataFile):
        self.dataFile = dataFile

    def score(self, modelPath):
        """Return the numerical score for the Stan program at the given filepath"""
        stdout_result = text_command(["Rscript", "elpd.R", modelPath, self.dataFile])
        return float(stdout_result.split('\n')[-1].strip())


# Compute the ELPD value of a model
# model is a list where its ith element is a string that represents the implementation for the ith top-level signature
def calculate_elpd(model_file, model_string, data_file):
    
    # use the elements of 'model' (type: list) to obtain the full 'name' of the model
    # Then use 'STAN' to compute ELPD of the model (based on the 'model name' obtained above)
    model_code_args = ["mstan", "-f", model_file, "concrete-model", "-s", model_string,]
    model_code = text_command(model_code_args)
    with tempfile.NamedTemporaryFile(suffix=".stan", mode="w") as f:
        f.write(model_code)
        f.flush()
        result = ModelEvaluator(data_file).score(f.name)
        return result
    #print(f"model: {','.join(model)} ELPD:{result}")


def get_all_model_strings(model_file):
    # return a list of all model strings given a mstan model file
    model_code_args = ["mstan", "-f", model_file, "list-all-models"]
    models = text_command(model_code_args)
    return models.split("\n")

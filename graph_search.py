import subprocess
import sys
import os.path

def text_command(args):
    """Run a shell command, return its stdout as a String or throw an exception if it fails."""
    result = subprocess.run(args, capture_output=True, text=True, check=False)

    stdout = result.stdout.strip()
    return stdout

class ModelEvaluator:
    def __init__(self, dataFile):
        self.dataFile = dataFile

    def score(self, modelPath):
        """Return the numerical score for the Stan program at the given filepath"""
        return float(text_command(["bash", "./score.sh", modelPath]))

model_dir = "temp-models/"

class ModelGraph:
    def __init__(self, modularStanFile):
        self.modularStanFile = modularStanFile

        # The model name without directory or extension
        filename = os.path.basename(modularStanFile)
        withoutStan, stan = os.path.splitext(filename)
        withoutM, m = os.path.splitext(withoutStan)
        self.modularStanFileBasename = withoutM if stan == ".stan" and m == ".m" else filename

    def execCommand(self, args):
        """Execute a command on this graph, given arguments to the `mstan` API"""
        return text_command(["./mstan", "exec", "-f", self.modularStanFile] + args)

    def getConcreteModel(self, modelID):
        """Build a concrete Stan program from a model ID and return its filepath."""
        outFile = model_dir + self.modularStanFileBasename + "_" + modelID + ".stan"
        self.execCommand(["-o", outFile, "get-model", "-s", modelID])
        return outFile

    def getModelNeighbors(self, modelID):
        """Return a list of the models that neighbor the given model in the model graph"""
        unparsedNeighbors = self.execCommand(["get-neighbors", "-s", modelID])
        return list(filter(lambda s: s, unparsedNeighbors.split('\n')))

    def getFirstModel(self):
        """Get an arbitrary model ID from the model graph"""
        return self.execCommand(["get-first-model"])

def modelSearch(modelGraph, modelEvaluator):
    """Return a model ID from the given model graph that scores well on the given model evaluator"""
    first = modelGraph.getFirstModel()

    concrete = modelGraph.getConcreteModel(first)

    print("model id:", first)
    print("filepath:", concrete)
    print("neighbors:", modelGraph.getModelNeighbors(first))
    print("score:", modelEvaluator.score(concrete))

defaultData = "test-data.r"
defaultProgram = "gq-concatenation.m.stan"

modularStanProgram = sys.argv[1] if len(sys.argv) > 1 else defaultProgram
testData = sys.argv[2] if len(sys.argv) > 2 else defaultData

modelSearch(ModelGraph(modularStanProgram), ModelEvaluator(testData))

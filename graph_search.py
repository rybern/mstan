import subprocess
import sys
import os.path
from queue import PriorityQueue

# DEBUG_IO = True
DEBUG_IO = False

def text_command(args):
    """Run a shell command, return its stdout as a String or throw an exception if it fails."""
    if DEBUG_IO:
        print("Running:", " ".join(args))

    try:
        result = subprocess.run(args, text=True, check=True,
                                stderr=subprocess.STDOUT, stdout=subprocess.PIPE)
        if DEBUG_IO:
            print("\t ..returned.")
        stdout = result.stdout.strip()
        return stdout
    except subprocess.CalledProcessError as exc:
        sys.exit("Error in `mstan`: \"" + exc.output.strip() + "\"")


class ModelEvaluator:
    def __init__(self, dataFile):
        self.dataFile = dataFile

    def score(self, modelPath):
        """Return the numerical score for the Stan program at the given filepath"""
        # return float(text_command(["bash", "./score.sh", modelPath]))
        stdout_result = text_command(["Rscript", "elpd/elpd.R", modelPath, self.dataFile])
        return float(stdout_result.split('\n')[-1].strip())

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

def modelGraphTest(modelGraph, modelEvaluator):
    """Print out some example usage of the model graph"""
    print("Model graph example:")

    first = modelGraph.getFirstModel()
    print("\tFIRST MODEL ID:\n\t\t", first)

    concrete = modelGraph.getConcreteModel(first)
    print("\tCONCRETE FILEPATH:\n\t\t", concrete)

    neighbors = modelGraph.getModelNeighbors(first)
    print("\tNEIGHBORS:\n\t\t", "\n\t\t ".join(neighbors))

    score = modelEvaluator.score(concrete)
    print("\tSCORE:\n\t\t", score)

def modelSearch(modelGraph, modelEvaluator, exhaustive=False):
    """Return a model ID from the given model graph that scores well on the given model evaluator"""

    numScores = 0
    numExpands = 0

    def score(modelID):
        concreteModelPath = modelGraph.getConcreteModel(modelID)
        nonlocal numScores
        numScores += 1
        return modelEvaluator.score(concreteModelPath)

    def expand(modelID):
        nonlocal numExpands
        numExpands += 1
        return modelGraph.getModelNeighbors(currentModel)

    maxScore, maxModel = None, None

    horizon = PriorityQueue()

    firstModel = modelGraph.getFirstModel()
    firstModelScore = score(firstModel)
    horizon.put((-firstModelScore, firstModel))

    visited = set()

    while not horizon.empty():
        negCurrentScore, currentModel = horizon.get()
        currentScore = -negCurrentScore

        if currentModel in visited:
            continue
        visited.add(currentModel)

        print("Visiting:")
        print("\tModel ID:\t",  currentModel)
        print("\tScore:\t\t", currentScore)

        if not maxScore or currentScore >= maxScore:
            maxScore, maxModel = currentScore, currentModel
        else:
            if not exhaustive: break

        for neighbor in expand(currentModel):
            if not neighbor in visited:
                score = score(neighbor)
                print("\tPush neighbor:\t", score)
                horizon.put((-score, neighbor))

    print()
    print("Winner:")
    print("\tModel ID:\t",  maxModel)
    print("\tScore:\t\t", maxScore)
    print(numScores, "scores")
    print(numExpands, "expands")

    return maxModel

if __name__ == "__main__":
    # The program was called as an executable

    defaultData = "test-data.r"
    defaultProgram = "examples/gq-concatenation.m.stan"

    if len(sys.argv) < 2:
        print("Expected arguments: [modular stan file] [data file path]")
        print("Using defaults.")

    modularStanProgram = sys.argv[1] if len(sys.argv) > 1 else defaultProgram
    testData = sys.argv[2] if len(sys.argv) > 2 else defaultData

    # modelGraphTest(ModelGraph(modularStanProgram), ModelEvaluator(testData))

    modelSearch(ModelGraph(modularStanProgram), ModelEvaluator(testData)) #, exhaustive = True)

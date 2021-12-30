import subprocess
import sys
import os.path
import json
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
        stdout_result = text_command(["Rscript", "elpd/elpd.R", modelPath, self.dataFile])
        return float(stdout_result.split('\n')[-1].strip())

        # return float(text_command(["bash", "./score.sh", modelPath]))

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

def modelGraphTest(modelGraph, modelEvaluator, firstModel = None):
    """Print out some example usage of the model graph"""
    print("Model graph example:")

    if not firstModel:
        firstModel = modelGraph.getFirstModel()
    print("\tFIRST MODEL ID:\n\t\t", firstModel)

    concrete = modelGraph.getConcreteModel(firstModel)
    print("\tCONCRETE FILEPATH:\n\t\t", concrete)

    neighbors = modelGraph.getModelNeighbors(firstModel)
    print("\tNEIGHBORS:\n\t\t", "\n\t\t ".join(neighbors))

    score = modelEvaluator.score(concrete)
    print("\tSCORE:\n\t\t", score)

def modelSearch(modelGraph, modelEvaluator, firstModel = None, exhaustive=False):
    """Return a model ID from the given model graph that scores well on the given model evaluator"""

    numExpands = 0
    scored = dict()

    def score(modelID):
        concreteModelPath = modelGraph.getConcreteModel(modelID)
        modelScore = modelEvaluator.score(concreteModelPath)
        nonlocal scored
        scored[modelID] = modelScore
        return modelScore

    def expand(modelID):
        nonlocal numExpands
        numExpands += 1
        return modelGraph.getModelNeighbors(modelID)

    maxScore, maxModelPath = None, []

    horizon = PriorityQueue()

    if not firstModel:
        firstModel = modelGraph.getFirstModel()
    firstModelScore = score(firstModel)
    horizon.put((-firstModelScore, [firstModel]))

    while not horizon.empty():
        negCurrentScore, currentPath = horizon.get()
        currentScore = -negCurrentScore

        print("Visiting:")
        print("\tModel ID:\t", currentPath[-1])
        print("\tScore:\t\t", currentScore)

        if not maxScore or currentScore >= maxScore:
            maxScore, maxModelPath = currentScore, currentPath
        else:
            if not exhaustive: break

        for neighbor in expand(currentPath[-1]):
            if not neighbor in scored:
                neighborScore = score(neighbor)
                print("\tPush neighbor:\t", neighbor, neighborScore)
                horizon.put((-neighborScore, currentPath + [neighbor]))

    print()
    print("Winner:")
    print("\tModel Path:\t",  maxModelPath)
    # print("\tModel ID:\t",  maxModelPath[-1])
    print("\tScore:\t\t", maxScore)
    print(len(scored), "scores")
    print(numExpands, "expands")

    # Return path, dict of scores
    return maxModelPath, scored

def id_to_dict(modelID):
    return {a:b for [a,b] in [pair.split(':') for pair in modelID.split(',')]}

def id_to_notes_id(modelID):
    return json.dumps(id_to_dict(modelID))

if __name__ == "__main__":
    # The program was called as an executable

    defaultData = "test-data.r"
    defaultProgram = "examples/gq-concatenation.m.stan"

    if len(sys.argv) < 2:
        print("Expected arguments: [modular stan file] [data file path] (notes file output)")
        print("Using defaults.")

    modularStanProgram = sys.argv[1] if len(sys.argv) > 1 else defaultProgram
    testData = sys.argv[2] if len(sys.argv) > 2 else defaultData
    notes_output_file = sys.argv[3] if len(sys.argv) > 3 else None
    startModel = sys.argv[4] if len(sys.argv) > 4 else None

    if startModel:
        path, scores = modelSearch(ModelGraph(modularStanProgram), ModelEvaluator(testData), startModel) #, exhaustive = True)
    else:
        path, scores = modelSearch(ModelGraph(modularStanProgram), ModelEvaluator(testData)) #, exhaustive = True)

    if notes_output_file:
        notes_dict = {}
        for modelID, score in scores.items():
            place = ""
            special_place = ""
            if modelID in path:
                ix = path.index(modelID)
                place = "#" + str(ix + 1) + " "
                if ix == 0:
                    special_place += "[START] "
                if ix == len(path) - 1:
                    special_place += "[GOAL] "

            text = "ELPD score: " + str(score)
            name = place + special_place + str(score)
            notes_dict[id_to_notes_id(modelID)] = [name, text]

        with open(notes_output_file, 'w') as outfile:
            json.dump(notes_dict, outfile)

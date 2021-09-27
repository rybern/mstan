import subprocess
import sys

def text_command(args):
    return subprocess.run(args, capture_output=True, text=True).stdout.strip()

class ModelEvaluator:
    def __init__(self, dataFile):
        self.dataFile = dataFile

    def score(self, modelPath):
        return float(text_command(["bash", "./score.sh", modelPath]))

model_dir = "temp-models/"

class ModelGraph:
    def __init__(self, modularStanFile):
        self.modularStanFile = modularStanFile

    def execCommand(self, args):
        return text_command(["./mockup", "exec", "-f", self.modularStanFile] + args)

    def getConcreteModel(self, modelID):
        outFile = model_dir + self.modularStanFile + "_" + modelID + ".stan"
        self.execCommand(["-o", outFile, "get-model", "-s", modelID])
        return outFile

    def getModelNeighbors(self, modelID):
        return list(filter(lambda s: s, self.execCommand(["get-neighbors", "-s", modelID]).split('\n')))

    def getFirstModel(self):
        return self.execCommand(["get-first-model"])

def modelSearch(modelGraph, modelEvaluator):
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

import subprocess
import sys
import os.path
import json
from queue import PriorityQueue

from model_graph import *

class ModelEvaluator:
    def __init__(self, dataFile):
        self.dataFile = dataFile

    def score(self, modelPath):
        """Return the numerical score for the Stan program at the given filepath"""
        stdout_result = text_command(["Rscript", "elpd.R", modelPath, self.dataFile])
        return float(stdout_result.split('\n')[-1].strip())

        # return float(text_command(["bash", "./score.sh", modelPath]))

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

if __name__ == "__main__":
    # The program was called as an executable

    defaultData = "examples/bernoulli_data.json"
    defaultProgram = "examples/bernoulli.m.stan"

    if len(sys.argv) < 2:
        print("Expected arguments: [modular stan file] [data file path] (notes file output) (start model ID)")
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

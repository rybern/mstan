import subprocess
import sys
import os.path
import json

from model_graph import *

class ModelEvaluator:
    def __init__(self, dataFile, script, extraArgs):
        self.dataFile = dataFile
        self.script = script
        self.extraArgs = extraArgs

    def score(self, modelPath):
        """Return the numerical score for the Stan program at the given filepath"""
        stdout_result = text_command(["Rscript", self.script, modelPath, self.dataFile] + self.extraArgs)
        # print(stdout_result)
        last_word = stdout_result.split('\n')[-1].strip()
        return None if last_word == "none" else last_word

        # return float(text_command(["bash", "./score.sh", modelPath]))

def exhaustive(modelGraph, modelEvaluator):

    def score(modelID):
        return modelEvaluator.score(modelGraph.getConcreteModel(modelID))

    scores = dict()

    ids = modelGraph.getAllModels()
    ids = ids[41:]

    f = open("temp-score-save-file.csv", 'a')

    for ID in ids:
        print("\tDONE ID: ", ID)
        scoreID = score(ID)
        scoreID = "none" if scoreID == None else scoreID
        f.write("\"" + ID + "\"," + scoreID + "\n")
        f.flush()
        print("\tScore:   ", scoreID)
        scores[ID] = scoreID

    f.close()

    print("DONE: ")
    print(scores)

    return scores

if __name__ == "__main__":
    # The program was called as an executable

    defaultData = "examples/birthday/births_usa_1969.json"
    defaultProgram = "examples/birthday/birthday.m.stan"
    defaultScript = "birthday_day.R"
    defaultSubgraph = "DayOfYearTrend:yes"

    if len(sys.argv) < 5:
        print("Expected arguments: [modular stan file] [data file path] [evaluation R script] (subgraph selection) (notes file output)")
        print("Using defaults.")

    modularStanProgram = sys.argv[1] if len(sys.argv) > 1 else defaultProgram
    testData = sys.argv[2] if len(sys.argv) > 2 else defaultData
    script = sys.argv[3] if len(sys.argv) > 3 else defaultScript
    subgraph = sys.argv[4] if len(sys.argv) > 4 else defaultSubgraph
    notes_output_file = sys.argv[5] if len(sys.argv) > 5 else None

    exhaustive(ModelGraph(modularStanProgram, subgraph), ModelEvaluator(testData, script, []))

    # if notes_output_file:
    #     notes_dict = {}
    #     for modelID, score in scores.items():
    #         place = ""
    #         special_place = ""
    #         if modelID in path:
    #             ix = path.index(modelID)
    #             place = "#" + str(ix + 1) + " "
    #             if ix == 0:
    #                 special_place += "[START] "
    #             if ix == len(path) - 1:
    #                 special_place += "[GOAL] "

    #         text = "ELPD score: " + str(score)
    #         name = place + special_place + str(score)
    #         notes_dict[id_to_notes_id(modelID)] = [name, text]

    #     with open(notes_output_file, 'w') as outfile:
    #         json.dump(notes_dict, outfile)

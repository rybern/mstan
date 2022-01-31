import pandas as pd
from collections import defaultdict
from typing import Dict, List, Tuple, Union



def model_string_to_dict(model_string: str) -> Dict[str, str]:
    """
    Convert a mstan model string name to a dictionary
    """
    sig_df = {}
    for substr in model_string.split(","):
        signature, implementation = substr.split(":")
        sig_df[signature] = implementation
    
    return sig_df


def row_to_string(row: pd.DataFrame) -> str:
    """
    Convert a dataframe row into a mstan model string name
    """
    if row.shape[0] != 1:
        raise Exception("Only a single row should be supplied")
    
    dict_form = row[row.columns[~row.isnull().any()]].to_dict()
    if "elpd" in dict_form:
        del dict_form["elpd"]
    return ",".join(list(map(lambda x: f"{x[0]}:{list(x[1].values())[0]}", dict_form.items())))


def upsert_model(df: pd.DataFrame, model_dict: Dict[str, Union[str, float]] = None, model_string: str = None, elpd: float = None) -> pd.DataFrame:
    """
    If the model exists in the dataframe, update the dataframe.
    If it does not exist, create the entry.
    Returns the actuated dataframe
    """
    if not model_dict:
        model_dict = model_string_to_dict(model_string)
    
    if not elpd:
        raise Exception("Must provide ELPD value")

    result = search_df(df, model_dict)
    if result.empty:
        model_dict["elpd"] = elpd
        return df.append(model_dict, ignore_index=True)
    
    df.loc[result.index, "elpd"] = elpd

    return df


def search_df(df: pd.DataFrame, model_dict: Dict[str, Union[str, float]] = None, model_string: str = None):

    """
    checks if a given model dict is in a df. returns the row if exists or an empty df if not
    """
    if not model_dict:
        model_dict = model_string_to_dict(model_string)

    result = df
    for key, val in model_dict.items():
        if key == "elpd":
            continue
        
        result = result.loc[result[key] == val]
    
    return result if not result.empty else pd.DataFrame()

def save_csv(df: pd.DataFrame, filename):
    df.to_csv(filename, index=False)

def read_csv(filename: str):
    return pd.read_csv(filename)

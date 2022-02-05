from operator import mod

from torch import mode
import elpd_df
import numpy as np
import random
import pandas as pd
import pathlib
from mstan_interface import calculate_elpd, get_all_model_strings
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression


def plot_probabilities(df, iteration):
    plt.figure()
    x = list(range(len(df.probability)))
    #plt.scatter(x, probabilities, linewidths=1)
    plt.plot(x, df.probability)
    plt.ylim(bottom=0.0)
    plt.xlabel('model index')
    plt.ylabel('probability')
    plt.savefig(f"model_pmf_{iteration}.png")

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15,10))
    fig.suptitle(f"Iteration {iteration}: prob-ELPD plot")
    ax1.set_title("only selected models")
    ax1.set_xlabel("probability")
    ax1.set_ylabel("ELPD")
    filtered = df[~df.elpd.isna() & df.selected]
    linear_regressor = LinearRegression()
    linear_regressor.fit(filtered.probability.values.reshape(-1, 1), filtered.elpd.values.reshape(-1, 1))
    ax1.scatter(filtered.probability, filtered.elpd)
    ax1.plot(filtered.probability, linear_regressor.predict(filtered.probability.values.reshape(-1, 1)), color="red")

    ax2.set_title("all models present in cached results")
    ax2.set_xlabel("probability")
    ax2.set_ylabel("ELPD")
    filtered = df[~df.elpd.isna()]
    linear_regressor = LinearRegression()
    linear_regressor.fit(filtered.probability.values.reshape(-1, 1), filtered.elpd.values.reshape(-1, 1))
    ax2.scatter(filtered.probability, filtered.elpd)
    ax2.plot(filtered.probability, linear_regressor.predict(filtered.probability.values.reshape(-1, 1)), color="red")
    fig.savefig(f"prob-epld_plot_{iteration}.png")


def plot_signatures(df):
    # plot elpd, prob for each signature
    df = df[~df.elpd.isna() & df.selected]

    signatures = list(df.drop(columns=["elpd", "probability", "selected"]).columns)

    for signature in signatures:
        fig, (elpd_ax, ax2) = plt.subplots(1, 2, figsize=(15, 10))
        fig.suptitle(f"elpd-prob plot for signature {signature}, using only selected models")
        filtered = df.loc[:, ["elpd", "probability", signature]]
        res = filtered.groupby(signature)
        #prob_ax = ax1.twinx()
        res.mean().plot.bar(ax=elpd_ax, secondary_y="probability")
        elpd_ax.set_title("mean of elpd and prob")

        #filtered.set_index("probability").groupby(signature).elpd.plot(ax=ax2, legend=True, style=".", ms=20)
        for name, group in res:
            ax2.scatter(x=group.probability, y=group.elpd, label=name)
        
        ax2.legend()
        ax2.set_xlabel("probability")
        ax2.set_ylabel("ELPD")
        #res.plot(, x="probability", y="elpd", ax=ax2, legend=True)

        fig.tight_layout()
        fig.savefig(f"sigplot_{signature}.png")



def bayesian_probabilstic_search(model_path, data_path, model_df_path, num_iterations=10):
    # model df must contain all the models
    model_df = elpd_df.read_csv(model_df_path)
    
    model_count = model_df.shape[0]

    model_df["probability"] = 1.0 / model_count
    model_df["selected"] = False

    previous_iteration_elpd = None
    previons_iteration_model_dict = None

    for iter in range(1, num_iterations + 1):
        print("-" * 20)
        print(f"iteration {iter}")
        draw = model_df.sample(weights=model_df.probability)
        model_df.loc[draw.index, "selected"] = True
        

        draw_string = elpd_df.row_to_string(draw.drop(columns="probability"))
        print(f"chose model {draw_string}, with probability", draw.probability.values[0])

        model_dict = elpd_df.model_string_to_dict(draw_string)
        del model_dict["selected"]
        draw_string = ",".join([f"{key}:{val}" for key, val in model_dict.items()])
        if not np.isnan(elpd_df.search_df(model_df, model_dict).elpd.values[0]):
            elpd = elpd_df.search_df(model_df, model_dict).elpd.values[0]
            print(f"using saved ELPD value {elpd}")
        else:
            print("calculating elpd value...")
            elpd = calculate_elpd(model_path, draw_string, data_path)
            #elpd = random.randint(500, 12000)
            print(f"calculated ELPD value {elpd}, saving to df")
            model_df = elpd_df.upsert_model(model_df, model_dict, elpd=elpd)

        if iter > 1:
            update_arr = np.zeros(model_count)
            if elpd > previous_iteration_elpd:
                deduction_dict = previons_iteration_model_dict
                increment_dict = model_dict
            elif elpd < previous_iteration_elpd:
                deduction_dict = model_dict
                increment_dict = previons_iteration_model_dict
            
            for key, value in model_dict.items():
                if key not in previons_iteration_model_dict:
                    continue
                    
                if value != previons_iteration_model_dict[key]:
                    bad_models = elpd_df.search_df(model_df, {key: deduction_dict[key]})
                    num_signatures = len(bad_models) - bad_models.isnull().sum(axis=1) - 1  # remove elpd column
                    reducted_probs = 0
                    for index, n_sigs in zip(num_signatures.index, num_signatures):
                        deduction_amount = bad_models.loc[[index], "probability"].values[0] / n_sigs
                        update_arr[index] -= deduction_amount
                        reducted_probs += deduction_amount
                    
                    good_models = elpd_df.search_df(model_df, {key: increment_dict[key]})
                    n_good_models = good_models.shape[0]
                    print(f"deducted {reducted_probs} amount of probability, redistributing to {n_good_models} models")
                    for index in good_models.index:
                        update_arr[index] += reducted_probs / n_good_models
            
            model_df["probability"] += update_arr

                            



        
        print(model_df)
        plot_probabilities(model_df, iter)
        previous_iteration_elpd = elpd
        previons_iteration_model_dict = model_dict

        elpd_df.save_csv(model_df.drop(columns=["probability", "selected"]), "birthday_df_prob.csv")
        elpd_df.save_csv(model_df, "bayesian_update_results.csv")
    
    plot_signatures(model_df)


if __name__ == "__main__":
    example_dir = pathlib.Path(__file__).resolve().parents[1].absolute().joinpath("examples")
    birthday_model_path = example_dir.joinpath("birthday/birthday.m.stan")
    birthday_data_path = example_dir.joinpath("birthday/births_usa_1969.json")
    birthday_df_path = pathlib.Path(__file__).resolve().parent.absolute().joinpath("birthday_df_prob.csv")
    bayesian_probabilstic_search(birthday_model_path, birthday_data_path, birthday_df_path, num_iterations=20)

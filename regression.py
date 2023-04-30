import logging
import os
import sys

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.linear_model import ElasticNet, LinearRegression
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import KFold, cross_val_score, train_test_split
from sklearn.preprocessing import OrdinalEncoder, StandardScaler

logger = logging.getLogger(__name__)
logger.setLevel(logging.WARNING)
logging.getLogger("matplotlib").setLevel(logging.WARNING)

fh = logging.StreamHandler()
fmt = logging.Formatter(
    "%(asctime)s %(levelname)s %(lineno)d:%(filename)s(%(process)d) - %(message)s"
)
fh.setFormatter(fmt)
logger.addHandler(fh)

file_path = os.path.dirname(os.path.realpath(__file__))

# --- all tuning parameters ---
lambdas = np.logspace(-4, 4, 9)  # from 1e-4 to 1e4, and with nine total samples
alphas = np.linspace(0, 1, 6)  # from 0 to 1, now with six evenly spaced samples


def preprocess_data(filename: str, response: str) -> tuple:
    "Take in raw data and convert it to a workable format/state"
    if not isinstance(filename, str):
        raise TypeError("Filename should be a string :)")

    try:
        datafile = f"{file_path}/data/{filename}"
        logger.debug(datafile)
        if not os.path.exists(datafile):
            raise OSError("Expected data file, didn't find it :/")

        df = pd.read_csv(datafile, sep=",")  # read and pass to dataframe
        df.iloc[:, 3:8] = OrdinalEncoder().fit_transform(df.iloc[:, 3:8])
        logger.debug(df.iloc[:, 3:8].head())
        logger.debug(len(set(df["plnCmd"])))

        if response.lower() == "damage" or response.lower() == "d":
            df = df.drop(columns=["Stun"])
        elif response.lower() == "stun" or response.lower() == "s":
            df = df.drop(columns=["Damage"])
        else:
            raise AttributeError("Didn't pass in a valid response variable.")

        # convert dataframe to numpy array for faster computations
        return (df.columns.to_numpy(), np.array(df))
    except (OSError, AttributeError) as error:
        print(error)
        print("Couldn't load in the data.")
        sys.exit("Check if things are right and try again :)")


def standardize(data) -> tuple:
    "Establish design matrix and response vector, prepare both for elastic net"
    names = data[:, :2]
    logger.debug(names[:5])

    y = data[:, 18]  # extract only the data from the output column
    y = (lambda c: c - c.mean())(y)  # IIFE to center response vector

    logger.debug(y.shape)
    logger.debug(y.mean())

    dm = np.delete(data, (0, 1, 18), axis=1)  # extract the design matrix
    X = StandardScaler().fit_transform(dm)  # standardize (center & scale)

    logger.debug(X.shape)
    logger.debug(X.mean(axis=0))
    logger.debug(X.std(axis=0))
    return (X, y, names)


def data_splits(
    filename: str, response: str, seed_test: bool = False, seed_cv: bool = False
) -> tuple:
    "Divide data into training & test sets, then establish five folds for train set CV"
    columns, data = preprocess_data(filename, response)
    X, y, names = standardize(data)
    train_X, test_X, train_y, test_y, names_train, names_test = train_test_split(
        X, y, names, test_size=0.2, random_state=0 if seed_test else None
    )

    train = (train_X, train_y, names_train)
    logger.debug([k.shape for k in train])
    test = (test_X, test_y, names_test)
    logger.debug([k.shape for k in test])

    kf = KFold(n_splits=5, shuffle=True, random_state=0 if seed_cv else None)
    return (columns, data, train, test, kf)


def simple_lr(x, y, kf=None, cv: bool = False, scorer: str = "mse"):
    "Run simple linear regression, with option for cross-validation"
    if cv:
        return cross_val_score(
            LinearRegression(),
            x.reshape(-1, 1),
            y,
            cv=kf,
            scoring="neg_mean_squared_error" if scorer == "mse" else "r2",
        ).mean()
    return (
        LinearRegression().fit(x.reshape(-1, 1), y).intercept_,
        LinearRegression().fit(x.reshape(-1, 1), y).coef_[0],
    )


def elastic_net(X, y, lmbd, a, kf=None, cv: bool = False, scorer: str = "mse"):
    "Run elastic net regression, with option for cross-validation"

    params = np.zeros((6, 9, 16))
    cv_errors = np.zeros((9, 6, 5))

    def en(lmbd, a):
        return ElasticNet(alpha=lmbd, l1_ratio=a).fit(X, y).coef_

    if not isinstance(lmbd, (int, float)) and not isinstance(a, (int, float)):
        for i_a, val_a in enumerate(a):
            for i_l, val_l in enumerate(lmbd):
                if cv:
                    cv_errors[i_l, i_a] = cross_val_score(
                        ElasticNet(alpha=val_l, l1_ratio=val_a),
                        X,
                        y,
                        cv=kf,
                        scoring="neg_mean_squared_error" if scorer == "mse" else "r2",
                    )
                else:
                    params[i_a, i_l] = en(val_l, val_a)
    else:
        params = en(lmbd, a)

    if cv:
        logger.debug(f"{cv_errors.shape}\n{cv_errors}")
        logger.debug(cv_errors.mean(axis=2))
        return cv_errors.mean(axis=2)
    return params


def plot_parameters(X, y, cols) -> None:
    "Generate plots of the regression coefficients across all tuning parameter values"
    B = elastic_net(X, y, lambdas, alphas)
    logger.debug(f"{B.shape}\n{B}")
    for index, alpha in enumerate(B):
        plt.figure(figsize=(8, 6))
        plt.xscale("log")
        # transpose so that each row one of the nine features with nine columns for TP
        # this way, each index (row) has the vector I need to plot points
        color = iter(plt.cm.tab20(np.linspace(0, 1, alpha.T.shape[0])))
        for i, b in enumerate(alpha.T):
            c = next(color)
            plt.plot(lambdas, b, color=c, label=f"{cols[i]}")
        plt.xticks(lambdas)
        plt.xlabel(r"Tuning parameter ($\lambda$)")
        plt.ylabel(r"Regression coefficients ($\hat{\beta}$)")
        plt.legend(title="Features", fontsize="small", loc="upper right")
        plt.savefig(f"{file_path}/www/img/regr/{cols[-1]}_params_{index}.png", dpi=200)


def cross_validation(X, y, kf, cols, scorer: str = "mse") -> tuple:
    "Based on cross-validation scores, plot them and retrieve optimal tuning parameters"
    cv_error = elastic_net(X, y, lambdas, alphas, kf, True, scorer)
    plt.figure(figsize=(8, 6))
    plt.xscale("log")
    [
        plt.plot(lambdas, cv, label=f"{round(alphas[i], 1)}")
        for i, cv in enumerate(cv_error.T)
    ]
    plt.xticks(lambdas)
    plt.xlabel(r"Tuning parameter ($\lambda$)")
    plt.ylabel(
        r"$CV_{(5)}$ negative mean squared error"
    ) if scorer == "mse" else plt.ylabel(r"$CV_{(5)}$ $R^2$ value")
    plt.legend(title=r"$\alpha$", fontsize="small")
    plt.savefig(f"{file_path}/www/img/regr/{cols[-1]}_cv.png", dpi=200)

    logger.debug(cv_error.argmax())
    logger.debug(cv_error.max())
    logger.debug(
        cv_error[
            cv_error.argmax() // cv_error.shape[1],  # gets the row
            cv_error.argmax() % cv_error.shape[1],  # gets the column
        ]
    )
    l_optimal = float(lambdas[cv_error.argmax() // cv_error.shape[1]])
    a_optimal = float(alphas[cv_error.argmax() % cv_error.shape[1]])
    return (l_optimal, a_optimal)


def testing(train: tuple, test: tuple, mean: float, lmbd: float, a: float) -> tuple:
    lr_r2 = (
        LinearRegression()
        .fit(train[0][:, 0].reshape(-1, 1), train[1])
        .score(test[0][:, 0].reshape(-1, 1), test[1])
    )
    lr_preds = (
        LinearRegression()
        .fit(train[0][:, 0].reshape(-1, 1), train[1])
        .predict(test[0][:, 0].reshape(-1, 1))
    )
    lr_mse = mean_squared_error(test[1] + mean, lr_preds + mean)
    lr_table = np.hstack(
        (test[2], (lr_preds + mean).reshape(-1, 1), (test[1] + mean).reshape(-1, 1))
    )
    en_r2 = (
        ElasticNet(alpha=lmbd, l1_ratio=a)
        .fit(train[0], train[1])
        .score(test[0], test[1])
    )
    en_preds = (
        ElasticNet(alpha=lmbd, l1_ratio=a).fit(train[0], train[1]).predict(test[0])
    )
    en_mse = mean_squared_error(test[1] + mean, en_preds + mean)
    en_table = np.hstack(
        (test[2], (en_preds + mean).reshape(-1, 1), (test[1] + mean).reshape(-1, 1))
    )

    scores = np.array([[lr_r2, lr_mse], [en_r2, en_mse]])
    tables = np.array([lr_table, en_table])
    return (scores, tables)


def main():
    columns, data, train, test, kf = data_splits("all.csv", "d", True, True)
    cols = columns[2:]
    plot_parameters(train[0], train[1], cols)
    l_optimal, a_optimal = cross_validation(train[0], train[1], kf, cols)
    print(l_optimal, a_optimal)
    # --- Playing around with simple LR ---
    lm_cv = simple_lr(train[0][:, 0], train[1], kf, True)  # standardized
    slope = simple_lr(train[0][:, 0], train[1])
    print(lm_cv, slope)
    lm_cv = simple_lr(data[:, 2], data[:, 18], kf, True)
    slope = simple_lr(data[:, 2], data[:, 18])  # same data, but not standardized
    print(lm_cv, slope)
    scores, tables = testing(train, test, data[:, 18].mean(), l_optimal, a_optimal)
    print(scores, tables.shape)


if __name__ == "__main__":
    main()

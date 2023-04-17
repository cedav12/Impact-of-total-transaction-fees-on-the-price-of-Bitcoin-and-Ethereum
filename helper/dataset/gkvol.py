import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


def garman_klass(row: pd.Series) -> float:
    """Compute Garman-Klass volatility"""
    log_hl = np.log(row.high / row.low) ** 2
    log_co = np.log(row.close / row.open) ** 2

    return np.sqrt(0.5 * log_hl - (2 * np.log(2) - 1) * log_co)

if __name__ == "__main__":
    df = pd.read_csv("../../data/eth.csv")
    df["sigma"] = df.apply(garman_klass, axis=1)
    df.to_csv("../../data/eth.csv")

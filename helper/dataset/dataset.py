import os
import pandas as pd


def create_dataset(coin: str = "btc", control_dim: int = 2436) -> pd.DataFrame:
    dir = os.path.join("../../data", coin.upper())
    combined_dataset = pd.DataFrame()
    for file in os.listdir(dir):
        df = pd.read_csv(os.path.join(dir, file))
        if df.shape[0] != control_dim:
            continue
        if combined_dataset.shape[0] == 0:
            combined_dataset = df
        else:
            for col in df.columns:
                if col not in combined_dataset.columns:
                    combined_dataset[col] = df[col]
    return combined_dataset


def update_dataset(dataset: pd.DataFrame, new_feature: pd.DataFrame) -> pd.DataFrame:
    if dataset.shape[0] != new_feature.shape[0]:
        raise Exception("Incompatible datasets: different dimensions!")
    for col in new_feature.columns:
        if col not in dataset.columns:
            dataset[col] = new_feature[col]
    return dataset


def merge_datasets(dataset: pd.DataFrame, new_feature: pd.DataFrame) -> pd.DataFrame:
    dataset = dataset.set_index("t")
    new_feature = new_feature.set_index("t")
    return dataset.join(new_feature, how="inner")


def save_dataset(dataset: pd.DataFrame, path: str) -> None:
    dataset.to_csv(path)


if __name__ == "__main__":
    for coin in ["btc", "eth"]:
        #dataset = create_dataset(coin)
        dataset = pd.read_csv(os.path.join("../../data", coin + "_g.csv"))
        feature = pd.read_csv(os.path.join("../../data", coin.upper() + f"/{coin}_transfers.csv"))
        dataset = merge_datasets(dataset, feature)
        save_dataset(dataset, os.path.join("../../data", coin + ".csv"))


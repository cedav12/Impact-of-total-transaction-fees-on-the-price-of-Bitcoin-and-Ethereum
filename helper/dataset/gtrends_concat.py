import sys
import warnings
import pandas as pd


def gtrends_concat(coin: str):
    gtrends = pd.read_csv(f"data/{coin.upper()}/google_trends.csv")
    df = pd.read_csv(f"data/{coin}.csv", index_col=False)
    # Delete index column if is present.
    if "Unnamed: 0" in df.columns:
        df = df.drop(columns=["Unnamed: 0"])
    # Set t as index for both DataFrames.
    df = df.set_index("t")
    gtrends = gtrends.set_index("t")
    # Merge that
    new_df = df.join(gtrends, how="inner")
    # Check whather there are no missing values in the new_df index.
    expected = pd.to_datetime(new_df.index[0])
    for date in new_df.index:
        if pd.to_datetime(date) != expected:
            warnings.warn("Missing dates in index.")
            resp = input("Do you still want to concatenate the datasets, although there are missing dates? (Y/n): ")
            if resp == "n":
                sys.exit()
            else:
                break
        expected += pd.DateOffset(days=1)
    expected_cols = df.shape[1] + gtrends.shape[1]
    if new_df.shape[1] != expected_cols:
        raise Exception(f"Expected number of columns{expected_cols}, but got {new_df.shape[1]}")

    # Save new dataframe
    new_df.to_csv(f"data/{coin}_g.csv")


if __name__ == "__main__":
    gtrends_concat("eth")

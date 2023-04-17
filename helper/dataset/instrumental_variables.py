import pandas as pd
import yfinance as yf


config = "BTC"

def yfinance_connect(ticker: str, start: str = "2016-01-01", end: str = "2022-09-01", interval: str = "1d") -> pd.DataFrame:
    tickers = {"sp500": "^GSPC", "vix": '^VIX', "gold": "GC=F"}
    new_data = yf.download(tickers[ticker], start=start, end=end, interval=interval)
    if ticker != "gold":
        new_data = new_data.resample("D").last().fillna(method="ffill")
        new_data = new_data.reindex(data.index)
    return new_data
    #sp500_data.to_csv(f"../../data/IV/{ticker}")

if __name__ == "__main__":
    data = pd.read_csv(f"../../data/{config.lower()}.csv")
    data = data.set_index("t")
    data.index = pd.to_datetime(data.index)
    for ticker in ["gold"]:
        new_feature = yfinance_connect(ticker)
        if ticker in ["sp500", "vix"]:
            new_feature = new_feature[["Adj Close"]]
            for i in range(new_feature.isna().sum().values[0]):
                new_feature.values[i] = new_feature.values[new_feature.isna().sum().values[0]]
            assert new_feature.isna().sum().values[0] == 0
        else:
            new_feature = new_feature[["Adj Close"]]
        new_feature.index = pd.to_datetime(new_feature.index)
        new_feature = new_feature.rename(columns={"Adj Close": ticker})
        data = data.join(new_feature, how="inner")
    data.to_csv(f"../../data/{config.lower()}.csv")

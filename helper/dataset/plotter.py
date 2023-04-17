import argparse
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import pandas as pd

parser = argparse.ArgumentParser()
parser.add_argument("--coin", default="btc", help="Possibilities {btc, eth}")
parser.add_argument("--series", default=["sigma", "close", "total_fees"], nargs="*", help="Series to be plot.")

def plotter(args: argparse.Namespace) -> None:
    df = pd.read_csv(f"../../data/{args.coin}.csv")
    series = args.series
    fig, axs = plt.subplots(len(series), 1, figsize=(8, 6 * len(series)), sharex=True)
    for i, s in enumerate(series):
        axs[i].plot(df.t, df[s])
        axs[i].set_ylabel(s)
        axs[i].set_xlabel("Date")
        axs[i].xaxis.set_major_locator(mdates.YearLocator())
    plt.xticks(rotation=45)
    plt.show()


if __name__ == "__main__":
    args = parser.parse_args([] if "__file__" not in globals() else None)
    plotter(args)

import argparse
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import pandas as pd
import numpy as np

parser = argparse.ArgumentParser()
parser.add_argument("--coin", default="btc", help="Possibilities {btc, eth}")
parser.add_argument("--series", default=["hash_rate_mean", "close"], nargs="*", help="Series to be plot.")

def plotter(args: argparse.Namespace) -> None:
    df = pd.read_csv(f"../../data/{args.coin}.csv")
    series = args.series
    fig, axs = plt.subplots(len(series), 1, figsize=(8, 6 * len(series)), sharex=True)
    for i, s in enumerate(series):
        axs[i].plot(df.t, df[s])
        axs[i].set_ylabel(s)
        axs[i].set_xlabel("Date")
        axs[i].xaxis.set_major_locator(mdates.YearLocator())
        #axs[i].set_title("Bitcoin")
    plt.xticks(rotation=45)
    plt.show()

def fee_trend():
    df = pd.read_csv(f"../../data/{args.coin}.csv")
    log_close = np.log(df["close"])
    log_fees = np.log(df["total_fees"])
    # Scatter plot
    plt.scatter(log_close, log_fees, s=4)
    # Calculate the coefficients of the best fit line
    coefficients = np.polyfit(log_close, log_fees, 1)
    # Generate y-values based on the best fit line
    y_fit = coefficients[0] * log_close + coefficients[1]
    # Plot the best fit line
    plt.plot(log_close, y_fit, color="orange")
    # Add labels and title
    plt.xlabel('Log(Close)')
    plt.ylabel('Log(Total Fees)')
    #plt.title('Log(Close) vs Log(Total Fees) with Regression Line')
    plt.show()

def fee_price():
    df = pd.read_csv(f"../../data/{args.coin}.csv")
    log_close = np.log(df["close"])
    log_fees = np.log(df["hash_rate_mean"])

    fig, ax1 = plt.subplots()
    color = 'tab:blue'
    ax1.set_xlabel('Date')
    ax1.set_ylabel('log(close)', color=color)
    ax1.plot(df.t, log_close, color=color)
    ax1.tick_params(axis='y', labelcolor=color)
    #ax1.xaxis.set_major_locator(mdates.YearLocator())
    ax2 = ax1.twinx()  # instantiate a second axes that shares the same x-axis
    color = 'tab:orange'
    ax2.set_ylabel('log(fees)', color=color)  # we already handled the x-label with ax1
    ax2.plot(df.t, log_fees, color=color)
    ax2.tick_params(axis='y', labelcolor=color)
    ax2.xaxis.set_major_locator(mdates.YearLocator())

    plt.axvline(x=751, color='g', linestyle='--')
    plt.axvline(x=1501, color='g', linestyle='--')
    # otherwise the right y-label is slightly clipped
    fig.tight_layout()
    for label in ax1.get_xticklabels():
        label.set_rotation(45)
    for label in ax2.get_xticklabels():
        label.set_rotation(45)
    plt.show()


if __name__ == "__main__":
    args = parser.parse_args([] if "__file__" not in globals() else None)
    plotter(args)


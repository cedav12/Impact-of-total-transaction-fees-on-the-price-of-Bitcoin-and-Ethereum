import requests
import os
import pandas as pd
import argparse
import json

from helper.creds.secret import API_KEY


parser = argparse.ArgumentParser()
parser.add_argument("--API", default="https://api.glassnode.com/v1/metrics/", type=str, help="Specify API structure.")
parser.add_argument("--asset", default="transactions/", type=str, help="Specify section.")
parser.add_argument("--endpoint", default="transfers_volume_median", type=str, help="Specify endpoint to load data from.")
parser.add_argument("--metric", default="ETH", type=str, help="define metrics")
parser.add_argument("--interval", default="24h", type=str, help="define granularity")
parser.add_argument("--start", default="2016-01-01", type=str, help="The start of the interval")
parser.add_argument("--end", default="2022-09-01", type=str, help="The end of the interval")
parser.add_argument("--col_name", default="transfers_med", type=str, help="Name the new column.")
parser.add_argument("--save_file", default="eth_transfers.csv", type=str,
                    help="Specify the path where to save data.")
"""
possibilities:
    {
    market: {price_usd_ohlc: {ETH,BTC},},
    fees: {gas_price{_mean/_median}: ETH,
            volume_sum{btc/eth},
    mining: {hash_rate_mean, volume_mined_sum/miners},
    supply: {current: btc/eth},
    transactions: {count: btc/eth},
    addresses: count
    }
"""


def main(args: argparse.Namespace) -> None:
    # Config params
    params = {
        "a": args.metric,
        "i": args.interval,
        "api_key": API_KEY
    }
    url = args.API + args.asset + args.endpoint
    # Make the API call and retrieve the data
    response = requests.get(url, params=params)
    try:
        data = pd.read_json(response.text, convert_dates=['t'])
    # When the ValueError arise, we should process the data "manually". This occurs when the data are too large.
    except ValueError:
        data = pd.DataFrame(json.loads(response.text))
        data.t = data.t.apply(lambda x: pd.to_datetime(x, unit="s"))
    # ohlc price requires the special care since we obtain columns instead of one.
    if "ohlc" in args.endpoint:
        # encode columns
        for col in ["open", "high", "low", "close"]:
            data[col] = data.o.apply(lambda x: x[col[0]])
        # delete the aggregated column
        data = data.drop(columns=["o"])
    # In all other cases we obtain just one column, which is denoted as "v".
    else:
        data = data.rename(columns={"v": args.col_name})

    # filter rows which are within range (start, end)
    start = pd.to_datetime(args.start)
    end = pd.to_datetime(args.end)
    data = data[(start <= data.t) & (data.t <= end)]
    data = data.set_index("t")

    # save data
    if args.save_file is not None:
        dir = os.path.join("../../data", args.metric)
        data.to_csv(os.path.join(dir, args.save_file))


if __name__ == "__main__":
    args = parser.parse_args([] if "__file__" not in globals() else None)
    main(args)

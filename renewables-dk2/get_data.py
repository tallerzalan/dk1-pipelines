# dk1 wind and solar per hour from energi data service: what was produced, and energinet's day-ahead forecast

import pandas as pd

from db import TODAY, connect, get, save, start_from

EDS = "https://api.energidataservice.dk/dataset/"
PRODUCTION = {
    "onshore": ["OnshoreWindGe50kW_MWh", "OnshoreWindLt50kW_MWh"],
    "offshore": ["OffshoreWindGe100MW_MWh", "OffshoreWindLt100MW_MWh"],
    "solar": ["SolarPowerLt10kW_MWh", "SolarPowerGe10Lt40kW_MWh", "SolarPowerGe40kW_MWh"],
}
FORECAST = {"Onshore Wind": "onshore", "Offshore Wind": "offshore", "Solar": "solar"}


def records(dataset, start, end):
    js = get(EDS + dataset, {"start": f"{start:%Y-%m-%d}", "end": f"{end:%Y-%m-%d}", "limit": 0,
                             "filter": '{"PriceArea":["DK2"]}'})
    return pd.DataFrame(js["records"])


con = connect()

# final numbers come 8-10 days late and can be revised, so the last 30 days are taken again
raw = records("ProductionConsumptionSettlement", start_from(con, "production", 30), TODAY + pd.Timedelta(days=1))
prod = pd.DataFrame({"time": pd.to_datetime(raw["HourUTC"])})
for name, cols in PRODUCTION.items():
    prod[name] = raw[cols].sum(axis=1, min_count=1)
save(con, "production", prod)

raw = records("Forecasts_Hour", start_from(con, "energinet", 3), TODAY + pd.Timedelta(days=2))
fc = raw[raw["ForecastType"].isin(FORECAST)].pivot_table(index="HourUTC", columns="ForecastType",
                                                        values="ForecastDayAhead")
fc = fc.rename(columns=FORECAST).reset_index()
fc["time"] = pd.to_datetime(fc.pop("HourUTC"))
save(con, "energinet", fc)

for table in ["production", "energinet"]:
    d = con.execute(f"SELECT * FROM {table} ORDER BY time").df()
    gaps = pd.date_range(d["time"].min(), d["time"].max(), freq="h").difference(d["time"])
    print(f"\n{table}: {d['time'].min()} to {d['time'].max()} utc, {len(d)} hours, {len(gaps)} missing")
    print(d.drop(columns="time").describe().round(0).to_string())
con.close()

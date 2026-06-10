import os
import pandas as pd
from datetime import datetime

#note that the limit is 50,000 for the request is 50K. If for some reason
#there are more than 50K people on Rikers, this will break. But, uh, that's unlikely.
#The app token (set via the SOCRATA_APP_TOKEN secret) just buys a higher rate limit;
#the request still works without one.
token = os.environ.get("SOCRATA_APP_TOKEN")
todays_prisoners = pd.read_json('https://data.cityofnewyork.us/resource/7479-ugqb.json?$limit=50000',
                                storage_options = {'X-App-Token': token} if token else None)

print("got file at " + str(datetime.now()))

todays_prisoners.to_csv("./dat/Daily_Inmates_In_Custody.csv", index=False)

import json
import csv
from typing import Any

OUTPUT_FILE = "maps.csv"

base_url = "https://trackmania.exchange/mapshow/"

with open("cur-sheet.csv", "r", encoding="utf-8") as f:
    reader = csv.DictReader(f)
    maps = [row for row in reader]

with open("oldlist.csv", "r", encoding="utf-8") as f:
    reader = csv.DictReader(f)
    oldlist = {int(row["TMX ID"]): row for row in reader}

with open("tags.json", "r", encoding="utf-8") as f:
    tmxtags = {x["ID"]: x for x in json.load(f)}

with open("umaps.json", "r", encoding="utf-8") as f:
    umaps = {x["tmxId"]: x for x in json.load(f)}

with open("rmaps.json", "r", encoding="utf-8") as f:
    rmaps = {x["tmxId"]: x for x in json.load(f)}

# with open("rmaps.json", "r", encoding="utf-8") as f:
#     umaps = {x["tmxId"]: x for x in json.load(f)}
#
fieldnames = [
    "tmxid",
    "name",
    "link",
    "status",
    "atPlugin",
    "tags",
    "oldReason",
    "atdiff",
]

with open(OUTPUT_FILE, "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writeheader()

    for m in maps:
        tmxid = int(m["tmxid"])

        status = ""
        mp = umaps.get(tmxid)
        rmap = rmaps.get(tmxid)
        atdiff = "-"
        if rmap is not None:
            status = "hidden; in plugin" if rmap["hiddenReason"] else "in main list"
            if not rmap["authorUid"]:
                status = "not on nadeo"
            tags = ", ".join(tmxtags[x]["Name"] for x in rmap["tags"])
            if rmap["currentWR"]:
                beaten = 1 if rmap["currentWR"]["time"] < rmap["authorMedal"] else 0
                atdiff = rmap["currentWR"]["time"] - rmap["authorMedal"]
            else:
                beaten = 0
        else:
            if mp is None:
                status = "beaten"
                tags = "?"
                beaten = "?"
            else:
                tags = ", ".join(tmxtags[x]["Name"] for x in mp["tags"])
                status = "hidden" if mp["hiddenReason"] else ""
                if not mp["authorUid"]:
                    status = "not on nadeo"
                if mp["currentWR"]:
                    beaten = 1 if mp["currentWR"]["time"] < mp["authorMedal"] else 0
                    atdiff = mp["currentWR"]["time"] - mp["authorMedal"]
                else:
                    beaten = 0

        old_list_status = ""
        if tmxid in oldlist:
            old_list_status = oldlist[tmxid]["Reason"]

        writer.writerow(
            {
                "tmxid": tmxid,
                "name": m["name"],
                "link": f"{base_url}{tmxid}",
                "status": status,
                "atPlugin": 1 if m["atPlugin"] == "TRUE" else 0,
                "tags": tags,
                "oldReason": old_list_status,
                "atdiff": abs(atdiff) if isinstance(atdiff, int) else atdiff,
            }
        )

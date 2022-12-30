import json
f = open("annotations.w3c.json")
data = json.load(f)

print(len(data))
minStart = 99999999
maxStart = 0

for x in range(len(data)):
    minStart = min(minStart, data[x]['target']['selector'][1]['start'])
    maxStart = max(maxStart, data[x]['target']['selector'][1]['start'])

dividedBy = 10 # to get roughly the first 10% annotations, change to 20 for 5%, 2 for 50% ...

midStart = (minStart + maxStart) / dividedBy
tempData = []

# to filter out part of the data
for x in range (len(data)):
    if (data[x]['target']['selector'][1]['start'] > midStart):
        continue
    tempData.append(data[x])

# write to new file
with open("output10.w3c.json", "w") as outfile:
    outfile.write(json.dumps(tempData))
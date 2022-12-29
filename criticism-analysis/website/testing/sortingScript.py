import json
f = open("annotations.w3c.json")
data = json.load(f)

data.sort(key=lambda x: (x['target']['selector'][1]
          ['start'], x['target']['selector'][1]['end']))

with open("sortedAnnotations.w3c.json", "w") as outfile:
    outfile.write(json.dumps(data))

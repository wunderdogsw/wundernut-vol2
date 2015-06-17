import sys
import re
import codecs

pattern = re.compile('[\W_]+', re.UNICODE)
words = []

with codecs.open(sys.argv[1], 'r', 'utf-8') as f:
    words = list(set([pattern.sub('', word.lower())
            for line in f
            for word in line.split()]))
    
sets = list(set([frozenset(word) for word in words]))
sets.sort(key = len, reverse = True)

mapper = {key: [] for key in sets}
[mapper[frozenset(word)].append(word) for word in words]

muhkeus = 0
muhkein = [] 

for i in sets:
    if muhkeus > len(i) * 2:
        break
    for j in sets:
        if muhkeus > len(i) + len(j):
            break
        elif len(i.union(j)) >= muhkeus:
            if len(i.union(j)) == muhkeus:
                if not [i, j] in muhkein and not [j, i] in muhkein:
                    muhkein.append([i, j])
            else:
                muhkeus = len(i.union(j))
                muhkein = [[i, j]]

for x in muhkein:
    print ' '.join(mapper[x[0]] + mapper[x[1]])

print 'muhkeus:', muhkeus, 'number of word pairs:', len(muhkein) 


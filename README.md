Temporal-Information-Extraction
===============================

This project uses mainly regular expressions with some additional tweaking functions to extract temporal information (dates, durations, time, etc) from, mainly, news articles or any other text.

Everything here should be viewed as a proof on concept. To code is not pretty and is targeted against the SemEval-2010 conference competition.

The code was evaluated against SemEval-2010 with the following results:


```bash
true positives:   672
true negatives:   51280
false positives:  53
false negatives:  1445

attribute type: +382.0 -16.0
attribute value: +350.0 -48.0

precision   0.93
recall      0.32
f1-measure  0.47
accuracy    0.97

attribute type       0.96 
attribute value      0.88 
```

Due to time constraints, the recall did not reach higher than 1/3 but could be improved greatly if I had more time to work on it.

A demo exists written in Play Framework which can be downloaded here:

https://github.com/AntonFagerberg/Temporal-Information-Extraction-Demo

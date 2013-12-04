Temporal Information Extraction
===============================

This project uses mainly regular expressions with some additional tweaking functions to extract temporal information (dates, durations, time, etc) from, mainly, news articles or any other text.

Everything here should be viewed as a proof on concept. To code is not pretty and it is only targeted against the SemEval-2010 conference competition and not to be used in a real product.

The code was evaluated against SemEval-2010 with the following results:

```bash
true positives:   1053
true negatives:   51246
false positives:  87
false negatives:  1064

attribute type: +546.0 -17.0
attribute value: +494.0 -69.0

precision   0.92
recall      0.50
f1-measure  0.65
accuracy    0.98

attribute type       0.97
attribute value      0.88
```

Due to time constraints, the recall did not reach higher than 50% but could be improved greatly if I had more time to work on it.
This is a proof of concept but my belief is that this could work very good given enough time and dedication.

### Demo

A demo exists written in Play Framework which can be downloaded here:

https://github.com/AntonFagerberg/Temporal-Information-Extraction-Demo

![Screenshot](https://raw.github.com/AntonFagerberg/Temporal-Information-Extraction-Demo/master/screenshot.png)

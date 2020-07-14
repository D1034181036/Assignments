File description

All of these are text files containing one document per line.

Each document is composed by its class and its terms.

Each document is represented by a "word" representing the document's class, a TAB character and then a sequence of "words" delimited by spaces, representing the terms contained in the document.

Pre-processing

From the original datasets, in order to obtain the present files, Ana applied the following pre-processing:
all-terms Obtained from the original datasets by applying the following transformations:
Substitute TAB, NEWLINE and RETURN characters by SPACE.
Keep only letters (that is, turn punctuation, numbers, etc. into SPACES).
Turn all letters to lowercase.
Substitute multiple SPACES by a single SPACE.
The title/subject of each document is simply added in the beginning of the document's text.
no-short Obtained from the previous file, by removing words that are less than 3 characters long. For example, removing "he" but keeping "him".
no-stop Obtained from the previous file, by removing the 524 SMART stopwords. Some of them had already been removed, because they were shorter than 3 characters.
stemmed Obtained from the previous file, by applying Porter's Stemmer to the remaining words. Information about stemming can be found here.
Last updated April 2007.
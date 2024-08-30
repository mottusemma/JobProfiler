### JobProfiler

Emma Mõttus, emma.mottus\@ut.ee

René Mõttus, rene.mottus\@ed.ac.uk

University of Tartu, Institute of Psychology

------------------------------------------------------------------------

### What needs doing?

Adding and updating tsv/md files to support different languages! Language options and displayed text have been soft-coded into the app, meaning they get read in from external files that can be altered without having to tweak the app code itself.

### App text, languages

Most text in the app (shorter texts, labels, question choices) comes from the file www/appText.tsv, from which text is fetched in the R app using a string ID (and a language parameter). For questions with a large number of response options, the responses are saved in separate tsv files in www/. Each of these files in www/ have a separate column for each language.

The rest of the text (mostly larger Markdown text blocks) can be found in folders corresponding to each language.

### Adding a new language

1.  Add the language + its abbreviation to the file appData/langOptions.tsv.
2.  Create a new folder in www/ with *the same abbreviation* as its name: the folder must include the files:
    -   1stpage.md

    -   B5.md

    -   FAQ.md

    -   info.md

    -   jobNames.tsv

    -   map.md
    
    -   about.md
3.  Add a new column to each of the tsv files in www/ (appText, countries, occupationList, questions), using the same abbreviation as its header.

### Tweaking text

Changes in the text can be made by rewriting the corresponding tsv cells or tweaking Markdown files. Take care not to change any code/ID columns.

### Saved responses

Each response is saved in a separate file with the name \[user session\]\_\[time of submission\].tsv. Data is saved in the following order:
-    Current user session (random 10-letter string)
-    Session ID picked up from the URL (NA if none, otherwise the argument of reference=)
-    Time of submission
-    Language selected
-    Previous completion of survey (true/false)
-    Whether the link was shared (t/f) \*
-    Who the link was shared by (NA if \* false)
-    Gender
-    Age
-    Country of residence (abbreviation)
-    Employment (t/f) \**
-    Job title (NA if \** false)
-    Length of time at current job (scale of 1-5 from shortest to longest, NA if \** false)
-    Job satisfaction (scale of 1-6 from least to most happy, NA if \** false)
-    Personality (matrix) question responses in order given in www/questions.tsv (scale of 1-6 as above)

# Covid-19-Inflation-Israel
To estimate the change in the weight of an item in the consumption basket relative to the
base months (January to February), or, in other words, to obtain the adjusted weight, we
used the daily volume of credit-card transactions. In particular, following Cavallo (2020), we multiply the monthly rate of change of
each item (as measured by the CBS) by its COVID-19-adjusted weight to obtain its adjusted contribution to the inflation of the COVID-19 basket.

The adjusted weights are calculated in the respective R codes that are using "cc-DATA.csv" that contains the raw c.c data and "group_names.xlsx" that helps with the classification. These 2 files need to be in the working directory for the code to run.
Except from the c.c raw daily data, all other relevant data can be found in "Covid-19 inflation.xlsx" that includes official inflation & weights + adjusted inflation & weights with excel formulas.

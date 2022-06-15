# Urban-quality-in-pandemic-times
This repository contains the data and the scripts used in the articule "Urban quality in pandemic times", by Guibor Camargo.


## Trakign data info
### Pre-filtered data
- 123'791.061 of traking points (TK)
- From 2020-01-01 to 2020-12-31 (365 days)
- 524.020 different IDs


### Sample selection

Its important to keep in mind, that the sample size selecction must have into consideration: a) The amount of traking data per individual, since its important to capture the movement of poeple outside their home; b)  the representativeness of the sample at the neighborhood level; and c) the geographic distribution (since it's possible that low income neighborhoods are not represented enough).

#### a) How many monthtly traking points (TK) sould an indiviudal must have to enter in the study
The sample contians exactly 12 months of traking data, however not all the users have a permant record over each month, so we aplied 3 rules to extract a final sample:

1. Individuals must have data over at least "$Y$" months
2. Individuals must have at least "$X$" traking poitns per month
3. Individuals must live inside Bogota's urban limit during the analized period.

Taking thise into acount, the next setp is to deside what the value of "$Y$" and "$X$" should be. To do so, we simulate the sample size in terms of individulas and TK as shown below:

![alt text](https://github.com/Guibi1994/Urban-quality-in-pandemic-times/blob/main/3.%20Graficas/0.%20Seleccion%20muestral%20Traking%20points.png)

We finally selected a mimum of 9 active months (out of 12), and a minimun of 30 TK each months for indiviudals to be considered part of the study. Evidently this could couse several issues when calculating neighborhood average exposure time, however if we weighted the amount of TK per individual when computing exposure time it could probably acount for some of this bias.


I created 2 choropleth maps of England & Wales which shows the average expenditure per age group on either Clothing & Footwear or Household Goods. 

The data sets i used were:
	-) The Retail Research Data (http://www.opendataprofiler.com/Default.aspx): it uses data obtained from the 2011 Census to build an output area 			classification (OAC). From here i used the distribution of the age groups per output area.
	-) The Family Spending Report, 2012 Edition (http://www.ons.gov.uk/ons/rel/family-spending/family-spending/family-spending-2012-edition/index.html):
		From here I used the "Household expenditure as a percentage of total expenditure by age of household reference person". Lincensed under 
		Crown Copyright 2014
	-) England and Wales population by age (http://www.theguardian.com/news/datablog/2010/feb/26/population-ethnic-race-age-statistics): 
		I used this data set to convert the age group ranges used in the family spending report to the age groups used in the retail research data. It 
		was originally provided by the Office for National Statistics, UK. Lincensed under Crown Copyright 2014.

The computation that combines the data sets and maps the percentage of total expenditure for different product groups by age of household reference person onto the output areas from the retail research data is given in the 'expenditure_analysis.R' file. This R-file produces a .csv-file all the expenditure groups given in the family spending report are mapped onto the output areas.

Once the relevant data is produced i used Quantum GIS to produce the graphics. A shape file can be found on the website of the retail research data. The background raster map is used the MiniScale map of Great Britain (https://www.ordnancesurvey.co.uk/business-and-government/products/miniscale.html). It is licensed under “Use limitation dependent upon licence” (details: http://www.ordnancesurvey.co.uk/business-and-government/licensing/index.html).

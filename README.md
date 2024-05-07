# dallas_restbreaks_constructionworkers
Code and data used for analysis of associations of Dallas, TX's 2016 rest break ordinance with rates of workers compensation claims in construction workers

Data were constructed from the following databases:

Bureau of labor statistics employment data	Bureau of labor statistics Quarterly Census of Employment and Wages	https://www.bls.gov/cew/downloadable-data-files.htm	I downloaded the data for each year (2013-2022) by clicking on each year under the column labeled "County High-Level"										
![image](https://github.com/schinasi/dallas_restbreaks_constructionworkers/assets/30327926/7c847def-5bb8-4f4a-be6c-38445f8d85a1)

Worker's compensation data	Texas Department of Insurance Worker's Compensation claim data. 	
https://wwwapps.tdi.texas.gov/inter/perlroot/sasweb9/cgi-bin/broker.exe?_service=wcExt&_program=progext.hsdata3.sas	I manually downloaded the data from this website. 	
	Data were filtered to the industry of interest (construction all) and indicated the county of the employer	
	The following contains information on the coding for the site: https://wwwapps.tdi.texas.gov/inter/perlroot/sasweb9/cgi-bin/broker.exe?_service=wcExt&_program=progext.hsdata3.sas&option=DEFINE	

 PRISM data	https://code.earthengine.google.com/?scriptPath=users%2Fleahschinasi%2Fdefault%3Atx_counties_PRISM																										
	Downloaded daily data for every county, day in TX using GEE (Raw data saved: C:/Users/lhs36/OneDrive - Drexel University/JPB_WORK/Texas/Data/GEE_PRISM_TEXAS_COUNTIES-20230725T190828Z-001). Then used R to calculate monthly averages. 																										
	"Used R to caculate monthly averages (Calculate the mean values for 'pr', 'tmmn', and 'tmmx' for each unique
#combination of 'GEOID', 'month', and 'year'). 



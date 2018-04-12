# cvapps

This app has been developed by the Data Sciences Unit of RMOD at Health Canada as part of the Open Data Initiative. This is a prototype experiment that utilizes publically available data (Canada Vigilance Adverse Reaction Online Database), and provide visualizations in an interactive format. Health Canada collects and maintains a high volume of adverse event reports associated with different drugs and products. This app allows users to effortlessly interact with the reports database, conduct searches and view results in highly interactive dashboards. To support innovation, coordination and to support Canadians, this interface permits the users to export search results (with no limitation to the number of rows) in various file formats such as CSV and Excel for further exploration and experimentation.

## Database Specifications

This app is connected to a [PostgreSQL](https://www.postgresql.org/) database. A [foreign data wrapper](https://laurenz.github.io/oracle_fdw/) creates a live link from the [canada vigilance adverse reaction online oracle database](https://www.canada.ca/en/health-canada/services/drugs-health-products/medeffect-canada/adverse-reaction-database.html) to the PostgreSQL database as the remote schema. This schema is copied to the current schema. This copy prevents excess demand on the foreign server and allows for cleaning and versioning of the database.

## Data model

######Schema: current
Tables:
cv_reports: reports
Columns correspond to fields in report to canada vigilance. They contain information about patient characteristics.

cv_drug_product_ingredients: drug_product_ingredients
Columns map between drug names, product ids, and active ingredients

cv_report_drug: report_drug 
Columns correspond to fields in report to canada vigilance that record data about the specific therapy the patient was undergoing.

######Schema: meddra
Tables: Dynamically named table based on latest version in the history table of the date_refresh table. Version 20.1 will have a table name v_20_1.
Columns contain [meddra](https://www.canada.ca/en/health-canada/services/drugs-health-products/medeffect-canada/adverse-reaction-database/about-medical-dictionary-regulatory-activities-canada-vigilance-adverse-reaction-online-database.html) hierarchy, a controlled vocabulary of medical terms for regulatory purposes. 

## Development

- [global.R](https://github.com/hres/cvapps/blob/master/apps/CVShiny/global.R) is only run once, when the application is first hosted. This file runs all the database queries and generates the lists of search terms. The data is shared across user sessions. The database connections are handled via dbPool (https://github.com/rstudio/pool). Ergo efficiencies gained in the queries of this file will not be noticed by users.

- [server.R](https://github.com/hres/cvapps/blob/master/apps/CVShiny/server.R) filters reports data to get a list of report_ids that map to the specified search terms. This list of report ids is then joined with other tables for the selected visualizations and are counted and then passed back to ui.R

- [ui.R](https://github.com/hres/cvapps/blob/master/apps/CVShiny/ui.R) passes search terms to backend and specifies layout of application. linechart.R formats data for linechartbindings.js, which has functions for the [nvd3](http://nvd3.org/index.html) javascript library
         
## Refresh 

(database/refresh.sh)[https://google.ca] is scheduled with cron on the server shiny.hres.ca to detect the most recent datintreceived entry in the reports table of the remote schema. If it does not match the most recent entry in the corresponding table of the current schema, a refresh is triggered. 
A refresh inserts a new entry into the history table of the date_refresh schema before the name of the current schema is changed to a date format: cv_YYYY_MM_DD for versioning purposes. The current schema is then regenerated from the new remote schema by creating a duplicate table of each remote table and indexing every column of the tables used by the app. The data in the reports table is cleaned to have an appropriate age group.
Out of date meddra is also detected and reported.



## Fetching the data ([data_import](data_import))
> (data source) -> Postgres DB

Input to these scripts are a data source, the output is data tables being uploaded to the Postgres database. These could be automated in the future for automatic updating of data sources.

- [data_import/meddra_import.R](data_import/meddra_import.R) needs to be run in order to collect the different versions of MedDRA hierarchies (from zip files) and have them in a searchable table. This table is currently uploaded to the PostGRES database.
(In this script, it is also joined to the cv_drug_rxn table so that we can quickly group reports by HLT. SOC is already present in CV data.)

- [data_import/cv_import.R](data_import/cv_import.R) scrapes the CV extract landing page for the link to the extracts zip file, downloads it locally, and parses through the text files, constructs the `AGE_GROUP, DATRECEIVED_CLEAN, DATRECEIVED_CHAR, DATINTRECEIVED_CLEAN, DATINTRECEIVED_CHAR` columns, and uploads all the tables to the Postgres database with the date suffix

- [cvapps/refresh.r](cvapps/refresh.r) automatically refreshes both database and meddra version and uploads them to the database if they are out of date. Meddra is updated by checking /home/shared/meddra/ for the largest version and comparing this to a history table in postgres. The tables in the current schema are updated by detecting dates that are out of range in the remote table and then recreating all existing tables.

## Fetching the data ([data_processing](data_processing))
> Postgres DB -> Postgres DB

These scripts read tables from Postgres, do processing and calculations, some table joins, and uploads tables back to Postgres. These tables are the tables which AREN'T straight from the raw data.

- [data_processing/main_calcs.R](data_processing/main_calcs.R) The main script for calculations of disproportionality statistics. Some may be directly from formulas, some may be based on the `PhViD` library. Currently uses only reports after 2006 to calculate PRR, ROR, RRR, RFET, BCPNN, and GPS for both preferred terms (PT) and high-level terms (HLT)

- [data_processing/stats_functions.R](data_processing/stats_functions.R) Contains refactored versions of functions from PhViD. Either gives the same result (with less overhead, and strings are not cast into factors) or adds additional data columns, so then we don't have to run multiple times for multiple stats (can be calculated at same time) or to get an upper bound.

- [data_processing/quarterly_script.R](data_processing/quarterly_script.R) A lot of commented out code that should work to calculate cumulative quarterly count tables. Results not currently used.

- [data_processing/asdf.R](data_processing/asdf.R) Random code scrap containing code which potentially makes something like the cv_drugs_rxn table? Also comments on fixes made to the stats calculations code, useful for coders to read and keep in mind when working with data...

## Apps ([apps](apps))
> Postgres DB -> R Shiny

These are used for exploring and analyzing the data.

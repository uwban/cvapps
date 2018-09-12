# cvapps

This app has been developed by the Data Sciences Unit of RMOD at Health Canada as part of the Open Data Initiative. This is a prototype experiment that utilizes publically available data (Canada Vigilance Adverse Reaction Online Database), and provide visualizations in an interactive format. Health Canada collects and maintains a high volume of adverse event reports associated with different drugs and products. This app allows users to effortlessly interact with the reports database, conduct searches and view results in highly interactive dashboards. To support innovation, coordination and to support Canadians, this interface permits the users to export search results (with no limitation to the number of rows) in various file formats such as CSV and Excel for further exploration and experimentation.

## Database Specifications
This app is connected to the Health Canada drug/event API.https://node.hres.ca/docs.html#drug-event-docs

## Data model

######Schema: current

Tables were originally stored on a Postgrel relational database. The data were transformed into JSON objects and indexed in Elasticsearch.

######Schema: meddra

Tables: Dynamically named table based on latest version in the history table of the date_refresh table. Version 20.1 will have a table name v_21_0.
Columns contain [meddra](https://www.canada.ca/en/health-canada/services/drugs-health-products/medeffect-canada/adverse-reaction-database/about-medical-dictionary-regulatory-activities-canada-vigilance-adverse-reaction-online-database.html) hierarchy, a controlled vocabulary of medical terms for regulatory purposes.



## Development

- [global.R](apps/CVShiny_api/global.R) is only run once, when the application is first hosted. This file runs all the database queries and generates the lists of search terms. The data is shared across user sessions. The database connections are handled via dbPool (https://github.com/rstudio/pool). Ergo efficiencies gained in the queries of this file will not be noticed by users.

- [server.R](apps/CVShiny_api/server.R) filters reports data to get a list of report_ids that map to the specified search terms. This list of report ids is then joined with other tables for the selected visualizations and are counted and then passed back to ui.R

- [ui.R](apps/CVShiny_api/ui.R) passes search terms to backend and specifies layout of application. linechart.R formats data for linechartbindings.js, which has functions for the [nvd3](http://nvd3.org/index.html) javascript library.
         

## Apps ([apps](apps/CVShiny_api))
> Health Canada data -> Elasticsearch -> API -> R Shiny

These are used for exploring and analyzing the data.

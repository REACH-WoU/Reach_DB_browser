## REACH Database Browser

### Description

The REACH Database Browser is a Shiny web application designed to facilitate the browsing and visualization of data from the REACH Ukrainian mission database. This app allows users to interactively analyze data, generate visualizations, and download frequency analysis results.

### Features

- **Database Request**: Starting page which contains a sidebar with a list of projects and questions. You can select a project and a questions to build a DAF and send a request for analysis.
- **Geospatial Request**: Select the geograpy you'd like to explore and build a custom request for data.
- **Questions observer**: Select the questions specific to the geography you've selected in **Geospatial Request**.
- **Categorical Analysis**: A page that contain a set of visualizations for categorical data. You can select a project and a variable to generate categorical visualizations. You can also choose answer option to generate a map with the data.
- **Numeric Analysis**: A page that contain a set of visualizations for numeric data. You can select a project and a variable to generate numeric visualizations. You can also choose numeric statistic to generate a map with corresponding z-column.
- **Timeline Views**: Analyze trends over time with both categorical and numeric data. You can select multiple questions to analyze trends over time.

### Usage

 - **Database Request**: Use the sidebar to select questions(you can also filter questions for a specific project), then build and download the table. Choose question you are interesting in and perform analysis request. Wait until data will be processed.
 - **Geospatial Request**: Starting page which contains a sidebar with a list of geospatial disaggregations. You can select an administrative level you'd like to browse and update the main panel map accordingly. After this the user can draw a polygon in the main panel outlining the geography they'd like to explore. It is recommended that the user limits themselves to a few oblasts when exploring adminstative levels of hromada or settlement. After selecting the polygon, the user can click **Get info** button to see which projects are present in the database for the selected geography. The user can select the projects of interest and click **Get questions** button to be redirected to the **Questions observer** page.
- **Questions observer**: Here the user can select the questions that interested them for the geography they've selected in the **Geospatial Request** page. The table of questions can be filtered using the top of the table. The user can filter questions, question types and projects. After selecting the questions of interest the user can click **Get request** to process the request for the frequency tables.
 - **Categorical**: You have 3 input fields: Project, Variable and Option(for a map). From your selection, the app will generate a set of visualizations for categorical question.
 - **Numeric**: You have 3 input fields: Project, Variable and Statistic(for a map). From your selection, the app will generate a set of visualizations for numeric question.
 - **Timeline Categorical View**: Timeline selector for categorical questions allows you to select multiple questions to analyze trends over time.
 - **Timeline Numeric View**: Timeline selector for numeric questions allows you to select multiple questions to analyze trends over time.

### Acknowledgements

This application was developed using the Shiny package in R by Ukrainian cross-cutting team.

## Contact

For any inquiries or issues, please contact [nestor.cheryba@reach-initiative.org], [bohdan.pelekh@reach-initiative.org]

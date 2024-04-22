# SyntheticHealthcareDataGenerator
**Synthetic Data Generation in Machine Learning for Medicine and Healthcare Data**

ABSTRACT

This project involved the development of a R Shiny web application for the Chronic Disease Informatics Group, part of Trinity Kidney Centre. The application provides a user-friendly dashboard where healthcare workers can input medical datasets, generate customisable synthetic data from the inputted file using machine learning, and display the data for analysis and download. This system utilises the synthpop package in R for synthesis and pre-processes the inputted file to ensure the method accounts for the correlations between each patient’s hospital visit information over time. The dataset created can be used by practitioners to perform research on anonymised medical information. 

PREFACE

The ‘Synthetic Data Generator for Medicine and Healthcare Data’ web application was published on behalf of the Chronic Disease Informatics Group (CDIG). The CDIG are a research group within the Trinity Kidney Centre, using novel statistical and semantic techniques to analyse patient-level risk concerning flares of ANCA-associated vasculitis. They requested a synthetic data generator which could create realistic synthetic ANCA-associated vasculitis patients from their patient information, in order to preserve patient privacy and to bolster the sample sizes of patients due to the rarity of the disease. The requirements of the system were for it to be user-friendly, flexible, and compatible with the small number of patients in the datasets. 

This application was designed as part of my coursework for the Management Science and Information Systems Studies program at Trinity College Dublin. I chose this project due to the exciting combination of learnings involved: to build and publish a robust, interactive web application, and to design a solution to aid healthcare research, a domain in which I am passionate. Producing this report was a fantastic opportunity for me to develop my skillset in a wide variety of areas, from software development to data analytics.
The web application allows medical professionals with no prior knowledge in synthetic data to upload their medical dataset, create customisable synthetic data, analyse the data created through multiple visualisations, and download the generated data for their own use. This functionality meets the majority of the terms of reference which were agreed with the client. 

There were certain challenges encountered over the course of the project which obstructed the completion of all of the terms of reference. Due to the small dataset used in testing and the method used for synthetic data generation, the patient data produced is not always realistic and useful for research. With further development, the accuracy of the algorithm can be boosted, improving the utility of the application.

I would like to deeply thank Dr. Tin Lok James Ng, my project supervisor, for his unwavering support and invaluable direction throughout the course of project. I would also like to extend my gratitude towards the CDIG, in particular Dr. Eithne Nic an Ríogh, for her feedback and guidance, significantly aiding the design of the system. 

INSTALLATION INSTRUCTIONS
1. Download the server.R and ui.R files to the same folder in your R working directory.
2. Install the following packages: shiny, synthpop, anytime, dplyr, tidyr, ggplot2, gridExtra, corrplot, vcd.
3. Use RStudio to run the application by opening one of these files and clicking 'Run App' in the top right.

HOW TO USE (from instructions on the web application)
1. Upload your file in CSV format.
2. Ensure the file contains a Patient ID column and a Date of Encounter column, selecting their positions in the dataset using the sidebar sliders.
3. Adjust the number of patients and the maximum number of encounters per patient to generate in the sidebar.
4. Click the 'Create Synthetic Data' button to generate.
5. Click the download button to save the synthetic data.

CREDITS

Packages used: shiny, synthpop, anytime, dplyr, tidyr, ggplot2, gridExtra, corrplot, vcd.
Hosting service: ShinyApps (requires package 'rsconnect').
Shiny: https://shiny.posit.co/
synthpop: https://www.synthpop.org.uk/index.html
ShinyApps: https://www.shinyapps.io/

LICENCE

Copyright (c) 2024, Matthew Hanrahan

All rights reserved.

This source code is licensed under the MIT license found in the
LICENSE file in the root directory of this source tree. 

CONTACT INFO

mahanrah@tcd.ie

LINK TO WEB APPLICATION

https://mahanrah.shinyapps.io/synthetic_data_generator_for_medicine_and_healthcare_data/ 

# PhytoCytoTraits

This programme aims to sort automatedly flow cytometry traits through a user friendly graphical interface. The application provide basic but complete pipeline for flow cytometry data, from QA-QC to unsupervised classifications.

## Installation

**R environment**

The version 1.0 of the PhytoCytoTraits application needs a recent version of R (version 4.3.x or upper). It is not available on the CRAN website.

The R-packages needed by PhytoCytoTraits application are: cluster, CytoDx, dplyr, DT, flowcore, gdata, ggExtra, ggplot2, markdown, plotly, shiny, shinyalert, shinycssloaders, shinydashboard, shinyFiles, shinyjs, shinyWidgets and, V8.

CytoDx package can be download using BiocManager package. 

**Installation**

## Graphical User Interface

To launch the Graphical User Interface of PhytoCytoTraits, run these command lines in the R console:

*shinyApp(ui, server)*

![Graphical Use Interface](https://github.com/user-attachments/assets/e65b06ba-877d-407b-a27d-846eeccc40ab)

## General workflow

## Funding

This programme has been developped by Arnaud Louchart and Dedmer Van de Waal in the frame of BloomTox project. BloomTox project has been funded by the European Union (ERC, BLOOMTOX, 101044452). Views and opinions expressed are however those of the author(s) only and do not necessarily reflect those of the European Union or the European Research Council Executive Agency. Neither the European Union nor the granting authority can be held responsible for them.

The project has been conducted at the Netherlands Institute of Ecology of the Royal Netherlands Academy of Arts and Sciences (NIOO-KNAW) during Arnaud Louchart postdoc (2023-2026).

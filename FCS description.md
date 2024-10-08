---
title: "Flow Cytometry Pipeline for automated traits analysis"
author: Arnaud Louchart & Dedmer Van de Waal
date: 31 July 2023
output: html_document
---

### Description

The programme starts with the data importation. On top of this box you will find a button to load your file. At present, the script requires one[^1] flow cytometry file of the *.fcs* extension. You may see error printed on screen before the importation of the file. These errors should disappear once the file is loaded.

[^1]: Important: Note that currently the script allows to upload only one file. Future implementation will contain a batch mode to analyse multiple files. Future implementation will also explore the possibility to load other format.

Once the file is imported several things are printed on screen:

-   Table

The table contains the raw data extracted from the *.fcs* file. Only the 10 first rows are displayed. You can navigate through the data using the scroll bar and the pagination. The table is reactive, you can also order the data by increasing/decreasing order through one column. It is also possible to export this table in *.csv* format by clicking on the button *Download Full Results*.

-   Histogram

An histogram of the raw data is printed on screen. Default is the first column of your table. Other variables are accessible by exploring the box under *Choose your variable* label*.*

-   Explanations

We provide definitions that may help you to understand the distribution within the scatters and fluorescence filters.

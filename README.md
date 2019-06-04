
# Interactive Phenotype Map

This is my second-semester project in Bioinformatics Institute.

## Project description
Currently, doctors and specialists have a problem with the analysis of large patient data obtained as a result of a genetic test. Sometimes it is impossible to find a connection between the individual phenotypes predicted in the course of interpretation, especially between drugs. Our application is designed to find these connections and help geneticists.

## Main goal and objectives were:

The main goal of this project is to make a convenient app for doctors and geneticists, where they can view connections between input phenotypes.

1. Make an app, which will visualize phenotypes
2. Find connections between risks and phenotypes from database

## Project details
The main packages, which are used here are Shiny ( version ‘1.2.0’) and VisNetwork( version ‘2.0.6’).
The main script is `Phenotype_map.R`. It works with large XML database, a demo version of it you can see in `disease_prod.xml` file. The whole database has more than 9 thousand different phenotypes.
The script checks all connections between input phenotypes and count frequency for phenotypes, that appear one or more times.

## How to use this app:
- Clone this repository or open in it in your browser - https://lonishin.shinyapps.io/interactive_phenotype_map/
- Check, that you have installed all libraries.
- In opened window (or in a website) type in the field on the left side all phenotypes which connections you would like to get. Also, you can see the list of possible phenotypes in the example
- After a while, you'll see a map of phenotypes (you can zoom in and out) and a table below, where you can see certain phenotype frequency, so if it is high, that means that this phenotype appears in many links.


#### This is how this program looks like in the browser:

![Example](https://github.com/Lonishin/interactive_genes/blob/master/example1.jpg)

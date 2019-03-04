#!/usr/bin/env Rscript
#Author: AKE Franz-Arnold
#EDF Lab r&D
#Date: 27/02/2019

#Project : Obtain & Plot Krona chart of Specified core microbiome from input dataset

#required libs
library(readr)
source("core_function.R")


#********
#Main
#********

#gets databases
otu_db = read_delim("meta_bact_absolu_otu_table.csv", delim = ";", col_types = cols())
otu_targets = read_delim("meta_bact_targets_2.csv", delim = ",", col_types = cols())
otu_annot = read_delim("meta_bact_absolu_otu_annotation.csv", delim = ",", col_types = cols())

#Get core microbiome with specified conditions
output_tab = get_core_dbase(otu_db_input = otu_db, otu_targets_input = otu_targets, otu_annot_input = otu_annot)

#Write to standard output
write.table(file = "data.txt", sep = "\t", x = output_tab, quote = F,
            col.names = F, row.names = F)
















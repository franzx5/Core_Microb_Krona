#!/usr/bin/env Rscript
#Author: AKE Franz-Arnold
#EDF Lab r&D
#Date: 27/02/2019

#Project : Obtain & Plot Krona chart of Specified core microbiome from input dataset

#required libs
library(limma)
library(readr)
source("../bin/core_function.R")


#********
#Main
#********

#Enter Path files:
OTU_DB_PATH = "../data/meta_bact_absolu_otu_table.csv"
OTU_TARGETS_PATH = "../data/meta_bact_targets.csv"
OTU_ANNOT_PATH = "../data/meta_bact_absolu_otu_annotation.csv"


#gets databases
otu_db = read_delim(OTU_DB_PATH, delim = ";", col_types = cols())
otu_targets = read_delim(OTU_TARGETS_PATH, delim = ",", col_types = cols())
otu_annot = read_delim(OTU_ANNOT_PATH, delim = ",", col_types = cols())

#Get core microbiome with specified conditions
output_tab = get_core_dbase(otu_db_input = otu_db, otu_targets_input = otu_targets, otu_annot_input = otu_annot)

#Write to standard output
write.table(file = "data.txt", sep = "\t", x = output_tab, quote = F,
            col.names = F, row.names = F)
















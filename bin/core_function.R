#!/usr/bin/env Rscript
#Author: AKE Franz-Arnold
#EDF Lab r&D
#Date: 27/02/2019

#function
#get core database based on specified conditions 
#by default take all samples
#return core microbiome database

get_core_dbase = function(otu_db_input, otu_targets_input, otu_annot_input){
  #'''
  #get core database based on specified conditions 
  #by default take all samples
  #return core microbiome database
  #'''
  otu_targets_temp = otu_targets_input
  flag_1 = F; 
  while(!isTRUE(flag_1)){
    choice_1 = readline(prompt = "Do you want to fix some variables ? : ")
    while(choice_1 == "y"){
      choice_2 = NULL; choice_3 = NULL; choice_4 = NULL; flag_2 = F; flag_3 = F
      #check valid input - condition number
      while(!isTRUE(flag_2)){
        print(colnames(otu_targets_temp))
        choice_2 = as.numeric(readline(prompt = "Which condition Marie ? (Enter choice number) : "))
        if(!isTRUE(choice_2 %in% 1:length(otu_targets_temp))){cat("Bad entry\n")}
        else{flag_2 = T}
      }
      levels_values = levels(as.factor(unlist(otu_targets_temp[,choice_2])))
      print(levels_values)
      #check valid input - condition names
      while(!isTRUE(flag_3)){
        choice_3 = readline(prompt = "which values ? (Enter name of the value) : ")
        choice_3 = unlist(strsplit(choice_3,split = " ")) 
        # print(choice_3)
        #if one condition selected
        if(identical(choice_3, character(0))){cat("Empty entry ! all_values taken by default\n"); break}
        else if(length(choice_3) == 1 & choice_3 %in% levels_values){
          otu_targets_temp = otu_targets_temp[which(otu_targets_temp[,choice_2] == choice_3),]
          flag_3 = T
        }else if(length(choice_3) > 1){   #if several conditions selected
          interest_rows_ind = c()
          for (i in 1:length(choice_3)){
            if(choice_3[i] %in% levels_values){
              interest_rows_ind = c(interest_rows_ind, which(otu_targets_temp[,choice_2] == choice_3[i]))
            }else{cat("Bad value name entry !"); break}
          }
          otu_targets_temp = otu_targets_temp[interest_rows_ind,]
          flag_3 = T
        }else{cat("Bad entry !\n")}
      }
      cat("condition specified ...\n")
      print(otu_targets_temp)
      choice_1 = readline(prompt = "Do you want to fix others variables ?")
      if(choice_1 == "y"){flag_1 = T}
    }
    if(choice_1 == "n" & !isTRUE(flag_1)){
      cat("All OTU dataset count taken by default ...\n")
      flag_1 = T
    }else{
      cat("Bad entry ! try again")
      flag_1 = F
    }
  }
  
  # print(otu_targets_temp)
  #dataset conditioned
  condt_otu_db = otu_db_input[,which(colnames(otu_db_input)[2:length(otu_db_input)] %in% otu_targets_temp$Samples)+1]
  cat("dataset conditioned obtained ...\n")
  #get_core_microb of conditioned dataset
  interest_rows_ind = c()
  for (i in 1:dim(condt_otu_db)[1]){
    if(0 %in% condt_otu_db[i,] == F){
      interest_rows_ind = c(interest_rows_ind,i)
    }
  }
  core_condt_otu_db = condt_otu_db[interest_rows_ind,]
  core_otu_annot = otu_annot_input[interest_rows_ind,]
  cat("\n\nconditioned dataset core microbiome obtained ...\n")
  print(core_condt_otu_db)
  cat("\n\ntaxonomics assignation associated ...\n")
  print(core_otu_annot)
  #identify freq of identicals taxons
  core_krona_db_annot = as.data.frame(table(do.call(paste,c(core_otu_annot[-1], sep = ","))))[,c(2,1)]
  taxons = t(as.data.frame(strsplit(as.character(core_krona_db_annot$Var1), split = ",")))
  rownames(taxons) = 1:dim(taxons)[1]
  output_tab = cbind(core_krona_db_annot$Freq,taxons)
  colnames(output_tab) = c("Frequence", colnames(core_otu_annot)[2:length(core_otu_annot)])
  cat("\n\nfreq of identified taxons obtained ...\n")
  print(output_tab)
  return(output_tab)
}
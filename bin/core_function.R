#!/usr/bin/env Rscript
#Author: AKE Franz-Arnold
#EDF Lab r&D
#Date: 27/02/2019

#function
#libraries

#get core database based on specified conditions 
#by default take all samples
#return core microbiome database

get_core_dbase = function(otu_db_input, otu_targets_input, otu_annot_input){
  #'''
  #get core database based on specified conditions 
  #by default take all samples
  #return core microbiome database
  #'''
  parts = vector()
  conditions = c()
  otu_targets_temp = otu_targets_input
  flag_1 = F; 
  while(!isTRUE(flag_1)){
    flag_1 = F; 
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
        #if one condition selected
        if(identical(choice_3, character(0))){cat("Empty entry ! all_values taken by default\n"); break}
        else if(length(choice_3) > 1){   #if several conditions selected
          interest_rows_ind = c()
          for (i in 1:length(choice_3)){
            if(choice_3[i] %in% levels_values){
              interest_rows_ind = c(interest_rows_ind, which(otu_targets_temp[,choice_2] == choice_3[i]))
            }else{cat("Bad value name entry !"); break}
          }
          otu_targets_temp = otu_targets_temp[interest_rows_ind,]
          flag_3 = T
        }else if(length(choice_3) == 1 & choice_3 %in% levels_values){
          otu_targets_temp = otu_targets_temp[which(otu_targets_temp[,choice_2] == choice_3),]
          flag_3 = T
        }else{cat("Bad entry !\n")}
      }
      cat("condition specified ...\n")
      print(otu_targets_temp)
      choice_1 = readline(prompt = "Do you want to fix others variables ?")
      if(choice_1 == "n"){flag_1 = T}
      conditions = c(choice_3, conditions)
    }
    if(choice_1 == "n" & !isTRUE(flag_1)){cat("All OTU dataset count taken by default ...\n"); flag_1 = T;}
    else if(choice_1 == "n" & isTRUE(flag_1)){cat("Choices validated\n")}
    else{cat("Bad entry ! try again"); flag_1 = F}
  }
  
  # print(conditions)
  # mat_all = c()
  # for(i in 1:length(conditions)){
  #   col_cond = which(otu_targets_input == conditions[i], arr.ind = T)[,'col'][1]
  #   cond_temp = (otu_targets_input[,col_cond] == conditions[i])
  #   colnames(cond_temp) <- conditions[i]
  #   mat_all = cbind(mat_all, cond_temp)
  # }
  # counts_cond = vennCounts(mat_all)
  # print(str(counts_cond))
  # 
  # # print(counts_cond)
  # vennDiagram(counts_cond, circle.col = rainbow(20), names = conditions)
  # stop()
  
  # #construct conditions vector
  # for(i in 1:length(conditions)){
  #   tab = as.data.frame(combn(conditions,i))
  #   for(j in 1:length(tab)){
  #     cond = paste(as.character(tab[,j]), collapse = "&")
  #     parts = c(parts, cond)
  #   }
  # }
  # #find occurence number for each conditions
  # liste_nb_occur = c()
  # mat_nb_occur = c()
  # for(i in 1:length(parts)){
  #   temp_targets = otu_targets_input
  #   value_vec = unlist(strsplit(parts[i], split = "&"))
  #   if(length(value_vec)== 1){
  #     nb_occur = length(which(otu_targets_input == value_vec))
  #     col_number_associated = which(otu_targets_input == value_vec, arr.ind = T)[,'col'][1]
  #     cond_temp = (otu_targets_input[,col_number_associated] == value_vec)
  #     colnames(cond_temp) = value_vec
  #   }else{
  #     for(j in 1:length(value_vec)){
  #       if(dim(temp_targets)[1]==0){break}
  #       temp_targets = temp_targets[which(temp_targets == value_vec[j], arr.ind = T)[,"row"],]
  #     }
  #     nb_occur = dim(temp_targets)[1]
  #     # cond_temp = as.data.frame(x = otu_targets_input$Samples %in% temp_targets$Samples)
  #     # colnames(cond_temp) = parts[i]
  #   }
  #   liste_nb_occur = c(liste_nb_occur, nb_occur)
  #   mat_nb_occur = cbind(mat_nb_occur, cond_temp)
  # }
  # 
  # #clean mat_nb_occur
  # # print(str(mat_nb_occur))
  # print(mat_nb_occur)
  # neg_ind = c()
  # for (i in 1:length(mat_nb_occur)){
  #   if(identical(mat_nb_occur[,i], rep(FALSE,dim(mat_nb_occur)[1]))){
  #     neg_ind = c(neg_ind, -i)
  #   }
  # }
  # print(neg_ind)
  # mat_nb_occur = mat_nb_occur[,neg_ind]
  # print(mat_nb_occur)
  # print(parts)
  # # print(mat_nb_occur)
  # stop()
  
  #dataset conditioned
  condt_otu_db = otu_db_input[,which(colnames(otu_db_input)[2:length(otu_db_input)] %in% otu_targets_temp$Samples)+1]
  cat("dataset conditioned obtained ...\n")
  print(conditions)
  #get for each conditions the samples associated...
  if(!(is.null(conditions))){
    collection_cond = c()
    for(i in 1:length(conditions)){
      col_cond = which(otu_targets_input == conditions[i], arr.ind = T)[,'col'][1]
      samples_cond = otu_targets_input$Samples[which(otu_targets_input[,col_cond] == conditions[i])]
      #get otu_tab samples for each condition
      otu_cond = as.data.frame(otu_db_input[,samples_cond])
      #get core logical tab
      otu_core_cond = matrix(apply(otu_cond, 1, function(x) !(0 %in% x)))
      colnames(otu_core_cond) = conditions[i]
      #adding
      collection_cond = cbind(collection_cond, otu_core_cond)
      #printing
      core_res = otu_cond[which(otu_core_cond == TRUE),]
      rownames(core_res) = otu_db_input$OTU[as.numeric(rownames(core_res))]
      print(paste('CORE to Condition: ', conditions[i]))
      print(paste('On a ', rownames(core_res)))
    }
    #Plotting Venn Diagrams
    res = vennCounts(collection_cond)
    vennDiagram(res, cex = 0.7, circle.col = c("red","blue","green","yellow","orange","gray"))
    title('Core microbiome for defined conditions')
  }
  #get_core_microb for All
  cat("Getting core microbiome for all conditions")
  interest_rows_ind = c()
  for (i in 1:dim(condt_otu_db)[1]){if(0 %in% condt_otu_db[i,] == F){interest_rows_ind = c(interest_rows_ind,i)}}
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
  output_tab = as.data.frame(cbind(core_krona_db_annot$Freq,taxons))
  colnames(output_tab) = c("Frequence", colnames(core_otu_annot)[2:length(core_otu_annot)])
  output_tab$Frequence = as.numeric(as.character(output_tab$Frequence))
  cat("\n\nfreq of identified taxons obtained ...\n")
  print(output_tab)
  return(output_tab)
}
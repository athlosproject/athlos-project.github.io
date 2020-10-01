# helpfunctions.r



labelling <- function(l.hds, m.hds, vbl_name = hd_vbl, ds_lab = ds_label, cat_lab = cat_label) {
  
  
  # If there are more than one variable (e.g., a categorical and a continuous) and
  # If labelling or labelling_c were called first, then
  # m.hds has been already created and
  # Other variables have been already relabelled in m.hds, therefore
  # m.hds should be used to modify it
  
  # Otherwise, l.hds should be used to start,
  # In particular, when there is a unique variable
  
  if (is.empty.list(m.hds)) {
    aux_list <- l.hds
  } else {
    aux_list <- m.hds
  }
  
  
  # Labelling of the tibbles with categorical data and creating new tibbles with all missings recodified as NA
  
  for(name in names(l.hds)) {
    if(vbl_name %in% names(l.hds[[name]])) {
      # In the aux_object we copy the old tibble to recodify all missing values as NA.
      aux_object <- aux_list[[name]]
      # Labelling of variables
      label(l.hds[[name]][[vbl_name]]) <- label(aux_object[[vbl_name]]) <- ds_lab
      # Labelling of categories (for continues variables, only missing values)
      l.hds[[name]][[vbl_name]] <- labelled(l.hds[[name]][[vbl_name]], labels = cat_lab, label = ds_lab)
      aux_object[[vbl_name]] <- car::recode(unclass(aux_object[[vbl_name]]), "miss_values_vector = NA")
      # Labelling of categories (for categorical variables, only non-missing values)
      aux_object[[vbl_name]] <- labelled(aux_object[[vbl_name]], labels = cat_lab[1:(length(cat_lab)-9)], label = ds_lab)
      # Saving the recodified tibble in list m.hds
      m.hds[[name]] <- aux_object
      rm(aux_object)
    }
  }
  return(list(l.hds,m.hds))
  
}



is.empty.list <- function(a_list) {
  if(is.list(a_list)) {
    if(length(a_list) == 0 && is.null(attributes(a_list))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}



labelling_c <- function(l.hds, m.hds, vbl_name = hd_vbl, ds_lab = ds_label) {
  
  # If there are more than one variable (e.g., a categorical and a continuous) and
  # If labelling or labelling_c were called first, then
  # m.hds has been already created and
  # Other variables have been already relabelled in m.hds, therefore
  # m.hds should be used to modify it
  
  # Otherwise, l.hds should be used to start,
  # In particular, when there is a unique variable
  
  if (is.empty.list(m.hds)) {
    aux_list <- l.hds
  } else {
    aux_list <- m.hds
  }
  
  # Labelling of the tibbles with continuous data and creating new tibbles with all missings recodified as NA
  
  for(name in names(l.hds)) {
    
    if(vbl_name %in% names(l.hds[[name]])) {
      # In the aux_object we copy the old tibble to recodify all missing values as NA.
      aux_object <- aux_list[[name]]
      # Labelling of variables
      label(l.hds[[name]][[vbl_name]]) <- label(aux_object[[vbl_name]]) <- ds_lab
      # Labelling of categories (for continues variables, only missing values)
      l.hds[[name]][[vbl_name]] <- labelled(l.hds[[name]][[vbl_name]], labels = cont_label, label = ds_lab)
      aux_object[[vbl_name]] <- car::recode(unclass(aux_object[[vbl_name]]), "miss_values_vector = NA")
      aux_object[[vbl_name]] <- remove_val_labels(aux_object[[vbl_name]])
      # Saving the recodified tibble in list m.hds
      m.hds[[name]] <- aux_object
      rm(aux_object)
    }
  }
  return(list(l.hds,m.hds))
  
}


# Creation of summary tables for categorical data




summaries <- function(l.hds, m.hds, lnames, vbl_name = hd_vbl, cat_lab = cat_label) {
  
  # Creation of columns with categories and labels
  t.hds <- frq(l.hds[[1]][vbl_name])[[1]][, c("val", "label")] 
  # For each wave/population in l.hds, add the correponding values
  for (i in seq_along(l.hds)) {
    t.hds[2 + i] <- frq(l.hds[[i]][vbl_name])[[1]][, "raw.prc"] 
  }
  # Add sample size for each wave/population
  t.hds[2 + length(cat_lab),] <- c("n", "sample size", sapply(l.hds,function(wave) length(wave[[1]]))
  )
  # Add wave/population names
  names(t.hds) <- c("val", "label", lnames)
  return(t.hds)
  
}




# Creation of summary tables for continuous data




summaries_c <- function(l.hds, m.hds, lnames, vbl_name = hd_vbl) {
  
  # Creation of column with summary table categories
  t.summ <- summary(m.hds[[1]][vbl_name])[1:6]
  # Adding of missing/no-missing values categories
  t.hds <- c(substr(t.summ, 1, regexpr(":", t.summ, fixed=T) - 1), 
             labels(Continuous_summary(l.hds[[1]][[vbl_name]], missing_values = miss_values_vector)$values_table)[[2]]
  )
  # For each wave/population in l.hds, add the correponding values
  for (i in seq_along(l.hds)) {
    # First, summary values
    t.summ <- summary(m.hds[[i]][vbl_name])[1:6]
    # Next, missing/no-missing values (the 1 in $values_table[1,] stands for absolute values, while a 2 would stand for percentages)
    t.hds <- cbind(t.hds, 
                   c(as.numeric(substr(t.summ, regexpr(":", t.summ, fixed=T) + 1, nchar(t.summ))), 
                     as.numeric(Continuous_summary(l.hds[[i]][[vbl_name]], missing_values = miss_values_vector)$values_table[1, ])
                   )
    )
  }
  # Add sample size for each wave/population
  t.hds <- rbind(t.hds, 
                 c("sample size", 
                   sapply(l.hds, function(wave) length(wave[[1]]))
                 )
  )
  # Add wave/population names
  dimnames(t.hds)[[2]] <- c(dimnames(summary(m.hds[[1]][vbl_name]))[[2]], lnames)
  return(t.hds)
  
}






# Creation of trajectories table for each population




trajectories <- function(m.hds, vbl_name, global.df = FALSE) {
  
  # First wave data
  dbb <- m.hds[[1]][, c("id", vbl_name)] %>% as.data.frame()
  
  if(length(m.hds) > 1) {
    # Merge with next waves data
    for(ind in 2:length(m.hds)) {
      dbb <- merge(dbb, m.hds[[ind]][, c("id", vbl_name)], by = "id", suffixes = c("", paste0(".", names(m.hds)[ind])), all = T)
    }
    names(dbb) <- c("id", names(m.hds))
    
    if(isTRUE(global.df)) {
      dbb <<- dbb
    }
    
    # Glue individual data through all waves into trajectories
    v.dbb <- dbb[, 2] %>% as.character()
    for(ind in 2:length(m.hds)) {
      v.dbb <- paste(v.dbb, dbb[, ind + 1], sep="")
    }
  }
  else {
    v.dbb <- dbb[, 2] %>% as.character()
  }
  
  # Trajectories and frequencies
  f.dbb <- frq(v.dbb)[[1]][, c("val", "frq", "raw.prc")]
  if(is.numeric(f.dbb$val)) {
    f.dbb <- transform(frq(v.dbb)[[1]][, c("val", "frq", "raw.prc")],
                       val = formatC(frq(v.dbb)[[1]]$val, digits = length(m.hds), width = length(m.hds), flag = "0"))
  }
  return(f.dbb)
  
}








# Save data tables




savingRD <- function(l.hds,vbl_name = hd_vbl) {
  
  for(index in seq_along(l.hds)){
    assign(vbl_name, l.hds[[index]])
    save(vbl_name, list = vbl_name, file = paste0(datafolder, names(l.hds)[index], "/", vbl_name, ".RData"))
  }
  
}

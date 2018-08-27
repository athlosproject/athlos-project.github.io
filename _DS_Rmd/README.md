<!--README-->

# _DS_Rmd folder

In this folder there are two files for each variable or group of variables (when they are harmonised together); on one hand an RMarkdown file (`*.Rmd`), and on the other hand an R file (`*.R`).

* The RMarkdown files include the introductory section `Description of DataSchema variable` of the harmonisation reports of the corresponding harmonised variables or group of variables, with their description.
* The R file includes related data saved as R variables to labelling suitably the data in the chunks of the reports, and also a short form to the path where RData files are saved. Concretely
  + A vector `miss_values_vector` containing the values assigned to missing data.
  + A string with the label for each variable harmonised. If there is only one, it is called `ds_label`.
  + A vector with the labels for all the categories of the data. Usually it is called `cat_label` for categorical variables and `cont_label` for continuous variables. In this last case, the categories are those of the missing values.
  + A string `datafolder` to make short the path to the folder where we save the harmonised data.
  + A string containing the short name of each variable harmonised. If there is only one, it is called `hd_vbl`.
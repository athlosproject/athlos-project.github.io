require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_1_ifs_derived_variables', 
                         variables=list('hemobcl','hemobsi', 'hemobch', 'hemobst', 'hemobre', 'hemobpu', 'hemobli', 'hemobpi', 'headlwc', 
                                        'headlba', 'headldr', 'headlea', 'headlbe', 'headlwa', 'headlho', 'headlpr', 'headlph', 'headlme', 
                                        'headlmo', 'headlma', 'headlsh'), 
                         missings = TRUE)
ELSA_w1_ifs <- opal.execute(o,'o1')

opal.assign.table.tibble(o, 'o2','ELSA.wave_1_core_data_v3', 
                         variables = list('cfmetm', 'hediz', 'cfdscr', 'mmwlka','mmwlkb', 'scqolo', 'pscedc', 'hepain', 'heinct', 'hehear', 
                                          'hehra', 'heeye', 'hefrnd', 'hepap', 'cflisen', 'cfani', 'ncorrec', 'cflisd', 'cfmscr', 'hegenh',
                                          'hegenhb','hehelf','hehelfb', 'hefla'),
                         missings = TRUE)
ELSA_w1_data <- opal.execute(o,'o2')

ELSA_w1 <- merge(ELSA_w1_ifs, ELSA_w1_data, by="id", all=T)
save(ELSA_w1, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/3_Health_Status_Funct_Limit/ELSA_w1.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################
require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_2_core_data_v4', 
                         variables=list( 'CfMetM', 'HeDiz', 'CfDScr', 'MMWlkA','MMWlkB', 'scqolo', 'PScedC', 'HePain', 'HeInct',  'Hehear',  
                                         'HeHra',  'Heeye',  'Hefrnd',  'Hepap', 'CfLisEn', 'CfAni', 'nncorre', 'CfLisD', 'Hehelf', 'HeFla'),
                         missings = TRUE)
ELSA_w2_data <- opal.execute(o,'o1')


opal.assign.table.tibble(o, 'o2','ELSA.wave_2_ifs_derived_variables', 
                         variables = list('hemobsi', 'hemobch', 'hemobcl', 'hemobst', 'hemobre', 'hemobpu', 'hemobli', 'hemobpi', 
                                          'headlwc', 'headlba', 'headldr', 'headlea', 'headlbe', 'headlwa', 'headlho', 'headlpr', 
                                          'headlph', 'headlme', 'headlmo', 'headlma', 'headlsh'),
                         missings = TRUE)
ELSA_w2_ifs <- opal.execute(o,'o2')

ELSA_w2 <- merge(ELSA_w2_ifs, ELSA_w2_data, by="id", all=T)
save(ELSA_w2, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/3_Health_Status_Funct_Limit/ELSA_w2.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################

require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_3_elsa_data_v4', 
                         variables=list('cfmetm', 'cfdscr', 'mmwlka','mmwlkb', 'scqolo', 'pscedc', 'hepain', 'heinct', 'hehear', 'hehra',  
                                        'heeye',  'hefrnd',  'hepap', 'hefunc','hemobwa',  'hemobsi', 'hemobch',  'hemobcl',  
                                        'hemobst', 'hemobre',  'hemobpu',  'hemobli',  'hemobpi',  'headlwc', 'headlba', 
                                        'headldr',  'headlea',  'headlbe',  'headlwa',  'headlho',  'headlpr',  'headlph',  'headlme',  
                                        'headlmo',  'headlma', 'headlsh', 'cflisen',  'cfani', 'ncorrect',  'cflisd', 'hegenh', 'hefla' ),
                         missings = TRUE)
ELSA_w3 <- opal.execute(o,'o1')

save(ELSA_w3, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/3_Health_Status_Funct_Limit/ELSA_w3.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################

require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_4_elsa_data_v3', 
                         variables=list('cfmetm', 'hediz', 'cfdscr', 'mmwlka','mmwlkb', 'scqolo', 'pscedc', 'hepain', 'heinct', 'hehear', 
                                        'hehra', 'heeye', 'hefrnd', 'hepap', 'hefunc','hemobwa', 'hemobsi', 'hemobch', 'hemobcl', 'hemobst', 
                                        'hemobre', 'hemobpu', 'hemobli', 'hemobpi', 'headlwc', 'headlba', 'headldr', 'headlea', 'headlbe', 
                                        'headlwa', 'headlho', 'headlpr', 'headlme', 'headlmo', 'headlma', 'headlsh', 'cflisen', 'cfani', 
                                        'ncorrect', 'cflisd', 'cfmscr', 'hehelf', 'hefla'),
                         missings = TRUE)
ELSA_w4_data <- opal.execute(o,'o1')

opal.assign.table.tibble(o, 'o2','ELSA.wave_4_ifs_derived_variables', variables=list('headlph'), missings = TRUE)
ELSA_w4_ifs <- opal.execute(o,'o2')

ELSA_w4 <- merge(ELSA_w4_ifs, ELSA_w4_data, by="id", all=T)
save(ELSA_w4, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/3_Health_Status_Funct_Limit/ELSA_w4.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################

require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_5_elsa_data_v4', 
                         variables=list('cfdscr', 'mmwlka','mmwlkb', 'scqolo', 'pscedc', 'hepain', 'heinct', 'hehear', 'hehra', 'heeye', 
                                        'hefrnd', 'hepap', 'HeFunc','hemobwa', 'hemobsi', 'hemobch', 'hemobcl','hemobst',  'hemobre',  
                                        'hemobpu', 'hemobli',  'hemobpi',  'headlwc',  'headlba',  'headldr',  'headlea',  'headlbe',  
                                        'headlwa', 'headlho',  'headlpr',  'headlme',  'headlmo',  'headlma',  'headlsh',  'cflisen',  
                                        'cfani',  'NCorrect', 'cflisd', 'hehelf', 'hefla'),
                         missings = TRUE)
ELSA_w5_data <- opal.execute(o,'o1')

opal.assign.table.tibble(o, 'o2','ELSA.wave_5_ifs_derived_variables', variables=list('headlph'), missings = TRUE)
ELSA_w5_ifs <- opal.execute(o,'o2')

ELSA_w5 <- merge(ELSA_w5_ifs, ELSA_w5_data, by="id", all=T)
save(ELSA_w5, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/3_Health_Status_Funct_Limit/ELSA_w5.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################

require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_6_elsa_data_v2', 
                         variables=list('CfDScr', 'MMWlkA','MMWlkB', 'scqolo', 'PScedC', 'HePain', 'HeInct', 
                                        'HeHear', 'HeHra', 'HeEye', 'Hefrnd', 'Hepap', 'HeFunc','hemobwa', 
                                        'hemobsi', 'hemobch', 'hemobcl', 'hemobst', 'hemobre', 'hemobpu', 
                                        'hemobli', 'hemobpi', 'headlwc', 'headlba', 'headldr', 'headlea', 
                                        'headlbe', 'headlwa', 'headlho', 'headlpr', 'headlph', 'headlme', 
                                        'headlmo', 'headlma', 'headlsh', 'CfLisEn', 'CfLisD', 'CfMScr', 
                                        'Hehelf', 'HeFla'),
                         missings = TRUE)
ELSA_w6 <- opal.execute(o,'o1')

save(ELSA_w6, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/3_Health_Status_Funct_Limit/ELSA_w6.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################

require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_7_elsa_data', 
                         variables=list('CfMetM', 'HeDiz', 'CfDScr', 'MMWlkA','MMWlkB', 'scqolo', 'PScedC', 'HePain', 'HeInct', 
                                        'HeHear', 'HeHra', 'HeEye', 'Hefrnd', 'Hepap', 'HeFunc','hemobwa', 'hemobsi', 'hemobch', 
                                        'hemobcl', 'hemobst', 'hemobre', 'hemobpu', 'hemobli', 'hemobpi', 'headlwc', 'headlba', 
                                        'headldr', 'headlea', 'headlbe', 'headlwa', 'headlho', 'headlpr', 'headlph', 'headlme', 
                                        'headlmo', 'headlma', 'headlsh', 'CfLisEn', 'CfAni', 'CfLisD', 'CfMScr', 'Hehelf', 'HeFla'),
                         missings = TRUE)
ELSA_w7 <- opal.execute(o,'o1')

save(ELSA_w7, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/3_Health_Status_Funct_Limit/ELSA_w7.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################


  

  

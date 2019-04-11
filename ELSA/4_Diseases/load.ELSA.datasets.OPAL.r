require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_1_core_data_v3', 
                         variables = list('hedia01', 'hedia02', 'hedia03', 'hedia04','hedia05', 'hedia06', 'hedia07', 'hedia08', 'hedia09', 'hedia10', 
                                          'heagd','hedib01','hedib02','hedib03','hedib04','hedib05','hedib06','hedib07','hedib08','hedib09','hedib10',
                                          'hedim01','hedim02','hedim03','hedim04','hedim05','hedim06','hedim07','heagf','heaga','heagb','heage','heagg'),
                         missings = TRUE)
ELSA_w1 <- opal.execute(o,'o1')

save(ELSA_w1, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/4_Diseases/ELSA_w1.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################
require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_2_core_data_v4', 
                         variables=list( 'HeDiaC7', 'HeACd', 'hedia01', 'hedia02','hedia03', 'hedia04', 'hedia05', 'hedia06', 'hedia07',  'hedia08',  
                                         'hedia09',  'HeDiaS7','HeAgd','hedib01','hedib02','hedib03','hedib04','HeDiaD2','HeDiDS2','hediad1','hedids1',
                                         'hediac1','hedias1','HeDiaS2','hedim01','hedim02','hedim03','hedim04','hedim05','hedim06','hedim07','hedim08','HeDiaD3','HeDiDS3',
                                         'HeAgf','HeDiaC2','HeDiaS2','HeAga','HeDiaC3','HeAgbRY','HeAgeRY','HeDiaS4','HeDiaD5','HeDiDS5','HeAggRY'),
                         missings = TRUE)

ELSA_w2 <- opal.execute(o,'o1')

save(ELSA_w2, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/4_Diseases/ELSA_w2.rdata')

opal.assign.table.tibble(o, 'o2','ELSA.wave_2_derived_variables', 
                         variables = list('hedibas','bhedib01','bhedib02','bhedib03','bhedib04','bhedib05','bhedib06','bhedib07','bhedib08','bhedib09','bhedib10',
                                          'hedimbp','hediman'),
                         missings = TRUE)
ELSA_w2_dv <- opal.execute(o,'o2')


save(ELSA_w2_dv, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/4_Diseases/ELSA_w2_dv.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################

require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_3_elsa_data_v4', 
                         variables=list('hedacdi', 'dheacd', 'heagd','hediadi','hedibas','dhedibas','hedbdas','hedbsas','dhediblu','hedblu','hedbdlu','hedimbp','dhedimbp','hediabp','hedasbp',
                                        'hedacbp','dhedimdi','dhedibar','hedibar','hedbdar','hedbsar','heagf','dhediman','hediman','hedacan','hedasan','hediaan','heaga','hediami','hedacmi','dhedimmi',
                                        'heagbry','dhedimst','hedacst','hediast','heagery','dhedimhf','dhedimhm','dhedibca','hedbsca','hedbdca','heaggry'),
                         missings = TRUE)
ELSA_w3 <- opal.execute(o,'o1')

save(ELSA_w3, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/4_Diseases/ELSA_w3.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################

require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_4_elsa_data_v3', 
                         variables=list('hedacdi', 'heacd', 'hediadi', 'heagd','hedibas','hedbdas','hedbsas','hedbslu','hediblu','hedbdlu','hedimbp','hediabp',
                                        'hedasbp','hedacbp','hedimdi','hedibar','hedbdar','hedbsar','heagf','hediman','hedacan','hedasan','hediaan','heaga','hediami',
                                        'hedacmi','hedimmi','heagbry','hedimst','hedacst','hediast','heagery','hedimhf','hedimhm','hedibca','hedbsca','hedbdca','heaggry'),
                         missings = TRUE)
ELSA_w4 <- opal.execute(o,'o1')

save(ELSA_w4, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/4_Diseases/ELSA_w4.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################

require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_5_elsa_data_v4', 
                         variables=list('hedacdi', 'heacd','heagd', 'hediadi','hedibas','hedbdas','hedbsas','hedblu','hediblu','hedbdlu','hedimbp','hediabp',
                                        'hedasbp','hedacbp','hedimdi','hedibar','hedbdar','hedbsar','heagf','hediman','hedacan','hedasan','hediaan','heaga','hediami',
                                        'hedacmi','hedimmi','heagbry','hedimst','hedacst','hediast','heagery','hedimhf','hedimhm','hedibca','hedbsca','hedbdca','heaggry'),
                         missings = TRUE)
ELSA_w5 <- opal.execute(o,'o1')

save(ELSA_w5, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/4_Diseases/ELSA_w5.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################

require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_6_elsa_data_v2', 
                         variables=list('hedacdi', 'HeACd','HeAgd', 'hediadi','hedibas','hedbdas','hedbsas','hedblu','hediblu','hedbdlu','hedimbp','hediabp',
                                        'hedasbp','hedacbp','hedimdi','hedibar','hedbdar','hedbsar','HeAgf','hediman','hedacan','hedasan','hediaan','HeAga','hediami',
                                        'hedacmi','hedimmi','HeAgbRY','hedimst','hedacst','hediast','HeAgeRY','hedimhf','hedimhm','hedibca','hedbsca','hedbdca','HeAggRY'),
                         missings = TRUE)
ELSA_w6 <- opal.execute(o,'o1')

save(ELSA_w6, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/4_Diseases/ELSA_w6.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################

require(opal)

# authenticate within opal server 
o <- opal.login(username = 'your username', password = 'your password', url = 'http://opal.pssjd.org')

opal.assign.table.tibble(o, 'o1','ELSA.wave_7_elsa_data', 
                         variables=list('hedacdi', 'HeACd', 'HeAgd', 'hediadi','hedibas','hedblu','hediblu','hedbdlu','hedimbp','hediabp','hedasbp','hedacbp','hedibar',
                                        'hedbdar','hedimdi','hedbsar','HeAgf', 'hediman','hedacan','hedasan','hediaan','HeAga','hediami','hedacmi','hedimmi','HeAgbRY',
                                        'hedimst','hedacst','hediast','HeAgeRY','hedimhf','hedimhm','hedibca','hedbsca','hedbdca','HeAggRY'),
                         missings = TRUE)
ELSA_w7 <- opal.execute(o,'o1')

save(ELSA_w7, file='M:/WPs/WP1/Maelstrom/data_process/athlos-project.github.io/ELSA/4_Diseases/ELSA_w7.rdata')

rm(list = ls())
gc(reset=T)

###########################################################################################################################################


  

  

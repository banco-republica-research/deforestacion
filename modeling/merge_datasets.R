##############################################################################################
##############################################################################################
###                MERGE DATA SETS AND CREATE DATAFRAMES FOR REGRESSION                    ###
###     THIS CODE WILL READ THE DISTANCE AND DEFORESTATION DATA TO RUN RD MODELS           ###
###     HERE WE WILL RUN THE MODELS FOR BOTH STRATEGIES (1: REMOVE CONTINENTAL BORDERS AND ###
###             2. REMOVE ALL COTREATMENT BORDERS AND USE EFFECTIVE BORDERS).              ###
##############################################################################################
##############################################################################################

# Set directories
setwd(Sys.getenv("DATA_FOLDER"))

########################################## STRATEGY 2: EFFECTIVE BORDERS ###############################################

#Import datasets (covariates)
defo <- read.csv(paste0("Dataframes", "/","dataframe_deforestacion.csv")) %>% dplyr::select(-X)
cov <- read.csv(paste0("Dataframes", "/","geographic_covariates_new.csv")) %>% dplyr::select(-c(prec_1km.1:prec_1km.12)) %>% dplyr::select(-c(sq_1km.2:sq_1km.7))
treecover <- read.csv(paste0("Dataframes", "/", "treecover_2000.csv")) %>% dplyr::select(ID, treecover_agg)
simci_coca <- read.csv(paste0( "Dataframes", "/", "coca_simci_extract.csv")) %>% dplyr::select(contains("coca"), ID)
simci_mining <- read.csv(paste0("Dataframes", "/", "illegal_mining_simci_extract.csv")) %>% dplyr::select(contains("EVOA"), ID)
codmun <- read.csv(paste0("Dataframes", "/", "colombia_municipios_code_r.csv")) %>% dplyr::select(ID, layer) %>% rename(codmun = layer)
conflict <- read.csv(paste0("Conflicto", "/", "conflicto_pre2000.csv")) %>% dplyr::select(codmun, ao_crea, discapital, pobreza, pob_q34, nbi,nbi_q34, pres_cerac_1, hom_pc, hom_q34)

list_files <- list.files(paste0("Dataframes/", "Estrategia 2"), full.names = TRUE)
rds_2000 <- list_files[str_detect(list_files, "dist_2000")][c(1:3)] %>%
  lapply(readRDS) %>%
  lapply(data.frame)

list_files <- list.files(paste0("Dataframes/", "Estrategia 2"), full.names = TRUE)
territories_2000 <- list_files[str_detect(list_files, "_2000")] %>%
  .[str_detect(., "terr")] %>%
  lapply(readRDS) %>%
  lapply(data.frame)


#Aggregate deforestation (2001 - 2016) and Coca crops (2001 - 2016)
defo$loss_sum <- rowSums(defo[, c(5:length(names(defo)) - 1 )])
loss_sum <- defo %>% mutate(loss_sum = loss_sum / 16)
simci_coca$coca_agg <- rowMeans(simci_coca[, c(1:16)], na.rm = T)


#Merge data
defo_dist <- lapply(rds_2000, function(x){
  merge(loss_sum, x, by.x = "ID", by.y = "ID") %>%
    mutate(., loss_sum = loss_sum * 100) %>%
    mutate(., dist_disc = ifelse(treatment == 1, 1, -1) * layer) %>%
    mutate(., dist_disc = dist_disc / 1000) %>%
    # mutate(., buffer_id = as.character(buffer_id)) %>% mutate(., buffer_id = as.factor(buffer_id))
    merge(., cov, by = "ID", all.x = T) %>%
    merge(., treecover, by = "ID") %>%
    merge(., simci_coca, by = "ID", all.x = T) %>%
    merge(., simci_mining, by = "ID", all.x = T) %>% 
    merge(., codmun, by = "ID", all.x = T) %>% 
    merge(., conflict, by = "codmun", all.x = T)
})

defo_dist_terr <- lapply(territories_2000, function(x){
  merge(loss_sum, x, by.x = "ID", by.y = "ID") %>%
    mutate(., loss_sum = loss_sum * 100) %>%
    mutate(., dist_disc = ifelse(treatment == 1, 1, -1) * dist) %>%
    mutate(., dist_disc = dist_disc / 1000) %>%
    # mutate(., buffer_id = as.character(buffer_id)) %>% mutate(., buffer_id = as.factor(buffer_id))
    merge(., cov, by = "ID", all.x = T) %>%
    merge(., treecover, by = "ID") %>%
    merge(., simci_coca, by = "ID", all.x = T) %>%
    merge(., simci_mining, by = "ID", all.x = T) %>% 
    merge(., codmun, by = "ID", all.x = T) %>% 
    merge(., conflict, by = "codmun", all.x = T)
})


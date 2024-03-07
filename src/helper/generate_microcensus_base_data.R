#' Main control script
#'
#' @description Use this script for controlling the processing.
#'
#' @author [name], [email@com]

#devtools::install_github("envima/envimaR")
library(envimaR)
library(rprojroot)
library(DBI)
appendpackagesToLoad= c("downloader")
appendProjectDirList = c("data/data_lev0/GhcnDaily","data/data_lev0/GhcnMonthly")
root_folder = find_rstudio_root_file()
source(file.path(root_folder, "src/functions/000_setup.R"))

# gemeinden_sf_3035 = st_read(paste0(envrmt$path_data_lev0,"/gemeinden_DE_3035.gpkg"))
# grid_demo_DE_sf = readRDS(paste0(envrmt$path_data_lev1,"/grid_demo_DE_sf.rds"))
# split_mz_data (poly= gemeinden_sf_3035, mz_data=grid_demo_DE_sf,state_id = state_id)
rm(grid_demo_DE_sf)
rm(gemeinden_sf_3035)
gc()

for (s_id in state_id){
mz_state = readRDS(paste0(envrmt$path_data_lev0,"/",s_id,"_mz_state.rds"))
state = readRDS(paste0(envrmt$path_data_lev0,"/",s_id,"_state.rds"))
ew = st_intersection(mz_state,state)
# "ALTER_KURZ"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner>0 & Merkmal == "ALTER_KURZ" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state)
names(state_tmp)[19] = "A_u18"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner>0 & Merkmal == "ALTER_KURZ" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "A_18_29"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "ALTER_KURZ" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "A_30_49"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "ALTER_KURZ" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "A_50_64"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "ALTER_KURZ" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "A_p65"

#"ALTER_10JG"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "A_u10"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "A_10_19"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "A_20_29"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "A_30_39"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "A_40_49"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "A_50_59"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "A_60_69"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "A_70_79"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "A_p80"

# FAMSTND_AUSF 1 2 3 4 5 6 7 8
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "Fa_1"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "Fa_2"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "Fa_3"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "Fa_4"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "Fa_5"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "Fa_6"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==7) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "Fa_7"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==8) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "Fa_8"

# GEBURTLAND_GRP 1 21 22 23 24
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "GEBURTLAND_GRP" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "GG_1"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "GEBURTLAND_GRP" & Auspraegung_Code==21) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "GG_21"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "GEBURTLAND_GRP" & Auspraegung_Code==22) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "GG_22"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "GEBURTLAND_GRP" & Auspraegung_Code==23) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "GG_23"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "GEBURTLAND_GRP" & Auspraegung_Code==24) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "GG_24"

# GESCHLECHT 1 2
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "GESCHLECHT" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "Sex_1"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "GESCHLECHT" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "Sex_2"

# RELIGION_KURZ 1 2 3
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "RELIGION_KURZ" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "Rel_1"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "RELIGION_KURZ" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "Rel_2"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "RELIGION_KURZ" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "Rel_3"

# STAATSANGE_GRP 1 21 22 23 24
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_GRP" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STG_1"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_GRP" & Auspraegung_Code==21) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STG_21"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_GRP" & Auspraegung_Code==22) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STG_22"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_GRP" & Auspraegung_Code==23) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STG_23"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_GRP" & Auspraegung_Code==24) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STG_24"

# STAATSANGE_HLND 1  2  3  4  5  6  7  8  9 10 11 12 13 14
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_1"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_2"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_3"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_4"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_5"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_6"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==7) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_7"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==8) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_8"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==9) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_9"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==10) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_10"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==11) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_11"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==12) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_12"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==13) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_13"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==14) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STH_14"

# STAATSANGE_KURZ 1 2
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_KURZ" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STK_1"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATSANGE_KURZ" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STK_2"

# STAATZHL 1 2 3 4
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATZHL" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STA_1"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATZHL" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STA_2"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATZHL" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STA_3"
state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 0 & Merkmal == "STAATZHL" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
names(state_tmp)[19] = "STA_4"

state_tmp$BEM = NULL
state_tmp$NBD = NULL
state_tmp$FK_S3 = NULL
state_tmp$ARS_0 = NULL
state_tmp$EWZ = NULL
state_tmp$KFL = NULL
saveRDS(state_tmp,paste0(envrmt$path_data_lev0,"/",s_id,"_state_bevoelkerung.rds"))
}

#bind_rows(state_tmp,state_tmp)

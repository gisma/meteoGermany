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

mz = c("haus","demo") #,"geb","wohn","demo", "fami")
for (mz_id in mz){
  for (s_id in state_id){
    if (mz_id == "demo"){
      cat(" reading and intersecting the demo data \n")
      mz_state = readRDS(paste0(envrmt$path_data_lev1,"/",s_id,"_mz_state.rds"))
      state = readRDS(paste0(envrmt$path_data_lev1,"/",s_id,"_state.rds"))
      ew = st_intersection(mz_state,state)
      cat("state ", s_id,"\n")
      cat("ALTER KURZ \n")
      # "ALTER_KURZ"
      state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner>0 & Merkmal == "ALTER_KURZ" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state)
      names(state_tmp)[19] = "A_u18"
      state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner>0 & Merkmal == "ALTER_KURZ" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "A_18_29"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ALTER_KURZ" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "A_30_49"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ALTER_KURZ" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "A_50_64"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ALTER_KURZ" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "A_p65"

      #"ALTER_10JG"
      cat("ALTER LANG \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "A_u10"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "A_10_19"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "A_20_29"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "A_30_39"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "A_40_49"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "A_50_59"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "A_60_69"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "A_70_79"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ALTER_10JG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "A_p80"

      # FAMSTND_AUSF 1 2 3 4 5 6 7 8
      cat("FAMSTDN \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "Fa_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "Fa_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "Fa_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "Fa_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "Fa_5"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "Fa_6"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==7) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "Fa_7"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMSTND_AUSF" & Auspraegung_Code==8) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "Fa_8"

      # GEBURTLAND_GRP 1 21 22 23 24
      cat("GEBURTLAND \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBURTLAND_GRP" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GG_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBURTLAND_GRP" & Auspraegung_Code==21) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GG_21"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBURTLAND_GRP" & Auspraegung_Code==22) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GG_22"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBURTLAND_GRP" & Auspraegung_Code==23) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GG_23"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBURTLAND_GRP" & Auspraegung_Code==24) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GG_24"

      # GESCHLECHT 1 2
      cat("SEX \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GESCHLECHT" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "Sex_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GESCHLECHT" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "Sex_2"

      # RELIGION_KURZ 1 2 3
      cat("RELIGION \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "RELIGION_KURZ" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "Rel_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "RELIGION_KURZ" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "Rel_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "RELIGION_KURZ" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "Rel_3"

      # STAATSANGE_GRP 1 21 22 23 24
      cat("STATSANGEHOERIGKEIT 1 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_GRP" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STG_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_GRP" & Auspraegung_Code==21) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STG_21"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_GRP" & Auspraegung_Code==22) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STG_22"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_GRP" & Auspraegung_Code==23) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STG_23"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_GRP" & Auspraegung_Code==24) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STG_24"

      # STAATSANGE_HLND 1  2  3  4  5  6  7  8  9 10 11 12 13 14
      cat("STANG 2 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_5"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_6"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==7) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_7"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==8) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_8"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==9) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_9"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==10) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_10"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==11) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_11"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==12) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_12"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==13) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_13"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_HLND" & Auspraegung_Code==14) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STH_14"

      # STAATSANGE_KURZ 1 2
      cat("STANG 3 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_KURZ" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STK_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATSANGE_KURZ" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STK_2"

      # STAATZHL 1 2 3 4
      cat("STANG 4 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATZHL" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STA_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATZHL" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STA_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATZHL" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STA_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "STAATZHL" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "STA_4"

      state_tmp$BEM = NULL
      state_tmp$NBD = NULL
      state_tmp$FK_S3 = NULL
      state_tmp$ARS_0 = NULL
      state_tmp$EWZ = NULL
      state_tmp$KFL = NULL
      saveRDS(state_tmp,paste0(envrmt$path_data_lev1,"/",s_id,"_state_mz_bevoelkerung.rds"))
    }
    else if (mz_id == "fami"){
      cat("state ", s_id," ", mz_id," \n")
      cat(" reading and intersecting the fami data \n")
      mz_state = readRDS(paste0(envrmt$path_data_lev1,"/",s_id,"_fami_state.rds"))
      state = readRDS(paste0(envrmt$path_data_lev1,"/",s_id,"_state.rds"))
      ew = st_intersection(mz_state,state)

      # INSGESAMT 0
      if (!file.exists(paste0(envrmt$path_data_lev1,"/",s_id,"_state_mz_familien.rds")))
      {
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "INSGESAMT" & Auspraegung_Code==0) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state)
        names(state_tmp)[19] = "GES_0"
        # FAMTYP_KIND 1  2  3  4  5  6  7  8  9 10 11 12 13
        cat("FAMTYP_KIND 13 \n")
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMTYP_KIND" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAM_1"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMTYP_KIND" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAM_2"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMTYP_KIND" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAM_3"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMTYP_KIND" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAM_4"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMTYP_KIND" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAM_5"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMTYP_KIND" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAM_6"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMTYP_KIND" & Auspraegung_Code==7) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAM_7"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMTYP_KIND" & Auspraegung_Code==8) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAM_8"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMTYP_KIND" & Auspraegung_Code==9) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAM_9"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMTYP_KIND" & Auspraegung_Code==10) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAM_10"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMTYP_KIND" & Auspraegung_Code==11) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAM_11"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMTYP_KIND" & Auspraegung_Code==12) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "GES_12"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMTYP_KIND" & Auspraegung_Code==13) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "GES_13"
        # FAMGROESS_KLASS 1  2  3  4  5
        cat("FAMGROESS_KLASS 5 \n")
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMGROESS_KLASS" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAK_1"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMGROESS_KLASS" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAK_2"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMGROESS_KLASS" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAK_3"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMGROESS_KLASS" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAK_4"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "FAMGROESS_KLASS" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "FAK_5"

        # HHTYP_SENIOR_HH 1 2 3
        cat("HHTYP_SENIOR_HH 3\n")
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_SENIOR_HH" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "HHT_1"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_SENIOR_HH" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "HHT_2"
        state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_SENIOR_HH" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
        names(state_tmp)[19] = "HHT_3"

        state_tmp$BEM = NULL
        state_tmp$NBD = NULL
        state_tmp$FK_S3 = NULL
        state_tmp$ARS_0 = NULL
        state_tmp$EWZ = NULL
        state_tmp$KFL = NULL
        rm(ew)
        saveRDS(state_tmp,paste0(envrmt$path_data_lev1,"/",s_id,"_state_mz_familien.rds"))
      }

    }
    else if (mz_id == "haus"){
      cat(" reading and intersecting the haus data \n")
      mz_state = readRDS(paste0(envrmt$path_data_lev1,"/",s_id,"_haus_state.rds"))
      state = readRDS(paste0(envrmt$path_data_lev1,"/",s_id,"_state.rds"))
      ew = st_intersection(mz_state,state)
      cat("state ", s_id,"\n")

      # INSGESAMT 0
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "INSGESAMT" & Auspraegung_Code==0) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state)
      names(state_tmp)[19] = "GEH_0"

      # HHTYP_FAM  1  2  3  4  5
      cat("HHTYP_FAM 5 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 1 & Merkmal == "HHTYP_FAM" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHF_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 2 & Merkmal == "HHTYP_FAM" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHF_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 3 & Merkmal == "HHTYP_FAM" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHF_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 4 & Merkmal == "HHTYP_FAM" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHF_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 5 & Merkmal == "HHTYP_FAM" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHF_5"



      # HHTYP_LEB 1  2  3  4  5 6 7
      cat("HHTYP_LEB 7 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHL_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHL_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHL_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHL_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHL_5"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHL_6"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==7) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHL_7"


      # HHGROESS_KLASS  1 2 3 4 5 6
      cat("HHGROESS_KLASS 6 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHGROESS_KLASS" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHK_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHGROESS_KLASS" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHK_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHGROESS_KLASS" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHK_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHGROESS_KLASS" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHK_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHGROESS_KLASS" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHK_5"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHGROESS_KLASS" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HHK_6"

      state_tmp$BEM = NULL
      state_tmp$NBD = NULL
      state_tmp$FK_S3 = NULL
      state_tmp$ARS_0 = NULL
      state_tmp$EWZ = NULL
      state_tmp$KFL = NULL
      saveRDS(state_tmp,paste0(envrmt$path_data_lev1,"/",s_id,"_state_mz_haus.rds"))


    }
    else if (mz_id == "wohn"){
      cat(" reading and intersecting the wohn data \n")
      mz_state = readRDS(paste0(envrmt$path_data_lev1,"/",s_id,"_wohn_state.rds"))
      state = readRDS(paste0(envrmt$path_data_lev1,"/",s_id,"_state.rds"))
      ew = st_intersection(mz_state,state)
      cat("state ", s_id,"\n")

      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "INSGESAMT" & Auspraegung_Code==0) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state)
      names(state_tmp)[19] = "GEW_0"

      # NUTZUNG_DETAIL_HHGEN 1 11 12 2 21 22 3 4 5 99
      cat("NUTZUNG_DETAIL_HHGEN 10 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "NUTZUNG_DETAIL_HHGEN" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "NUTZ_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "NUTZUNG_DETAIL_HHGEN" & Auspraegung_Code==11) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "NUTZ_11"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "NUTZUNG_DETAIL_HHGEN" & Auspraegung_Code==12) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "NUTZ_12"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "NUTZUNG_DETAIL_HHGEN" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "NUTZ_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "NUTZUNG_DETAIL_HHGEN" & Auspraegung_Code==21) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "NUTZ_21"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "NUTZUNG_DETAIL_HHGEN" & Auspraegung_Code==22) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "NUTZ_22"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "NUTZUNG_DETAIL_HHGEN" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "NUTZ_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "NUTZUNG_DETAIL_HHGEN" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "NUTZ_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "NUTZUNG_DETAIL_HHGEN" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "NUTZ_5"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "NUTZUNG_DETAIL_HHGEN" & Auspraegung_Code==99) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "NUTZ_99"



      # WOHNEIGENTUM  1  2  3  4  99
      cat("WOHNEIGENTUM 5 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 1 & Merkmal == "WOHNEIGENTUM" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOE_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 1 & Merkmal == "WOHNEIGENTUM" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOE_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 1 & Merkmal == "WOHNEIGENTUM" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOE_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 1 & Merkmal == "WOHNEIGENTUM" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOE_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter(Einwohner > 1 & Merkmal == "WOHNEIGENTUM" & Auspraegung_Code==99) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOE_99"

      # WOHNFLAECHE_10S  01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 99
      cat("WOHNFLAECHE 0 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_5"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_6"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==7) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_7"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==8) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_8"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==9) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_9"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==10) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_10"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==11) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_11"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==12) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_12"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==13) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_13"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==14) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_14"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==15) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_15"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==16) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_16"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==17) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_17"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HHTYP_LEB" & Auspraegung_Code==99) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "WOH_99"


      # RAUMANZAHL  1 2 3 4 5 6 7 99
      cat("RAUMANZAHL 8 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "RAUMANZAHL" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "RAU_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "RAUMANZAHL" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "RAU_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "RAUMANZAHL" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "RAU_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "RAUMANZAHL" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "RAU_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "RAUMANZAHL" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "RAU_5"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "RAUMANZAHL" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "RAU_6"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "RAUMANZAHL" & Auspraegung_Code==7) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "RAU_7"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "RAUMANZAHL" & Auspraegung_Code==99) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "RAU_99"

      # GEBAEUDEART_SYS  1 2 3 4 5 6 7 99
      cat("GEBÄUDEART 8 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBAEUDEART_SYS" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEBS_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBAEUDEART_SYS" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEBS_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBAEUDEART_SYS" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEBS_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBAEUDEART_SYS" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEBS_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBAEUDEART_SYS" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEBS_5"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBAEUDEART_SYS" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEBS_6"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBAEUDEART_SYS" & Auspraegung_Code==7) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEBS_7"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBAEUDEART_SYS" & Auspraegung_Code==99) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEBS_99"

      # BAUJAHR_MZ  1 2 3 4 5 6 7 8 9 10
      cat("BAUJAHR 10 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "BAUJAHR_MZ" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "BJZ_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "BAUJAHR_MZ" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "BJZ_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "BAUJAHR_MZ" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "BJZ_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "BAUJAHR_MZ" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "BJZ_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "BAUJAHR_MZ" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "BJZ_5"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "BAUJAHR_MZ" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "BJZ_6"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "BAUJAHR_MZ" & Auspraegung_Code==7) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "BJZ_7"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "BAUJAHR_MZ" & Auspraegung_Code==8) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "BJZ_8"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "BAUJAHR_MZ" & Auspraegung_Code==9) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "BJZ_9"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "BAUJAHR_MZ" & Auspraegung_Code==10) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "BJZ_10"

      # EIGENTUM  1 2 3 4 5 6 7 8
      cat("EIGENTUM 8 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "EIGENTUM" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "EIG_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "EIGENTUM" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "EIG_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "EIGENTUM" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "EIG_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "EIGENTUM" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "EIG_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "EIGENTUM" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "EIG_5"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "EIGENTUM" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "EIG_6"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "EIGENTUM" & Auspraegung_Code==7) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "EIG_7"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "EIGENTUM" & Auspraegung_Code==8) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "EIG_8"

      # GEBTYPBAUWEISE  1 2 3 4
      cat("GEBTYPBAUWEISE 4 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPBAUWEISE" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GET_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPBAUWEISE" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GET_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPBAUWEISE" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GET_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPBAUWEISE" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GET_4"

      # GEBTYPGROESSE  1 2 3 4 5 6 7 8 9 10
      cat("GEBTYPGROESSE 10 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPGROESSE" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEG_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPGROESSE" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEG_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPGROESSE" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEG_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPGROESSE" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEG_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPGROESSE" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEG_5"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPGROESSE" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEG_6"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPGROESSE" & Auspraegung_Code==7) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEG_7"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPGROESSE" & Auspraegung_Code==8) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEG_8"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPGROESSE" & Auspraegung_Code==9) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEG_9"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "GEBTYPGROESSE" & Auspraegung_Code==10) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "GEG_10"

      # HEIZTYP  1 2 3 4 5 6
      cat("HEIZTYP 6 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HEIZTYP" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HEI_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HEIZTYP" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HEI_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HEIZTYP" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HEI_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HEIZTYP" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HEI_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HEIZTYP" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HEI_5"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "HEIZTYP" & Auspraegung_Code==6) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "HEI_6"


      # ZAHLWOHNGN_HHG  1 2 3 4 5
      cat("ZAHLWOHN 5 \n")
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ZAHLWOHNGN_HHG" & Auspraegung_Code==1) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "ZWO_1"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ZAHLWOHNGN_HHG" & Auspraegung_Code==2) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "ZWO_2"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ZAHLWOHNGN_HHG" & Auspraegung_Code==3) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "ZWO_3"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ZAHLWOHNGN_HHG" & Auspraegung_Code==4) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "ZWO_4"
      state_tmp = right_join(st_drop_geometry(ew %>% filter( Merkmal == "ZAHLWOHNGN_HHG" & Auspraegung_Code==5) %>% count(ADE,GF,BSG,ARS,AGS,SDV_ARS,GEN,BEZ,IBZ,SN_L,SN_R,SN_K,SN_V1,SN_V2,SN_G,NUTS,WSK,DEBKG_ID)),state_tmp)
      names(state_tmp)[19] = "ZWO_5"
      state_tmp$BEM = NULL
      state_tmp$NBD = NULL
      state_tmp$FK_S3 = NULL
      state_tmp$ARS_0 = NULL
      state_tmp$EWZ = NULL
      state_tmp$KFL = NULL
      saveRDS(state_tmp,paste0(envrmt$path_data_lev1,"/",s_id,"_state_mz_wohn.rds"))
    }
  }
}
#bind_rows(state_tmp,state_tmp)

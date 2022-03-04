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
      # Statistisches Bundesamt, Zensus 2011
      # Version 1; 13.02.2018
      #
      # Merkmale und Merkmalsausprägungen
      # Bevölkerung je Hektar
      # Ergebnisse des Zensus am 9. Mai 2011 in Gitterzellen
      #
      # Hier werden die Ausprägungen der einzelnen Merkmale aufgelistet.
      # Ausführliche Merkmalsdefinitionen sind auf dem Tabellenblatt "Merkmalsdefinitionen" zu finden.
      #
      #
      # Merkmal	Code	Text	Erläuterungen
      #
      # INSGESAMT			Gesamtzahl der Einheiten in der Gitterzelle
      # Kann aufgrund der Geheimhaltung  von der Summe über alle Ausprägungen der anderen Merkmale abweichen
      # 0	Einheiten insgesamt
      #
      #
      # ALTER_10JG			Alter (10er-Jahresgruppen)
      # 1	 Unter 10
      # 2	 10 - 19
      # 3	 20 - 29
      # 4	 30 - 39
      # 5	 40 - 49
      # 6	 50 - 59
      # 7	 60 - 69
      # 8	 70 - 79
      # 9	 80 und älter
      #
      #
      # ALTER_KURZ			Alter (5 Altersklassen)
      # 1	Unter 18
      # 2	18 - 29
      # 3	30 - 49
      # 4	50 - 64
      # 5	65 und älter
      #
      #
      # FAMSTND_AUSF			Familienstand (ausführlich)
      # 1	Ledig
      # 2	Verheiratet
      # 3	Verwitwet
      # 4	Geschieden
      # 5	Eingetr. Lebenspartnerschaft
      # 6	Eingetr. Lebensparter/-in verstorben
      # 7	Eingetr. Lebenspartnerschaft aufgehoben
      # 8	Ohne Angabe
      #
      #
      # GEBURTLAND_GRP			Geburtsland (Gruppen)
      # 1	Deutschland
      # 21	EU27-Land
      # 22	Sonstiges Europa
      # 23	Sonstige Welt
      # 24	Sonstige
      #
      #
      # GESCHLECHT			Geschlecht
      # 1	Männlich
      # 2	Weiblich
      #
      #
      # RELIGION_KURZ			Religion
      # 1	Römisch-katholische Kirche (öffentlich-rechtlich)
      # 2	Evangelische Kirche (öffentlich-rechtlich)
      # 3	Sonstige, keine, ohne Angabe
      #
      #
      # STAATSANGE_GRP			Staatsangehörigkeitsgruppen
      # 1	Deutschland
      # 21	EU27-Land
      # 22	Sonstiges Europa
      # 23	Sonstige Welt
      # 24	Sonstiges
      #
      #
      # STAATSANGE_HLND			Staatsangehörigkeit nach ausgewählten Ländern
      # 1	Deutschland
      # 2	Bosnien und Herzegowina
      # 3	Griechenland
      # 4	Italien
      # 5	Kasachstan
      # 6	Kroatien
      # 7	Niederlande
      # 8	Österreich
      # 9	Polen
      # 10	Rumänien
      # 11	Russ. Föderation
      # 12	Türkei
      # 13	Ukraine
      # 14	Sonstige
      #
      #
      # STAATSANGE_KURZ			Staatsangehörigkeit
      # 1	Deutschland
      # 2	Ausland
      #
      #
      # STAATZHL			Zahl der Staatsangehörigkeiten
      # 1	Eine Staatsangehörigkeit
      # 2	Mehrere, deutsch und ausländisch
      # 3	Mehrere, nur ausländisch
      # 4	Nicht bekannt

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
      # Statistisches Bundesamt, Zensus 2011
      # Version 1; 21.11.2017
      #
      # Merkmale und Merkmalsausprägungen
      # Familien je Hektar
      # Ergebnisse des Zensus am 9. Mai 2011 in Gitterzellen
      #
      # Hier werden die Ausprägungen der einzelnen Merkmale aufgelistet.
      # Ausführliche Merkmalsdefinitionen sind auf dem Tabellenblatt "Merkmalsdefinitionen" zu finden.
      #
      #
      # Merkmal	Code	Text	Erläuterungen
      #
      # INSGESAMT			Gesamtzahl der Einheiten in der Gitterzelle
      # Kann aufgrund der Geheimhaltung  von der Summe über alle Ausprägungen der anderen Merkmale abweichen
      # 0	Einheiten insgesamt
      #
      #
      # FAMTYP_KIND			Typ der Kernfamilie (nach Kindern)
      # 1	Ehepaare ohne Kind
      # 2	Ehepaare, mind. 1 Kind < 18
      # 3	Ehepaare alle Kinder ≥ 18
      # 4	Eingetr. Lebenspartnerschaften ohne Kind
      # 5	Eingetr. Lebenspartnerschaften mind. 1 Kind < 18
      # 6	Eingetr. Lebenspartnerschaften alle Kinder ≥ 18
      # 7	Nichteheliche Lebensgem. ohne Kind
      # 8	Nichteheliche Lebensgem. mind. 1 Kind < 18
      # 9	Nichteheliche Lebensgem. alle Kinder ≥ 18
      # 10	Alleinerziehende Väter mind. 1 Kind < 18
      # 11	Alleinerziehende Väter alle Kinder ≥ 18
      # 12	Alleinerziehende Mütter mind. 1 Kind < 18
      # 13	 Alleinerziehende Mütter alle Kinder ≥ 18
      #
      # FAMGROESS_KLASS			Größe der Kernfamilie
      # 1	2 Personen
      # 2	3 Personen
      # 3	4 Personen
      # 4	5 Personen
      # 5	6 und mehr Personen
      #
      # HHTYP_SENIOR_HH			Seniorenstatus eines privaten Haushalts
      # 1	Haushalte mit ausschließlich Senioren/-innen
      # 2	Haushalte mit Senioren/-innen und Jüngeren
      # 3	Haushalte ohne Senioren/-innen

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
      # Statistisches Bundesamt, Zensus 2011
      # Version 1; 21.11.2017
      #
      # Merkmale und Merkmalsausprägungen
      # Haushalte je Hektar
      # Ergebnisse des Zensus am 9. Mai 2011 in Gitterzellen
      #
      # Hier werden die Ausprägungen der einzelnen Merkmale aufgelistet.
      # Ausführliche Merkmalsdefinitionen sind auf dem Tabellenblatt "Merkmalsdefinitionen" zu finden.
      #
      #
      # Merkmal	Code	Text	Erläuterungen
      #
      # INSGESAMT			Gesamtzahl der Einheiten in der Gitterzelle
      # Kann aufgrund der Geheimhaltung  von der Summe über alle Ausprägungen der anderen Merkmale abweichen
      # 0	Einheiten insgesamt
      #
      #
      # HHTYP_FAM			Typ des privaten Haushalts (nach Familien)
      # 1	Einpersonenhaushalte (Singlehaushalte)
      # 2	Paare ohne Kind(er)
      # 3	Paare mit Kind(ern)
      # 4	Alleinerziehende Elternteile
      # 5	Mehrpersonenhaushalte ohne Kernfamilie
      #
      # HHTYP_LEB			Typ des privaten Haushalts (nach Lebensform)
      # 1	Einpersonenhaushalte (Singlehaushalte)
      # 2	Ehepaare
      # 3	Eingetr. Lebenspartnerschaften
      # 4	 Nichteheliche Lebensgemeinschaften
      # 5	Alleinerziehende Mütter
      # 6	Alleinerziehende Väter
      # 7	Mehrpersonenhaushalte ohne Kernfamilie
      #
      # HHGROESS_KLASS			Größe des privaten Haushalts
      # 1	1 Person
      # 2	2 Personen
      # 3	3 Personen
      # 4	4 Personen
      # 5	5 Personen
      # 6	6 und mehr Personen


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
      # Wohnungen und Gebäude je Hektar
      # Ergebnisse des Zensus am 9. Mai 2011 in Gitterzellen
      #
      # Hier werden die Ausprägungen der einzelnen Merkmale aufgelistet.
      # Ausführliche Merkmalsdefinitionen sind auf dem Tabellenblatt "Merkmalsdefinitionen" zu finden.
      #
      #
      # Merkmal	Code	Text	Erläuterungen
      #
      # INSGESAMT			Gesamtzahl der Einheiten in der Gitterzelle
      # Kann aufgrund der Geheimhaltung  von der Summe über alle Ausprägungen der anderen Merkmale abweichen
      # 0	Einheiten insgesamt
      #
      #
      # NUTZUNG _DETAIL_HHGEN			Nutzung nach Belegung durch Haushalt
      # 1	 Von Eigentümer/-in bewohnt
      # 11	 Eigentum: mit aktuell geführtem Haushalt
      # 12	 Eigentum: ohne aktuell geführtem Haushalt
      # 2	 Zu Wohnzwecken vermietet
      # 21	 Vermietet: mit aktuell geführtem Haushalt
      # 22	 Vermietet: ohne aktuell geführtem Haushalt
      # 3	 Ferien- und Freizeitwohnung
      # 4	 Leer stehend
      # 5	 Diplomaten-/Streitkräftewohnung
      # 99	 Gewerbl. Nutzung
      #
      # WOHNEIGENTUM			Eigentumsverhältnisse der Wohnung
      # 1	 Privatperson/-en
      # 2	 Privatwirtschaftliche Unternehmen (jur. Personen)
      # 3	 Öffentliche Unternehmen, Kirchen o.ä.
      # 4	 Wohnungsgenossenschaft
      # 99	 Trifft nicht zu (da keine Eigentumswohnung)
      #
      # WOHNFLAECHE_10S			Fläche der Wohnung (10m2 Intervalle)
      # 01	Unter 30
      # 02	30 - 39
      # 03	40 - 49
      # 04	50 - 59
      # 05	60 - 69
      # 06	70 - 79
      # 07	80 - 89
      # 08	90 - 99
      # 09	100 - 109
      # 10	110 - 119
      # 11	120 - 129
      # 12	130 - 139
      # 13	140 - 149
      # 14	150 - 159
      # 15	160 -169
      # 16	170 - 179
      # 17	180 und mehr
      # 99	t.n.z., gewerblich
      #
      # RAUMANZAHL			Zahl der Räume
      # 1	 1 Raum
      # 2	 2 Räume
      # 3	 3 Räume
      # 4	 4 Räume
      # 5	 5 Räume
      # 6	 6 Räume
      # 7	 7 und mehr Räume
      # 99	 t.n.z., gewerblich
      #
      # GEBAEUDEART_SYS			Art des Gebäudes
      # 1	 Gebäude mit Wohnraum
      # 11	 Wohngebäude
      # 111	 Wohngebäude (ohne Wohnheime)
      # 112	 Wohnheim
      # 12	 Sonstiges Gebäude mit Wohnraum
      #
      # BAUJAHR_MZ			Baujahr (Mikrozensus-Klassen)
      # 1	 Vor 1919
      # 2	 1919 - 1948
      # 3	 1949 - 1978
      # 4	 1979 - 1986
      # 5	 1987 - 1990
      # 6	 1991 - 1995
      # 7	 1996 - 2000
      # 8	 2001 - 2004
      # 9	 2005 - 2008
      # 10	 2009 und später
      #
      # EIGENTUM			Eigentumsform des Gebäudes
      # 1	 Gemeinschaft von Wohnungseigentümern/-innen
      # 2	 Privatperson/en
      # 3	 Wohnungsgenossenschaft
      # 4	 Kommune oder Kommunales Wohnungsunternehmen
      # 5	 Privatwirtschaftliches Wohnungsunternehmen
      # 6	 Anderes privatwirtschaftliches Unternehmen
      # 7	 Bund oder Land
      # 8	 Organisation ohne Erwerbszweck (z.B. Kirche)
      #
      # GEBTYPBAUWEISE			Gebäudetyp-Bauweise
      # 1	 Freistehendes Haus
      # 2	 Doppelhaus hälfte
      # 3	 Gereihtes Haus
      # 4	 Anderer Gebäudetyp
      #
      # GEBTYPGROESSE			Gebäudetyp-Größe
      # 1	 Freistehendes Einfamilienhaus
      # 2	 Einfamilienhaus: Doppelhaushälfte
      # 3	 Einfamilienhaus: Reihenhaus
      # 4	 Freistehendes Zweifamilienhaus
      # 5	 Zweifamilienhaus: Doppelhaushälfte
      # 6	 Zweifamilienhaus: Reihenhaus
      # 7	 Mehrfamilienhaus: 3-6 Wohnungen
      # 8	 Mehrfamilienhaus: 7-12 Wohnungen
      # 9	 Mehrfamilienhaus: 13 und mehr Wohnungen
      # 10	 Anderer Gebäudetyp
      #
      # HEIZTYP			Heizungsart
      # 1	 Fernheizung (Fernwärme)
      # 2	 Etagenheizung
      # 3	 Blockheizung
      # 4	 Zentralheizung
      # 5	 Einzel-/Mehrraumöfen (auch Nachtspeicherheizung)
      # 6	 Keine Heizung im Gebäude oder in den Wohnungen
      #
      # ZAHLWOHNGN_HHG			Zahl der Wohnungen im Gebäude
      # 1	 1 Wohnung
      # 2	 2 Wohnungen
      # 3	 3 - 6 Wohnungen
      # 4	 7 - 12 Wohnungen
      # 5	 13 und mehr Wohnungen


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

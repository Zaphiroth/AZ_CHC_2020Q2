# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q2
# Purpose:      Readin Raw Data
# programmer:   Zhe Liu
# Date:         2020-08-17
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Mapping table ----
# PCHC code
pchc.universe <- read.xlsx("02_Inputs/Universe_PCHCCode_20200927.xlsx", sheet = "PCHC")

pchc.mapping1 <- pchc.universe %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping2 <- pchc.universe %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping3 <- bind_rows(pchc.mapping1, pchc.mapping2) %>% 
  distinct(province, city, district, hospital, pchc)

pchc.mapping4 <- pchc.mapping3 %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district))) %>% 
  ungroup()

# molecule
ims_prod_ref <- fread("02_Inputs/cn_prod_ref_201903_1.txt") %>% 
  setDF() %>% 
  mutate(Pack_Id = str_pad(Pack_Id, 7, "left", pad = "0")) %>% 
  select(Pack_Id, NFC123_Code)

ims.mol.raw <- read.xlsx("02_Inputs/ims_chpa_to20Q1.xlsx", startRow = 3, cols = 1:21)

ims.mol1 <- ims.mol.raw[, 1:21] %>% 
  distinct() %>% 
  filter(!is.na(Pack_Id)) %>% 
  left_join(ims_prod_ref, by = "Pack_Id") %>% 
  select(packid = Pack_ID, Corp_ID, Corp_Desc, MNF_TYPE, MnfType_Desc, 
         Mnf_Desc, ATC4_Code, NFC123_Code, Prd_desc, Pck_Desc, 
         Molecule_Desc)

ims_prod_ref <- fread("02_Inputs/cn_prod_ref_201903_1.txt") %>% 
  setDF() %>% 
  mutate(Pack_Id = str_pad(Pack_Id, 7, "left", pad = "0"))

ims_mol_lkp_ref <- fread("02_Inputs/cn_mol_lkp_201903_1.txt") %>%
  setDF() %>%
  arrange(Pack_ID, Molecule_ID) %>%
  mutate(Pack_ID  = str_pad(Pack_ID , 7, "left", pad = "0"))

ims_mol_ref <- fread("02_Inputs/cn_mol_ref_201903_1.txt")

ims_corp_ref <- fread("02_Inputs/cn_corp_ref_201903_1.txt")

ims.mol2 <- ims_mol_lkp_ref %>%
  left_join(ims_mol_ref, by = c("Molecule_ID" = "Molecule_Id")) %>%
  arrange(Pack_ID, Molecule_Desc) %>%
  group_by(Pack_ID) %>%
  summarise(Molecule_Desc = paste(Molecule_Desc, collapse = "+")) %>%
  ungroup() %>%
  left_join(ims_prod_ref, by = c("Pack_ID" = "Pack_Id")) %>%
  left_join(ims_corp_ref, by = "Corp_ID") %>% 
  select(packid = Pack_ID, Corp_ID, Corp_Desc, ATC4_Code, NFC123_Code,
         Prd_desc, Pck_Desc, Molecule_Desc)

ims.mol <- ims.mol2 %>% 
  filter(!(packid %in% ims.mol1$packid)) %>% 
  mutate(Corp_ID = stri_pad_left(Corp_ID, 4, 0)) %>% 
  bind_rows(ims.mol1) %>% 
  filter(!(packid %in% c("4777502", "4777504")))

# target city
target.city <- c("北京", "上海", "杭州", "广州", "南京", "苏州", "宁波", 
                 "福州", "无锡", "温州", "济南", "青岛", "金华", "常州", 
                 "徐州", "台州", "厦门")


##---- Formatting raw data ----
# history
history.az <- read_feather("02_Inputs/data/01_AZ_CHC_Total_Raw.feather") %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)

history.pfizer <- read_feather("02_Inputs/data/01_Pfizer_CHC_Total_Raw.feather") %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)

history.servier <- read_feather("02_Inputs/data/01_Servier_CHC_Total_Raw.feather") %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)

# Guangzhou
raw.gz1 <- read.csv('02_Inputs/data/gzs 20q1.csv') %>% 
  mutate(date = gsub('[-]', '0', period), 
         quarter_m = stri_sub(date, 5, 6)) %>% 
  distinct(year = stri_sub(date, 1, 4), 
           quarter = ifelse(quarter_m %in% c("01", "02", "03"), 
                            stri_paste(year, "Q1"), 
                            ifelse(quarter_m %in% c("04", "05", "06"), 
                                   stri_paste(year, "Q2"), 
                                   ifelse(quarter_m %in% c("07", "08", "09"), 
                                          stri_paste(year, "Q3"), 
                                          ifelse(quarter_m %in% c("10", "11", "12"), 
                                                 stri_paste(year, "Q4"), 
                                                 year)))), 
           date, 
           province = '广东', 
           city = '广州', 
           hospital = name, 
           packid = stri_pad_left(pfc, 7, 0), 
           units = unit, 
           sales = value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'hospital')) %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)

raw.gz2 <- read.xlsx('02_Inputs/data/2jd_date.xlsx') %>% 
  mutate(quarter_m = stri_sub(date, 5, 6)) %>% 
  distinct(year = stri_sub(date, 1, 4), 
           quarter = ifelse(quarter_m %in% c("01", "02", "03"), 
                            stri_paste(year, "Q1"), 
                            ifelse(quarter_m %in% c("04", "05", "06"), 
                                   stri_paste(year, "Q2"), 
                                   ifelse(quarter_m %in% c("07", "08", "09"), 
                                          stri_paste(year, "Q3"), 
                                          ifelse(quarter_m %in% c("10", "11", "12"), 
                                                 stri_paste(year, "Q4"), 
                                                 year)))), 
           date = stri_sub(date, 1, 6), 
           province = '广东', 
           city = '广州', 
           hospital = name, 
           packid = stri_pad_left(pfc, 7, 0), 
           units = unit, 
           sales = value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'hospital')) %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)

# AZ
raw.az.ah <- read.xlsx('02_Inputs/data/AZ_ah_CHC_2020Q1Q2.xlsx')
raw.az.bj <- read.xlsx('02_Inputs/data/AZ_bj_CHC_2020Q1Q2.xlsx')
raw.az.js <- read.xlsx('02_Inputs/data/AZ_js_CHC_2020Q1Q2.xlsx')
raw.az.sd <- read.xlsx('02_Inputs/data/AZ_sd_CHC_2020Q1Q2.xlsx')
raw.az.zj <- read.xlsx('02_Inputs/data/AZ_zj_CHC_2020Q1Q2.xlsx')
raw.az.fj <- read.xlsx('02_Inputs/data/AZ_fj_CHC_2020Q1Q2(predicted by all_raw_data_m3_AZ_6p_171819_CHC_v2).xlsx')

raw.az <- bind_rows(raw.az.ah, raw.az.bj, raw.az.js, 
                    raw.az.sd, raw.az.zj, raw.az.fj) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           # atc4 = ATC4_Code, 
           # nfc = NFC123_Code, 
           # molecule = Molecule_Desc, 
           # product = Prd_desc, 
           # Corp_ID, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  bind_rows(raw.gz1, raw.gz2, history.az) %>% 
  filter(quarter %in% c('2019Q1', '2019Q2', '2020Q1', '2020Q2'), 
         !is.na(pchc), !is.na(packid)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  left_join(ims.mol, by = 'packid') %>% 
  select(year, date, quarter, province, city, district, pchc, atc4 = ATC4_Code, 
         nfc = NFC123_Code, molecule = Molecule_Desc, product = Prd_desc, 
         corp = Corp_Desc, packid, units, sales)

write.xlsx(raw.az, '03_Outputs/01_Raw_AZ.xlsx')

# Servier
raw.servier.ah <- read_xlsx('02_Inputs/data/Servier_ah_CHC_2020Q1Q2.xlsx')
raw.servier.bj <- read_xlsx('02_Inputs/data/Servier_bj_CHC_2020Q1Q2.xlsx')
raw.servier.js <- read_xlsx('02_Inputs/data/Servier_js_CHC_2020Q1Q2.xlsx')
raw.servier.sd <- read_xlsx('02_Inputs/data/Servier_sd_CHC_2020Q1Q2.xlsx')
raw.servier.zj <- read_xlsx('02_Inputs/data/Servier_zj_CHC_2020Q1Q2.xlsx')
raw.servier.fjsd <- read_xlsx('02_Inputs/data/Servier_fjsd_CHC_2020Q1Q2(predicted by all_raw_data_packid_Servier_171819_CHC_m_v4).xlsx')

raw.servier <- bind_rows(raw.servier.ah, raw.servier.bj, raw.servier.js, 
                         raw.servier.sd, raw.servier.zj, 
                         raw.servier.fjsd[raw.servier.fjsd$Province == '福建省', ]) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           # atc4 = ATC4_Code, 
           # nfc = NFC123_Code, 
           # molecule = Molecule_Desc, 
           # product = Prd_desc, 
           # Corp_ID, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  bind_rows(raw.gz1, raw.gz2, history.servier) %>% 
  filter(quarter %in% c('2019Q1', '2019Q2', '2020Q1', '2020Q2'), 
         !is.na(pchc), !is.na(packid)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  left_join(ims.mol, by = 'packid') %>% 
  select(year, date, quarter, province, city, district, pchc, atc4 = ATC4_Code, 
         nfc = NFC123_Code, molecule = Molecule_Desc, product = Prd_desc, 
         corp = Corp_Desc, packid, units, sales)

write.xlsx(raw.servier, '03_Outputs/01_Raw_Servier.xlsx')

# Pfizer
raw.pfizer.ah <- read.xlsx('02_Inputs/data/Pfizer_ah_CHC_2020Q1Q2.xlsx')
raw.pfizer.bj <- read.xlsx('02_Inputs/data/Pfizer_bj_CHC_2020Q1Q2.xlsx')
raw.pfizer.js <- read.xlsx('02_Inputs/data/Pfizer_js_CHC_2020Q1Q2.xlsx')
raw.pfizer.sd <- read.xlsx('02_Inputs/data/Pfizer_sd_CHC_2020Q1Q2.xlsx')
raw.pfizer.zj <- read.xlsx('02_Inputs/data/Pfizer_zj_CHC_2020Q1Q2.xlsx')
raw.pfizer.fj <- read.xlsx('02_Inputs/data/Pfizer_fj_CHC_2020Q1Q2_0827(predicted by all_raw_data_packid_Pfizer_171819_CHC_m_v3).xlsx')

raw.pfizer <- bind_rows(raw.pfizer.ah, raw.pfizer.bj, raw.pfizer.js, 
                        raw.pfizer.sd, raw.pfizer.zj, raw.pfizer.fj) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           # atc4 = ATC4_Code, 
           # nfc = NFC123_Code, 
           # molecule = Molecule_Desc, 
           # product = Prd_desc, 
           # Corp_ID, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  bind_rows(raw.gz1, raw.gz2, history.pfizer) %>% 
  filter(quarter %in% c('2019Q1', '2019Q2', '2020Q1', '2020Q2'), 
         !is.na(pchc), !is.na(packid)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  left_join(ims.mol, by = 'packid') %>% 
  select(year, date, quarter, province, city, district, pchc, atc4 = ATC4_Code, 
         nfc = NFC123_Code, molecule = Molecule_Desc, product = Prd_desc, 
         corp = Corp_Desc, packid, units, sales)

write.xlsx(raw.pfizer, '03_Outputs/01_Raw_Pfizer.xlsx')


##---- TA ----
# market definition
market.mapping <- read.xlsx("02_Inputs/Market_Def_2020_CHC_0909.xlsx", 
                            sheet = "MKT DEF")[, -1] %>% 
  select(`小市场`, `大市场`, `购买方式`, flag_mkt = flag) %>% 
  distinct() %>% 
  group_by(`小市场`) %>% 
  mutate(`购买方式` = paste(unique(`购买方式`), collapse = "+")) %>% 
  ungroup()

market.cndrug <- read.xlsx("02_Inputs/Market_Def_2020_CHC_0909.xlsx", 
                           sheet = "XZK-其他降脂中药")

# GI
gi.1 <- raw.az %>% 
  mutate(
    flag_mkt = case_when(
      molecule %in% c("ESOMEPRAZOLE", "OMEPRAZOLE", "PANTOPRAZOLE", 
                      "LANSOPRAZOLE", "RABEPRAZOLE") ~ 1, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

gi.2 <- raw.az %>% 
  mutate(
    flag_mkt = case_when(
      product %in% c("YI LI AN           LZB") ~ 2, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

gi.3 <- raw.az %>% 
  mutate(
    flag_mkt = case_when(
      stri_sub(atc4, 1, 4) == "A06A" & 
        stri_sub(nfc, 1, 1) %in% c("A", "B", "D") ~ 3, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

# gi.4 <- raw.az %>% 
#   mutate(
#     flag_mkt = case_when(
#       product == "SHU TAI QING       S5J" & 
#         molecule == "MACROGOL(S)+POTASSIUM+SODIUM" & 
#         corp == "STAIDSON BEIJING" & 
#         stri_sub(packid, 1, 5) == "32464" ~ 4, 
#       product == "MOVICOL            NR-" & 
#         molecule == "MACROGOL(S)+POTASSIUM+SODIUM" & 
#         corp == "NORGINE LIM UK" & 
#         stri_sub(packid, 1, 5) == "66157" ~ 4, 
#       product == "FORLAX             IPS" & 
#         molecule == "MACROGOL(S)" & 
#         corp == "IPSEN" & 
#         stri_sub(packid, 1, 5) == "14351" ~ 4, 
#       product == "CHANG SONG         C&Q" & 
#         molecule == "MACROGOL(S)" & 
#         corp == "CQ.PHARSCIN PHARM" & 
#         stri_sub(packid, 1, 5) == "32293" ~ 4, 
#       product == "RUN KE LONG        H3U" & 
#         molecule == "MACROGOL(S)" & 
#         corp == "HN.WARRANT PHARM" & 
#         stri_sub(packid, 1, 5) == "32652" ~ 4, 
#       product == "MACROGOL 4000 POWD HMA" & 
#         molecule == "MACROGOL(S)" & 
#         corp == "HB.MAYINGLONG PH" & 
#         stri_sub(packid, 1, 5) == "41490" ~ 4, 
#       product == "YOU SAI LE         CQ&" & 
#         molecule == "MACROGOL(S)" & 
#         corp == "CQ.SINO BIOPHARM" & 
#         stri_sub(packid, 1, 5) == "57040" ~ 4, 
#       TRUE ~ 0
#     )
#   ) %>% 
#   filter(flag_mkt != 0)

raw.gi <- bind_rows(gi.1, gi.2, gi.3) %>% 
  mutate(TA = 'GI') %>% 
  group_by(year, date, quarter, province, city, district, pchc, TA, atc4, nfc, 
           molecule, product, corp, packid, flag_mkt) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(market.mapping, by = 'flag_mkt')

# RE
re.5 <- raw.az %>% 
  mutate(
    flag_mkt = case_when(
      product %in% c("SYMBICORT TURBUHAL AZN", "SERETIDE           GSK", 
                     "FOSTER             C5I", "RELVAR") ~ 5, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

re.6 <- raw.az %>% 
  mutate(
    flag_mkt = case_when(
      product == "BRICANYL           AZM" & 
        molecule == "TERBUTALINE" & 
        stri_sub(packid, 1, 5) == "14018" & 
        stri_sub(nfc, 1, 2) == "RG" & 
        stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
      product == "SU SHUN            SU9" & 
        molecule == "TERBUTALINE" & 
        stri_sub(packid, 1, 5) == "16352" & 
        stri_sub(nfc, 1, 2) == "FM" & 
        stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
      product == "SU SHUN            SU9" & 
        molecule == "TERBUTALINE" & 
        stri_sub(packid, 1, 5) == "16352" & 
        stri_sub(nfc, 1, 2) == "FQ" & 
        stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
      product == "SALBUTAMOL SULFATE SSZ" & 
        molecule == "SALBUTAMOL" & 
        stri_sub(packid, 1, 5) == "56285" & 
        stri_sub(nfc, 1, 2) == "RG" & 
        stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
      product == "VENTOLIN           GSK" & 
        molecule == "SALBUTAMOL" & 
        stri_sub(packid, 1, 5) == "02003" & 
        stri_sub(nfc, 1, 2) == "RG" & 
        stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
      product == "SALBUTAMOL         JO-" & 
        molecule == "SALBUTAMOL" & 
        stri_sub(packid, 1, 5) == "01734" & 
        stri_sub(nfc, 1, 2) == "FM" & 
        stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
      product == "SALBUTAMOL  SULFAT SHT" & 
        molecule == "SALBUTAMOL" & 
        stri_sub(packid, 1, 5) == "52133" & 
        stri_sub(nfc, 1, 2) == "FQ" & 
        stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
      product == "DA FEN KE CHUANG   SZA" & 
        molecule == "SALBUTAMOL" & 
        stri_sub(packid, 1, 5) == "36434" & 
        stri_sub(nfc, 1, 2) == "RG" & 
        stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
      product == "SALBUTAMOL  SULFAT SFU" & 
        molecule == "SALBUTAMOL" & 
        stri_sub(packid, 1, 5) == "55281" & 
        stri_sub(nfc, 1, 2) == "FQ" & 
        stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
      product == "ATROVENT           B.I" & 
        molecule == "IPRATROPIUM BROMIDE" & 
        stri_sub(packid, 1, 5) == "04354" & 
        stri_sub(nfc, 1, 2) == "RG" & 
        stri_sub(atc4, 1, 4) == "R03K" ~ 6, 
      product == "COMBIVENT          B.I" & 
        molecule == "IPRATROPIUM BROMIDE+SALBUTAMOL" & 
        stri_sub(packid, 1, 5) == "07319" & 
        stri_sub(nfc, 1, 2) == "RG" & 
        stri_sub(atc4, 1, 4) == "R03L" ~ 6, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

re.7 <- raw.az %>% 
  mutate(
    flag_mkt = case_when(
      stri_sub(atc4, 1, 4) == "R05C" & nfc != "ABD" ~ 7, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

re.8 <- raw.az %>% 
  mutate(
    flag_mkt = case_when(
      stri_sub(atc4, 1, 3) == "R03" ~ 8, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

raw.re <- bind_rows(re.5, re.6, re.7, re.8) %>% 
  mutate(TA = 'RE') %>% 
  group_by(year, date, quarter, province, city, district, pchc, TA, atc4, nfc, 
           molecule, product, corp, packid, flag_mkt) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(market.mapping, by = 'flag_mkt')

# CV
cv.9 <- raw.servier %>% 
  mutate(
    flag_mkt = case_when(
      stri_sub(atc4, 1, 4) == "C07A" & 
        stri_sub(nfc, 1, 1) %in% c("A", "B") ~ 9, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

cv.10 <- raw.servier %>% 
  mutate(
    flag_mkt = case_when(
      molecule == "IVABRADINE" ~ 10, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

cv.11 <- raw.servier %>% 
  mutate(
    flag_mkt = case_when(
      stri_sub(atc4, 1, 4) == "C08A" ~ 11, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

cv.12 <- raw.servier %>% 
  mutate(
    flag_mkt = case_when(
      product == "EXFORGE            NVR" ~ 12, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

cv.13 <- raw.pfizer %>% 
  mutate(
    flag_mkt = case_when(
      product == "CADUET             PFZ" ~ 13, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0) %>% 
  bind_rows(raw.servier) %>% 
  mutate(
    flag_mkt = case_when(
      !is.na(flag_mkt) ~ flag_mkt, 
      stri_sub(atc4, 1, 3) %in% c("C03", "C07", "C08", "C09") ~ 13, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

cv.14 <- raw.pfizer %>% 
  mutate(
    flag_mkt = case_when(
      atc4 == "C10A1" ~ 14,
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

cv.15 <- raw.pfizer %>% 
  mutate(
    flag_mkt = case_when(
      product %in% c("CADUET             PFZ", "VYTORIN            MSD", 
                     "EZETROL            SG7") ~ 15, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

cv.16 <- raw.az %>% 
  mutate(
    flag_mkt = case_when(
      atc4 == "B01C2" ~ 16, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

cv.17 <- raw.pfizer %>% 
  mutate(
    flag_mkt = case_when(
      molecule %in% c("ATORVASTATIN", "ROSUVASTATIN", "SIMVASTATIN", 
                      "PITAVASTATIN", "PRAVASTATIN", "FLUVASTATIN", 
                      "EZETIMIBE", "LOVASTATIN", "EZETIMIBE+SIMVASTATIN") ~ 17, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

cv.18 <- raw.pfizer %>% 
  mutate(
    flag_mkt = case_when(
      product == "CADUET             PFZ" ~ 18, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0) %>% 
  bind_rows(raw.az) %>% 
  mutate(
    flag_mkt = case_when(
      !is.na(flag_mkt) ~ flag_mkt, 
      product %in% c("XUE ZHI KANG       BWX", "ZHI BI TAI         DJP", 
                     "JIANG ZHI TONG MAI YYK") ~ 18,
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

cv.19 <- raw.az %>% 
  filter(stri_sub(packid, 1, 5) %in% market.cndrug$PROD_COD) %>% 
  mutate(flag_mkt = 19)

raw.cv <- bind_rows(cv.9, cv.10, cv.11, cv.12, cv.13, cv.14, 
                    cv.15, cv.16, cv.17, cv.18, cv.19) %>% 
  mutate(TA = 'CV') %>% 
  group_by(year, date, quarter, province, city, district, pchc, TA, atc4, nfc, 
           molecule, product, corp, packid, flag_mkt) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(market.mapping, by = 'flag_mkt')

# DM
dm.20 <- raw.servier %>% 
  mutate(
    flag_mkt = case_when(
      stri_sub(atc4, 1, 4) %in% c("A10L", "A10H", "A10M", "A10J", 
                                  "A10K", "A10P", "A10S", "A10N") ~ 20, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

dm.21 <- raw.servier %>% 
  mutate(
    flag_mkt = case_when(
      product %in% c("EUCREAS            NVR", "KOMBIGLYZE XR      AZN", 
                     "TRAJENTA DUO       B.I", "ONGLYZA            AZN", 
                     "JANUVIA            MSD", "GALVUS             NVR", 
                     "TRAJENTA           B.I", "NESINA             TAK", 
                     "JANUMET            MSD") ~ 21, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

dm.22 <- raw.az %>% 
  mutate(
    flag_mkt = case_when(
      product %in% c("INVOKANA           MCK") ~ 22, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0) %>% 
  bind_rows(raw.servier) %>% 
  mutate(
    flag_mkt = case_when(
      !is.na(flag_mkt) ~ flag_mkt, 
      product %in% c("FORXIGA            AZN", "JARDIANCE          B.I") ~ 22, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

raw.dm <- bind_rows(dm.20, dm.21, dm.22) %>% 
  mutate(TA = 'DM') %>% 
  group_by(year, date, quarter, province, city, district, pchc, TA, atc4, nfc, 
           molecule, product, corp, packid, flag_mkt) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(market.mapping, by = 'flag_mkt')

# Renal
renal.23 <- raw.az %>% 
  mutate(
    flag_mkt = case_when(
      !(molecule %in% c("FOLIC ACID", "CYANOCOBALAMIN+FOLIC ACID+NICOTINAMIDE")) & 
        stri_sub(atc4, 1, 3) == "B03" ~ 23, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

renal.24 <- raw.az %>% 
  mutate(
    flag_mkt = case_when(
      molecule == "POLYSTYRENE SULFONATE" ~ 24, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

renal.25 <- raw.az %>% 
  mutate(
    flag_mkt = case_when(
      product == "Lokelma" ~ 25, 
      TRUE ~ 0
    )
  ) %>% 
  filter(flag_mkt != 0)

raw.renal <- bind_rows(renal.23, renal.24, renal.25) %>% 
  mutate(TA = 'Renal') %>% 
  group_by(year, date, quarter, province, city, district, pchc, TA, atc4, nfc, 
           molecule, product, corp, packid, flag_mkt) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(market.mapping, by = 'flag_mkt')

# bind
raw.total <- bind_rows(raw.gi, raw.re, raw.cv, raw.dm, raw.renal) %>% 
  filter(!is.na(pchc), pchc != '#N/A', units > 0, sales > 0) %>% 
  left_join(ims.mol, by = 'packid') %>% 
  group_by(year, date, quarter, province, city, district, pchc, TA, atc4, 
           molecule = Molecule_Desc, product, packid, flag_mkt) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(raw.total, '03_Outputs/01_AZ_CHC_Raw.xlsx')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC 2020Q2
# Purpose:      Summary
# programmer:   Zhe Liu
# Date:         2020-09-02
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin info ----
# pack info
product.info1 <- bind_rows(raw.az.ah, raw.az.bj, raw.az.js, 
                           raw.az.sd, raw.az.zj, raw.az.fj, 
                           raw.servier.ah, raw.servier.bj, raw.servier.js, 
                           raw.servier.sd, raw.servier.zj, raw.servier.fjsd, 
                           raw.pfizer.ah, raw.pfizer.bj, raw.pfizer.js, 
                           raw.pfizer.sd, raw.pfizer.zj, raw.pfizer.fj) %>% 
  distinct(packid = packcode, 
           Molecule_C = Molecule_Desc_ZB, 
           `剂型` = Dosage, 
           `规格` = SPEC, 
           `转换比` = PACK, 
           Pack_DESC = Pck_Desc)

product.info2 <- read.xlsx("02_Inputs/Product standardization master data-A-S-0313.xlsx") %>% 
  distinct(packid = stri_pad_left(PACK_ID, 7, 0), Molecule_C = MOLE_NAME_CH, 
           PROD_DES_C = PROD_NAME_CH, Pack_DESC = PCK_DESC, `转换比` = PACK, 
           `剂型` = DOSAGE, `规格` = SPEC)

product.info3 <- read.xlsx("02_Inputs/packid_prod_20181112.xlsx") %>% 
  distinct(packid = stri_pad_left(pack_id, 7, 0), Molecule_C = gene_name, 
           PROD_DES_C = ims_product_cn, Pack_DESC = Pck_Desc, 
           `转换比` = PckSize_Desc) %>% 
  mutate(`剂型` = str_trim(str_replace(Pack_DESC, "[0-9]{1,}.{1,}", ""), "right"),
         `规格` = sapply(Pack_DESC, function(x) {
           paste(str_extract_all(x, "[0-9]{1,}[a-zA-Z]{1,}", simplify = TRUE), collapse = " ")
         }))

product.info4 <- ims.mol %>% 
  distinct(packid, 
           Pack_DESC = Pck_Desc)

product.info <- bind_rows(product.info1, product.info2, 
                          product.info3, product.info4) %>% 
  filter(!is.na(packid)) %>% 
  group_by(packid) %>% 
  summarise(Molecule_C = first(na.omit(Molecule_C)), 
            PROD_DES_C = first(na.omit(PROD_DES_C)), 
            `剂型` = first(na.omit(`剂型`)), 
            `规格` = first(na.omit(`规格`)), 
            `转换比` = first(na.omit(`转换比`)), 
            Pack_DESC = first(na.omit(Pack_DESC))) %>% 
  ungroup()

# corp info
corp.info <- read_xls("02_Inputs/Corp E & C name.xls") %>% 
  distinct(Corporation, Corporation_C) %>% 
  right_join(ims.mol, by = c('Corporation' = 'Corp_Desc')) %>% 
  distinct(packid, Corp_EName = Corporation, CORP_DES_C = Corporation_C, 
           Corp_TYPE = MNF_TYPE)

# city info
city.en <- read.xlsx("02_Inputs/City_CN_EN.xlsx")

# VBP info
vbp1 <- read.xlsx("02_Inputs/交付相关- VBP匹配.xlsx", sheet = 2) %>% 
  mutate(packid = stri_pad_left(Pack_Id, 7, 0)) %>% 
  distinct(packid, IS_VBP_molecule, IS_VBP_form)

vbp2 <- read.xlsx("02_Inputs/交付相关- VBP匹配.xlsx", sheet = 3) %>% 
  mutate(city = gsub("市", "", `城市`), 
         packid = stri_pad_left(Pack_Id, 7, 0)) %>% 
  distinct(city, packid, VBPDate1 = VBPDate, IS_VBP_pack1 = IS_VBP_pack)

vbp3 <- read.xlsx("02_Inputs/交付相关- VBP匹配.xlsx", sheet = 4) %>% 
  mutate(province = gsub("省", "", `省份`), 
         packid = stri_pad_left(Pack_Id, 7, 0)) %>% 
  select(province, packid, VBPDate2 = VBPDate, IS_VBP_pack2 = IS_VBP_pack)

# EDL info
edl1 <- read.xlsx('02_Inputs/交付相关- EDL汇总.xlsx', sheet = 2) %>% 
  distinct(packid = PACK_COD, EDL_DESC1 = EDL_DESC)

edl2 <- read.xlsx('02_Inputs/交付相关- EDL汇总.xlsx', sheet = 3) %>% 
  distinct(molecule = Molecule.Composition, pack = stri_trim_both(Pack_m1), 
           EDL_DESC2 = EDL_DESC)


##---- Result ----
# 2020Q1 & 2020Q2
az.chc <- proj.price %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) %in% market.cndrug$PROD_COD, 
                          'TCM Others', packid), 
         atc4 = if_else(packid == 'TCM Others', 'TCM Others', atc4), 
         molecule = if_else(packid == 'TCM Others', 'TCM Others', molecule), 
         product = if_else(packid == 'TCM Others', 'TCM Others', product)) %>% 
  group_by(year, quarter, province, city, TA, flag_mkt, atc4, molecule, product, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(market.mapping, by = 'flag_mkt') %>% 
  left_join(city.en, by = c('province' = 'Province_C', 'city' = 'City_C')) %>% 
  left_join(product.info, by = 'packid') %>% 
  left_join(corp.info, by = 'packid') %>% 
  left_join(vbp2, by = c('city', 'packid')) %>% 
  left_join(vbp3, by = c('province', 'packid')) %>% 
  left_join(edl1, by = 'packid') %>% 
  mutate(pack = stri_trim_both(stri_sub(Pack_DESC, 1, -4))) %>% 
  left_join(edl2, by = c('molecule', 'pack')) %>% 
  mutate(`IMS 药品ID` = stri_paste(stri_sub(packid, 1, 5), "-", packid), 
         `IMS 药品ID` = if_else(packid == "TCM Others", "TCM Others", `IMS 药品ID`), 
         VBP_Excu = if_else(is.na(VBPDate1), VBPDate2, VBPDate1), 
         VBP = if_else(is.na(IS_VBP_pack1), IS_VBP_pack2, IS_VBP_pack1), 
         VBP = if_else(is.na(VBP), 'N', VBP), 
         EDL_DESC = if_else(is.na(EDL_DESC1), EDL_DESC2, EDL_DESC1), 
         `Total Unit` = round(units), 
         `Value (RMB)` = round(sales), 
         `Counting Unit` = round(units * `转换比`), 
         `价格` = round(sales / units),
         `单位` = NA_character_) %>% 
  filter(!(`小市场` == 'Non-Oral Expectorant Market' & 
             !(`剂型` %in% c("粉针剂", "雾化溶液", "吸入剂", "注射液")))) %>% 
  select(Market = `小市场`, 
         Year = year, 
         YQ = quarter, 
         Province_C = province, 
         City_C = city, 
         Province_E, 
         City_E, 
         Molecule_C, 
         PROD_DES_C, 
         `剂型`, 
         `规格`, 
         `转换比`, 
         `单位`, 
         Pack_DESC, 
         CORP_DES_C, 
         `购买方式`, 
         `Total Unit`, 
         `Value (RMB)`, 
         `Counting Unit`, 
         `价格`, 
         Pack_ID = packid, 
         `IMS 药品ID`, 
         Mole_Ename = molecule, 
         Prod_Ename = product, 
         Corp_EName, 
         Corp_TYPE, 
         `ATC Code IV` = atc4, 
         TA, 
         VBP_Excu, 
         VBP, 
         EDL_DESC)

write.xlsx(az.chc, '03_Outputs/06_AZ_CHC_2020Q1Q2.xlsx')

# QC
chk <- az.chc %>% 
  filter(is.na(EDL_DESC), packid != 'TCM Others') %>% 
  select(packid, molecule, pack) %>% 
  distinct()

# delivery
az.delivery.history <- read_xlsx('02_Inputs/AZ_CHC_2017Q1_2020Q1_0831.xlsx')

az.delivery <- az.chc %>% 
  filter(YQ == '2020Q2') %>% 
  bind_rows(az.delivery.history) %>% 
  arrange(YQ, City_C, Market, Pack_ID)

write.xlsx(az.delivery, '03_Outputs/06_AZ_CHC_2017Q1_2020Q2.xlsx')



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
           Pack_DESC = Pck_Desc) %>% 
  mutate(first_num_position = stri_locate_first(Pack_DESC, regex = "\\d")[,1],
         last_space_position = stri_locate_last(Pack_DESC, regex = "\\s")[,1],
         `剂型` = str_squish(substr(Pack_DESC, 1, first_num_position - 1)),
         `规格` = str_squish(substr(Pack_DESC, first_num_position, 
                                    last_space_position - 1)),
         `转换比` = as.integer(str_squish(substr(Pack_DESC, last_space_position, 
                                                 nchar(Pack_DESC)))))

product.info <- bind_rows(product.info2, product.info3, product.info4, product.info1) %>% 
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
vbp1 <- read.xlsx("02_Inputs/VBP匹配 for AZ CHC.xlsx", sheet = 2) %>% 
  mutate(city = gsub("市", "", `城市`),
         packid = stri_pad_left(Pack_Id, 7, 0)) %>% 
  distinct(city, packid, VBP_Excu1 = VBP_Excu, VBP1 = VBP)

vbp2 <- read.xlsx("02_Inputs/VBP匹配 for AZ CHC.xlsx", sheet = 3) %>% 
  mutate(province = gsub("省", "", `省份`),
         packid = stri_pad_left(Pack_Id, 7, 0)) %>% 
  distinct(province, packid, VBP_Excu2 = VBP_Excu, VBP2 = VBP)

vbp3 <- read.xlsx("02_Inputs/VBP匹配 for AZ CHC.xlsx", sheet = 4) %>% 
  mutate(packid = stri_pad_left(Pack_Id, 7, 0)) %>% 
  distinct(packid, VBP3 = VBP)

# EDL info
edl1 <- read.xlsx('02_Inputs/交付相关- EDL汇总.xlsx', sheet = 2) %>% 
  distinct(packid = PACK_COD, EDL_DESC1 = EDL_DESC)

edl2 <- read.xlsx('02_Inputs/交付相关- EDL汇总.xlsx', sheet = 3) %>% 
  distinct(molecule = Molecule.Composition, pack = stri_trim_both(Pack_m1), 
           EDL_DESC2 = EDL_DESC) %>% 
  filter(!(molecule == 'TRADITIONAL CHINESE MEDICINE' & pack == 'CAP 300MG')) %>% 
  filter(!(molecule == 'EPOETIN(UNSPECIFIED)' & pack == 'AMP 3000IU 1ML'))

# factor info
# factor.info <- read.xlsx('02_Inputs/Factor.xlsx') %>% 
#   pivot_longer(cols = ends_with('市'), 
#                names_to = 'city', 
#                names_prefix = '?市', 
#                values_to = 'factor')


##---- Result ----
az.delivery.history <- read_xlsx('02_Inputs/AZ_CHC_2017Q1_2020Q1_0831.xlsx')

# CV Market
az.chc.cv <- proj.price %>% 
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
  left_join(product.info, by = 'packid') %>% 
  filter(`小市场` %in% c('Crestor Market', 'Brilinta Market', 'HTN Market')) %>% 
  mutate(`小市场` = 'CV Market', 
         `购买方式` = 'ATC+商品名+类别')

# PPI IV Market & PPI Oral Market
az.chc.ppi <- proj.price %>% 
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
  left_join(product.info, by = 'packid') %>% 
  filter(`小市场` %in% c('PPI (Oral/IV) Market')) %>% 
  mutate(`小市场` = if_else(`剂型` %in% c('粉针剂'), 
                         'PPI IV Market', 'PPI Oral Market'))

# XZK Market(Excl Potent Statin)
az.chc.xzk <- proj.price %>% 
  left_join(market.mapping, by = 'flag_mkt') %>% 
  left_join(product.info, by = 'packid') %>% 
  filter(`小市场` %in% c('XZK Market')) %>% 
  filter(!(molecule %in% c('ATORVASTATIN+AMLODIPINE', 'ATORVASTATIN', 'ROSUVASTATIN'))) %>% 
  mutate(`小市场` = 'XZK Market(Excl Potent Statin)') %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) %in% market.cndrug$PROD_COD, 
                          'TCM Others', packid), 
         atc4 = if_else(packid == 'TCM Others', 'TCM Others', atc4), 
         molecule = if_else(packid == 'TCM Others', 'TCM Others', molecule), 
         product = if_else(packid == 'TCM Others', 'TCM Others', product)) %>% 
  group_by(year, quarter, province, city, TA, flag_mkt, atc4, molecule, product, 
           packid, `小市场`, `大市场`, `购买方式`, Molecule_C, PROD_DES_C, 
           `剂型`, `规格`, `转换比`, Pack_DESC) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# IOAD Market
az.chc.ioad <- proj.price %>% 
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
  left_join(product.info, by = 'packid') %>% 
  filter(`小市场` %in% c('Onglyza Market', 'Forxiga(SGLT2) Market')) %>% 
  mutate(`小市场` = 'IOAD Market', 
         `购买方式` = '商品名')

# total market
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
  left_join(product.info, by = 'packid') %>% 
  bind_rows(az.chc.cv, az.chc.ppi, az.chc.xzk, az.chc.ioad) %>% 
  left_join(city.en, by = c('province' = 'Province_C', 'city' = 'City_C')) %>% 
  left_join(corp.info, by = 'packid') %>% 
  left_join(vbp1, by = c("city", "packid")) %>% 
  left_join(vbp2, by = c("province", "packid")) %>% 
  left_join(vbp3, by = c("packid")) %>% 
  left_join(edl1, by = 'packid') %>% 
  mutate(pack = stri_trim_both(stri_sub(Pack_DESC, 1, -4)), 
         Pack_DESC = stri_paste(stri_trim_right(gsub('.{3}$', '', product)), 
                                ' ', Pack_DESC), 
         Pack_DESC = stri_paste(stri_trim_right(gsub('.{3}$', '', Pack_DESC)), 
                                ' ', stri_trim_both(stri_sub(Pack_DESC, -3, -1)))) %>% 
  left_join(edl2, by = c('molecule', 'pack')) %>% 
  mutate(Molecule_C = if_else(packid == 'TCM Others', 'TCM Others', Molecule_C), 
         PROD_DES_C = if_else(packid == 'TCM Others', 'TCM Others', PROD_DES_C), 
         Pack_DESC = if_else(packid == 'TCM Others', 'TCM Others', Pack_DESC), 
         CORP_DES_C = if_else(packid == 'TCM Others', 'TCM Others', CORP_DES_C), 
         Corp_EName = if_else(packid == 'TCM Others', 'TCM Others', Corp_EName), 
         Corp_TYPE = case_when(
           Corp_EName == 'JS.DISAINUO PHARM' ~ 'L', 
           Corp_EName == 'SIHUAN GROUP' ~ 'L', 
           Corp_EName == 'ZAMBON GROUP' ~ 'I', 
           Corp_EName == 'KAIFENG PHARM FTY' ~ 'L', 
           TRUE ~ Corp_TYPE
         ), 
         `IMS 药品ID` = stri_paste(stri_sub(packid, 1, 5), "-", packid), 
         `IMS 药品ID` = if_else(packid == "TCM Others", "TCM Others", `IMS 药品ID`), 
         VBP_Excu = ifelse(is.na(VBP_Excu1), VBP_Excu2, VBP_Excu1), 
         VBP = ifelse(is.na(VBP1), VBP2, VBP1), 
         VBP = ifelse(is.na(VBP), VBP3, VBP), 
         EDL_DESC = if_else(is.na(EDL_DESC1), EDL_DESC2, EDL_DESC1), 
         `Total Unit` = units, 
         `Value (RMB)` = sales, 
         `Counting Unit` = units * `转换比`, 
         `价格` = sales / units,
         `单位` = NA_character_) %>% 
  filter(!(`小市场` == 'Non-Oral Expectorant Market' & 
             !(`剂型` %in% c("粉针剂", "冻干粉针剂", "雾化溶液", "吸入剂", 
                           "吸入溶液剂", "注射液", "小容量注射液", "大容量注射液")))) %>% 
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

# write.xlsx(az.chc, '03_Outputs/06_AZ_CHC_2020Q1Q2.xlsx')

# QC
# chk <- az.chc %>% 
#   filter(is.na(EDL_DESC), packid != 'TCM Others') %>% 
#   select(packid, molecule, pack) %>% 
#   distinct()

# delivery
onglyza.history1 <- az.delivery.history %>% 
  filter(City_C == '广州', grepl('ONGLYZA', Prod_Ename)) %>% 
  mutate(Market = 'Onglyza Market', 
         `购买方式` = '商品名')

onglyza.history2 <- az.delivery.history %>% 
  filter(City_C == '广州', grepl('ONGLYZA', Prod_Ename)) %>% 
  mutate(Market = 'IOAD Market', 
         `购买方式` = '商品名')

az.delivery <- bind_rows(az.chc, az.chc.sh) %>% 
  filter(YQ == '2020Q2') %>% 
  bind_rows(az.delivery.history, onglyza.history1, onglyza.history2) %>% 
  group_by(Pack_ID) %>% 
  mutate(Pack_DESC = first(na.omit(Pack_DESC))) %>% 
  ungroup() %>% 
  mutate(`剂型` = if_else(Pack_ID == 'TCM Others', NA_character_, `剂型`), 
         `规格` = if_else(Pack_ID == 'TCM Others', NA_character_, `规格`), 
         `转换比` = ifelse(Pack_ID == 'TCM Others', NA, `转换比`), 
         `ATC Code IV` = if_else(Pack_ID == 'TCM Others', NA_character_, `ATC Code IV`), 
         Molecule_C = if_else(Pack_ID == '3955510', '氯沙坦钾', Molecule_C), 
         Molecule_C = if_else(Pack_ID == '6460306', '替米沙坦', Molecule_C), 
         Molecule_C = if_else(Pack_ID == '7097402', '氨氯地平', Molecule_C), 
         Molecule_C = if_else(Pack_ID == '6900404', '兰索拉唑', Molecule_C), 
         CORP_DES_C = if_else(Pack_ID == '4486402', '天年药业(哈尔滨)有限公司', CORP_DES_C), 
         PROD_DES_C = case_when(
           Pack_ID == '3955510' ~ '倍怡', 
           Pack_ID == '6081314' ~ '瑞舒伐他汀钙片', 
           Pack_ID == '6460306' ~ '尚尔宁', 
           Pack_ID == '6780104' ~ '二甲双胍格列吡嗪胶囊', 
           Pack_ID == '6875606' ~ '瑞舒伐他汀钙片', 
           Pack_ID == '6875608' ~ '瑞舒伐他汀钙片', 
           Pack_ID == '6883304' ~ '硝苯地平片', 
           Pack_ID == '6900404' ~ '兰索拉唑肠溶胶囊', 
           Pack_ID == '7064906' ~ '美达新', 
           Pack_ID == '7064908' ~ '美达新', 
           Pack_ID == '7065202' ~ '匹伐他汀钙片', 
           Pack_ID == '7076406' ~ '沙格列汀片', 
           Pack_ID == '7092902' ~ '格瑞舒', 
           Pack_ID == '7097402' ~ '苯磺酸氨氯地平片', 
           Pack_ID == '7100902' ~ '多索茶碱葡萄糖注射液', 
           Pack_ID == '7107402' ~ '卡托普利片', 
           Pack_ID == '7125702' ~ '替格瑞洛片', 
           Pack_ID == '7154602' ~ '卡托普利片', 
           Pack_ID == '7155702' ~ '阿替洛尔片', 
           Pack_ID == '7160802' ~ '孚来乐', 
           Pack_ID == '7160902' ~ '万瑞平', 
           TRUE ~ PROD_DES_C
         ), 
         Molecule_C = case_when(
           Pack_ID == '3955510' ~ '氯沙坦钾', 
           Pack_ID == '6460306' ~ '替米沙坦', 
           Pack_ID == '7097402' ~ '氨氯地平', 
           Pack_ID == '6900404' ~ '兰索拉唑', 
           TRUE ~ Molecule_C
         ), 
         # Molecule_C = if_else(is.na(Molecule_C), Mole_Ename, Molecule_C), 
         # PROD_DES_C = if_else(is.na(PROD_DES_C), Prod_Ename, PROD_DES_C), 
         # CORP_DES_C = if_else(is.na(CORP_DES_C), Corp_EName, CORP_DES_C), 
         VBP = if_else(Pack_ID == 'TCM Others', 'N', VBP)) %>% 
  group_by(Market, Year, YQ, Province_C, City_C, Province_E, City_E, Molecule_C, 
           PROD_DES_C, `剂型`, `规格`, `转换比`, `单位`, Pack_DESC, CORP_DES_C, 
           `购买方式`, Pack_ID, `IMS 药品ID`, Mole_Ename, Prod_Ename, Corp_EName, 
           Corp_TYPE, `ATC Code IV`, TA, VBP_Excu, VBP, EDL_DESC) %>% 
  summarise(`Total Unit` = sum(`Total Unit`, na.rm = TRUE), 
            `Value (RMB)` = sum(`Value (RMB)`, na.rm = TRUE), 
            `Counting Unit` = sum(`Counting Unit`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(`价格` = `Value (RMB)` / `Total Unit`, 
         `价格` = ifelse(Pack_ID == 'TCM Others', NA, `价格`), 
         `Counting Unit` = ifelse(Pack_ID == 'TCM Others', NA, `Counting Unit`)) %>% 
  filter(`Total Unit` > 0, `Value (RMB)` > 0) %>% 
  select(Market, Year, YQ, Province_C, City_C, Province_E, City_E, Molecule_C, 
         PROD_DES_C, `剂型`, `规格`, `转换比`, `单位`, Pack_DESC, CORP_DES_C, 
         `购买方式`, `Total Unit`, `Value (RMB)`, `Counting Unit`, `价格`, 
         Pack_ID, `IMS 药品ID`, Mole_Ename, Prod_Ename, Corp_EName, Corp_TYPE, 
         `ATC Code IV`, TA, VBP_Excu, VBP, EDL_DESC) %>% 
  arrange(YQ, City_C, Market, Pack_ID)

write.xlsx(az.delivery, '03_Outputs/06_AZ_CHC_2017Q1_2020Q2.xlsx')


##---- Adjustment ----
# 1
az.delivery1 <- read_xlsx('05_Internal_Review/AZ_CHC_2017Q1_2020Q2_v9_hx_m.xlsx')

az.delivery2 <- az.delivery1 %>% 
  arrange(Pack_ID, YQ) %>% 
  group_by(Pack_ID) %>% 
  mutate(Molecule_C = first(na.omit(Molecule_C))) %>% 
  ungroup()

write.xlsx(az.delivery2, '05_Internal_Review/AZ_CHC_2017Q1_2020Q2_v10_hx_m.xlsx')

# 2
onglyza1 <- az.delivery2 %>% 
  filter(Market == 'NIAD Market', grepl('ONGLYZA', Prod_Ename)) %>% 
  mutate(Market = 'Onglyza Market')

onglyza2 <- az.delivery2 %>% 
  filter(Market == 'NIAD Market', grepl('ONGLYZA', Prod_Ename)) %>% 
  mutate(Market = 'IOAD Market')

az.delivery3 <- az.delivery2 %>% 
  filter(!(Market %in% c('Onglyza Market', 'IOAD Market') & grepl('ONGLYZA', Prod_Ename))) %>% 
  bind_rows(onglyza1, onglyza2)

write.xlsx(az.delivery3, '05_Internal_Review/AZ_CHC_2017Q1_2020Q2_v11_hx_m.xlsx')

# 3
az.delivery5 <- read_excel('06_Deliveries/AZ_CHC_2017Q1_2020Q2_20201020.xlsx', sheet = 2)

az.delivery6 <- az.delivery5 %>% 
  mutate(`购买方式` = if_else(Market == 'CV Market', 'ATC+商品名+类别', `购买方式`), 
         `购买方式` = if_else(Market == 'IOAD Market', '商品名', `购买方式`), 
         `购买方式` = if_else(Market %in% c('PPI IV Market', 'PPI Oral Market', 'PPI (Oral/IV) Market'), 
                          '通用名+商品名', `购买方式`), 
         `购买方式` = if_else(Market == 'NIAD Market', '类别', `购买方式`), 
         `购买方式` = if_else(Market == 'Onglyza Market', '商品名', `购买方式`), 
         Corp_TYPE = if_else(Corp_EName == 'ZAMBON GROUP', 'I', Corp_TYPE), 
         `IMS 药品ID` = stri_paste(stri_sub(Pack_ID, 1, 5), "-", Pack_ID), 
         # `转换比` = if_else(Pack_ID == '1624102', 14, `转换比`), 
         # `转换比` = if_else(Pack_ID == '0200308', 100, `转换比`), 
         # `转换比` = if_else(Pack_ID == '3770808', 14, `转换比`), 
         # `转换比` = if_else(Pack_ID == '4451702', 10, `转换比`), 
         # `转换比` = if_else(Pack_ID == '1869704', 30, `转换比`), 
         # `转换比` = if_else(Pack_ID == '6572004', 45, `转换比`), 
         # `转换比` = if_else(Pack_ID == '4083806', 14, `转换比`), 
         # `转换比` = if_else(Pack_ID == '0236008', 10, `转换比`), 
         # `转换比` = if_else(Pack_ID == '3673204', 14, `转换比`), 
         # `转换比` = if_else(Pack_ID == '1076106', 30, `转换比`), 
         # `转换比` = if_else(Pack_ID == '0217904', 24, `转换比`), 
         # `转换比` = if_else(Pack_ID == '6048804', 1, `转换比`), 
         # `转换比` = if_else(Pack_ID == '3768308', 30, `转换比`), 
         prodid = stri_sub(Pack_ID, 1, 5)) %>% 
  arrange(desc(YQ), Mole_Ename, Prod_Ename, Corp_EName, Pack_DESC, Molecule_C, 
          PROD_DES_C, CORP_DES_C, `剂型`, `规格`, `转换比`) %>% 
  group_by(prodid) %>% 
  mutate(Mole_Ename = last(Mole_Ename), 
         Prod_Ename = last(Prod_Ename), 
         Corp_EName = last(Corp_EName), 
         Molecule_C = last(Molecule_C), 
         PROD_DES_C = last(PROD_DES_C), 
         CORP_DES_C = last(CORP_DES_C)) %>% 
  ungroup() %>% 
  group_by(Pack_ID) %>% 
  mutate(Pack_DESC = last(Pack_DESC), 
         `剂型` = first(`剂型`), 
         `规格` = first(na.omit(`规格`)), 
         `转换比` = first(`转换比`)) %>% 
  ungroup() %>% 
  mutate(first_num_position = stri_locate_first(Pack_DESC, regex = "\\d")[,1],
         last_space_position = stri_locate_last(Pack_DESC, regex = "\\s")[,1],
         `转换比` = as.numeric(str_squish(substr(Pack_DESC, last_space_position, 
                                              nchar(Pack_DESC)))), 
         `转换比` = if_else(is.na(`转换比`) & PROD_DES_C != 'TCM Others', 1, `转换比`)) %>% 
  # mutate(`转换比` = case_when(
  #   `剂型` == '粉雾剂' & PROD_DES_C == '硫酸沙丁胺醇粉雾剂' ~ 1, 
  #   `剂型` == '气雾剂' ~ 1, 
  #   `剂型` == '吸入粉雾剂' & `转换比` >= 60 ~ 1, 
  #   TRUE ~ `转换比`
  # )) %>% 
  group_by(Market, Year, YQ, Province_C, City_C, Province_E, City_E, Molecule_C, 
           PROD_DES_C, `剂型`, `规格`, `转换比`, `单位`, Pack_DESC, CORP_DES_C, 
           `购买方式`, Pack_ID, `IMS 药品ID`, Mole_Ename, Prod_Ename, Corp_EName, 
           Corp_TYPE, `ATC Code IV`, TA, VBP_Excu, VBP, EDL_DESC) %>% 
  summarise(`Total Unit` = sum(`Total Unit`, na.rm = TRUE), 
            `Value (RMB)` = sum(`Value (RMB)`, na.rm = TRUE), 
            `Counting Unit` = sum(`Counting Unit`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(`Counting Unit` = `Total Unit` * `转换比`, 
         `价格` = `Value (RMB)` / `Total Unit`) %>% 
  select(Market, Year, YQ, Province_C, City_C, Province_E, City_E, Molecule_C, 
         PROD_DES_C, `剂型`, `规格`, `转换比`, `单位`, Pack_DESC, CORP_DES_C, 
         `购买方式`, `Total Unit`, `Value (RMB)`, `Counting Unit`, `价格`, 
         Pack_ID, `IMS 药品ID`, Mole_Ename, Prod_Ename, Corp_EName, Corp_TYPE, 
         `ATC Code IV`, TA, VBP_Excu, VBP, EDL_DESC)

chk <- az.delivery6 %>% 
  distinct(Pack_ID, Market, Molecule_C, PROD_DES_C, `剂型`, `规格`, `转换比`, 
           Pack_DESC, CORP_DES_C, `购买方式`, `IMS 药品ID`, Mole_Ename, 
           Prod_Ename, Corp_EName, Corp_TYPE, `ATC Code IV`, TA) %>% 
  add_count(Market, Pack_ID) %>% 
  filter(n > 1)

chk <- az.delivery6 %>% 
  distinct(prodid = stri_sub(Pack_ID, 1, 5), Market, Molecule_C, PROD_DES_C, 
           CORP_DES_C, `购买方式`, Mole_Ename, 
           Prod_Ename, Corp_EName, Corp_TYPE, `ATC Code IV`, TA) %>% 
  add_count(Market, prodid) %>% 
  filter(n > 1)

chk <- az.delivery6 %>% 
  filter(转换比 > 30) %>% 
  distinct(PROD_DES_C, 剂型, 规格, 转换比, Pack_ID)
table(chk$`剂型`)

az.diff <- az.delivery5 %>% 
  group_by(Market, YQ, Province_E, City_E, Pack_ID) %>% 
  summarise(units_pre = sum(`Total Unit`, na.rm = TRUE), 
            sales_pre = sum(`Value (RMB)`, na.rm = TRUE), 
            dosageunits_pre = sum(`Counting Unit`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  full_join(az.delivery6, by = c('Market', 'YQ', 'Province_E', 'City_E', 'Pack_ID')) %>% 
  group_by(Market, Year, YQ, Province_C, City_C) %>% 
  summarise(`Total Unit` = sum(`Total Unit`, na.rm = TRUE), 
            `Value (RMB)` = sum(`Value (RMB)`, na.rm = TRUE), 
            `Counting Unit` = sum(`Counting Unit`, na.rm = TRUE), 
            units_pre = sum(units_pre, na.rm = TRUE), 
            sales_pre = sum(sales_pre, na.rm = TRUE), 
            dosageunits_pre = sum(dosageunits_pre, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(units_diff = round(`Total Unit` - units_pre), 
         sales_diff = round(`Value (RMB)` - sales_pre), 
         dosageunits_diff = round(`Counting Unit` - dosageunits_pre))

write.xlsx(az.diff, '05_Internal_Review/AZ_Comparison.xlsx')
write.xlsx(az.delivery6, '03_Outputs/AZ_CHC_2017Q1_2020Q2_uniform.xlsx')

# 4
az.delivery7 <- read_excel('06_Deliveries/AZ_CHC_2017Q1_2020Q2_20201023.xlsx')

delivery.info <- az.delivery7 %>% 
  distinct(Market, Molecule_C, PROD_DES_C, `剂型`, `规格`, `转换比`, `单位`, 
           Pack_DESC, CORP_DES_C, `购买方式`, Pack_ID, `IMS 药品ID`, 
           Mole_Ename, Prod_Ename, Corp_EName, Corp_TYPE, `ATC Code IV`, TA, 
           VBP_Excu, VBP, EDL_DESC)

januvia.price <- az.delivery7 %>% 
  filter(Year %in% c('2018', '2019'), Province_C == '江苏', 
         Pack_ID %in% c('4268602', '4268604')) %>% 
  group_by(Year, City_C, Pack_ID) %>% 
  summarise(`价格` = sum(`Value (RMB)`, na.rm = TRUE) / sum(`Total Unit`, na.rm = TRUE)) %>% 
  ungroup()

servier.js.raw1 <- read.xlsx('02_Inputs/raw/【CHC项目】-江苏2019年Q1交付表-20190617.xlsx')
# servier.js.raw2 <- read.xlsx('02_Inputs/raw/CHC江苏2018Q12年-181129.xlsx')

# servier.js.raw2 <- read.xlsx('02_Inputs/raw/【CHC项目】-江苏2019年Q2交付表-2019008.xlsx')
# servier.js.raw3 <- read.xlsx('02_Inputs/raw/【CHC项目】2019年Q3江苏交付表-20191126.xlsx')
# servier.js.raw4 <- read.xlsx('02_Inputs/raw/【CHC项目新】19年Q4北京江苏安徽交付表-20200311.xlsx')
# servier.zj.raw1 <- read.xlsx('02_Inputs/raw/【CHC项目】2019Q1Q2浙江交付表-20191116.xlsx')
# servier.zj.raw2 <- read.xlsx('02_Inputs/raw/[CHC项目]-2019年Q3Q4浙江数据交付表.xlsx')

# js.raw1 <- read.xlsx('02_Inputs/raw/【AZ项目】-江苏2019年Q1交付表-20190617.xlsx')
# js.raw2 <- read.xlsx('02_Inputs/raw/【AZ项目】-江苏2019年Q2交付表-201908.xlsx')
# js.raw3 <- read.xlsx('02_Inputs/raw/【AZ项目】2019年Q3江苏交付表-20191126.xlsx')
# js.raw4 <- read.xlsx('02_Inputs/raw/【AZ项目新】19年Q4北京江苏安徽交付表-20200311.xlsx')
# zj.raw1 <- read.xlsx('02_Inputs/raw/【AZ项目】2019Q1Q2浙江交付表-20191116.xlsx')
# zj.raw2 <- read.xlsx('02_Inputs/raw/[AZ项目]-2019年Q3Q4浙江数据交付表.xlsx')

az.delivery8 <- servier.js.raw1 %>% 
  filter(`省份` == '江苏省', `城市` %in% c('常州市', '南京市', '苏州市', '无锡市'), 
         `商品名` == '捷诺维', 
         grepl('社区卫生服务中心|社区服务中心', `医院名称`)) %>% 
  group_by(year = as.character(Year), 
           quarter = `季度`, 
           province = gsub('省', '', `省份`), 
           city = gsub('市', '', `城市`)) %>% 
  summarise(units = sum(`数量`, na.rm = TRUE), 
            sales = sum(`金额`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(proj_rate = case_when(
           city == '常州' ~ 207815 / 107085, 
           city == '南京' ~ 3781719 / 2360480, 
           city == '苏州' ~ 3140528 / 2680147, 
           city == '无锡' ~ 1677183 / 777776, 
           TRUE ~ NaN
         ), 
         div_rate = case_when(
           city == '常州' ~ 100730 / 207815, 
           city == '南京' ~ 1423940 / 3781719, 
           city == '苏州' ~ 460382 / 3140528, 
           city == '无锡' ~ 899413 / 1677183, 
           TRUE ~ NaN
         )) %>% 
  mutate(`4268602` = sales * proj_rate * (1 - div_rate), 
         `4268604` = sales * proj_rate * div_rate) %>% 
  select(Year = year, YQ = quarter, Province_C = province, City_C = city, 
         `4268602`, `4268604`) %>% 
  pivot_longer(cols = c(`4268602`, `4268604`), 
               names_to = 'Pack_ID', 
               values_to = 'Value (RMB)') %>% 
  left_join(city.en, by = c('Province_C', 'City_C')) %>% 
  left_join(januvia.price, by = c('Year', 'City_C', 'Pack_ID')) %>% 
  left_join(delivery.info, by = c('Pack_ID')) %>% 
  mutate(`Total Unit` = `Value (RMB)` / `价格`, 
         `Counting Unit` = `Total Unit` * `转换比`) %>% 
  bind_rows(az.delivery7) %>% 
  select(Market, Year, YQ, Province_C, City_C, Province_E, City_E, Molecule_C, 
         PROD_DES_C, `剂型`, `规格`, `转换比`, `单位`, Pack_DESC, CORP_DES_C, 
         `购买方式`, `Total Unit`, `Value (RMB)`, `Counting Unit`, `价格`, 
         Pack_ID, `IMS 药品ID`, Mole_Ename, Prod_Ename, Corp_EName, Corp_TYPE, 
         `ATC Code IV`, TA, VBP_Excu, VBP, EDL_DESC) %>% 
  arrange(YQ, City_C, Market, Pack_ID)

write.xlsx(az.delivery8, '03_Outputs/AZ_CHC_2017Q1_2020Q2_20201026.xlsx')

# 5
az.delivery9 <- read_excel('06_Deliveries/AZ_CHC_2017Q1_2020Q2_20201027.xlsx', sheet = 3)

az.delivery10 <- az.delivery9 %>% 
  filter(Province_C == '江苏', City_C %in% c('常州', '南京', '苏州', '无锡'), 
         YQ == '2019Q1', PROD_DES_C == '捷诺维') %>% 
  mutate(growth = if_else(Pack_ID == '4268602', 1.6125, 
                          if_else(Pack_ID == '4268604', 1.8494, 
                                  NaN)), 
         `Total Unit` = `Total Unit` / growth, 
         `Value (RMB)` = `Value (RMB)` / growth, 
         `Counting Unit` = `Counting Unit` / growth, 
         Year = '2018', 
         YQ = '2018Q1') %>% 
  select(-growth) %>% 
  bind_rows(az.delivery9) %>% 
  arrange(YQ, City_C, Market, Pack_ID)

write.xlsx(az.delivery10, '03_Outputs/AZ_CHC_2017Q1_2020Q2_20201027.xlsx')









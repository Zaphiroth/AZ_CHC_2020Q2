# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC 2020Q2
# Purpose:      Review
# programmer:   Zhe Liu
# Date:         2020-09-09
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Internal data 2020 ----
az.internal.2020 <- read.xlsx('05_Internal_Review/AZ BJ CHC 2020.xlsx') %>% 
  filter(grepl('社区卫生服务中心', `机构名称`), PCHC != 0) %>% 
  mutate(`月份` = as.character(`月份`), 
         `销售价格` = `销售金额` / `销售数量`, 
         packid = case_when(
           `品规` == 'Symbicort Tur. 80d' ~ '1906606', 
           `品规` == 'Symbicort Tur. 160d*60' ~ '1906604', 
           `品规` == 'Symbicort Tur. 320d' ~ '1906608', 
           `品规` == 'Onglyza 7*5mg' ~ '5890602', 
           TRUE ~ NA_character_
         )) %>% 
  filter(!is.na(packid))

raw.bj <- raw.total %>% 
  filter(city == '北京', 
         year == '2020', 
         flag_mkt %in% c(5, 21), 
         product %in% c('ONGLYZA            AZN', 'SYMBICORT TURBUHAL AZN')) %>% 
  full_join(az.internal.2020, by = c('pchc' = 'PCHC', 'date' = '月份', 'packid')) %>% 
  group_by(pchc, packid) %>% 
  mutate(flag = if_else(!all(is.na(units)) & !all(is.na(`销售数量`)), 1, 0)) %>% 
  ungroup() %>% 
  filter(flag == 1) %>% 
  mutate(price = sales / units, 
         sales_diff = sales - `销售金额`, 
         units_diff = units - `销售数量`, 
         price_diff = price - `销售价格`) %>% 
  arrange(units_diff)

write.xlsx(raw.bj, '05_Internal_Review/TEMP.xlsx')

raw.sym <- raw.bj %>% 
  filter(packid %in% c('1906604', '1906606', '1906608'))

raw.ong <- raw.bj %>% 
  filter(packid %in% c('5890602'))


chk <- az.delivery %>% 
  filter(City_C == '北京', YQ %in% c('2020Q1', '2020Q2'), 
         Market == 'Symbicort Market', Prod_Ename == 'SYMBICORT TURBUHAL AZN')

read.xlsx('05_Internal_Review/AZ BJ CHC 2020.xlsx') %>% 
  filter(`机构子类型` == '社区卫生服务中心') %>% 
  mutate(`月份` = as.character(`月份`), 
         `销售价格` = `销售金额` / `销售数量`, 
         packid = case_when(
           `品规` == 'Symbicort Tur. 80d' ~ '1906606', 
           `品规` == 'Symbicort Tur. 160d*60' ~ '1906604', 
           `品规` == 'Symbicort Tur. 320d' ~ '1906608', 
           `品规` == 'Onglyza 7*5mg' ~ '5890602', 
           TRUE ~ NA_character_
         )) %>% 
  filter(!is.na(packid)) %>% 
  # filter(packid %in% c('1906604', '1906606', '1906608')) %>%
  filter(packid %in% c('5890602')) %>%
  filter(`月份` %in% c(202001, 202002, 202003)) %>% 
  group_by(packid) %>% 
  summarise(units = sum(`销售数量`), 
            sales = sum(`销售金额`)) %>% 
  ungroup()

# Symbicort 2020Q1 internal BJ units = 16506-1478+6358+344 = 21730

# Symbicort 2020Q1 cmax BJ units = 19150+303+254 = 19707

chk <- raw.bj %>% 
  filter(!is.na(year), !is.na(`销售数量`))


##---- Internal data 2019 ----
# raw
bj.sym <- raw.az.bj %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  bind_rows(history.az) %>% 
  filter(year %in% c('2019', '2020'), 
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
         corp = Corp_Desc, packid, units, sales) %>% 
  filter(city == '北京', product == 'SYMBICORT TURBUHAL AZN') %>% 
  group_by(quarter, pchc, product) %>% 
  summarise(units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(product = 'Symbicort Tur.')

bj.ong <- raw.servier.bj %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  bind_rows(history.servier) %>% 
  filter(year %in% c('2019', '2020'), 
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
         corp = Corp_Desc, packid, units, sales) %>% 
  filter(city == '北京', product == 'ONGLYZA            AZN') %>% 
  group_by(quarter, pchc, product) %>% 
  summarise(units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(product = 'Onglyza')

# internal
pchc.mapping.bj1 <- pchc.mapping3 %>% 
  filter(city == '北京') %>% 
  group_by(hospital) %>% 
  summarise(pchc = first(na.omit(pchc))) %>% 
  ungroup()

pchc.mapping.bj2 <- read.xlsx('05_Internal_Review/CHC Universe  0409.xlsx', 
                              sheet = 2)[, 2:7] %>% 
  filter(`地级市` == '北京市') %>% 
  distinct(hospital = PCHC_Name, pchc = PCHC_Code)

pchc.mapping.bj <- bind_rows(pchc.mapping.bj1, pchc.mapping.bj2) %>% 
  group_by(hospital) %>% 
  summarise(pchc = first(na.omit(pchc))) %>% 
  ungroup()

az.internal.2019 <- read_xls('05_Internal_Review/onglyza&Symbicort By Qtr.xls') %>% 
  filter(!is.na(BrandNameC), grepl('社区卫生服务中心', `机构名称`), !grepl('服务站', `机构名称`)) %>% 
  left_join(pchc.mapping.bj, by = c('机构名称' = 'hospital')) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(bind_rows(bj.sym, bj.ong), by = c('BrandNameC' = 'product', 'YQtr' = 'quarter', 'pchc')) %>% 
  group_by(BrandNameC, pchc) %>% 
  filter(!all(is.na(units))) %>% 
  ungroup() %>% 
  mutate(units = if_else(is.na(units), 0, units), 
         units_diff = `销售数量` - units)

write.xlsx(az.internal.2019, '05_Internal_Review/BJ_ONGLYZA&SYMBICORT_Match.xlsx')


##---- All internal check ----
pchc.mapping.city <- pchc.mapping3 %>% 
  group_by(province, city, hospital) %>% 
  summarise(district = first(na.omit(district)), 
            pchc = first(na.omit(pchc))) %>% 
  ungroup()

internal.raw <- read.csv('05_Internal_Review/All_Internal_Data/CHC check data.csv', encoding = 'UTF-8')
internal.mapping <- read.xlsx('05_Internal_Review/All_Internal_Data/internal_packid_mapping.xlsx')

internal.data <- internal.raw %>% 
  mutate(city = gsub('市', '', X.U.FEFF.CityNameC)) %>% 
  left_join(pchc.mapping.city, by = c('city', 'Ins.Name' = 'hospital'))

chk <- internal.data %>% 
  filter(is.na(pchc)) %>% 
  distinct(city, Ins.Name) %>% 
  filter(grepl('社区卫生服务中心', Ins.Name), !grepl('服务站|卫生室', Ins.Name)) %>% 
  arrange(city, Ins.Name)

write.xlsx(chk, '05_Internal_Review/All_Internal_Data/check1.xlsx')

# internal
internal.raw <- read.csv('05_Internal_Review/All_Internal_Data/CHC check data.csv', encoding = 'UTF-8')
internal.mapping <- read.xlsx('05_Internal_Review/All_Internal_Data/internal_packid_mapping.xlsx')

internal.pack <- internal.raw %>% 
  filter(InsSubType_SUB == '社区卫生服务中心') %>% 
  mutate(city = gsub('市', '', X.U.FEFF.CityNameC), 
         sales = stri_replace_all_fixed(Actual, ',', ''), 
         sales = as.numeric(sales), 
         units = stri_replace_all_fixed(Actual.Qty, ',', ''), 
         units = as.numeric(units)) %>% 
  left_join(internal.mapping, by = c('SkuNameE' = 'SKU_E_NAME')) %>% 
  filter(!is.na(Packid)) %>% 
  group_by(YQ = Quarter, City_C = city, Pack_ID = stri_pad_left(Packid, 7, 0)) %>% 
  summarise(Actual = sum(sales, na.rm = TRUE), 
            Actual.Qty = sum(units, na.rm = TRUE)) %>% 
  ungroup()

# delivery
delivery.raw <- read_xlsx('06_Deliveries/AZ_CHC_2017Q1_2020Q2_20200916.xlsx')

delivery.pack <- delivery.raw %>% 
  filter(Pack_ID %in% unique(internal.pack$packid)) %>% 
  group_by(quarter = YQ, city = City_C, packid = stri_pad_left(Pack_ID, 7, 0)) %>% 
  summarise(sales_delivery = first(`Value (RMB)`)) %>% 
  ungroup()

# contrast
contrast.pack <- delivery.raw %>% 
  filter(stri_sub(Prod_Ename, -3, -1) %in% c('AZM', 'AZN') | Prod_Ename == 'XUE ZHI KANG       BWX', 
         Year %in% c('2018', '2019', '2020')) %>% 
  select(-Market, -`购买方式`) %>% 
  distinct() %>% 
  left_join(internal.pack, by = c("YQ", "City_C", "Pack_ID")) %>% 
  mutate(Actual = if_else(is.na(Actual), 0, Actual), 
         Actual.Qty = if_else(is.na(Actual.Qty), 0, Actual.Qty), 
         `value - actual` = `Value (RMB)` - Actual, 
         `unit - actual` = `Counting Unit` - Actual.Qty)

write.xlsx(contrast.pack, '05_Internal_Review/All_Internal_Data/Internal_Check.xlsx')


##---- Betaloc ZOK ----
# 2019
raw.2019 <- read_feather("02_Inputs/data/all_raw_data_packid_Servier_171819_ahbjjs19Q4_update.feather")
colnames(raw.2019) <- c("Year", "Month", "季度", "省份", "城市", "区县", 
                        "医院名称", "医院库类型", "通用名", "商品名", 
                        "化学名", "匹配名", "药品名", "剂型", "规格", 
                        "生产企业", "采购价", "采购数量", "采购金额", 
                        "价格转换比", "最小使用单位数量", "packcode_m")

raw.az.2019 <- raw.2019 %>% 
  mutate(year = as.character(Year),
         date = stri_paste(year, stri_sub(Month, -2, -1)),
         quarter = `季度`,
         province = `省份`,
         city = ifelse(`城市` == "市辖区", "北京", 
                       ifelse(stri_sub(`城市`, 1, 2) == "山东", stri_sub(`城市`, 4, 5), 
                              gsub("市", "", `城市`))),
         city = ifelse(city == "安吉县", "湖州", city),
         district = `区县`, 
         hospital = `医院名称`, 
         packid = packcode_m, 
         price = `采购价`, 
         units = `采购数量`, 
         sales = `采购金额`) %>% 
  left_join(ims.mol, by = 'packid') %>% 
  filter(year == '2019', 
         city %in% target.city, 
         # stri_sub(Prd_desc, -3, -1) %in% c('AZM', 'AZN') | Prd_desc == 'XUE ZHI KANG       BWX', 
         grepl('社区卫生服务', hospital)) %>% 
  distinct() %>% 
  group_by(year, quarter, province, city, district, hospital, Prd_desc, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# 2020
raw.2020 <- read.csv('02_Inputs/data/ahbjjszj2020Q1Q2_packid_moleinfo.csv')

raw.az.2020 <- raw.2020 %>% 
  mutate(year = as.character(Year), 
         quarter = Quarter, 
         date = as.character(Month), 
         province = gsub('省|市', '', Province), 
         city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
         district = County, 
         hospital = Hospital_Name, 
         packid = stri_pad_left(packcode, 7, 0), 
         price = Price, 
         units = Value / Price, 
         sales = Value) %>% 
  filter(year == '2020', 
         # stri_sub(Prd_desc, -3, -1) %in% c('AZM', 'AZN') | Prd_desc == 'XUE ZHI KANG       BWX', 
         grepl('社区卫生服务', hospital)) %>% 
  distinct() %>% 
  group_by(year, quarter, province, city, district, hospital, Prd_desc, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# raw.betaloc.zok.2020 <- bind_rows(raw.servier.bj, raw.servier.js) %>% 
#   distinct(year = as.character(Year), 
#            quarter = Quarter, 
#            date = as.character(Month), 
#            province = gsub('省|市', '', Province), 
#            city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
#            district = County, 
#            hospital = Hospital_Name, 
#            packid = stri_pad_left(packcode, 7, 0), 
#            units = if_else(is.na(Volume), Value / Price, Volume), 
#            sales = Value) %>% 
#   filter(year == '2020', city %in% c('北京', '苏州'), 
#          stri_sub(packid, 1, 5) == '39609', 
#          grepl('社区卫生服务', hospital)) %>% 
#   group_by(year, quarter, province, city, district, hospital, packid) %>% 
#   summarise(sales = sum(sales, na.rm = TRUE)) %>% 
#   ungroup()

# Suzhou
raw.sz <- bind_rows(raw.az.2019, raw.az.2020) %>% 
  filter(city == '苏州')

write.xlsx(raw.sz, '05_Internal_Review/Betaloc_ZOK_Check/Suzhou.xlsx')

# delivery
az.delivery4 <- read.xlsx('02_Inputs/AZ_CHC_2017Q1_2020Q2_20200916.xlsx') %>% 
  filter(!(YQ == '2020Q1' & City_C == '南京' & Pack_ID == '0237616' & `购买方式` == '通用名')) %>% 
  distinct(Year, YQ, City_C, TA, Market, Prod_Ename, Pack_ID, `Value.(RMB)`) %>% 
  filter(Year %in% c('2018', '2019', '2020'), City_C %in% target.city, 
         stri_sub(Prod_Ename, -3, -1) %in% c('AZM', 'AZN') | Prod_Ename == 'XUE ZHI KANG       BWX') %>% 
  group_by(quarter = YQ, city = City_C, TA, Market, product = Prod_Ename, packid = Pack_ID) %>% 
  summarise(sales = sum(`Value.(RMB)`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(city, TA, Market, product, packid, quarter) %>% 
  pivot_wider(id_cols = c(city, TA, Market, product, packid), 
              names_from = quarter, 
              values_from = sales) %>% 
  mutate(flag = 'Projection')

# sales check
packid.market <- az.delivery %>% 
  distinct(Pack_ID, TA, Market)

data1 <- read_feather('05_Internal_Review/Betaloc_ZOK_Check/01_AZ_CHC_Raw_with_TA.feather') %>% 
  filter(year %in% c('2018', '2019'), city %in% target.city) %>% 
  distinct(quarter, date, city, pchc, TA, packid, sales) %>% 
  left_join(packid.market, by = c('packid' = 'Pack_ID', 'TA')) %>% 
  filter(!is.na(Market)) %>% 
  left_join(ims.mol, by = 'packid') %>% 
  filter(stri_sub(Prd_desc, -3, -1) %in% c('AZM', 'AZN') | Prd_desc == 'XUE ZHI KANG       BWX') %>% 
  group_by(quarter, city, TA, Market, product = Prd_desc, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

data2 <- imp.total %>% 
  filter(year %in% c('2020'), city %in% target.city) %>% 
  filter(stri_sub(product, -3, -1) %in% c('AZM', 'AZN') | product == 'XUE ZHI KANG       BWX') %>% 
  distinct(quarter, date, city, pchc, TA, flag_mkt, product, packid, sales) %>% 
  left_join(packid.market, by = c('packid' = 'Pack_ID', 'TA')) %>% 
  filter(!is.na(Market)) %>% 
  group_by(quarter, city, TA, Market, product, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

az.city <- bind_rows(data1, data2) %>% 
  group_by(quarter, city, TA, Market, product, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(city, TA, Market, product, packid, quarter) %>% 
  pivot_wider(id_cols = c(city, TA, Market, product, packid), 
              names_from = quarter, 
              values_from = sales) %>% 
  mutate(flag = 'Sample') %>% 
  bind_rows(az.delivery4)

write.xlsx(az.city, '05_Internal_Review/Betaloc_ZOK_Check/AZ_City.xlsx')

# ins check
betaloc.zok.ins <- pchc.mapping3 %>% 
  distinct(city, pchc) %>% 
  filter(city %in% c('北京', '苏州')) %>% 
  count(city, name = 'CHC_n')

write.xlsx(betaloc.zok.ins, '05_Internal_Review/Betaloc_ZOK_Check/Universe_INS.xlsx')

# top
raw.top <- read_excel('05_Internal_Review/Betaloc_ZOK_Check/ZOK Data Check.xlsx', sheet = 2)

chk <- bind_rows(raw.az.2019, raw.az.2020) %>% 
  distinct(city, hospital)

betaloc.zok.top <- bind_rows(raw.az.2019, raw.az.2020) %>% 
  filter(stri_sub(packid, 1, 5) == '39609') %>% 
  mutate(hospital_update = case_when(
    hospital %in% c('相城区元和街道社区卫生服务中心') ~ '苏州市相城区元和街道社区卫生服务中心', 
    hospital %in% c('苏州市姑苏区平江街道白塔社区卫生服务中心') ~ '苏州市姑苏区平江街道白塔社区卫生服务中心', 
    hospital %in% c('苏州市姑苏区娄门街道娄江社区卫生服务中心') ~ '苏州市姑苏区平江街道娄江社区卫生服务中心', 
    hospital %in% c('相城区黄桥街道社区卫生服务中心') ~ '苏州市相城区黄桥街道社区卫生服务中心', 
    hospital %in% c('苏州市姑苏区留园街道社区卫生服务中心') ~ '苏州市姑苏区虎丘街道留园社区卫生服务中心', 
    hospital %in% c('苏州市虎丘区浒墅关分区阳山花苑社区卫生服务中心', '苏州高新区浒墅关分区阳山花苑社区卫生服务中心') ~ '苏州高新区阳山街道社区卫生服务中心', 
    hospital %in% c('苏州市吴江市松陵镇社区卫生服务中心', '吴江市松陵镇社区卫生服务中心') ~ '吴江区松陵镇社区卫生服务中心', 
    hospital %in% c('苏州市常熟市虞山镇藕渠社区卫生服务中心', '藕渠社区卫生服务中心') ~ '常熟市琴川街道藕渠社区卫生服务中心', 
    hospital %in% c('苏州市张家港市杨舍镇社区卫生服务中心', '杨舍镇社区卫生服务中心') ~ '张家港经济技术开发区(杨舍镇)社区卫生服务中心', 
    hospital %in% c('北京市密云县鼓楼社区卫生服务中心') ~ '北京市密云区鼓楼社区卫生服务中心', 
    hospital %in% c('北京市海淀区万寿路社区卫生服务中心') ~ '北京市海淀区万寿路社区卫生服务中心(北京市海淀区万寿路医院)', 
    hospital %in% c('北京市海淀区双榆树社区卫生服务中心(北京市海淀中医医院)') ~ '北京市海淀区双榆树社区卫生服务中心', 
    hospital %in% c('北京市丰台区南苑乡果园鑫福里社区卫生服务站') ~ '北京市丰台区南苑乡鑫福里社区卫生服务中心', 
    hospital %in% c('北京市通州区梨园社区卫生服务中心') ~ '北京市通州区梨园镇梨园社区卫生服务中心', 
    hospital %in% c('首都医科大学附属北京安贞医院大屯社区卫生服务中心') ~ '北京市朝阳区首都医科大学附属北京安贞医院大屯社区卫生服务中心', 
    hospital %in% c('北京市丰台区西罗园社区卫生服务中心') ~ '西罗园社区卫生服务中心', 
    hospital %in% c('北京市昌平区回龙观社区卫生服务中心') ~ '回龙观社区卫生服务中心', 
    hospital %in% c('北京市大兴区亦庄社区卫生服务中心') ~ '北京市大兴区亦庄镇社区卫生服务中心', 
    TRUE ~ hospital
  )) %>% 
  filter(hospital_update %in% raw.top$`Sub Ins Name`) %>% 
  group_by(quarter, hospital_update) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(hospital_update) %>% 
  mutate(SUM = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(hospital_update, SUM), 
              names_from = quarter, 
              values_from = sales) %>% 
  right_join(raw.top[c('CityNameC', 'Sub Ins Code', 'Sub Ins Name', 'Ins type')], by = c('hospital_update' = 'Sub Ins Name')) %>% 
  mutate(hospital_update = factor(hospital_update, levels = raw.top$`Sub Ins Name`)) %>% 
  arrange(hospital_update) %>% 
  select(CityNameC, `Sub Ins Code`, `Sub Ins Name` = hospital_update, `Ins type`, starts_with('20'), SUM)

write.xlsx(betaloc.zok.top, '05_Internal_Review/Betaloc_ZOK_Check/Betaloc_ZOK_Top.xlsx')



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC 2020Q2
# Purpose:      Price
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











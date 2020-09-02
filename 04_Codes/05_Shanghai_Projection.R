# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC 2020Q2
# Purpose:      Shanghai projection
# programmer:   Zhe Liu
# Date:         2020-09-02
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Sample growth ----
# Shanghai sample
delivery.az <- read_xlsx('02_Inputs/AZ_CHC_2017Q1_2020Q1_0831.xlsx')

sh.sample <- delivery.az %>% 
  filter(City_C %in% c("上海"), 
         YQ %in% c("2019Q1", "2019Q2"))

# Shanghai growth
sh.growth <- imp.total %>% 
  filter(city %in% c("北京"), 
         quarter %in% c("2019Q1", "2019Q2", "2020Q1", "2020Q2")) %>% 
  mutate(province = "上海", 
         city = "上海", 
         packid = if_else(stri_sub(packid, 1, 5) %in% market.cndrug$PROD_COD, 
                          'TCM Others', packid)) %>% 
  group_by(quarter, city, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = quarter, values_from = sales) %>% 
  mutate(growth_1920Q1 = `2020Q1` / `2019Q1`, 
         growth_1920Q2 = `2020Q2` / `2019Q2`, 
         growth_1920Q1 = if_else(growth_1920Q1 > 3, 1, growth_1920Q1), 
         growth_1920Q2 = if_else(growth_1920Q2 > 3, 1, growth_1920Q2), 
         growth_1920Q1 = if_else(is.na(growth_1920Q1), 1, growth_1920Q1), 
         growth_1920Q2 = if_else(is.na(growth_1920Q2), 1, growth_1920Q2)) %>% 
  select(city, packid, growth_1920Q1, growth_1920Q2) %>% 
  pivot_longer(cols = starts_with('growth'), 
               names_to = 'quarter', 
               names_prefix = 'growth_1920', 
               values_to = 'growth') %>% 
  mutate(quarter = stri_paste('2019', quarter))


##---- KNN model ----
# ims sales
raw.ims <- fread("02_Inputs/cn_IMS_Sales_Fdata_201903_1.txt", stringsAsFactors = FALSE)

ims.sales <- raw.ims %>% 
  mutate(date = gsub("M", "", Period_Code),
         packid = stri_pad_left(Pack_ID, 7, 0),
         sample_flag = if_else(packid %in% sh.growth$packid, 1, 0)) %>% 
  filter(Geography_id == "CHT", date >= "201701", packid %in% sh.sample$Pack_ID) %>% 
  group_by(date, packid) %>% 
  summarise(sales = sum(LC, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(date, sales, fill = 0) %>% 
  mutate(train_flag = if_else(packid %in% sh.growth$packid, 1, 0))

# set
train.sh <- ims.sales[ims.sales$train_flag == 1, ]
test.sh <- ims.sales[ims.sales$train_flag == 0, ]

train.sh.tmp <- select(train.sh, -packid, -train_flag)
test.sh.tmp <- select(test.sh, -packid, -train_flag)

# model
sh.model <- kknn(`201701` ~ ., train = train.sh.tmp, test = test.sh.tmp, k = 3, scale = TRUE)

sh.indice <- as.data.frame(sh.model$C) %>% 
  lapply(function(x) {
    train.sh$packid[x]
  }) %>% 
  as.data.frame(col.names = c("pack_1", "pack_2", "pack_3")) %>% 
  bind_cols(test.sh[, c("packid")]) %>% 
  setDT() %>% 
  melt(id.vars = "packid", variable.name = "knn_level", value.name = "knn_pack")

sh.weight <- as.data.frame(sh.model$D) %>% 
  lapply(function(x) {
    1 / (x+1)
  }) %>% 
  as.data.frame(col.names = c("pack_1", "pack_2", "pack_3")) %>% 
  mutate(weight_sum = pack_1 + pack_2 + pack_3,
         pack_1 = pack_1 / weight_sum,
         pack_2 = pack_2 / weight_sum,
         pack_3 = pack_3 / weight_sum) %>% 
  bind_cols(test.sh[, c("packid")]) %>% 
  select(-weight_sum) %>% 
  setDT() %>% 
  melt(id.vars = "packid", variable.name = "knn_level", value.name = "knn_weight")


##---- Growth ----
# growth
weight.growth <- sh.indice %>% 
  left_join(sh.weight, by = c("packid", "knn_level")) %>% 
  left_join(sh.growth, by = c("knn_pack" = "packid")) %>% 
  group_by(quarter, city, packid) %>% 
  summarise(growth = sum(growth * knn_weight, na.rm = TRUE)) %>% 
  select(quarter, city, packid, growth)

# growth add
surplus <- setdiff(sh.sample$Pack_ID[!(sh.sample$Pack_ID %in% sh.growth$packid)], ims.sales$packid)

surplus.growth <- data.frame(city = "上海",
                             packid = surplus) %>% 
  merge(data.frame(quarter = c('2019Q1', '2019Q2'))) %>% 
  mutate(growth = 1)

sh.growth.add <- bind_rows(merge(sh.growth, 0), 
                           merge(weight.growth, 1), 
                           merge(surplus.growth, 2))


##---- Result ----
proj.sh <- sh.sample %>% 
  # group_by(year, quarter, province, city, TA, atc4, molecule_desc, packid) %>% 
  # summarise(units = sum(units, na.rm = TRUE),
  #           sales = sum(sales, na.rm = TRUE)) %>% 
  # ungroup() %>% 
  left_join(sh.growth.add, by = c("YQ" = "quarter", "City_C" = "city", 
                                  "Pack_ID" = "packid")) %>% 
  mutate(`Total Unit` = `Total Unit` * growth, 
         `Value (RMB)` = `Value (RMB)` * growth, 
         `Counting Unit` = `Counting Unit` * growth) %>% 
  filter(`Total Unit` > 0, `Value (RMB)` > 0, `Counting Unit` > 0) %>% 
  mutate(Year = "2020", 
         YQ = gsub("2019", "2020", YQ)) %>% 
  # group_by(year, quarter, province, city, TA, atc4, packid) %>% 
  # summarise(units = sum(units, na.rm = TRUE),
  #           sales = sum(sales, na.rm = TRUE)) %>% 
  # ungroup() %>% 
  # mutate(price = sales / units) %>% 
  select(-growth, -y)

write.xlsx(proj.sh, "03_Outputs/07_AZ_CHC_Projection_Shanghai.xlsx")



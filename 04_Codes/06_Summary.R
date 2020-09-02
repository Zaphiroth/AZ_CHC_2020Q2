# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC 2020Q2
# Purpose:      Summary
# programmer:   Zhe Liu
# Date:         2020-09-02
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin info ----
# pack info
product.info1 <- read.xlsx("02_Inputs/Product standardization master data-A-S-0313.xlsx") %>% 
  distinct(packid = PACK_ID, molecule_cn = MOLE_NAME_CH, 
           product_cn = PROD_NAME_CH, pck_desc = PCK_DESC, 
           corp_cn = CORP_NAME_CH, size = PACK, dosage = DOSAGE, spec = SPEC)

product.info2 <- read.xlsx("02_Inputs/packid_prod_20181112.xlsx") %>% 
  select(packid = pack_id, molecule_cn = gene_name, product_cn = ims_product_cn, 
         pck_desc = Pck_Desc, corp_cn = ims_corp, size = PckSize_Desc) %>% 
  mutate(dosage = str_trim(str_replace(pck_desc, "[0-9]{1,}.{1,}", ""), "right"),
         spec = sapply(pck_desc, function(x) {
           paste(str_extract_all(x, "[0-9]{1,}[a-zA-Z]{1,}", simplify = TRUE), collapse = " ")
         })) %>% 
  distinct()

product.info <- bind_rows(product.info1, product.info2) %>% 
  group_by(packid) %>% 
  summarise(molecule_cn = first(na.omit(molecule_cn)), 
            product_cn = first(na.omit(product_cn)), 
            pck_desc = first(na.omit(pck_desc)), 
            corp_cn = first(na.omit(corp_cn)), 
            size = first(na.omit(size)), 
            dosage = first(na.omit(dosage)), 
            spec = first(na.omit(spec))) %>% 
  ungroup() %>% 
  filter(!is.na(molecule_cn))

# corp
corp.cn <- read_xls("02_Inputs/Corp E & C name.xls") %>% 
  distinct(Corporation, Corporation_C)

# city
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
















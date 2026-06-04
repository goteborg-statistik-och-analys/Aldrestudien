# =============================================================================
# ÄLDRESTUDIEN
# Del 2, bearbeta data
# =============================================================================

library(here)
library(readxl)

# LADDA DATA ----
source(here("R", "1_hamta_data.R"))


# KONFIGURATION OCH FUNKTIONER ----
source(here("R", "config", "settings.R"))
source(here("R", "functions", "data_utils.R"))


# Läs in tabellen från individdatabasen
aldre_boende_individdata <- readRDS(here("input", "aldre_boende.rds"))
gbg_smahus <- readRDS(here("input", "gbg_smahus.rds"))
gr_smahus <- readRDS(here("input", "gr_smahus.rds"))

# Läs in extratabell
flyttyp_inflytt <- read_excel(here("input", "inflytt_flyttyp.xlsx"))



# ANTAL OCH ANDEL ÄLDRE I BEFOLKNINGEN ----
# =============================================================================

# Riket och storstäder
antal_andel_aldre_historisk <- demografisk_statistik$folkmangd |>
  mutate(
    alder_num = extrahera_alder_numerisk(ålder),
    år_num = as.numeric(år),
    befolkningskategori = kategorisera_befolkning(
      alder_num,
      aldre_grans = ALDER_ALDRE_GRANS,
      yngre_max = ALDER_ARBETSFOR_MAX
    )
  ) |>
  group_by(region, år_num, befolkningskategori) |>
  summarise(antal = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |>
  group_by(region, år_num) |>
  mutate(
    totalt_befolkning = sum(antal),
    andel = antal / totalt_befolkning
  ) |>
  ungroup() |>
  rename(år = år_num)

# GR kommunerna
andel_gr_kommuner_historiskt <- demografisk_statistik$folkmangd_gr |> 
  mutate(
    alder_num = extrahera_alder_numerisk(ålder),
    år_num = as.numeric(år),
    befolkningskategori = kategorisera_befolkning(
      alder_num,
      aldre_grans = ALDER_ALDRE_GRANS,
      yngre_max = ALDER_ARBETSFOR_MAX
    )
  ) |>
  group_by(region, år_num, befolkningskategori) |>
  summarise(antal = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |>
  group_by(region, år_num) |>
  mutate(
    totalt_befolkning = sum(antal),
    andel = antal / totalt_befolkning
  ) |>
  ungroup() |>
  rename(år = år_num)

# Göteborgsregionen exkl Göteborg
andel_gr_historiskt <- demografisk_statistik$folkmangd_gr |> 
  filter(region != "Göteborg") |> 
  mutate(
    alder_num = extrahera_alder_numerisk(ålder),
    år_num = as.numeric(år),
    befolkningskategori = kategorisera_befolkning(
      alder_num,
      aldre_grans = ALDER_ALDRE_GRANS,
      yngre_max = ALDER_ARBETSFOR_MAX
    )
  ) |>
  group_by(år_num, befolkningskategori) |>
  summarise(antal = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |>
  group_by(år_num) |>
  mutate(
    totalt_befolkning = sum(antal),
    andel = antal / totalt_befolkning
  ) |>
  ungroup() |>
  rename(år = år_num) |> 
  mutate(region = "Göteborgsregionen exkl Göteborg")


# PROGNOS ÖVER ANTAL ÄLDRE i RIKET OCH STORSTÄDERNA ----
# =============================================================================

# Kombinera prognoser från riket, Stockholm, Malmö och Göteborg
prognos_kombinerad <- demografisk_statistik$pr_riket |>
  mutate(region = "Riket") |>
  rename(Folkmängd = Antal) |>
  bind_rows(demografisk_statistik$pr_st_ma_gr |> 
              filter(region %in% c("Stockholm", "Malmö"))) |>
  bind_rows(
    demografisk_statistik$pr_gbg |>
      mutate(region = "Göteborg") |>
      rename(
        ålder = Ålder,
        kön = Kön,
        år = Prognosår,
        Folkmängd = NoContent
      ) |>
      filter(kön != "Totalt (Båda kön)")
  )

# Beräkna antal och andel äldre i prognosen
antal_andel_aldre_prognos <- prognos_kombinerad |>
  mutate(
    alder_num = extrahera_alder_numerisk(ålder),
    år_num = as.numeric(år),
    befolkningskategori = kategorisera_befolkning(
      alder_num,
      aldre_grans = ALDER_ALDRE_GRANS,
      yngre_max = ALDER_ARBETSFOR_MAX
    )
  ) |>
  group_by(region, år_num, befolkningskategori) |>
  summarise(antal = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |>
  group_by(region, år_num) |>
  mutate(
    totalt_befolkning = sum(antal),
    andel = antal / totalt_befolkning
  ) |>
  ungroup() |>
  rename(år = år_num)

# Kombinera historisk och prognos för riket och storstäderna
prognos_historisk_aldre <- antal_andel_aldre_historisk |>
  bind_rows(antal_andel_aldre_prognos) |> 
  arrange(region, år)


# PROGNOS I GR KOMMUNERNA, RIKET, GÖTEBORG OCH GR EXKL GÖTEBORG ----
# =============================================================================

# Prognos över GR exkl Göteborg
prognos_gr_exkl_gbg <- demografisk_statistik$pr_st_ma_gr |> 
  filter(!region %in% c("Stockholm", "Malmö", "Göteborg")) |> 
  group_by(år, ålder) |> 
  summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |> 
  mutate(region = "Göteborgsregionen exkl Göteborg")

# Kombinera samtliga prognoser
prognos_kombinerad_gr <- demografisk_statistik$pr_riket |>
  mutate(region = "Riket") |>
  rename(Folkmängd = Antal) |>
  bind_rows(demografisk_statistik$pr_st_ma_gr |> 
              filter(!region %in% c("Stockholm", "Malmö"))) |>
  bind_rows(prognos_gr_exkl_gbg) |> 
  bind_rows(
    demografisk_statistik$pr_gbg |>
      mutate(region = "Göteborg") |>
      rename(
        ålder = Ålder,
        kön = Kön,
        år = Prognosår,
        Folkmängd = NoContent
      ) |>
      filter(kön != "Totalt (Båda kön)")
  )

# Andel äldre i prognosen
andel_aldre_prognos_gr <- prognos_kombinerad_gr |>
  mutate(
    alder_num = extrahera_alder_numerisk(ålder),
    år_num = as.numeric(år),
    befolkningskategori = kategorisera_befolkning(
      alder_num,
      aldre_grans = ALDER_ALDRE_GRANS,
      yngre_max = ALDER_ARBETSFOR_MAX
    )
  ) |>
  group_by(region, år_num, befolkningskategori) |>
  summarise(antal = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |>
  group_by(region, år_num) |>
  mutate(
    totalt_befolkning = sum(antal),
    andel = antal / totalt_befolkning
  ) |>
  ungroup() |>
  rename(år = år_num)

# Kombinera historiskt och i prognosen
prognos_historisk_alder_gr <- antal_andel_aldre_historisk  |> 
  filter(!region %in% c("Malmö", "Stockholm")) |> 
  bind_rows(andel_gr_kommuner_historiskt |> 
              filter(region != "Göteborg")) |> 
  bind_rows(andel_gr_historiskt) |> 
  # prognos
  bind_rows(andel_aldre_prognos_gr)


# FOLKÖKNING HISTORISKT OCH I PROGNOS ----
# =============================================================================

# Folkökning av den äldre gruppen - historiskt
folkokning_aldre_historisk <- demografisk_statistik$folkmangd |>
  mutate(
    alder_num = extrahera_alder_numerisk(ålder),
    år_num = as.numeric(år)
  ) |>
  filter(
    år_num != min(år_num),
    alder_num >= ALDER_ALDRE_GRANS,
    region == "Göteborg"
  ) |>
  mutate(aldersgrupp_bred = case_when(
    alder_num >= ALDER_MYCKET_ALDRE_GRANS ~ "80 år och över",
    TRUE ~ "65-79 år"
  )) |>
  group_by(år_num, aldersgrupp_bred) |>
  summarise(antal = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |>
  rename(år = år_num)

# Folkökning i prognosen
folkokning_aldre_prognos <- demografisk_statistik$pr_gbg |>
  mutate(
    alder_num = extrahera_alder_numerisk(Ålder),
    år_num = as.numeric(Prognosår)
  ) |>
  filter(
    alder_num >= ALDER_ALDRE_GRANS,
    Kön == "Totalt (Båda kön)"
  ) |>
  mutate(aldersgrupp_bred = case_when(
    alder_num >= ALDER_MYCKET_ALDRE_GRANS ~ "80 år och över",
    TRUE ~ "65-79 år"
  )) |>
  group_by(år_num, aldersgrupp_bred) |>
  summarise(antal = sum(NoContent, na.rm = TRUE), .groups = "drop") |>
  rename(år = år_num)

# Kombinera och beräkna förändring år-för-år
folkokning_aldre_prognos_historisk <- folkokning_aldre_historisk |>
  bind_rows(folkokning_aldre_prognos) |>
  arrange(år) |>
  group_by(aldersgrupp_bred) |>
  mutate(folkokning = antal - lag(antal)) |>
  ungroup()


# BEFOLKNINGSSAMMANSÄTTNING ----
# =============================================================================

# Startbefolkning per åldersklasser 
befolkning_start_aldersgrupp <- demografisk_statistik$folkmangd |>
  mutate(
    alder_num = extrahera_alder_numerisk(ålder),
    år_num = as.numeric(år),
    alder_num_start = alder_num + 1,
    år_start = år_num + 1
  ) |> 
  filter(alder_num_start >= 55) |> 
  mutate(
    aldersgrupp_aldre = skapa_aldersgrupper_yngre_aldre(alder_num_start)
  ) |>
  group_by(region, år_start, aldersgrupp_aldre) |>
  summarise(antal = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |>
  rename(år = år_start)

# Befolkningssammansättning per 10-årsklasser historiskt
befolkning_aldersgrupp_10ar <- demografisk_statistik$folkmangd |>
  mutate(
    alder_num = extrahera_alder_numerisk(ålder),
    aldersgrupp_10ar = skapa_aldersgrupper_10ar(alder_num)
  ) |>
  group_by(region, år, kön, aldersgrupp_10ar) |>
  summarise(antal = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |>
  group_by(region, år, kön) |>
  mutate(
    totalt = sum(antal),
    andel = antal / totalt * 100
  ) |>
  ungroup()


# DEMOGRAFISK FÖRSÖRJNINGSKVOT ----
# =============================================================================

demografisk_forsorjningskvot <- prognos_historisk_aldre |>
  mutate(forsorjningsgrupp = case_when(
    befolkningskategori == "Arbetsfӧr befolkning" ~ "arbetsfor",
    TRUE ~ "ej_arbetsfor"
  )) |>
  group_by(region, år, forsorjningsgrupp) |>
  summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = forsorjningsgrupp,
    values_from = antal
  ) |>
  mutate(forsorjningskvot = ej_arbetsfor / arbetsfor * 100)

# Försörjningskvot för övriga kommuner
demografisk_forsorjningskvot_kommuner <- demografisk_statistik$pr_alla_kommuner |> 
  filter(!grepl("län", region, ignore.case = TRUE)) |> 
  mutate(
    alder_num = extrahera_alder_numerisk(ålder),
    år_num = as.numeric(år),
    befolkningskategori = kategorisera_befolkning(
      alder_num,
      aldre_grans = ALDER_ALDRE_GRANS,
      yngre_max = ALDER_ARBETSFOR_MAX
    )
  ) |>
  group_by(region, år_num, befolkningskategori) |>
  summarise(antal = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |> 
  mutate(forsorjningsgrupp = case_when(
    befolkningskategori == "Arbetsfӧr befolkning" ~ "arbetsfor",
    TRUE ~ "ej_arbetsfor"
  )) |>
  group_by(region, år_num, forsorjningsgrupp) |>
  summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = forsorjningsgrupp,
    values_from = antal
  ) |>
  mutate(forsorjningskvot = ej_arbetsfor / arbetsfor * 100)


# DÖDA ----
# =============================================================================

# Andel döda per åldersgrupp (5-årsintervall)
doda_per_aldersgrupp_5ar <- demografisk_statistik$doda |>
  mutate(
    alder_num = extrahera_alder_numerisk(ålder),
    aldersgrupp_5ar = skapa_aldersgrupper_5ar(alder_num)
  ) |>
  group_by(region, år, aldersgrupp_5ar) |>
  summarise(antal = sum(Antal, na.rm = TRUE), .groups = "drop") |>
  group_by(region, år) |>
  mutate(
    totalt_doda = sum(antal, na.rm = TRUE),
    andel_doda = antal / totalt_doda
  ) |>
  ungroup()


# DÖDSRISKER PER ÅLDER ----
# =============================================================================
# Beräkna dödsrisker historiskt och i prognos - startbefolkning per ålder

# # Startbefolkning historiskt, obs sista året i denna tabell är första startbefolkningen i prognosen
# # Vi joinar tabellerna sedan så vi behöver inte hantera det
# startbefolkning_historiskt <- demografisk_statistik$folkmangd |> 
#   filter(region == "Riket") |> 
#   mutate(
#     alder_num = extrahera_alder_numerisk(ålder),
#     år_num = as.numeric(år),
#     alder_start = alder_num + 1,
#     år_start = år_num + 1,
#     alder_start = if_else(alder_start >= 101, 100, alder_start)
#   ) |>
#   select(
#     region,
#     alder_num = alder_start,
#     kön,
#     år = år_start,
#     startbefolkning = Folkmängd
#   ) |>
#   filter(alder_num >= ALDER_ALDRE_GRANS) |>
#   group_by(region, år, kön, alder_num) |>
#   summarise(startbefolkning = sum(startbefolkning, na.rm = TRUE), .groups = "drop")
# 
# # Ta fram startbefolkningen för prognosen
# startbefolkning_prognos <- demografisk_statistik$pr_riket |> 
#   mutate(
#     alder_num = extrahera_alder_numerisk(ålder),
#     år_num = as.numeric(år),
#     alder_start = alder_num + 1,
#     år_start = år_num + 1,
#     alder_start = if_else(alder_start >= 101, 100, alder_start), 
#     region = "Riket"
#   ) |> 
#   select(
#     region,
#     alder_num = alder_start,
#     kön,
#     år = år_start,
#     startbefolkning = Antal
#   ) |>
#   filter(alder_num >= ALDER_ALDRE_GRANS) |>
#   group_by(region, år, kön, alder_num) |>
#   summarise(startbefolkning = sum(startbefolkning, na.rm = TRUE), .groups = "drop")
# 
# # Koppla startbefolkningen historiskt och prognos
# startbefolkning_dodsrisker <- startbefolkning_historiskt |> 
#   bind_rows(startbefolkning_prognos)

# # Koppla döda till startbefolkning
# startbefolkning_dodsrisker_historisk_prognos <- startbefolkning_dodsrisker |>  
#   left_join(
#     medelfolkmangd_dodsrisker_historisk |> 
#       mutate(
#         år_num = as.numeric(år)
#       ) |>
#       select(region, alder_num, kön, år = år_num, antal_doda),
#     by = c("region", "alder_num", "kön", "år")
#   ) |>
#   mutate(andel_doda = antal_doda / startbefolkning) |> 
#   left_join(
#     demografisk_statistik$pr_riket_dodsrisker |> 
#       select(-födelseregion) |> 
#       mutate(
#         alder_num = extrahera_alder_numerisk(ålder),
#         år_num = as.numeric(år)
#       ) |> 
#       select(alder_num, kön, år = år_num, andel_doda_pr = Antal),
#     by = c("alder_num", "kön", "år")
#   ) |> 
#   mutate(dodsrisker = if_else(is.na(andel_doda), andel_doda_pr, andel_doda), 
#          antal_doda = dodsrisker * startbefolkning) |> 
#   select(region, år, kön, alder_num, startbefolkning, antal_doda, dodsrisker)
# 
# 
# 
# # Koppla döda till startbefolkning
# startbefolkning_dodsrisker_historisk_prognos <- startbefolkning_dodsrisker |>  
#   left_join(
#    demografisk_statistik$doda |> 
#      filter(region == "Riket") |> 
#       mutate(
#         alder_num = extrahera_alder_numerisk(ålder),
#         år_num = as.numeric(år)
#       ) |>
#       select(region, alder_num, kön, år = år_num, antal_doda = Antal),
#     by = c("region", "alder_num", "kön", "år")
#   ) |>
#   mutate(andel_doda = antal_doda / startbefolkning) |> 
#   left_join(
#     demografisk_statistik$pr_riket_dodsrisker |> 
#       select(-födelseregion) |> 
#       mutate(
#         alder_num = extrahera_alder_numerisk(ålder),
#         år_num = as.numeric(år)
#       ) |> 
#       select(alder_num, kön, år = år_num, andel_doda_pr = Antal),
#     by = c("alder_num", "kön", "år")
#   ) |> 
#   mutate(dodsrisker = if_else(is.na(andel_doda), andel_doda_pr, andel_doda), 
#          antal_doda = dodsrisker * startbefolkning) |> 
#   select(region, år, kön, alder_num, startbefolkning, antal_doda, dodsrisker)

# # Slå ihop historisk och prognos
# dodsrisker_prognos_historisk_riket <- startbefolkning_dodsrisker_historisk_prognos |> 
#   mutate(aldersgrupp_aldre = skapa_aldersgrupper_aldre(alder_num)) |>
#   group_by(region, år, aldersgrupp_aldre) |>
#   summarise(
#     antal_bef = sum(startbefolkning, na.rm = TRUE),
#     antal_doda = sum(antal_doda, na.rm = TRUE),
#     .groups = "drop"
#   ) |>
#   mutate(andel_doda = antal_doda / antal_bef) |> 
#   arrange(aldersgrupp_aldre, år)


# Räkna ut dödsriskerna historisk med medelfolkmängd
# Vi beräknar dödsriskerna som medelbefolkningen * dödstalen

# Funktion för att beräkna medelfolkmängd
berakna_medelfolkmangd <- function(data) {
  data |>
    mutate(
      alder_num = as.numeric(gsub("\\+? år", "", ålder)),
      år = as.numeric(år)
    ) |>
    left_join(
      data |>
        mutate(
          alder_num = as.numeric(gsub("\\+? år", "", ålder)) + 1,
          år = as.numeric(år) + 1
        ) |>
        select(region, alder_num, kön, år, folkmangd_foregaende = Folkmängd),
      by = c("region", "alder_num", "kön", "år")
    ) |>
    mutate(
      medelfolkmangd = (Folkmängd + folkmangd_foregaende) / 2
    ) |>
    select(-folkmangd_foregaende)
}

# Beräkna dödsriskerna historiskt
medelfolkmangd_dodsrisker_historisk <- demografisk_statistik$folkmangd |> 
  filter(region == "Riket") |> 
  berakna_medelfolkmangd() |> 
  filter(år != 1968) |> 
  select(region, år, kön, alder_num, medelfolkmangd) |> 
  left_join(
    demografisk_statistik$doda |> 
      filter(region == "Riket") |> 
      mutate(
        alder_num = extrahera_alder_numerisk(ålder),
        år_num = as.numeric(år)
      ) |>
      select(region, alder_num, kön, år = år_num, antal_doda = Antal),
    by = c("region", "alder_num", "kön", "år")
  ) |> 
  mutate(andel_doda = antal_doda / medelfolkmangd)

# Räkna ut dödsriskerna per åldersgrupp historiskt
dodsrisker_aldergrupp_historisk <- medelfolkmangd_dodsrisker |> 
  filter(alder_num >= ALDER_ALDRE_GRANS) |> 
  mutate(aldersgrupp_aldre = skapa_aldersgrupper_aldre(alder_num)) |>
  group_by(region, år, aldersgrupp_aldre) |>
  summarise(
    antal_bef = sum(medelfolkmangd, na.rm = TRUE),
    antal_doda = sum(antal_doda, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(andel_doda = antal_doda / antal_bef) |> 
  arrange(aldersgrupp_aldre, år)

# Räkna ut dödsriskerna per åldersgrupp i prognosen
# Börja med att ta fram medelfolkmängden
medelfolkmangd_dodsrisker_pr <- demografisk_statistik$pr_riket |> 
  mutate(region = "Riket") |> 
  rename(Folkmängd = Antal) |> 
  bind_rows(demografisk_statistik$folkmangd |> 
              filter(region == "Riket", 
                     år == 2025)) |> 
  berakna_medelfolkmangd() |> 
  select(region, ålder, kön, år, medelfolkmangd) |> 
  mutate(alder_num = extrahera_alder_numerisk(ålder)) |> 
  left_join(
    demografisk_statistik$pr_riket_dodsrisker |> 
      select(-födelseregion) |> 
      mutate(
        alder_num = extrahera_alder_numerisk(ålder),
        år_num = as.numeric(år)
      ) |> 
      select(alder_num, kön, år = år_num, andel_doda_pr = Antal),
    by = c("alder_num", "kön", "år")
  ) |> 
  mutate(antal_doda = andel_doda_pr * medelfolkmangd)

# Räkna ut dödsriskerna per åldersgrupp
dodsrisker_aldergrupp_pr <- medelfolkmangd_dodsrisker_pr  |> 
  filter(alder_num >= ALDER_ALDRE_GRANS, 
         år != 2025) |> 
  mutate(aldersgrupp_aldre = skapa_aldersgrupper_aldre(alder_num)) |>
  group_by(region, år, aldersgrupp_aldre) |>
  summarise(
    antal_bef = sum(medelfolkmangd, na.rm = TRUE),
    antal_doda = sum(antal_doda, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(andel_doda = antal_doda / antal_bef) |> 
  arrange(aldersgrupp_aldre, år) 
  
# Slå ihop historisk och prognos
dodsrisker_prognos_historisk_riket <- dodsrisker_aldergrupp_historisk |> 
  bind_rows(dodsrisker_aldergrupp_pr) |> 
  arrange(region, aldersgrupp_aldre, år)



# FLYTTMÖNSTER BLAND ÄLDRE ----
# =============================================================================

andel_flytt_aldre <- demografisk_statistik$flyttningar |>
  left_join(
    demografisk_statistik$folkmangd |>
      select(region, ålder, kön, år, folkmangd = Folkmängd),
    by = c("region", "ålder", "kön", "år")
  ) |>
  mutate(
    alder_num = extrahera_alder_numerisk(ålder),
    aldersgrupp_aldre = skapa_aldersgrupper_yngre_aldre(alder_num),
    år_num = as.numeric(år)
  ) |>
  filter(alder_num >= YNGRE_ALDRE_GRANS) |>
  group_by(region, år_num, aldersgrupp_aldre) |>
  summarise(
    across(
      c(
        inflyttningar, utflyttningar, folkmangd
      ),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(
        inflyttningar, utflyttningar
      ),
      ~ .x / folkmangd,
      .names = "andel_{.col}"
    ),
    totalt_flytt = inflyttningar + utflyttningar
  ) |>
  rename(år = år_num)

# INFLYTTNIGNSRISKER FRÅN GR OCH ÖVRIGA RIKET
# =======================================================================
# Dra ifrån GR befolkningen från riket för att få fram "övriga riket"
startbefolkning_riket <- demografisk_statistik$folkmangd |> 
  filter(region == "Riket") |> 
  rename(Folkmängd_riket = Folkmängd) |> 
  left_join(demografisk_statistik$folkmangd_gr |> 
              mutate(region_gr = "Göteborgsregionen") |> 
              group_by(region_gr, kön, ålder, år) |>
              summarise(Folkmängd_gr = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |> 
              rename(region = region_gr), 
            by = c("år", "kön", "ålder")
  ) |> 
  mutate(Folkmängd = Folkmängd_riket - Folkmängd_gr) |> 
  select(region = region.x, ålder, kön, år, Folkmängd)

# Kombinera övriga riket och GR exkl Göteborg
startbefolkning_riket_gr <- startbefolkning_riket|> 
  bind_rows(demografisk_statistik$folkmangd_gr |> 
              filter(region != "Göteborg") |> 
              mutate(region_gr = "Göteborgsregionen") |> 
              group_by(region_gr, kön, ålder, år) |>
              summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |> 
              rename(region = region_gr)
  ) |> 
  mutate(
    alder_num = extrahera_alder_numerisk(ålder),
    år_num = as.numeric(år),
    alder_start = alder_num + 1,
    år_start = år_num + 1,
    alder_start = if_else(alder_start == 101, 100, alder_start), 
    aldersgrupp_aldre = skapa_aldersgrupper_yngre_aldre(alder_start), 
  ) |>
  select(
    region,
    alder_num = alder_start,
    kön,
    år = år_start,
    aldersgrupp_aldre, 
    startbefolkning = Folkmängd
  ) |> 
  filter(alder_num >= YNGRE_ALDRE_GRANS) |> 
  mutate(alder_grupp = case_when(
    aldersgrupp_aldre %in% c("55-59 år", "60-64 år") ~ "55-64 år",
    aldersgrupp_aldre %in% c("65-69 år", "70-74 år", "75-79 år") ~ "65-79 år",
    TRUE ~ "80+ år"
  )) |> 
  group_by(region, år, alder_grupp) |> 
  summarise(startbefolkning = sum(startbefolkning, na.rm = TRUE), .groups = "drop")

# Beräkna inflyttningsriskerna per flyttyp
inflyttrisk_gr_riket <- flyttyp_inflytt |> 
  mutate(alder_grupp = case_when(
    Ålder %in% c("55-59 år", "60-64 år") ~ "55-64 år",
    Ålder %in% c("65-69 år", "70-74 år", "75-79 år") ~ "65-79 år",
    TRUE ~ "80+ år"
  )) |> 
  rename(flyttyp = Flyttyp, 
         år = År, 
         inflytt = NoContent) |>
  mutate(region = case_when(
    flyttyp == "Inflytt från kommun i GR" ~ "Göteborgsregionen", 
    TRUE ~ "Riket"
  )) |> 
  group_by(region, år, alder_grupp) |> 
  summarise(inflytt = sum(inflytt, na.rm = TRUE), .groups = "drop") |> 
  left_join(startbefolkning_riket_gr,
            by = c("region", "år", "alder_grupp")) |> 
  mutate(inflyttrisk = inflytt / startbefolkning * 100)


# HUSHÅLLSTYP ----
# =============================================================================

hushallstyp_aldre <- demografisk_statistik$hushallstyp |>
  mutate(
    alder_num = extrahera_alder_numerisk(ålder),
    aldersgrupp_aldre = skapa_aldersgrupper_aldre(alder_num),
    hushallstyp_kategori = kategorisera_hushallstyp(hushållstyp),
    år_num = as.numeric(år)
  ) |>
  group_by(region, år_num, aldersgrupp_aldre, kön, hushallstyp_kategori) |>
  summarise(antal = sum(`Antal personer`, na.rm = TRUE), .groups = "drop") |>
  group_by(region, år_num, kön, aldersgrupp_aldre) |>
  mutate(
    totalt = sum(antal),
    andel = antal / totalt
  ) |>
  ungroup() |>
  rename(år = år_num)


# PROGNOS MELLANOMRÅDE ----
# =============================================================================

# Senaste historiska året per mellanområde
befolkning_mellanomrade_senaste_ar <- demografisk_statistik$bef_mo |>
  mutate(
    alder_num = extrahera_alder_numerisk(Ålder),
    år_num = as.numeric(År),
    alder_num = if_else(alder_num == 100, 99, alder_num)
  ) |>
  filter(år_num == max(år_num), alder_num >= ALDER_ALDRE_GRANS) |>
  group_by(Område, år_num) |>
  summarise(antal_senaste_ar = sum(Folkmängd, na.rm = TRUE), .groups = "drop")

# Senaste prognosåret per mellanområde
prognos_mellanomrade_senaste_ar <- demografisk_statistik$pr_gbg_mo |>
  mutate(
    alder_num = extrahera_alder_numerisk(Ålder),
    år_num = as.numeric(Prognosår)
  ) |>
  filter(år_num == max(år_num), alder_num >= ALDER_ALDRE_GRANS) |>
  group_by(Område, år_num) |>
  summarise(antal_senaste_prognosar = sum(NoContent, na.rm = TRUE), .groups = "drop")

# Jämför prognos mot senaste året
prognos_mellanomrade_jamforelse <- prognos_mellanomrade_senaste_ar |>
  left_join(
    befolkning_mellanomrade_senaste_ar,
    by = "Område",
    suffix = c("_prognos", "_historisk")
  ) |>
  mutate(
    skillnad_antal = antal_senaste_prognosar - antal_senaste_ar,
    skillnad_procent = skillnad_antal / antal_senaste_ar
  )


# HUR BOR DOM ÄLDRE ----
# =============================================================================

# Boendeform över tid
boende_utveckling_over_tid <- aldre_boende_individdata |>
  filter(!is.na(bost_kat)) |>
  mutate(aldersgrupp_aldre = skapa_aldersgrupper_aldre(alder)) |>
  group_by(ar, aldersgrupp_aldre, bost_kat) |>
  summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
  group_by(ar, aldersgrupp_aldre) |>
  mutate(
    totalt = sum(antal, na.rm = TRUE),
    andel = antal / totalt
  ) |>
  ungroup()

# Boendeform per mellanområde (senaste året)
boende_per_mellanomrade <- aldre_boende_individdata |>
  filter(!is.na(bost_kat), ar == 2024) |>
  mutate(aldersgrupp_aldre = skapa_aldersgrupper_aldre(alder)) |>
  group_by(aldersgrupp_aldre, mellanområde, bost_kat) |>
  summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop") |>
  group_by(mellanområde, aldersgrupp_aldre) |>
  mutate(
    totalt = sum(antal, na.rm = TRUE),
    andel = antal / totalt
  ) |>
  ungroup()


# SMÅSHUS GÖTEBORG OCH GR
# =============================================================================
gbg_smahus_totalt <- gbg_smahus |> 
  group_by(ar, alder_grupp) |> 
  summarise(antal = sum(antal, na.rm = TRUE), 
            antal_totalt = sum(antal_totalt, na.rm = TRUE), .groups = "drop") |> 
  mutate(andel_aldre_smahus = antal / antal_totalt * 100)

gbg_smahus_mo <- gbg_smahus |> 
  mutate(andel = antal / antal_totalt * 100)

gr_smahus_andel <- gr_smahus |> 
  mutate(andel = antal / antal_totalt * 100)

bef_mo <- demografisk_statistik$bef_mo

# EXPORTERA BEARBETAD DATA ----
# =============================================================================

bearbetade_data_tabeller <- list(
  prognos_historisk_aldre = prognos_historisk_aldre,
  prognos_historisk_alder_gr = prognos_historisk_alder_gr, 
  befolkning_aldersgrupp_10ar = befolkning_aldersgrupp_10ar,
  folkokning_aldre_prognos_historisk = folkokning_aldre_prognos_historisk,
  befolkning_start_aldersgrupp = befolkning_start_aldersgrupp,
  demografisk_forsorjningskvot = demografisk_forsorjningskvot,
  demografisk_forsorjningskvot_kommuner = demografisk_forsorjningskvot_kommuner, 
  doda_per_aldersgrupp_5ar = doda_per_aldersgrupp_5ar,
  dodsrisker_prognos_historisk_riket = dodsrisker_prognos_historisk_riket,
  andel_flytt_aldre = andel_flytt_aldre,
  hushallstyp_aldre = hushallstyp_aldre,
  prognos_mellanomrade_jamforelse = prognos_mellanomrade_jamforelse,
  boende_utveckling_over_tid = boende_utveckling_over_tid,
  boende_per_mellanomrade = boende_per_mellanomrade, 
  bef_mo = bef_mo, 
  gbg_smahus_totalt = gbg_smahus_totalt, 
  gbg_smahus_mo = gbg_smahus_mo, 
  gr_smahus_andel = gr_smahus_andel, 
  inflyttrisk_gr_riket = inflyttrisk_gr_riket 
)

saveRDS(bearbetade_data_tabeller, here("output", "data_tabeller.rds"))

# =============================================================================
# KONFIGURATION - ÄLDRESTUDIEN
# =============================================================================

# ÅRTAL ----
AR_HISTORISK_START <- 1968
AR_HISTORISK_SLUT <- 2025
AR_PROGNOS_START <- 2026
AR_PROGNOS_SLUT <- 2050

# ÅLDERSGRÄNSER ----
YNGRE_ALDRE_GRANS <- 55
ALDER_ALDRE_GRANS <- 65
ALDER_MYCKET_ALDRE_GRANS <- 80
ALDER_ARBETSFOR_MAX <- 19
ALDER_ARBETSFOR_MIN <- 20
ALDER_ARBETSFOR_MAX_OVER <- 64

# ÅLDERSGRUPPER FÖR ÄLDRE (65+) ----
ALDERSGRUPPER_ALDRE <- c(
  "65-69 år" = "65-69 år",
  "70-74 år" = "70-74 år",
  "75-79 år" = "75-79 år",
  "80-84 år" = "80-84 år",
  "85-89 år" = "85-89 år",
  "90+ år" = "90+ år"
)

# ÅLDERSGRUPPER FÖR HELA BEFOLKNINGEN (5-årsintervall) ----
ALDERSGRUPPER_5AR <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24",
  "25-29", "30-34", "35-39", "40-44", "45-49",
  "50-54", "55-59", "60-64", "65-69", "70-74",
  "75-79", "80-84", "85-89", "90-94", "95+"
)

# REGIONKODER ----
REGION_GOTEBORG <- "1480"
REGION_STOCKHOLM <- "0180"
REGION_MALMO <- "1280"
REGION_RIKET <- "00"

REGIONER_STORSTADER <- c(
  REGION_GOTEBORG,
  REGION_STOCKHOLM,
  REGION_MALMO
)

# REGIONKODER GÖTEBORGSREGIONEN ----
GR_KOMMUNER <- c("1440", "1489", "1480", "1401", "1384", "1482", "1441", 
                 "1462", "1481", "1402", "1415", "1419", "1407")

# HUSHÅLLSTYPER ----
HUSHALLSTYP_ENSAMBOENDE <- c("ensamstående utan barn", "övriga hushåll utan barn")
HUSHALLSTYP_LABELS <- c(
  "Ensamboende",
  "Boende med partner och/eller med barn",
  "Uppgift saknas"
)

# BEFOLKNINGSKATEGORIER ----
BEFOLKNING_KATEGORIER <- c(
  "Äldre" = "Äldre",
  "Arbetsfӧr befolkning" = "Arbetsfӧr befolkning",
  "Yngre" = "Yngre"
)
# =============================================================================
# HJÄLPFUNKTIONER FÖR DATABEARBETNING - ÄLDRESTUDIEN
# =============================================================================
# 
# Dessa funktioner används 3+ gånger i bearbetningsskriptet och är därför
# utbrutna som återanvändbara funktioner enligt kodprinciperna.
# =============================================================================

#' Extrahera numerisk ålder från textformat
#'
#' @param alder_text Vektor med ålder i textformat (t.ex. "65 år", "100+ år")
#' @return Numerisk vektor med åldrar
#' @examples
#' extrahera_alder_numerisk(c("65 år", "70 år", "100+ år"))
extrahera_alder_numerisk <- function(alder_text) {
  gsub("[-+]? år$", "", alder_text) |> 
    as.numeric()
}


#' Skapa åldersgrupper för äldre befolkning (65+)
#'
#' Skapar standardiserade åldersgrupper för befolkning 65 år och äldre.
#' Åldrar under 65 returnerar NA.
#'
#' @param alder_num Numerisk ålder
#' @return Character vektor med åldersgrupper, NA för åldrar < 65
#' @examples
#' skapa_aldersgrupper_aldre(c(50, 65, 75, 85, 95))
#' # [1] NA        "65-69 år" "75-79 år" "85-89 år" "90+ år"
skapa_aldersgrupper_aldre <- function(alder_num) {
  case_when(
    alder_num < 65 ~ NA_character_,
    alder_num >= 65 & alder_num <= 69 ~ "65-69 år",
    alder_num >= 70 & alder_num <= 74 ~ "70-74 år",
    alder_num >= 75 & alder_num <= 79 ~ "75-79 år",
    alder_num >= 80 & alder_num <= 84 ~ "80-84 år",
    alder_num >= 85 & alder_num <= 89 ~ "85-89 år",
    TRUE ~ "90+ år"
  )
}


#' Skapa åldersgrupper för den yngre äldre befolkning (55+)
#'
#' Skapar standardiserade åldersgrupper för befolkning 55 år och äldre.
#' Åldrar under 55 returnerar NA.
#'
#' @param alder_num Numerisk ålder
#' @return Character vektor med åldersgrupper, NA för åldrar < 55
#' @examples
#' skapa_aldersgrupper_aldre(c(50, 65, 75, 85, 95))
#' # [1] NA        "65-69 år" "75-79 år" "85-89 år" "90+ år"
skapa_aldersgrupper_yngre_aldre <- function(alder_num) {
  case_when(
    alder_num < 55 ~ NA_character_,
    alder_num >= 55 & alder_num <= 59 ~ "55-59 år", 
    alder_num >= 60 & alder_num <= 64 ~ "60-64 år", 
    alder_num >= 65 & alder_num <= 69 ~ "65-69 år",
    alder_num >= 70 & alder_num <= 74 ~ "70-74 år",
    alder_num >= 75 & alder_num <= 79 ~ "75-79 år",
    alder_num >= 80 & alder_num <= 84 ~ "80-84 år",
    alder_num >= 85 & alder_num <= 89 ~ "85-89 år",
    TRUE ~ "90+ år"
  )
}

#' Skapa åldersgrupper för hela befolkningen (10-årsintervall)
#'
#' @param alder_num Numerisk ålder
#' @return Factor med åldersgrupper
#' @examples
#' skapa_aldersgrupper_10ar(c(5, 15, 25, 75))
skapa_aldersgrupper_10ar <- function(alder_num) {
  aldersgrupp <- case_when(
    alder_num <= 5 ~ "0-5",
    alder_num <= 10 ~ "6-10",
    alder_num <= 15 ~ "11-15",
    alder_num <= 20 ~ "16-20",
    alder_num <= 25 ~ "21-25",
    alder_num <= 30 ~ "26-30",
    alder_num <= 35 ~ "31-35",
    alder_num <= 40 ~ "36-40",
    alder_num <= 45 ~ "41-45",
    alder_num <= 50 ~ "46-50",
    alder_num <= 55 ~ "51-55",
    alder_num <= 60 ~ "56-60",
    alder_num <= 65 ~ "61-65",
    alder_num <= 70 ~ "66-70",
    alder_num <= 75 ~ "71-75",
    alder_num <= 80 ~ "76-80",
    alder_num <= 85 ~ "81-85",
    alder_num <= 90 ~ "86-90",
    alder_num <= 95 ~ "91-95",
    TRUE ~ "96+"
  )
  
  factor(
    aldersgrupp,
    levels = c(
      "0-5", "6-10", "11-15", "16-20",
      "21-25", "26-30", "31-35", "36-40",
      "41-45", "46-50", "51-55", "56-60",
      "61-65", "66-70", "71-75", "76-80",
      "81-85", "86-90", "91-95", "96+"
    )
  )
}


#' Skapa åldersgrupper för hela befolkningen (5-årsintervall)
#'
#' @param alder_num Numerisk ålder
#' @return Factor med åldersgrupper
#' @examples
#' skapa_aldersgrupper_5ar(c(5, 15, 25, 75))
skapa_aldersgrupper_5ar <- function(alder_num) {
  cut(
    alder_num,
    breaks = c(seq(0, 95, by = 5), Inf),
    right = FALSE,
    labels = c(
      "0-4", "5-9", "10-14", "15-19", "20-24",
      "25-29", "30-34", "35-39", "40-44", "45-49",
      "50-54", "55-59", "60-64", "65-69", "70-74",
      "75-79", "80-84", "85-89", "90-94", "95+"
    )
  )
}


#' Kategorisera befolkning efter ålder
#'
#' @param alder_num Numerisk ålder
#' @param aldre_grans Åldersgräns för äldre (default 65)
#' @param yngre_max Max ålder för yngre (default 19)
#' @return Character vektor med befolkningskategori
#' @examples
#' kategorisera_befolkning(c(15, 35, 70))
kategorisera_befolkning <- function(alder_num, 
                                    aldre_grans = 65, 
                                    yngre_max = 19) {
  case_when(
    alder_num >= aldre_grans ~ "Äldre",
    alder_num <= yngre_max ~ "Yngre",
    TRUE ~ "Arbetsfӧr befolkning"
  )
}


#' Kategorisera hushållstyp
#'
#' @param hushallstyp Vektor med hushållstyp från SCB
#' @return Character vektor med kategoriserad hushållstyp
#' @examples
#' kategorisera_hushallstyp(c("ensamstående utan barn", "samboende med barn"))
kategorisera_hushallstyp <- function(hushallstyp) {
  ensamboende_typer <- c("ensamstående utan barn", "övriga hushåll utan barn")
  
  case_when(
    hushallstyp %in% ensamboende_typer ~ "Ensamboende",
    hushallstyp == "uppgift saknas" ~ "Uppgift saknas",
    TRUE ~ "Boende med partner och/eller med barn"
  )
}
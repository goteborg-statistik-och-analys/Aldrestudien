# =============================================================================
# ÄLDRESTUDIEN
# Del 1, hämta data
# =============================================================================

# Detta script är till för att analysera den äldre befolkningen i Göteborg i jämförelse med Riket och övriga storstäder
#

# Frågeställningar: 
# Hur ser den äldre befolkningens utveckling ut i prognos och historisk i Göteborg, storstäder och riket?
# Hur är andelen i prognos jämförelse med historisk?
# Vad förklarar ökningen av antalet äldre?
# Hur ser flyttmönstrarna ut i de olika kohorterna? Här kollar vi hur flyttinsensiva kohorterna är i åldrarna 65-74 år
# Hur bor dom äldre?
# I vilka områden i Göteborg kommer dom äldre att öka som mest?
# Hur ser hushållsammansättningen ut i Göteborg bland dom äldre, hur många är ensamboende och hur har det förändrats över tid?
# Vart flyttar man ifrån?


# Ladda in masterscript
library(tidyverse)
library(pxweb)

# Skapar mapparna i projektet
dir.create("input", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
dir.create("R", showWarnings = FALSE)


# HÄMTA DATA
# =============================================================================

hamta_data <- function(hamta_ny = TRUE) {
  
  if(hamta_ny) {
    message("Hämtar ny statistik från statistikdatabasen...")
  
    # Antal år vi ska hämta för
    AR <- 1968:2024
    ar <- as.character(AR)
    ar_ckm <- as.character(2025)
    
    # År för prognos
    PROGNOS_AR <- 2026:2050
    prognos_ar <- as.character(PROGNOS_AR)
    
    # Ålder, ta bort totalt
    ALDER <- 0:100
    alder <- if_else(as.character(ALDER) == "100", "100+", as.character(ALDER))
    
    # Ålder för den äldre befolkningen
    YNGRE_ALDRE <- 55:100
    ALDER_ALDRE <- 60:100
    alder_aldre <- if_else(as.character(ALDER_ALDRE) == "100", "100+", as.character(ALDER_ALDRE))
    yngre_aldre <- if_else(as.character(YNGRE_ALDRE) == "100", "100+", as.character(YNGRE_ALDRE))
    
    # Ålder CKM
    alder_ckm <- c(as.character(0:99), "100+1")
    yngre_aldre_ckm <- c(as.character(55:99), "100+1")
    
    # Kommunkoder och koden för riket
    # Vi hämtar Malmö, Göteborg, Stockholm och Riket
    gr_kommuner_kod <- c("1440", "1489", "1480", "1401", "1384", "1482", "1441", 
                         "1462", "1481", "1402", "1415", "1419", "1407")
    region_kod <- c("0180", "1280", "00", "1480")
    region_kod_st_ma_gr <- c("0180", "1280", gr_kommuner_kod)
    
    
    # SPECIFICERA VILKA TABELLER VI SKA HÄMTA FRÅN SCB --------
    
    # CKM TABELLER: 
    
    # Hämta folkmängden CKM
    pxweb_query_list <-
      list(
        "Region" = region_kod, 
        "Alder" = alder_ckm, 
        "Kon" = c("*"), 
        "Civilstand" = c("SC"),
        "Tid" = c("*"), 
        "ContentsCode" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningCKM",
        query = pxweb_query_list
      )
    
    folkmangd_ckm <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    # Hämta folkmängd för GR kommunerna CKM
    pxweb_query_list <-
      list(
        "Region" = gr_kommuner_kod, 
        "Alder" = alder_ckm, 
        "Kon" = c("*"), 
        "Civilstand" = c("SC"),
        "Tid" = ar_ckm, 
        "ContentsCode" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningCKM",
        query = pxweb_query_list
      )
    
    folkmangd_gr_ckm <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    # Hamta antalet döda CKM
    pxweb_query_list <-
      list(
        "Region" = region_kod, 
        "Alder" = alder_ckm, 
        "Kon" = c("*"), 
        "Tid" = ar_ckm, 
        "ContentsCode" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101I/DodaFodelsearKCKM",
        query = pxweb_query_list
      )
    
    doda_ckm <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    # Hämta flyttningar
    pxweb_query_list <-
      list(
        "Region" = region_kod, 
        "Alder" = yngre_aldre_ckm, 
        "Fodelseregion" = "samt",
        "Kon" = c("*"), 
        "Tid" = ar_ckm, 
        "ContentsCode" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/FlyttFodRegCKM",
        query = pxweb_query_list
      )
    
    flyttningar_ckm <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    # HISTORISK STATISTIK (EJ CKM) ---------------
    
    # Hämta folkmängden 
    pxweb_query_list <-
      list(
        "Region" = region_kod, 
        "Alder" = alder, 
        "Kon" = c("*"), 
        "Tid" = c("*"), 
        "ContentsCode" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy",
        query = pxweb_query_list
      )
    
    folkmangd <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    # Hämta folkmängd för GR kommunerna
    pxweb_query_list <-
      list(
        "Region" = gr_kommuner_kod, 
        "Alder" = alder, 
        "Kon" = c("*"), 
        "Tid" = c("*"), 
        "ContentsCode" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy",
        query = pxweb_query_list
      )
    
    folkmangd_gr <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    # Hamta antalet döda
    pxweb_query_list <-
      list(
        "Region" = region_kod, 
        "Alder" = alder, 
        "Kon" = c("*"), 
        "Tid" = ar, 
        "ContentsCode" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101I/DodaFodelsearK",
        query = pxweb_query_list
      )
    
    doda <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    # Hämta flyttningar
    pxweb_query_list <-
      list(
        "Region" = region_kod, 
        "Alder" = yngre_aldre, 
        "Kon" = c("*"), 
        "Tid" = c("*"), 
        "ContentsCode" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97",
        query = pxweb_query_list
      )
    
    flyttningar <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    # Hämta hushållstyp
    pxweb_query_list <-
      list(
        "Region" = region_kod, 
        "Alder" = alder_aldre, 
        "Kon" = c("*"), 
        "Tid" = c("*"), 
        "Hushallstyp" = c("*"),
        "ContentsCode" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101S/HushallT04kon",
        query = pxweb_query_list
      )
    
    hushallstyp <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    
    # Hämta befolkningsprognosen för riket
    pxweb_query_list <-
      list(
        "Alder" = c("*"), 
        "Kon" = c("*"), 
        "Tid" = prognos_ar, 
        "ContentsCode" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0401/BE0401A/BefolkprognRevNb",
        query = pxweb_query_list
      )
    
    pr_riket <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    
    # Hämta dödlighetsutvecklingen i prognosen för Riket
    pxweb_query_list <-
      list(
        "Alder" = c("*"),
        "Fodelseregion" = c("90"), 
        "Kon" = c("*"), 
        "Tid" = prognos_ar, 
        "ContentsCode" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0401/BE0401D/BefProgDodstalNb",
        query = pxweb_query_list
      )
    
    pr_riket_dodsrisker <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    
    # Hämta befolkningsprognosen för Stockholms stad och Malmö stad från SCB
    pxweb_query_list <-
      list(
        "Region" = region_kod_st_ma_gr,
        "Alder" = c("*"), 
        "Kon" = c("*"), 
        "Tid" = prognos_ar, 
        "ContentsCode" = "000004LG"
      )
    
    px_data <-
      pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0401/BE0401A/BefProgOsiktRegN",
        query = pxweb_query_list
      )
    
    pr_st_ma_gr <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    
    # Hämta befolkningsprognosen för samtliga kommuner för ett år
    # Detta för att beräkna försörjningskvoten för övriga kommuner
    pxweb_query_list <-
      list(
        "Region" = c("*"),
        "Alder" = c("*"), 
        "Kon" = c("*"), 
        "Tid" = c("2032", "2050"), 
        "ContentsCode" = "000004LG"
      )
    
    px_data <-
      pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0401/BE0401A/BefProgOsiktRegN",
        query = pxweb_query_list
      )
    
    pr_alla_kommuner <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    # HÄMTA STATISTIK FRÅN STATISTIKDATABASEN I GÖTEBORG ----------
    
    # Ålder i Göteborg
    alder_aldre_gbg <- if_else(as.character(ALDER_ALDRE) == "100", "100- år", paste(as.character(ALDER_ALDRE), "år"))

    
    # Hämta befolkningsprognosen för Göteborg
    pxweb_query_list <-
      list(
        "Ålder" = c("*"), 
        "Kön" = c("*"), 
        "Prognosår" = prognos_ar
      )
    
    px_data <-
      pxweb_get(
        url = "https://statistikdatabas.goteborg.se/api/v1/sv/1. Göteborg och dess delområden/Kommun/Befolkning/Befolkningsprognos/10_Kommunprognos_2026.px",
        query = pxweb_query_list
      )
    
    pr_gbg <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    # Hämta befolkningsprognosen för mellanområden
    pxweb_query_list <-
      list(
        "Område" = c("*"), 
        "Ålder" = c("*"), 
        "Prognosår" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://statistikdatabas.goteborg.se/api/v1/sv/1. Göteborg och dess delområden/Mellanområden 2021-/Befolkning/Befolkningsprognos/14_PrognosMO21.px",
        query = pxweb_query_list
      )
    
    pr_gbg_mo <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    # Hämta flyttmönster per mellanområden i Göteborg
    pxweb_query_list <-
      list(
        "Mellanområde 2021-" = c("*"), 
        "Ålder" = c("65-74 år", "75- år"), 
        "År" = c("*"), 
        "Flyttyp" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://statistikdatabas.goteborg.se/api/v1/sv/1.%20G%C3%B6teborg%20och%20dess%20delomr%C3%A5den/Mellanomr%C3%A5den%202021-/Befolkning/Flyttningar/20_MO21_Flytt_Alla.px",
        query = pxweb_query_list
      )
    
    pr_mo_flytt <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    # Hämta folkmänd per mellanområde
    pxweb_query_list <-
      list(
        "Område" = c("*"), 
        "Ålder" = c("*"), 
        "År" = c("*")
      )
    
    px_data <-
      pxweb_get(
        url = "https://statistikdatabas.goteborg.se/api/v1/sv/1.%20G%C3%B6teborg%20och%20dess%20delomr%C3%A5den/Mellanomr%C3%A5den%202021-/Befolkning/Folkm%C3%A4ngd/Folkm%C3%A4ngd%20hel%C3%A5r/10_FolkmHelar_MO21.px",
        query = pxweb_query_list
      )
    
    bef_mo <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    
    # Kombinera CKM tabellerna med de historiska tabellerna ----------------
    
    # Folkmängd 
    folkmangd_hist_ckm <- folkmangd_ckm |> 
      select(-civilstånd) |> 
      filter(kön != "totalt, samtliga män och kvinnor") |> 
      bind_rows(folkmangd) |> 
      arrange(region, år, ålder, kön)
    
    # Folkmängd GR
    folkmangd_hist_gr_ckm <- folkmangd_gr_ckm |> 
      select(-civilstånd) |> 
      filter(kön != "totalt, samtliga män och kvinnor") |> 
      bind_rows(folkmangd_gr) |> 
      arrange(region, år, ålder, kön)
    
    # Döda
    doda_hist_ckm <- doda_ckm |> 
      filter(kön != "totalt, samtliga män och kvinnor") |> 
      bind_rows(doda) |> 
      arrange(region, år, ålder, kön)
    
    # Flyttningar
    flyttningar_hist_ckm <- flyttningar_ckm |> 
      select(region, ålder, kön, år, inflyttningar = `Samtliga inflyttningar`, utflyttningar = `Samtliga utflyttningar`) |> 
      filter(kön != "totalt, samtliga män och kvinnor") |> 
      bind_rows(flyttningar |> 
                  select(region, ålder, kön, år, inflyttningar = Inflyttningar, utflyttningar = Utflyttningar)) |> 
      arrange(region, år, ålder, kön)
    
    # Lägg till alla tabeller i en lista för enklare hantering 
    data <- list(
      folkmangd = folkmangd_hist_ckm, 
      folkmangd_gr = folkmangd_hist_gr_ckm,
      doda = doda_hist_ckm, 
      flyttningar = flyttningar_hist_ckm, 
      hushallstyp = hushallstyp, 
      pr_riket = pr_riket, 
      pr_st_ma_gr = pr_st_ma_gr,
      pr_alla_kommuner = pr_alla_kommuner,
      pr_riket_dodsrisker = pr_riket_dodsrisker, 
      pr_gbg = pr_gbg, 
      pr_gbg_mo = pr_gbg_mo, 
      pr_mo_flytt = pr_mo_flytt,
      bef_mo = bef_mo
    )
    
    # Spara listan i RDS format
    saveRDS(data, "input/demografisk_statistik.rds")
    return(data)
  
  }
  
  # Om hamta_ny = FALSE så hämtas datan från mappen input istället för via API
  else {
    
    message("Läser in sparad statistik...")
    data <- readRDS("input/demografisk_statistik.rds")
    message("Inläsning av sparad statistik lyckades")
    return(data)
    
  }
  
}


# Hämta statistiken
demografisk_statistik <- hamta_data(hamta_ny = FALSE)



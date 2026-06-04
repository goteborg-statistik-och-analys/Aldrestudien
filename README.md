# Äldrestudien

> Den äldre befolkningen förväntas öka snabbt under de kommande åren, både nationellt och i Göteborg. Mot den bakgrunden har vi fördjupat oss i gruppen för att besvara frågor om vad som driver ökningen, hur äldre bor i dag och i vilka delar av staden ökningen väntas bli störst.

## Om projektet

Äldrestudien analyserar den äldre befolkningens utveckling i Göteborg, med jämförelser mot riket och andra storstäder. Studien omfattar historisk utveckling samt prognoser fram till 2050.

Rapporten är byggd i Quarto med en anpassad rapportmall och följer Göteborgs Stads grafiska profil avseende färger, typografi och visualiseringar.

## Källor

Studien bygger på data från två huvudkällor:

**SCB:s öppna statistikdatabas**
- Folkmängd, döda och flyttningar för Göteborg, Stockholm, Malmö och riket
- Hushållssammansättning för den äldre befolkningen
- Riksprognosen (beräknad 2026) samt kommunprognoser för Stockholm och Malmö (beräknade 2024)

**Göteborgs Stads statistikdatabas**
- Folkmängd och flyttningar per mellanområde i Göteborg
- Befolkningsprognos för Göteborg och dess mellanområden (beräknad 2026, framtagen av Statistik och analys, Stadsledningskontoret)

**Individdatabasen**
- Statistik över hur den äldre befolkningen bor, uppdelat på boendeform och område. Senaste tillgängliga år är 2024.

All statistik hämtas via öppna API:er och bearbetas i R.

## Struktur

```
Äldrestudien/
├── R/
│   ├── 1_hamta_data.R              # Hämtar data från SCB och Göteborgs statistikdatabas
│   ├── 2_bearbeta_data.R           # Bearbetar och sammanställer data
│   └── functions/
│       ├── data_utils.R            # Återanvändbara funktioner för databearbetning
│       ├── settings.R              # Konfiguration (årtal, åldersgränser, regionkoder m.m.)
│       └── visualisering_
│           interaktiva_funktioner.R # Funktioner för interaktiva diagram
├── images/                         # Bilder som används i rapporten
├── Rapport äldrestudien.qmd        # Rapporten
└── styles.css                      # Anpassad styling enligt Göteborgs grafiska profil
```

## Så här kör du projektet

Kör skripten i följande ordning innan rapporten renderas:

1. `R/1_hamta_data.R` – hämtar statistik från SCB och Göteborgs statistikdatabas
2. `R/2_bearbeta_data.R` – bearbetar datan och sparar tabeller till `output/`
3. Rendera `Rapport äldrestudien.qmd` i RStudio

## Producerad av

**Statistik och analys, Stadsledningskontoret, Göteborgs Stad**

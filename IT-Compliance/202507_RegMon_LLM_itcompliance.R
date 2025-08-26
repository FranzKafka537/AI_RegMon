# 202507_RegMon_LLM.R - Angepasst für OpenRouter.ai API und Brevo API (Tagesanalyse-Fokus)

library(httr)
library(dplyr)
library(lubridate)
library(tidyRSS)
library(jsonlite)
library(stringr)
library(readr) 

# ===============================================
# BREVO E-MAIL-FUNKTION
# ===============================================

brevo_email_send <- function(email_body, subject, from_email, to_email_string) {
  
  tryCatch({
    # Split the comma-separated string into a vector of email addresses
    to_emails <- str_split(to_email_string, ",\\s*")[[1]]
    
    # Create the list of recipients in the correct JSON format
    to_list <- lapply(to_emails, function(email) list(email = trimws(email)))
    
    # Email data preparation
    email_data <- list(
      sender = list(name = "KI-RegMon Service [BDO]", email = from_email),
      to = to_list, # Die Empfängerliste in korrektem JSON-Format
      subject = subject,
      htmlContent = email_body
    )
    
    # Send email
    response <- httr::POST(
      url = "https://api.brevo.com/v3/smtp/email",
      httr::add_headers(
        "api-key" = Sys.getenv("BREVO_API_KEY"),
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(email_data, auto_unbox = TRUE),
      encode = "raw"
    )
    
    # Check status code and print detailed error if it fails
    if (httr::status_code(response) %in% c(200, 201)) {
      message("Brevo API: SUCCESS")
      return(list(success = TRUE, status_code = httr::status_code(response)))
    } else {
      error_message <- httr::content(response, "text", encoding = "UTF-8")
      message(paste0("Brevo API: FAILED with Status ", httr::status_code(response)))
      message("Brevo API: Fehlermeldung:")
      message(error_message)
      return(list(success = FALSE, status_code = httr::status_code(response), error = error_message))
    }
    
  }, error = function(e) {
    message("Brevo API: An error occurred during the request.")
    message(e$message)
    return(list(success = FALSE, status_code = NA, error = e$message))
  })
}

# ---- Konfiguration ----
historic_file <- "regulatory_news_history_itcompliance.csv"

openrouter_api_key <- Sys.getenv("OPENROUTER_API_KEY") # Liest den Schlüssel aus der Umgebungsvariablen

model_id <- "mistralai/mistral-small-3.2-24b-instruct:free"
api_url <- "https://openrouter.ai/api/v1/chat/completions"

thematic_focus <- "Regulatorische Entwicklungen und neue Vorgaben in den Bereichen Informationssicherheit und IT-Security, insbesondere Cybersicherheit, Datenintegrität und Business Continuity Management, die für Finanzdienstleister (primär Banken) relevant sind. Der Fokus liegt auf der Bewertung der Auswirkungen dieser Regelwerke (wie DORA und dem zukünftigen NIS2-Gesetz) auf Compliance, Risikomanagement, IT-Governance und die operative Umsetzung bei unseren Kunden. Ziel ist es, Beratungsbedarfe und Handlungsempfehlungen für unsere Finanzkunden zu identifizieren. Warnmeldungen der BaFin sind dabei nur interessant, wenn tatsächlich ein Gesetzesvorhaben oder eine Regulatorik direkt damit verknüpft ist."


# ===============================================
# BREVO E-MAIL-KONFIGURATION (VEREINFACHT)
# ===============================================

sender_email <- Sys.getenv("EMAIL_ADDRESS")
empfaengerliste <- Sys.getenv("EMAIL_RECIPIENTS_IT")
brevo_api_key <- Sys.getenv("BREVO_API_KEY")

if (sender_email == "" || empfaengerliste == "" || brevo_api_key == "") {
  warning("E-Mail-Zugangsdaten (EMAIL_ADDRESS, EMAIL_RECIPIENTS_IT oder BREVO_API_KEY) fehlen. E-Mail-Versand wird übersprungen.")
  email_sending_enabled <- FALSE
} else {
  email_sending_enabled <- TRUE
  message("✅ Brevo E-Mail-Konfiguration erfolgreich geladen")
}
# ==============================================================================
# SCHRITT 1: RSS-Feed-Daten abrufen und verwalten
# ==============================================================================

rss_feeds_with_sources <- list(
  list(url = "https://www.eba.europa.eu/news-press/news/rss.xml", source = "EBA"),
  list(url = "https://www.bundesbank.de/service/rss/de/633290/feed.rss", source = "Bundesbank"),
  list(url = "https://www.bundesbank.de/service/rss/de/633278/feed.rss", source = "Bundesbank"),
  list(url = "https://www.bundesbank.de/service/rss/de/800838/feed.rss", source = "Bundesbank"),
  list(url = "https://www.bundesbank.de/service/rss/de/633286/feed.rss", source = "Bundesbank"),
  list(url = "https://www.bundesbank.de/service/rss/de/633302/feed.rss", source = "Bundesbank"),
  list(url = "https://www.bafin.de/DE/Service/TopNavigation/RSS/_function/rssnewsfeed.xml", source = "BaFin"),
  list(url = "https://www.bankingsupervision.europa.eu/rss/press.html", source = "ECB"),
  list(url = "https://www.bankingsupervision.europa.eu/rss/pub.html", source = "ECB"),
  list(url = "https://www.bankingsupervision.europa.eu/rss/speeches.html", source = "ECB"),
  list(url = "https://www.esma.europa.eu/rss.xml", source = "ESMA"),
  list(url = "https://www.bis.org/doclist/all_pressrels.rss", source = "BIS"),
  list(url = "https://www.bmi.bund.de/DE/service/rss-newsfeed/function/rssnewsfeed-neue-inhalte.xml", source = "BMI"),
  list(url = "https://www.bmjv.de/SiteGlobals/Functions/RSSNewsfeed/DE/RSSNewsfeed/RSSNewsfeedGesetzgebungsverfahren.xml", source = "BMJV"),
  list(url = "https://www.bmjv.de/SiteGlobals/Functions/RSSNewsfeed/DE/RSSNewsfeed/RSSNewsfeedPressemitteilungen.xml", source = "BMJV"),
  list(url = "https://www.bundeswirtschaftsministerium.de/SiteGlobals/BMWI/Functions/RSSFeed/DE/RSSFeed-DigitaleWelt.xml", source = "BMWE"),
  list(url = "https://www.bsi.bund.de/SiteGlobals/Functions/RSSFeed/RSSNewsfeed/RSSNewsfeed_Presse_Veranstaltungen.xml", source = "BSI")
  )

# Initialisierung von all_new_feed_items mit korrektem Date-Typ für item_pub_date
all_new_feed_items <- data.frame(
  item_title = character(),
  item_link = character(),
  item_description = character(),
  item_pub_date = as.Date(character()),
  source = character(),
  stringsAsFactors = FALSE
)

message("Starte den Abruf von RSS-Feeds...")

for (feed_info in rss_feeds_with_sources) {
  url <- feed_info$url
  source_name <- feed_info$source
  
  message(paste0("Rufe Feed ab: ", source_name, " (", url, ")"))
  tryCatch({
    feed_data_raw <- tidyfeed(url)
    
    # Sicherstellen, dass die tibble feed_data_raw nicht leer ist
    if (nrow(feed_data_raw) == 0) {
      message(paste0("Feed '", source_name, "' (", url, ") enthält keine Einträge. Überspringe."))
      next
    }
    
    # Sicherstellen, dass alle notwendigen Spalten existieren
    required_cols <- c("item_title", "item_link", "item_description", "item_pub_date")
    
    for (col in required_cols) {
      if (!col %in% names(feed_data_raw)) {
        feed_data_raw[[col]] <- NA_character_
        message(paste0("Spalte '", col, "' fehlte und wurde mit NA-Werten hinzugefügt."))
      }
    }
    
    # Nun kann der Datenrahmen sicher erstellt werden
    current_feed_items <- feed_data_raw %>%
      select(all_of(required_cols)) %>%
      mutate(
        # Korrigieren des Datumsformats und leere Beschreibungen ersetzen
        item_pub_date = suppressWarnings(as.Date(item_pub_date)),
        item_description = if_else(is.na(item_description) | item_description == "", item_title, item_description),
        source = source_name
      ) %>%
      filter(!is.na(item_title) & !is.na(item_link) & !is.na(item_pub_date))
    
    all_new_feed_items <- bind_rows(all_new_feed_items, current_feed_items)
    
  }, error = function(e) {
    # Ändere hier von warning() zu message()
    message(paste0("Fehler beim Abruf oder der Verarbeitung von Feed '", source_name, "' (", url, "): ", e$message))
  })
}

message(paste0("Anzahl der insgesamt abgerufenen RSS-Einträge: ", nrow(all_new_feed_items)))


# Historische Daten laden oder neue, leere erstellen
if (file.exists(historic_file)) {
  df_historic <- read.csv(historic_file, fileEncoding = "UTF-8", sep = ";")
  # Entferne die esg_relevanz Spalte, falls sie existiert
  if ("esg_relevanz" %in% names(df_historic)) {
    df_historic <- select(df_historic, -esg_relevanz)
  }
  # Sicherstellen, dass item_pub_date aus CSV auch als Date geladen wird
  df_historic$item_pub_date <- suppressWarnings(as.Date(df_historic$item_pub_date, tryFormats = c("%Y-%m-%d", "%d.%m.%Y", "%m/%d/%Y")))
  
  message(paste0("Historische Daten aus '", historic_file, "' geladen. ", nrow(df_historic), " Einträge."))
} else {
  df_historic <- data.frame(
    item_title = character(),
    item_link = character(),
    item_description = character(),
    item_pub_date = as.Date(character()),
    source = character(),
    llm_label = character(),
    llm_begruendung = character(),
    stringsAsFactors = FALSE
  )
  message(paste0("Historische Datei '", historic_file, "' nicht gefunden. Eine neue, leere Datei wird erstellt (oder überschrieben)."))
}

# ===============================================
# SPALTEN-BEREINIGUNG UND -ERWEITERUNG
# ===============================================

# Source-Spalte hinzufügen falls nicht vorhanden
if (!"source" %in% names(df_historic)) {
  df_historic$source <- "Unknown"
  message("Source-Spalte zu historischen Daten hinzugefügt")
}

# llm_tagesbewertung entfernen falls vorhanden
if ("llm_tagesbewertung" %in% names(df_historic)) {
  df_historic <- select(df_historic, -llm_tagesbewertung)
  message("llm_tagesbewertung Spalte entfernt")
}

# Kombiniere neue und historische Daten und entferne Duplikate
combined_df <- bind_rows(df_historic, all_new_feed_items) %>%
  distinct(item_link, .keep_all = TRUE) # Duplikate basierend auf dem Link entfernen

df <- combined_df

# Sicherstellen, dass die LLM-Spalten existieren (falls df_historic leer war oder Spalten fehlen)
if(!"llm_label" %in% names(df)) df$llm_label <- NA_character_
if(!"llm_begruendung" %in% names(df)) df$llm_begruendung <- NA_character_
if(!"source" %in% names(df)) df$source <- "Unknown"

message(paste0("Anzahl der Einträge nach Duplikatsentfernung: ", nrow(df)))

# ==============================================================================
# ENDE SCHRITT 1
# ==============================================================================

# Korrigierte bewerte_eintrag Funktion
bewerte_eintrag <- function(title, description) {
  prompt_text <- paste0(
    "Bitte klassifiziere folgenden regulatorischen Newseintrag thematisch für die Bereiche ",
    thematic_focus, ". Gib eine thematische Relevanz (ja/nein) an und begründe diese kurz. Antworte im genauen Format: 'Label: <ja/nein>. Grund: <max. 30 Wörter Erklärung>.'",
    "\n\nTitel: ", title, "\nBeschreibung: ", description
  )
  
  request_body <- list(
    model = model_id,
    messages = list(
      list(role = "user", content = prompt_text)
    ),
    temperature = 0.1,
    max_tokens = 150
  )
  
  json_body <- toJSON(request_body, auto_unbox = TRUE)
  
  resp <- tryCatch({
    POST(
      url = api_url,
      add_headers(
        "Authorization" = paste("Bearer", openrouter_api_key),
        "Content-Type" = "application/json",
        "User-Agent" = "R-LLM-Script/1.0"
      ),
      body = json_body
    )
  }, error = function(e) {
    warning(paste0("Netzwerk- oder HTTP-Fehler beim Senden der Anfrage: ", e$message))
    return(NULL)
  })
  
  if (is.null(resp)) {
    return(list(label="API-Fehler", begruendung="Netzwerk- oder Übertragungsfehler", raw=""))
  }
  
  if (http_status(resp)$category == "Success") {
    # Sichere JSON-Parsing mit mehreren Fallback-Optionen
    raw_response <- content(resp, "text", encoding = "UTF-8")
    
    # Debug: Zeige die rohe Antwort
    # message("DEBUG: Raw response length: ", nchar(raw_response))
    # message("DEBUG: Raw response snippet: ", substr(raw_response, 1, 200))
    
    # Versuche das JSON zu parsen
    response_content <- tryCatch({
      fromJSON(raw_response, simplifyVector = FALSE)
    }, error = function(e) {
      # message("DEBUG: Erster JSON-Parse-Versuch fehlgeschlagen: ", e$message)
      
      # Fallback: Versuche die Antwort zu reparieren, falls sie abgeschnitten ist
      tryCatch({
        # Finde das letzte vollständige JSON-Objekt
        last_brace <- max(gregexpr("\\}", raw_response)[[1]])
        if (last_brace > 0) {
          cleaned_response <- substr(raw_response, 1, last_brace)
          # message("DEBUG: Versuche mit bereinigter Antwort (Länge: ", nchar(cleaned_response), ")")
          fromJSON(cleaned_response, simplifyVector = FALSE)
        } else {
          return(NULL)
        }
      }, error = function(e2) {
        # message("DEBUG: Auch bereinigter JSON-Parse fehlgeschlagen: ", e2$message)
        return(NULL)
      })
    })
    
    if (is.null(response_content)) {
      # Fallback: Versuche Text direkt aus der rohen Antwort zu extrahieren
      # message("DEBUG: Versuche direkte Text-Extraktion aus roher Antwort")
      
      # Suche nach "content":"..." Pattern
      content_match <- regexec('"content"\\s*:\\s*"([^"]*)"', raw_response)
      if (content_match[[1]][1] != -1) {
        extracted_text <- regmatches(raw_response, content_match)[[1]][2]
        # message("DEBUG: Text direkt extrahiert: ", extracted_text)
        
        # Parse Label und Begründung
        label_match <- regexec("Label:\\s*(ja|nein)", extracted_text, ignore.case = TRUE)
        label <- if (label_match[[1]][1] != -1) tolower(regmatches(extracted_text, label_match)[[1]][2]) else "unbekannt"
        
        begruendung_match <- regexec("Grund:\\s*(.+)", extracted_text, ignore.case = TRUE)
        begruendung <- if (begruendung_match[[1]][1] != -1) regmatches(extracted_text, begruendung_match)[[1]][2] else "Parsing-Fehler"
        
        return(list(label=trimws(label), begruendung=trimws(begruendung), raw=extracted_text))
      } else {
        return(list(label="API-Fehler", begruendung="JSON-Parsing fehlgeschlagen", raw=substr(raw_response, 1, 200)))
      }
    }
    
    # Standard JSON-Parsing erfolgreich
    # message("DEBUG: JSON-Parsing erfolgreich")
    
    # Sichere Navigation durch die JSON-Struktur
    text <- NULL
    
    tryCatch({
      if (!is.null(response_content$choices) && length(response_content$choices) > 0) {
        first_choice <- response_content$choices[[1]]
        if (!is.null(first_choice$message) && !is.null(first_choice$message$content)) {
          text <- first_choice$message$content
        }
      }
    }, error = function(e) {
      # message("DEBUG: Fehler beim Navigieren durch JSON-Struktur: ", e$message)
    })
    
    if (is.null(text) || is.na(text) || nchar(text) == 0) {
      return(list(label="API-Fehler", begruendung="Leere Antwort erhalten", raw=raw_response))
    }
    
    # Text-Extraktion
    # message("DEBUG: Extrahierter Text: ", text)
    
    label_match <- regexec("Label:\\s*(ja|nein)", text, ignore.case = TRUE)
    label <- if (label_match[[1]][1] != -1) tolower(regmatches(text, label_match)[[1]][2]) else "unbekannt"
    
    begruendung_match <- regexec("Grund:\\s*(.+)", text, ignore.case = TRUE)
    begruendung <- if (begruendung_match[[1]][1] != -1) regmatches(text, begruendung_match)[[1]][2] else "Keine Begründung gefunden"
    
    label <- trimws(label)
    begruendung <- trimws(begruendung)
    
    return(list(label=label, begruendung=begruendung, raw=text))
    
  } else {
    # HTTP-Fehlerbehandlung
    error_content <- content(resp, "text", encoding = "UTF-8")
    error_message <- if (!is.null(error_content) && nchar(error_content) > 0) error_content else "Unbekannter API-Fehler"
    
    status_code <- resp$status_code
    
    if (status_code == 402) {
      warning(paste0("OpenRouter API-Fehler (", status_code, " - Credits aufgebraucht?): ", error_message))
      return(list(label="Fehler: Tokens", begruendung="Tokens aufgebraucht", raw=error_message))
    } else if (status_code == 429) {
      warning(paste0("OpenRouter API-Fehler (", status_code, " - Rate Limit): ", error_message))
      return(list(label="Fehler: Rate Limit", begruendung="Rate Limit erreicht", raw=error_message))
    } else {
      warning(paste0("OpenRouter API-Fehler (", status_code, "): ", error_message))
      return(list(label="Fehler: API", begruendung=paste0("API-Fehler (", status_code, ")"), raw=error_message))
    }
  }
}

# Filtere Einträge vom heutigen Tag, die noch nicht bewertet sind
heute <- Sys.Date()
neue_eintraege_zu_bewerten <- df %>%
  filter(as.Date(item_pub_date) == heute) %>%
  filter(is.na(llm_label) | llm_label == "")

message(paste0("Heute ist ", heute, ". Einträge im DF von heute: ", 
               sum(as.Date(df$item_pub_date) == heute, na.rm = TRUE)))
message(paste0("Anzahl der neuen/unbewerteten Einträge vom heute, die bewertet werden sollen: ", nrow(neue_eintraege_zu_bewerten)))

# Verarbeite neue Einträge zur LLM-Bewertung (nur die noch unbewerteten vom heutigen Tag)
if(nrow(neue_eintraege_zu_bewerten) > 0) {
  message(paste0("Verarbeite ", nrow(neue_eintraege_zu_bewerten), " neue Einträge (vom ", format(heute, "%d.%m.%Y"), ") zur LLM-Bewertung..."))
  
  for(i in 1:nrow(neue_eintraege_zu_bewerten)) {
    entry_index_in_df <- which(df$item_link == neue_eintraege_zu_bewerten$item_link[i] & df$item_pub_date == as.character(neue_eintraege_zu_bewerten$item_pub_date[i]))
    
    if(length(entry_index_in_df) == 1) { # Sicherstellen, dass der Eintrag eindeutig ist
      titel <- neue_eintraege_zu_bewerten$item_title[i]
      beschreibung <- neue_eintraege_zu_bewerten$item_description[i]
      
      bewertung <- bewerte_eintrag(titel, beschreibung)
      
      df[entry_index_in_df, "llm_label"] <- bewertung$label
      df[entry_index_in_df, "llm_begruendung"] <- bewertung$begruendung
      # df[entry_index_in_df, "llm_raw_response"] <- bewertung$raw # Optional, wenn du die Rohantwort speichern willst
      
      # Kleine Pause, um Rate Limits zu vermeiden
      Sys.sleep(1) 
    }
  }
} else {
  message("Keine neuen Einträge oder unbewertete Einträge vom ", format(heute, "%d.%m.%Y"), " gefunden, die eine LLM-Bewertung benötigen.")
}

# ---- Historische Daten speichern (aktualisiert) ----
# item_pub_date vor dem Speichern wieder in Character umwandeln, um Kompatibilität bei zukünftigem Laden zu gewährleisten
df$item_pub_date <- as.character(df$item_pub_date) 
df <- df %>% arrange(desc(item_pub_date), item_title)
write.csv2(df, file = historic_file, row.names = FALSE, fileEncoding = "UTF-8")
message(paste0("Aktualisierte Daten mit ", nrow(df), " Einträgen in '", historic_file, "' gespeichert."))

# ==============================================================================
# SCHRITT 3: E-Mail-Bericht generieren und versenden 
# ==============================================================================

# --- Manueller Override für den E-Mail-Versand ---
force_email_send_override <- TRUE
include_irrelevant_news_override <- TRUE

bericht_filename <- paste0("Regulierungsmonitor_Tagesbericht_", format(today, "%Y-%m-%d"), ".html")

df$item_pub_date <- as.Date(df$item_pub_date)
today <- Sys.Date()

if (include_irrelevant_news_override) {
  relevante_news_fuer_bericht <- df %>%
    filter(as.Date(item_pub_date) == today) %>%
    filter(!is.na(llm_label) & llm_label != "") %>%
    mutate(
      Datum = format(as.Date(item_pub_date), "%d.%m.%Y"),
      `Relevanz (LLM)` = llm_label,
      `Begründung (LLM)` = llm_begruendung,
      Status = case_when(
        tolower(llm_label) == "ja" ~ "⭐ RELEVANT",
        tolower(llm_label) == "nein" ~ "❌ NICHT RELEVANT",
        TRUE ~ "❓ UNBEKANNT"
      )
    ) %>%
    select(Datum, item_title, item_link, `Relevanz (LLM)`, `Begründung (LLM)`, Status, source) %>%
    arrange(desc(Status))
  
  message(paste0("TEST-MODUS: Alle bewerteten News werden eingeschlossen (", nrow(relevante_news_fuer_bericht), " Einträge)."))
  
} else {
  relevante_news_fuer_bericht <- df %>%
    filter(as.Date(item_pub_date) == today) %>%
    filter(tolower(llm_label) == "ja") %>%
    mutate(
      Datum = format(as.Date(item_pub_date), "%d.%m.%Y"),
      `Relevanz (LLM)` = llm_label,
      `Begründung (LLM)` = llm_begruendung
    ) %>%
    select(Datum, item_title, item_link, `Relevanz (LLM)`, `Begründung (LLM)`, source)
}

# ===============================================
# VEREINFACHTE E-MAIL-BODY ERSTELLUNG
# ===============================================
#

create_simple_email_body <- function() {
  
 # unsichtbare Preheader-Text hinzugefügt
  preheader_text <- '<div style="display:none;font-size:1px;color:#ffffff;line-height:1px;max-height:0px;max-width:0px;opacity:0;overflow:hidden;mso-hide:all;">Ihr tägliches Regulatorik-Update: KI-gestützte Nachrichten von RegMon.</div>'
  
  
  # Header als HTML mit angepasstem Styling
  header_section <- paste0(preheader_text,'
<div style="font-family: \'Trebuchet MS\', Arial, sans-serif; font-size: 14px; max-width: 600px; margin: 0 auto; padding: 20px; background-color: #ffffff;">

<h1 style="color: #2c3e50; font-size: 22px; text-align: center; margin-bottom: 20px;">RegMon-Service: KI-gestütztes Regulatorik-Update</h1>

<div style="height:20px; font-size:1px; line-height:1px; mso-height-rule:exactly;">&nbsp;</div>

', if(include_irrelevant_news_override) {
  '<div style="background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 12px; margin: 15px 0; border-radius: 5px; font-size: 14px;">
  <strong>⚠️ TEST-MODUS AKTIV:</strong> Diese E-Mail enthält sowohl relevante als auch als irrelevant bewertete News zu Testzwecken.
  </div>'
} else {
  ""
}, '

<div style="background-color: #e3f2fd; border: 1px solid #2196f3; padding: 12px; margin: 15px 0; border-radius: 5px; font-size: 14px;">
  <strong>🤖 WICHTIGER HINWEIS:</strong> Die folgenden Bewertungen basieren auf einer LLM-Einwertung (aktuelles Modell: Mistral Small 3.2 24B). Die KI-Bewertungen sind kritisch zu prüfen und dienen ausschließlich als vorbereitende Hilfestellung für Ihre Analyse. Die finale Relevanz-Entscheidung und die Ableitung von Maßnahmen verbleiben in der vollen Verantwortung des zuständigen Fachexperten.
</div>

<p style="font-size: 14px; line-height: 1.5; margin: 15px 0;">Sehr geehrte/r Empfänger,</p>

<p style="font-size: 14px; line-height: 1.5; margin: 15px 0;">dies ist Ihr automatisches Regulatorik-Update für den <strong>', format(today, "%d.%m.%Y"), '</strong>. Sie erhalten diese E-Mail, da neue relevante Informationen zu Ihrem Themenschwerpunkt gefunden wurden.</p>

<p style="font-size: 14px; line-height: 1.5; margin: 15px 0;"><strong>Anzahl verarbeiteter Einträge:</strong> ', sum(as.Date(df$item_pub_date) == today, na.rm = TRUE), '</p>

<p style="font-size: 14px; line-height: 1.5; margin: 15px 0;">Fokus dieses KI-generierten Berichts: IT-Compliance für Finanzdienstleister mit Schwerpunkt auf DORA und NIS2.</p>

<hr style="border: 1px solid #ddd; margin: 25px 0;">
')
  
  # News-Einträge als HTML mit angepasstem Styling
  if (nrow(relevante_news_fuer_bericht) > 0) {
    
    news_items <- ""
    counter <- 1
    
    for (i in 1:nrow(relevante_news_fuer_bericht)) {
      
      # Status-Emoji basierend auf Relevanz
      if ("Status" %in% names(relevante_news_fuer_bericht)) {
        status_emoji <- if (relevante_news_fuer_bericht$Status[i] == "⭐ RELEVANT") {
          "✅ <strong>RELEVANT</strong>"
        } else if (relevante_news_fuer_bericht$Status[i] == "❌ NICHT RELEVANT") {
          "❌ <strong>NICHT RELEVANT</strong>"
        } else {
          "❓ <strong>UNBEKANNT</strong>"
        }
      } else {
        status_emoji <- "✅ <strong>RELEVANT</strong>"
      }
      
      # Einzelner News-Eintrag als HTML mit kleinerer Schrift
      news_items <- paste0(news_items, '
<div style="background-color: #f8f9fa; border-left: 4px solid #007bff; padding: 15px; margin: 15px 0; border-radius: 5px;">

<h4 style="color: #2c3e50; font-size: 16px; margin: 0 0 10px 0; line-height: 1.3;">', counter, '. ', relevante_news_fuer_bericht$item_title[i], '</h4>

<p style="font-size: 13px; margin: 8px 0; line-height: 1.4;"><strong>Status:</strong> ', status_emoji, '</p>
<p style="font-size: 13px; margin: 8px 0; line-height: 1.4;"><strong>Datum:</strong> ', relevante_news_fuer_bericht$Datum[i], '</p>
<p style="font-size: 13px; margin: 8px 0; line-height: 1.4;"><strong>Quelle:</strong> ', relevante_news_fuer_bericht$source[i], '</p>
<p style="font-size: 13px; margin: 8px 0; line-height: 1.4;"><strong>Link:</strong> <a href="', relevante_news_fuer_bericht$item_link[i], '" style="color: #007bff; text-decoration: none;">Zum Artikel</a></p>
<p style="font-size: 13px; margin: 8px 0; line-height: 1.4;"><strong>KI-Bewertung:</strong> ', relevante_news_fuer_bericht$`Begründung (LLM)`[i], '</p>

</div>
')
      counter <- counter + 1
    }
    
    news_section <- paste0('
<h2 style="color: #2c3e50; font-size: 18px; margin: 25px 0 15px 0;">Regulatorische Einträge vom ', format(today, "%d.%m.%Y"), '</h2>

', news_items)
    
  } else {
    news_section <- '
<h2 style="color: #2c3e50; font-size: 18px; margin: 25px 0 15px 0;">Regulatorische Einträge</h2>
<p style="font-size: 14px; line-height: 1.5; font-style: italic;">Heute wurden keine relevanten Einträge gefunden.</p>
'
  }
  
  # Footer als HTML mit kleinerer Schrift
  footer_section <- paste0('

<hr style="border: 1px solid #ddd; margin: 25px 0;">

<p style="font-size: 11px; color: #666; text-align: center; line-height: 1.4; margin: 15px 0;">
<em>Automatisch generiert vom RegMon-Service', 
                           if (include_irrelevant_news_override) ' im <strong>TEST-MODUS</strong>' else '', 
                           '.<br>Bei Fragen wenden Sie sich an das RegMon-Team (Aktuell: Tobi).</em>
</p>

</div>')
  
  # Vollständigen HTML-Body zusammensetzen
  complete_html <- paste0(header_section, news_section, footer_section)
  
  return(complete_html)
}

# E-Mail-Body generieren (vereinfacht)
html_email_body <- create_simple_email_body()

# ===============================================
# E-MAIL-VERSAND (inkl. Retry-Logik)
# ===============================================

if (nrow(relevante_news_fuer_bericht) > 0 || force_email_send_override || include_irrelevant_news_override) {
  
  email_subject <- if (include_irrelevant_news_override) {
    paste0("🤖 RegMon-Service [TEST-MODUS]: Regulatorik-Update - ", format(today, "%d.%m.%Y"))
  } else {
    paste0("🤖 RegMon-Service: Regulatorik-Update - ", format(today, "%d.%m.%Y"))
  }
  
  if (email_sending_enabled) {
    message("📧 Sende E-Mail...")
    
    # --- WIEDERHOLUNGSLOGIK HIER ---
    max_retries <- 3
    retry_delay_seconds <- 10
    
    email_success <- FALSE
    for (i in 1:max_retries) {
      if (i > 1) {
        message(paste0("❌ Versuch ", i - 1, " fehlgeschlagen. Warte ", retry_delay_seconds, " Sekunden und versuche es erneut..."))
        Sys.sleep(retry_delay_seconds)
      }
      
      email_result <- brevo_email_send(
        email_body = html_email_body,
        subject = email_subject,
        from_email = sender_email,
        to_email_string = empfaengerliste
      )
      
      if (email_result$success) {
        email_success <- TRUE
        message("✅ E-Mail erfolgreich versendet!")
        break # Schleife verlassen, wenn erfolgreich
      }
    }
    
    if (!email_success) {
      message("❌ E-Mail-Versand fehlgeschlagen nach ", max_retries, " Versuchen.")
    }
    
  } else {
    message("📧 E-Mail-Versand deaktiviert")
  }
  
} else {
  message("📧 Keine E-Mail nötig - keine relevanten News heute")
}

message("--- Skriptausführung beendet ---")
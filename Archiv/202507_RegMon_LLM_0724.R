# 202507_RegMon_LLM.R - Angepasst für OpenRouter.ai API (Tagesanalyse-Fokus)

library(httr)
library(dplyr)
library(lubridate)
library(tidyRSS)
library(jsonlite)
library(blastula) # blastula hier oben laden, da es später genutzt wird
library(stringr) # Füge stringr hinzu, falls für paste0 oder andere String-Operationen benötigt
library(readr) # Füge readr hinzu, falls für read_csv2/write_csv2 benötigt

# ---- Konfiguration ----
historic_file <- "Regulatory_News_History.csv"

# !!! DEIN OPENROUTER.AI API SCHLÜSSEL HIER EINFÜGEN !!!
# BEACHTE: Der hier angegebene Schlüssel ist ein Beispiel und funktioniert NICHT.
# Bitte durch deinen echten, geheimen Schlüssel ersetzen!
openrouter_api_key <- Sys.getenv("OPENROUTER_API_KEY") # Liest den Schlüssel aus der Umgebungsvariablen

model_id <- "mistralai/mistral-small-3.2-24b-instruct:free"
api_url <- "https://openrouter.ai/api/v1/chat/completions"

thematic_focus <- "Regulatorische Entwicklungen und neue Vorgaben in den Bereichen Informationssicherheit und IT-Security, insbesondere Cybersicherheit, Datenintegrität und Business Continuity Management, die für Finanzdienstleister (primär Banken) relevant sind. Der Fokus liegt auf der Bewertung der Auswirkungen dieser Regelwerke (wie DORA und dem zukünftigen NIS2-Gesetz) auf Compliance, Risikomanagement, IT-Governance und die operative Umsetzung bei unseren Kunden. Ziel ist es, Beratungsbedarfe und Handlungsempfehlungen für unsere Finanzkunden zu identifizieren."

# --- E-Mail Konfiguration ---
# Umgebungsvariablen für E-Mail-Zugangsdaten abrufen
# WICHTIG: Diese Secrets MÜSSEN in GitHub Actions definiert sein!
sender_email <- Sys.getenv("EMAIL_ADDRESS")
email_password <- Sys.getenv("EMAIL_PASSWORD")
smtp_host <- Sys.getenv("SMTP_HOST")
smtp_port <- as.numeric(Sys.getenv("SMTP_PORT")) # Port sollte als numerischer Wert verwendet werden

# Überprüfen, ob die E-Mail-Zugangsdaten vorhanden sind
# Dies dient als Sicherheitsnetz, falls Secrets nicht richtig gesetzt sind
if (sender_email == "" || email_password == "" || smtp_host == "" || is.na(smtp_port)) {
  warning("E-Mail-Zugangsdaten (EMAIL_ADDRESS, EMAIL_PASSWORD, SMTP_HOST oder SMTP_PORT) sind nicht als Umgebungsvariablen gesetzt. E-Mail-Versand wird übersprungen.")
  email_sending_enabled <- FALSE
} else {
  email_sending_enabled <- TRUE
  # Erstelle die SMTP-Anmeldeinformationen mit korrekter Funktion
  # creds_envvar erwartet nur den Namen der Passwort-Umgebungsvariable
  smtp_send_credentials <- blastula::creds_envvar(
    user = sender_email,
    pass_envvar = "EMAIL_PASSWORD",
    host = smtp_host,
    port = smtp_port,
    use_ssl = TRUE
  )
}

# ==============================================================================
# SCHRITT 1: RSS-Feed-Daten abrufen und verwalten
# ==============================================================================

rss_feed_urls <- c(
  "https://www.eba.europa.eu/news-press/news/rss.xml",
  "https://www.bundesbank.de/service/rss/de/633290/feed.rss",
  "https://www.bundesbank.de/service/rss/de/633278/feed.rss",
  "https://www.bundesbank.de/service/rss/de/800838/feed.rss",
  "https://www.bundesbank.de/service/rss/de/633286/feed.rss",
  "https://www.bundesbank.de/service/rss/de/633302/feed.rss",
  "https://www.bafin.de/DE/Service/TopNavigation/RSS/_function/rssnewsfeed.xml",
  "https://www.bankingsupervision.europa.eu/rss/press.html",
  "https://www.bankingsupervision.europa.eu/rss/pub.html",
  "https://www.bankingsupervision.europa.eu/rss/speeches.html",
  "https://www.esma.europa.eu/rss.xml",
  "https://www.bis.org/doclist/all_pressrels.rss"
)

# Initialisierung von all_new_feed_items mit korrektem Date-Typ für item_pub_date
all_new_feed_items <- data.frame(
  item_title = character(),
  item_link = character(),
  item_description = character(),
  item_pub_date = as.Date(character()), # Als leeres Date-Objekt initialisieren
  stringsAsFactors = FALSE
)

message("Starte den Abruf von RSS-Feeds...")

for (url in rss_feed_urls) {
  message(paste0("Rufe Feed ab: ", url))
  tryCatch({
    feed_data_raw <- tidyfeed(url)
    
    # Sicherstellen, dass die Spalten existieren und in den richtigen Typ konvertiert werden
    current_feed_items <- data.frame(
      item_title = if("item_title" %in% names(feed_data_raw)) feed_data_raw$item_title else NA_character_,
      item_link = if("item_link" %in% names(feed_data_raw)) feed_data_raw$item_link else NA_character_,
      item_description = if("item_description" %in% names(feed_data_raw)) feed_data_raw$item_description else NA_character_,
      # Konvertiere item_pub_date direkt hier zu Date.
      item_pub_date = suppressWarnings(if("item_pub_date" %in% names(feed_data_raw) && !all(is.na(feed_data_raw$item_pub_date))) {
        as.Date(feed_data_raw$item_pub_date)
      } else if ("item_pub_pub_date" %in% names(feed_data_raw) && !all(is.na(feed_data_raw$item_pub_pub_date))) {
        as.Date(feed_data_raw$item_pub_pub_date)
      } else {
        as.Date(NA_character_)
      }),
      stringsAsFactors = FALSE
    )
    
    # Sicherstellen, dass item_description nicht leer ist, wenn es null ist
    current_feed_items <- current_feed_items %>%
      mutate(
        item_description = if_else(is.na(item_description), item_title, item_description)
      ) %>%
      filter(!is.na(item_title) & !is.na(item_link) & !is.na(item_pub_date)) # Entferne Einträge ohne Kerninfos
    
    all_new_feed_items <- bind_rows(all_new_feed_items, current_feed_items)
    
  }, error = function(e) {
    warning(paste0("Fehler beim Abruf oder der Verarbeitung von Feed '", url, "': ", e$message))
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
    item_pub_date = as.Date(character()), # Initialisiere als Date
    llm_label = character(),
    llm_begruendung = character(),
    llm_tagesbewertung = character(),
    stringsAsFactors = FALSE
  )
  message(paste0("Historische Datei '", historic_file, "' nicht gefunden. Eine neue, leere Datei wird erstellt (oder überschrieben)."))
}

# Kombiniere neue und historische Daten und entferne Duplikate
combined_df <- bind_rows(df_historic, all_new_feed_items) %>%
  distinct(item_link, .keep_all = TRUE) # Duplikate basierend auf dem Link entfernen

df <- combined_df

# Sicherstellen, dass die LLM-Spalten existieren (falls df_historic leer war oder Spalten fehlen)
if(!"llm_label" %in% names(df)) df$llm_label <- NA_character_
if(!"llm_begruendung" %in% names(df)) df$llm_begruendung <- NA_character_
if(!"llm_tagesbewertung" %in% names(df)) df$llm_tagesbewertung <- NA_character_

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

message(paste0("Debug: Heute ist ", heute, ". Einträge im DF vom heute: ", 
               sum(as.Date(df$item_pub_date) == heute, na.rm = TRUE)))
message(paste0("Debug: Anzahl der neuen/unbewerteten Einträge vom heute, die bewertet werden sollen: ", nrow(neue_eintraege_zu_bewerten)))

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
# SCHRITT 2: Tageszusammenfassung generieren (News des Tages filtern)
# ==============================================================================

today <- Sys.Date()

# Filter die Einträge für heute, die als relevant bewertet wurden
relevante_heute_eintraege <- df %>%
  filter(as.Date(item_pub_date) == today) %>% # Konvertiere hier nochmal zu Date für den Filter
  filter(tolower(llm_label) == "ja") # Stelle sicher, dass "ja" in Kleinbuchstaben gefiltert wird

# Setze llm_tagesbewertung für die relevanten Einträge auf NA, um Neubewertung zu ermöglichen
# Nur für relevante Einträge, um unnötige LLM-Aufrufe zu vermeiden
if (nrow(relevante_heute_eintraege) > 0) {
  # Indices der relevanten Einträge in df finden, um sie zu aktualisieren
  relevant_indices_in_df <- which(df$item_link %in% relevante_heute_eintraege$item_link & as.Date(df$item_pub_date) == today)
  # Wichtig: Hier NA_character_ setzen, da llm_tagesbewertung vom Typ Character ist
  df[relevant_indices_in_df, "llm_tagesbewertung"] <- NA_character_ 
  
  message(paste0("Generiere Tageszusammenfassung für ", nrow(relevante_heute_eintraege), " relevante News des heutigen Tages..."))
  
  # Bereite die Details der relevanten News für den Prompt vor
  news_details_for_summary <- paste0(
    "Titel: ", relevante_heute_eintraege$item_title,
    "\nBeschreibung: ", relevante_heute_eintraege$item_description,
    "\nBegründung (LLM-Label): ", relevante_heute_eintraege$llm_begruendung,
    "\n---\n", collapse="" # Trennlinie zwischen den Artikeln
  )
  
  # Füge den TryCatch-Block auch hier für die Zusammenfassungs-API-Anfrage ein
  summary_resp <- tryCatch({
    POST(
      url = api_url,
      add_headers(
        "Authorization" = paste("Bearer", openrouter_api_key),
        "Content-Type" = "application/json",
        "User-Agent" = "R-LLM-Script/1.0"
      ),
      body = toJSON(list( # <-- toJSON-Aufruf beginnt hier
        model = model_id,
        messages = list(list(role = "user", content = paste0(
          "Du bist ein Experte für Finanzregulierung und IT-Security. Fasse die folgenden regulatorischen News des heutigen Tages (", format(today, "%d.%m.%Y"), ") zusammen. ",
          "Konzentriere dich auf die Kernaussagen und mögliche Implikationen für Beratungsinstitute oder Kunden. ",
          "Gib eine ausführliche Einordnung zu jeder relevanten News (maximal 70 Wörter pro News). ",
          "Nutze Stichpunkte oder eine nummerierte Liste, wenn sinnvoll und vermeide einleitende Floskeln. ",
          "Strukturiere deine Antwort so, dass sie direkt in einen E-Mail-Text übernommen werden kann. Verwende für jeden News-Eintrag den Titel als Überschrift oder Fettformatierung.",
          "\n\nNews:\n", news_details_for_summary
        )))), # <-- Ende der messages-Liste und des paste0-Aufrufs
        temperature = 0.5,
        max_tokens = 700
      ), auto_unbox = TRUE) # <-- toJSON-Aufruf endet hier und schließt den 'body' Parameter ab
  }, error = function(e) {
    warning(paste0("Netzwerk- oder HTTP-Fehler beim Senden der Zusammenfassungsanfrage: ", e$message))
    return(NULL)
  })
  
  if (!is.null(summary_resp) && http_status(summary_resp)$category == "Success") {
    response_content_summary <- fromJSON(content(summary_resp, "text", encoding = "UTF-8"))
    summary_text <- tryCatch(
      response_content_summary$choices[[1]]$message$content,
      error = function(e) {
        warning("Fehler bei OpenRouter API Extraktion für Zusammenfassung: ", e$message)
        return("Fehler bei Zusammenfassungsextraktion")
      }
    )
    
    # Speichere die generierte Zusammenfassung im DataFrame
    df[relevant_indices_in_df, "llm_tagesbewertung"] <- summary_text
    message("Tageszusammenfassung erfolgreich generiert und gespeichert.")
    
  } else {
    error_content_summary <- if (!is.null(summary_resp)) content(summary_resp, "text", encoding = "UTF-8") else "Keine Antwort erhalten."
    status_code <- if (!is.null(summary_resp)) summary_resp$status_code else "NA"
    warning("Fehler bei OpenRouter API Anfrage für Zusammenfassung: ", http_status(summary_resp)$reason, " - Statuscode: ", status_code)
    message(paste0("API Fehler (Zusammenfassung ", status_code, "): ", error_content_summary))
    summary_text <- "Keine Tageszusammenfassung generiert aufgrund eines API-Fehlers."
  }
} else {
  message("Keine relevanten News für heute gefunden, keine Tageszusammenfassung generiert.")
  summary_text <- "Heute wurden keine neuen regulatorischen Entwicklungen identifiziert, die eine Zusammenfassung erfordern."
}

# Aktualisierte Daten mit llm_tagesbewertung speichern
# item_pub_date wurde oben schon zu character konvertiert, also hier direkt speichern
df$item_pub_date <- as.character(df$item_pub_date) 
df <- df %>% arrange(desc(item_pub_date), item_title)
write.csv2(df, file = historic_file, row.names = FALSE, fileEncoding = "UTF-8")
message(paste0("Aktualisierte Daten mit ", nrow(df), " Einträgen in '", historic_file, "' gespeichert."))


# ==============================================================================
# SCHRITT 3: E-Mail-Bericht generieren und versenden
# ==============================================================================

# --- Manueller Override für den E-Mail-Versand (optional) ---
# Setze diesen auf TRUE, um die E-Mail immer zu senden, auch wenn keine relevanten News gefunden wurden.
force_email_send_override <- TRUE # Standard: FALSE. Zum Aktivieren auf TRUE setzen.

bericht_filename <- paste0("Regulierungsmonitor_Tagesbericht_", format(today, "%Y-%m-%d"), ".html")

# Daten für den Bericht filtern: Alle relevanten Einträge vom heutigen Tag
bericht_daten <- df %>%
  filter(as.Date(item_pub_date) == today) %>%
  filter(tolower(llm_label) == "ja") %>%
  select(item_title, item_link, item_description, llm_begruendung, llm_tagesbewertung) %>%
  # Füge die generierte Tageszusammenfassung zu jedem relevanten Eintrag hinzu
  mutate(Tageszusammenfassung = summary_text)

# Sicherstellen, dass die Spalten korrekt umbenannt werden für den Bericht
bericht_daten <- bericht_daten %>%
  select(
    Titel = item_title,
    Link = item_link,
    Beschreibung = item_description,
    Relevanz_Begruendung = llm_begruendung,
    Tageszusammenfassung
  )

# E-Mail-Inhalt mit blastula erstellen
# Verwende `md()` für Markdown-Formatierung, um den LLM-Output korrekt darzustellen
email_body <- md(
  paste0(
    "## Täglicher Regulatorischer News-Monitor - ", format(today, "%d.%m.%Y"), "\n\n",
    "Sehr geehrte Damen und Herren,\n\n",
    "Hier ist Ihre tägliche Zusammenfassung der wichtigsten regulatorischen Entwicklungen im Bereich Informationssicherheit und IT-Security:\n\n",
    "### **Tageszusammenfassung:**\n\n",
    summary_text, "\n\n", # Hier die von der LLM generierte Zusammenfassung einfügen
    "### **Detaillierte relevante Nachrichten des Tages:**\n\n",
    if(nrow(bericht_daten) > 0) {
      paste(apply(bericht_daten, 1, function(row) {
        paste0(
          "**Titel:** [", row["Titel"], "](", row["Link"], ")\n",
          "**Beschreibung:** ", row["Beschreibung"], "\n",
          "**Relevanzbegründung:** ", row["Relevanz_Begruendung"], "\n\n"
        )
      }), collapse = "\n")
    } else {
      "Keine weiteren relevanten Einzeleinträge für heute."
    },
    "\n\n",
    "Mit freundlichen Grüßen,\n",
    "Ihr RegMon-Team"
  )
)

# Erstelle die E-Mail und versende sie
# Der Versand findet statt, wenn relevante Einträge gefunden wurden ODER der Override aktiv ist
# --- KORRIGIERTER BLOCK FÜR DEN E-MAIL-VERSAND ---

# Der Versand findet statt, wenn relevante Einträge gefunden wurden ODER der Override aktiv ist
if (nrow(relevante_heute_eintraege) > 0 || force_email_send_override) {
  
  # Erstelle das E-Mail-Objekt
  email <- compose_email(
    body = email_body,
    footer = md("Dies ist ein automatischer Bericht des täglichen Regulatorischen News-Monitors.")
  )
  
  # Stelle sicher, dass der E-Mail-Versand aktiviert ist
  if (email_sending_enabled) {
    message("Versuche, E-Mail zu versenden...")
    tryCatch({
      
      # KORREKTE METHODE: Alle Parameter werden direkt an smtp_send() übergeben.
      # Der Pipe-Operator (%>%) wird hier nicht für from, to, oder subject benötigt.
      smtp_send(
        email = email,
        from = sender_email,
        to = "tob.rohleder@gmail.com", # <-- WICHTIG: HIER DEINE KORREKTE EMPFÄNGER-ADRESSE EINTRAGEN!
        subject = paste0("Täglicher Regulatorischer News-Monitor - ", format(today, "%d.%m.%Y")),
        credentials = smtp_send_credentials
      )
      
      message("E-Mail wurde erfolgreich und fehlerfrei an den SMTP-Server übergeben.")
      
    }, error = function(e) {
      # Dieser Block wird nur bei echten Fehlern (z.B. falsches Passwort, Server nicht erreichbar) ausgeführt.
      warning("FEHLER BEIM E-MAIL-VERSAND: ", e$message)
      message("Der Versand ist fehlgeschlagen. Überprüfen Sie die SMTP-Zugangsdaten (Host, Port, Passwort) in Ihren Umgebungsvariablen.")
    })
    
  } else {
    message("E-Mail-Versand ist deaktiviert, da die Zugangsdaten nicht konfiguriert sind.")
  }
  
} else {
  message("Keine relevanten Einträge für den heutigen Tag gefunden. Es wird keine E-Mail versendet.")
}

message("--- Skriptausführung beendet ---")

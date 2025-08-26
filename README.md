# RegMon - Regulatorischer KI-Monitor

RegMon automatisiert und potimiert die Überwachung neuer Regulatorik und Compliance-News mithilfe von LLM-Technologien. Es wurde entwickelt, um Fachexperten und Entscheidungsträger in der Finanzdienstleistungsbranche (z.B. im Risikomanagement und in der Compliance) zeitnah mit kuratierten und relevanten Informationen zu versorgen. Dies ermöglicht ein proaktives Handeln und die frühzeitige Anpassung an neue aufsichtsrechtliche Anforderungen (z.B. IT-Compliance, ESG-Compliance, etc.).

**Kernfunktionen:**
* **Erkennung:** Scannt neue regulatorische Nachrichten via RSS-Feeds nach relevanten Beiträgen.
* **Klassifizierung:** Dichotome Einordnung in relevant bzw. unrelevant.
* **Begründung:** Stellt eine prägnante Herleitung der Klassifizierungseinordnung zur Verfügung.
* **Verteilung:** Bereitet relevante News auf und versendet diese täglich, anlassbezogen an eine definierte Empfängergruppe per E-Mail.
* **Flexiblität:** Die thematischen (Neu-)Ausrichtung des Tools kann über eine Schwerpunktbeschreibung per Promt zügig neu definiert werden.

**Verwendete Technologien:**
* **R:** Für die gesamte Verarbeitung.
* **OpenRouter.ai:** Für die LLM-Anbindung via API.
* **Mistral Small 3.2 24B:** Als zugrundeliegendes LLM-Modell.
* **Brevo:** Für den stabilen E-Mail-Versandt via API.

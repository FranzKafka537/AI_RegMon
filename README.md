# RegMon - Regulatory Monitor utilizing AI

RegMon automates and optimizes the monitoring of new regulatory and compliance news using LLM technologies. It was developed to provide experts and decision-makers in the financial services industry (e.g., in risk management and compliance) with timely, curated, and relevant information. This enables proactive management and the early adaptation to new supervisory requirements (e.g., IT compliance, ESG compliance, etc.).

**Core Features:**
* **Detection:** Scans new regulatory news from RSS feeds for relevant articles.
* **Classification:** Produces a dichotomous classification of news as either relevant or irrelevant.
* **Justification:** Provides a concise derivation for the classification.
* **Distribution:** Prepares and sends daily, event-based updates of relevant news to a defined recipient group via email.
* **Flexibility:** The tool's thematic focus can be quickly redefined via a prompt-based description.

**Technologies Used:**
* **R:** For the entire processing workflow.
* **OpenRouter.ai:** For the LLM API connection.
* **Mistral Small 3.2 24B:** As the underlying LLM model.
* **Brevo:** For reliable email delivery via API.

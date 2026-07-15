# Stroke Prediction Studio — Render deployment

Pacchetto di pubblicazione del prototipo SU 2026, esclusivamente per ricerca.

## Privacy

Il repository contiene soltanto il frontend, il backend e l'artefatto predittivo
minimizzato. Non contiene i workbook di origine, il dataset merged o record paziente.

## Pubblicazione

Il servizio usa Docker perché combina Python e R. `render.yaml` configura un Web
Service Render e il server usa automaticamente le variabili `HOST` e `PORT`.

## Avvertenza

I modelli non hanno validazione esterna e non devono guidare diagnosi, triage o terapia.

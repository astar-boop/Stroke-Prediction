# SU 2026 Prediction Web App

App web non-Shiny per le predizioni individuali SU 2026.

La release aggiornata include i modelli selezionati dall'ultima analisi validata e il nuovo task **mRS sfavorevole a 3 mesi (3–6)** al landmark di 24 ore. Per ogni task è possibile scegliere l'algoritmo; nel nuovo task il LASSO è il default per il Brier score puntuale più basso ed Elastic Net è segnalato per la ROC-AUC puntuale più alta, senza superiorità dimostrata.

## Avvio

```bash
python3 SU2026_prediction_web/server.py 3840
```

Aprire:

```text
http://127.0.0.1:3840/
```

## Architettura

- `server.py`: server HTTP Python senza dipendenze esterne.
- `static/`: frontend HTML/CSS/JavaScript.
- `predict_bridge.R`: bridge minimale verso `su2026_prediction_artifacts.rds`.

La UI e il server applicativo non sono in R. Il bridge R resta solo per interrogare gli stessi artefatti dei modelli già valutati nella nested cross-validation. L'app è un prototipo esclusivamente di ricerca: i refit non hanno validazione esterna e non devono guidare decisioni cliniche individuali.

## GPG Simulation – Gender Pay Gap Simulator für Hochschulprofessuren
Dieses Repository wurde als Arbeitsprobe für Bewerbungen angelegt. 
Zur Ausführung beide Dateien in einen Ordner kopieren und App starten.

Es handelt sich hierbei um eine Gruppenarbeit, wobei die Simulation und dazugehörige App mein Anteil ist.
Es gab außerdem eine Shiny App für die Analyse, die Hauptaufgabe für das Projekt war. Da der Simulations und  Sim Validierungsteil mein eigenes Werk ist
und in der Analyse mein Input nicht von dem anderer Gruppenmitglieder zu unterscheiden ist, verzichte ich hier darauf den Analyse Code hochzuladen.

## Motivation
Ein interaktives R/Shiny-Tool zur Simulation variabler Vergütungsbestandteile von Universitätsprofessuren (W2/W3) über mehrere Jahrzehnte. 
Das Modell dient der Untersuchung struktureller geschlechtsspezifischer Gehaltsunterschiede (Gender Pay Gap) unter kontrollierten,
frei konfigurierbaren Bedingungen.

Reale Personaldaten zu variablen Vergütungsbestandteilen (Leistungsbezüge, Funktionszuschläge, Berufungs-/Bleibezuschläge) waren uns in diesem Projekt nicht verfügbar,
und Kausalzusammenhänge lassen sich aus Querschnittsdaten kaum isolieren. Diese Simulation erzeugt synthetische Längsschnittdaten, mit denen sich:

Hypothesen zu Entstehungsmechanismen eines GPG testen lassen,
Analyseverfahren (z. B. t-Tests, Regressionen) validieren lassen,
realistische Szenarien (von Gleichstellung bis ausgeprägtem GPG) durchspielen lassen.

## Features

Demografische Simulation: Berufungsalter, Ruhestand, Kinderzahl, Nationalität und Familienstand, jeweils geschlechtsspezifisch parametrisierbar
Drei Vergütungskomponenten:

Leistungsbezüge (LBe) in 4 unabhängigen Stufen (befristet/entfristet)
Funktionszuschläge (LFunk) mit zeitabhängiger Eurostaffel
Berufungs-/Bleibezuschläge (BB), befristet oder unbefristet
Drittmittel (Gamma-verteilt, jährlich variierend)

Gender-Faktoren: multiplikative Skalierung von Vergabewahrscheinlichkeiten und Beträgen für jede Komponente
Szenarien-Presets: z. B. „Kein GPG" vs. „Deutlicher GPG"
Gender Check: t-Tests pro Jahr zum Vergleich der Vergütungskomponenten zwischen Geschlechtern
Flexible Datenausgabe: Zeitraum, Variablenauswahl (Haupt- und Hilfsvariablen) und CSV-Export
Reproduzierbarkeit: konfigurierbarer Seed für identische Simulationsläufe

## Validierung

Demografischer Abgleich: Frauenanteile (nach W2/W3), Kinderzahlen und Nationalitätenverteilung wurden mit dem Hochschulpersonalbericht des Statistischen Bundesamtes sowie internen Daten der Universität Göttingen abgeglichen
Reproduzierbarkeit: Bei identischem Seed liefert die Simulation exakt identische Ergebnisse; unterschiedliche Seeds führen zu unterschiedlichen, aber stochastisch konsistenten Verläufen
Neutralitätstest: Bei allen Gender-Faktoren = 1,0 zeigen die im Gender Check-Tab bereitgestellten t-Tests über die Vergütungskomponenten (LBe, LFunk, BB, Drittmittel) keine systematischen Mittelwertunterschiede zwischen Geschlechtern
Sensitivität: Eine gezielte Reduktion einzelner Gender-Faktoren (z. B. gender_factor_lfunk < 1) erzeugt im Gender Check eine entsprechend gerichtete, statistisch erkennbare Differenz — das Modell reagiert also wie erwartet auf die eingestellten Effekte

## Grenzen

Das Modell bildet Vergabeentscheidungen als stochastische Prozesse mit konfigurierbaren Wahrscheinlichkeiten ab.
Verhandlungsverhalten, institutionelle Pfadabhängigkeiten und W-Tabellengehälter selbst sind nicht Teil der Simulation. 
Ergebnisse sind im Sinne von Größenordnungen und qualitativen Mustern zu interpretieren, nicht als quantitative Vorhersage realer Gehaltsverläufe.

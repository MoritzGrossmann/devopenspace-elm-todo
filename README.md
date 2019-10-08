# devopenspace-elm-todo

in diesem Schritt sollst Du eine Statusleiste oben in den `TaskList` und
`TaskLists` Pages einbauen, die den eingeloggten Benutzer anzeigt.

Außerdem kannst Du noch einen Knopf/Link einfügen, der auf die `Login`-Seite
navigiert und damit den Benutzer abmeldet.

---

Im [Auth-Modul](./Elm/Auth.elm) kannst Du die `getUserName` Funktion verwenden
um den angemeldeten Benutzer-Name aus der Session zu laden.

Wir empfehlen als **Komponente** nur eine *einfache* `view` *Funktion* zu
schreiben, die Leiste darstellt und die dann in den *Pages* zu verwenden.

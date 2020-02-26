# generelle Anmerkung: ich verstehe die Spezifikation in der Aufgabe nicht. 
# Vielleicht ist es auch ein generelles Verständnisproblem für S3-Klassen.
# Zum einen erstellen wir einen Konstruktor und ein Interface für die bb-Klasse.
# Diese erzeugen aus einem übergebenen Objekt ein bb-Objekt.
# Außerdem schreiben wir bb-Methode (generisch), die dann die Regex-Logik macht.
# Mir ist nicht klar, wie die beiden Sachen zusammenpassen - im Konstruktor bzw.
# Interface die bb-Methode zu dispatchen will man ja nicht.
# Andererseits funktioniert die generische Methode ja auch ohne die
# Klassendefinition - wozu brauchen wir die überhaupt?
# In den Testfällen werden zwei Dinge geprüft, das Ergebnis der Regex und ob das
# Ergebnis ein bb-Objekt ist. Ich verstehe nicht warum, man kann die zwei Sachen
# doch trennen?

new_bb <- function(text = "") {
  checkmate::assert_character(unlist(unclass(text)))
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  structure(gsub(pattern = match, replacement = "\\1\\2b\\2\\3", text), class = "bb")
}

validate_bb <- function(text) {
  checkmate::assert_class(text, "bb")
  text
}

bb_interface <- function(text = "") {
  validate_bb(new_bb(text))
}

bb <- function(text) {
  UseMethod("bb")
}

bb.default <- function(text) {
  validate_bb(new_bb(text))
}

bb.list <- function(text, ...) {
  text <- lapply(text, bb)
  class(text) <- append(class(text), "bb")
  text
}

bb.factor <- function(text, ...) {
  levels(text) <- bb(levels(text))
  class(text) <- append(class(text), "bb")
  text
}
texttest <- "Bedeutet nach jedem Vokal oder Diphtong die Konsonanten..."
new_bb(texttest)
# Haskell Backend für die Todo-Demo

## Build

    stack build

## Run

    stack run

## API

### User

#### Benutzer registrieren

registriert einen Benutzer an der API

- **Route** `user/register`
- **VERB** `POST`
- **Body** Json-Object der Form `{ "name":[UserName], "password":[gewünschtes Passwort] }`
- **RETURN** gültiger JWT-Token als JSON-String

#### Benutzer einloggen

hiermit kann ein Benutzer seinen JWT-Token mittels *basic-auth* Abfragen

**Wichtig**: die Credentials (Benutzername und Passwort wie im register angegeben) müssen per
*basic-auth* übergeben werden!

- **Route** `user/login`
- **VERB** `GET`
- **RETURN** gültiger JWT-Token als JSON-String

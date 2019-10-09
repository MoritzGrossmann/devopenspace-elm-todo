import { Elm } from '../src/Main.elm'
import { bindPorts } from './ports'

function startApp(flags) {
    var app = Elm.Main.init({ flags: flags });
    bindPorts(app);
}

// Ans Browser-document binden, damit das Backend die Flags übergeben kann
document.startApp = startApp;
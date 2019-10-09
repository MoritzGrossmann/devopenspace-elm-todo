import { Elm } from '../src/Main.elm'
import { bindPorts } from './ports'

var app = Elm.Main.init({ flags: { baseUrlPath: "/", apiUrl: "https://leastfixedpoint.net/todo/api" } });
bindPorts(app);
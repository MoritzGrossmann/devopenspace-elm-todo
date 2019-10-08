import { Elm } from './src/Main.elm'

var app = Elm.Main.init({ flags: { baseUrlPath: "/", apiUrl: "https://leastfixedpoint.net/todo/api" } });
window.initPorts(app);
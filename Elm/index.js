import { Elm } from './src/Main.elm'
// TODO: change to public API Url later
var app = Elm.Main.init({ flags: { baseUrlPath: "/", apiUrl: "https://leastfixedpoint.net/todo/api" } });
window.initPorts(app);
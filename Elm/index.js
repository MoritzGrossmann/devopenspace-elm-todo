import { Elm } from './src/Main.elm'
// TODO: change to public API Url later
var app = Elm.Main.init({ flags: { baseUrlPath: "", apiUrl: "http://localhost:8080/api" } });

app.ports.store.subscribe(function(data) {
    let storageKey = data[0];
    let value = data[1];
    if (value === null) {
        localStorage.removeItem(storageKey);
    } else {
        localStorage.setItem(storageKey, value);
    }
});

app.ports.request.subscribe(function(storageKey) {
   let value = localStorage.getItem(storageKey);
   if(app.ports.receive) {
       app.ports.receive.send([storageKey, value])
   }
});

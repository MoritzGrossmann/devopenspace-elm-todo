window.initPorts = function (app) {
  if (app && app.ports) {

    if (app.ports.store) {
      app.ports.store.subscribe(function (data) {
        if (data && data.length >= 2) {
          let storageKey = data[0];
          let value = data[1];
          if (value === null) {
            localStorage.removeItem(storageKey);
          } else {
            localStorage.setItem(storageKey, value);
          }
        }
      });
    }

    if (app.ports.receive) {
      window.addEventListener("storage", function (e) {
        app.ports.receive.send([e.key, e.newValue]);
      });
    }

    if (app.ports.request && app.ports.receive) {
      app.ports.request.subscribe(function (storageKey) {
        let value = localStorage.getItem(storageKey);
        app.ports.receive.send([storageKey, value]);
      });
    }
  }
}
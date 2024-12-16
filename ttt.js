window.addEventListener("load", function() {
  Scheme.load_main("ttt.wasm", {
    user_imports: {
      window: {
        log(text) { console.log(text); }
      }
    }
  });
});

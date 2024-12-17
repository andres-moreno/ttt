window.addEventListener("load", async ()  => {
  const [proc] = await Scheme.load_main("ttt.wasm", {
    user_imports: {
      core: {
        log(text) { console.log(text); }
      }
    }
  });
  proc.call_async();
});

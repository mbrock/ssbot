(() => {
  // index.js
  var prolog = pl.create(1e3);
  var webSocketProtocol = location.protocol == "https:" ? "wss:" : "ws:";
  var webSocketURL = `${webSocketProtocol}//${location.host}/ws`;
  var webSocket = new WebSocket(webSocketURL);
  webSocket.onmessage = async (event) => {
    let { goal, id } = JSON.parse(event.data);
    console.log("query " + id + ": " + goal);
    try {
      await prolog.queryPromise(goal);
      for await (let answerObject of prolog.promiseAnswers()) {
        let answer = prolog.format_answer(answerObject);
        webSocket.send(JSON.stringify({ id, answer }));
      }
      console.log("done " + id);
      webSocket.send(JSON.stringify({ id, done: true }));
    } catch (error) {
      console.log("error " + id + ": " + error);
      webSocket.send(JSON.stringify({ id, error: error.message }));
    }
  };
  webSocket.onclose = (event) => {
    console.log("WebSocket closed");
  };
  webSocket.onerror = (event) => {
    console.log("WebSocket error");
  };
})();
//# sourceMappingURL=index.js.map

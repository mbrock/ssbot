//
// Tau Prolog is loaded in the browser.
//
// The following code is executed when the page is loaded.
//
// It starts a WebSocket client that connects to the server.
//
// The server is the SWI-Prolog HTTP server.
//
// The server can send a Prolog query to the client,
// which executes the query and sends the result back.
//

let prolog = pl.create(1000)

let webSocketProtocol = (
  location.protocol == "https:" ? "wss:" : "ws:")
let webSocketURL = `${webSocketProtocol}//${location.host}/websocket`
let webSocket = new WebSocket(webSocketURL)

webSocket.onmessage = async event => {
  let { goal, id } = JSON.parse(event.data)
  console.log("query " + id + ": " + goal)
  try {
    await prolog.queryPromise(goal)
    for await (let answerObject of prolog.promiseAnswers()) {
      let answer = prolog.format_answer(answerObject)
      webSocket.send(JSON.stringify({ id, answer }))
    }
    console.log("done " + id)
    webSocket.send(JSON.stringify({ id, done: true }))
  } catch (error) {
    console.log("error " + id + ": " + error)
    webSocket.send(JSON.stringify({ id, error: error.message }))
  }
}

webSocket.onclose = event => {
  console.log("WebSocket closed", event)
}

webSocket.onerror = event => {
  console.log("WebSocket error", event)
}


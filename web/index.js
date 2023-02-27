import {EditorState} from "@codemirror/state"
import {EditorView, keymap} from "@codemirror/view"
import {defaultKeymap} from "@codemirror/commands"

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

window.prolog = pl.create(1000)

prolog.consult(`
  :- use_module(library(dom)).
  :- use_module(library(js)).
  :- use_module(library(format)).
  :- use_module(library(concurrent)).
`)

let webSocketProtocol = (
  location.protocol == "https:" ? "wss:" : "ws:")
let webSocketURL = `${webSocketProtocol}//${location.host}/websocket`
let webSocket = new WebSocket(webSocketURL)

webSocket.onmessage = async event => {
  let { goal, id } = JSON.parse(event.data)
  goal = `${goal}.`
  console.log("query " + id + ": " + goal)
  try {
    await prolog.promiseQuery(goal)
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

function onTelegramAuth(user) {
  webSocket.send(JSON.stringify(["auth", "telegram", user]))
}

let myKeymap = keymap.of([{
  key: "Enter",
  run: view => {
    // navigate to /?query=... (don't use History API)
    let query = view.state.doc.toString()
    location.href = `/?query=${encodeURIComponent(query)}`
  }
}, ...defaultKeymap])

let startState = EditorState.create({
  doc: "",
  extensions: [myKeymap]
})

let view = new EditorView({
  state: startState,
  parent: document.querySelector("#editor")
})



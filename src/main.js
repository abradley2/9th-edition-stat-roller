import { Elm } from './Main.elm'

const node = document.createElement('div')

document.body.appendChild(node)

const seed = crypto.getRandomValues(new Uint16Array(1))[0]

Elm.Main.init({
  node,
  flags: {
    seed
  }
})

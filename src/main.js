import { Elm } from './App.elm'
import FocusMenu from './FocusMenu'
import FocusTrap from './FocusTrap'

const loader = document.getElementById('loader')
document.body.removeChild(loader)

const node = document.createElement('div')
document.body.appendChild(node)

const seed = crypto.getRandomValues(new Uint16Array(1))[0]
const seeds = crypto.getRandomValues(new Uint16Array(1000))

window.customElements.define('focus-menu', FocusMenu)
window.customElements.define('focus-trap', FocusTrap)

Elm.App.init({
  node,
  flags: {
    seed,
    seeds: [...seeds]
  }
})

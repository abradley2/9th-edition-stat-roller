import * as Sentry from "@sentry/browser"
import { Integrations } from "@sentry/tracing"
import { Elm } from './Main.elm'
import FocusMenu from './FocusMenu'
import FocusTrap from './FocusTrap'
import TextInput from './TextInput'

if (process.env.NODE_ENV !== 'development') {
  Sentry.init({
    dsn: "https://d1699f9897784c9e9939be0751ab9524@o609383.ingest.sentry.io/5792114",
    integrations: [new Integrations.BrowserTracing()],
    tracesSampleRate: 1.0,
    environment: process.env.NODE_ENV
  });
}

const loader = document.getElementById('loader')
document.body.removeChild(loader)

const node = document.createElement('div')
document.body.appendChild(node)

const seed = crypto.getRandomValues(new Uint16Array(1))[0]
const seeds = crypto.getRandomValues(new Uint16Array(1000))

window.customElements.define('focus-menu', FocusMenu)
window.customElements.define('focus-trap', FocusTrap)
window.customElements.define('text-input', TextInput)

Elm.Main.init({
  node,
  flags: {
    seed,
    seeds: [...seeds]
  }
})

export default class FocusTrap extends HTMLElement {
  static get observedAttributes () {
    return ['active']
  }

  disconnectedCallback () {
    if (this.onDisconnect) this.onDisconnect()
  }

  attributeChangedCallback (name, oldVal, newVal) {
    const node = this
    node.tabIndex = 0
    if (name === 'active') {
      const active = newVal === 'true'

      if (!active) {
        if (node.onDisconnect) node.onDisconnect()
      }

      if (active) {
        function handleFocusOut (e) {
          if (!node.contains(e.relatedTarget)) {
            e.preventDefault()

            // allow for other programatic focus changes to occur
            setTimeout(() => {
              if (!node.contains(document.activeElement)) node.focus()
            }, 100)
          }
        }

        node.addEventListener('focusout', handleFocusOut)

        node.onDisconnect = function () {
          node.removeEventListener('focusout', handleFocusOut)
        }
      }
    }
  }
}

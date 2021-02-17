export default class FocusTrap extends HTMLElement {
  static get observedAttributes () {
    return ['active']
  }

  disconnectedCallback () {
    if (this.onDisconnect) this.onDisconnect()
  }

  attributeChangedCallback (name, oldVal, newVal) {
    const node = this
    let borrowed

    if (name === 'active') {
      const active = newVal === 'true'

      if (!active) {
        if (node.onDisconnect) node.onDisconnect()
      }

      if (active) {
        borrowed = document.activeElement
        node.tabIndex = 0
        setTimeout(() => node.focus(), 1)
        function handleFocusOut (e) {
          if (!node.contains(e.relatedTarget)) {
            e.preventDefault()

            // allow for other programatic focus changes to occur
            setTimeout(() => {
              if (!node.contains(document.activeElement)) node.focus()
            }, 10)
          }
        }

        node.addEventListener('focusout', handleFocusOut)

        node.onDisconnect = function () {
          node.removeEventListener('focusout', handleFocusOut)
          node.tabIndex = -1
          if (borrowed) {
            setTimeout(() => borrowed.focus(), 1)
          }
        }
      }
    }
  }
}

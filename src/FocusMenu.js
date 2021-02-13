export default class FocusMenu extends HTMLElement {
  static get observedAttributes () {
    return ['show']
  }

  attributeChangedCallback (name, oldVal, newVal) {
    const node = this
    const children = this.children

    if (name === 'show') {
      const show = newVal === 'true'

      if (!show) {
        if (node.onDisconnect) {
          node.onDisconnect()
          node.onDisconnect = undefined
        }
      }

      if (show) {
        function handleKeyPress (e) {
          if (e.key === 'ArrowUp') {
            if (
              [...children].includes(document.activeElement) &&
              document.activeElement.previousSibling
            ) {
              document.activeElement.previousSibling.focus()
            }
          }
          if (e.key === 'ArrowDown') {
            if (
              [...children].includes(document.activeElement) &&
              document.activeElement.nextSibling
            ) {
              document.activeElement.nextSibling.focus()
            }
          }
        }

        function handleFocusOut (e) {
          if (node.contains(e.relatedTarget)) {
            return
          }
          node.dispatchEvent(new Event('requestedclose'))
          node.onDisconnect()
        }

        document.addEventListener('keydown', handleKeyPress)
        node.addEventListener('focusout', handleFocusOut)

        node.onDisconnect = function () {
          document.removeEventListener('keydown', handleKeyPress)
          node.removeEventListener('focusout', handleFocusOut)
        }

        setTimeout(() => {
          if (node.firstChild) {
            node.firstChild.focus()
          }
        }, 1)
      }
    }
  }

  disconnectedCallback () {
    if (this.onDisconnect) this.onDisconnect()
  }
}

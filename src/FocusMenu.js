export default class FocusMenu extends HTMLElement {
  static get observedAttributes () {
    return ['show']
  }

  disconnectedCallback () {
    if (this.onDisconnect) this.onDisconnect()
  }

  attributeChangedCallback (name, oldVal, newVal) {
    const node = this
    const children = this.children

    if (name === 'show') {
      const show = newVal === 'true'

      if (!show) {
        if (node.onDisconnect) {
          // TODO: Tie this explicitly to selection change
          if (this.prevElement) {
            const prev = this.prevElement
            setTimeout(() => prev.focus(), 0)
            this.prevElement = undefined
          }

          node.onDisconnect()
          node.onDisconnect = undefined
        }
      }

      if (show) {
        this.prevElement = document.activeElement
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
          if (node.onDisconnect) node.onDisconnect()
        }

        document.addEventListener('keydown', handleKeyPress)
        node.addEventListener('focusout', handleFocusOut)

        node.onDisconnect = function () {
          document.removeEventListener('keydown', handleKeyPress)
          node.removeEventListener('focusout', handleFocusOut)
          node.onDisconnect = undefined
        }

        setTimeout(() => {
          if (node.firstChild) node.firstChild.focus()
        })
      }
    }
  }
}

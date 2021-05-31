const proxyAttributes = {
  'input-class': 'class',
  'input-id': 'id',
  'input-placeholder': 'placeholder'
}

export default class TextInput extends HTMLElement {
  static observedAttributes = ['decorated-value', 'raw-value'].concat(Object.keys(proxyAttributes))

  constructor(...args) {
    super(...args)
    this.input = document.createElement('input')

  }

  attributeChangedCallback(name, oldVal, newVal) {
    Object.keys(proxyAttributes).forEach((k) => {
      if (name === k) {
        this.input.setAttribute(proxyAttributes[k], newVal)
      }
    })

    if (name === 'input-class') {
      this.input.className = newVal
    }

    if (name === 'decorated-value') {
      if (!this.focused) {
        this.input.value = newVal
      }
    }
  }

  connectedCallback() {
    const input = this.input
    input.style.width = 'inherit'
    input.setAttribute("spellcheck", "false")
    input.setAttribute("autocomplete", "false")


    input.onfocus = () => {
      this.focused = true
      input.value = this.getAttribute("raw-value")
      setTimeout(() => {
        input.select()
      })
    }

    input.onblur = () => {
      this.focused = false
      this.input.value = this.getAttribute('decorated-value')
      const event = new CustomEvent('newval', {
        detail: input.value
      })
      this.dispatchEvent(event)
    }

    input.oninput = () => {
      const event = new CustomEvent('newval', {
        detail: input.value
      })
      this.dispatchEvent(event)
    }

    this.appendChild(input)
  }
}

'use strict';

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

exports._customElement = function _customElement(observedAttributes) {
  return function(constructor) {
    return function (_connectedCallback) {
      return function (_disconnectedCallback) {
        return function (_attributeChangedCallback) {
          return function (_adoptedCallback) {
            return function (_HTMLElement) {
              _inherits(_class, _HTMLElement);

              function _class() {
                _classCallCheck(this, _class);

                var _this = _possibleConstructorReturn(this, (_class.__proto__ || Object.getPrototypeOf(_class)).call(this));

                constructor(_this);
                return _this;
              }

              _createClass(_class, [{
                key: "connectedCallback",
                value: function connectedCallback() {
                  _connectedCallback(this);
                }
              }, {
                key: "disconnectedCallback",
                value: function disconnectedCallback() {
                  _disconnectedCallback(this);
                }
              }, {
                key: "attributeChangedCallback",
                value: function attributeChangedCallback(attributeName, oldValue, newValue) {
                  _attributeChangedCallback(this, attributeName, oldValue, newValue);
                }
              }, {
                key: "adoptedCallback",
                value: function adoptedCallback() {
                  _adoptedCallback(this);
                }
              }], [{
                key: "observedAttributes",
                get: function get() {
                  return observedAttributes;
                }
              }]);

              return _class;
            }(HTMLElement);
          };
        };
      };
    };
  };
};


exports._registerElement = function(elementName) {
  return function(customElement) {
    return function() {
      customElements.define(elementName, customElement);
    };
  };
};


exports._onMount = function(connectedCallback) {
  return function(customElement) {
    return function() {
      customElement.prototype.connectedCallback = connectedCallback
      return customElement
    };
  };
};

exports._onUnmount = function(disconnectedCallback) {
  return function(customElement) {
    return function() {
      customElement.prototype.disconnectedCallback = disconnectedCallback;
      return customElement;
    };
  };
};

exports._onAdopt = function(adoptedCallback) {
  return function(customElement) {
    return function() {
      customElement.prototype.adoptedCallback = adoptedCallback;
      return customElement;
    };
  };
};

exports._on = function(attribute) {
  return function(attributeChangedCallback) {
    return function(customElement) {
      return function() {
        var observedAttributes =
            customElement.observedAttributes.indexOf(attribute) === -1 ?
            customElement.observedAttributes.concat([attribute]) :
            customElement.observedAttributes

        Object.defineProperty(customElement, 'observedAttributes', {
          key: 'observedAttributes',
          get: function get() {
            return observedAttributes
          },
          enumerable: false,
          configurable: true
        })

        var previousCallback = customElement.prototype.attributeChangedCallback
        customElement.prototype.attributeChangedCallback = function(attr, oldValue, newValue) {
          if (previousCallback) {
            previousCallback(attr, oldValue, newValue);
          }
          if (attr === attribute) {
            attributeChangedCallback(customElement, oldValue, newValue);
          }
        };
        return customElement;
      };
    };
  };
};

(function () {
  var VERSION = "2026-06-16";

  if (window.__emacsReadOnlyCaret
      && window.__emacsReadOnlyCaret.version === VERSION) {
    return true;
  }

  var state = {
    version: VERSION,
    markActive: false,
    caret: null,
    style: null
  };

  function ensureStyle() {
    if (state.style && document.documentElement.contains(state.style)) {
      return;
    }

    state.style = document.createElement("style");
    state.style.id = "__emacs_readonly_caret_style";
    state.style.textContent = [
      "@keyframes __emacs_readonly_caret_blink {",
      "  0%, 55% { opacity: 1; }",
      "  56%, 100% { opacity: 0; }",
      "}",
      "#__emacs_readonly_caret {",
      "  position: fixed;",
      "  width: 2px;",
      "  min-height: 1em;",
      "  background: #2563eb;",
      "  pointer-events: none;",
      "  z-index: 2147483647;",
      "  animation: __emacs_readonly_caret_blink 1s steps(1, start) infinite;",
      "}"
    ].join("\n");
    document.documentElement.appendChild(state.style);
  }

  function ensureCaret() {
    ensureStyle();

    if (state.caret && document.documentElement.contains(state.caret)) {
      return state.caret;
    }

    state.caret = document.createElement("div");
    state.caret.id = "__emacs_readonly_caret";
    state.caret.setAttribute("aria-hidden", "true");
    document.documentElement.appendChild(state.caret);
    return state.caret;
  }

  function visibleTextNode(node) {
    if (!node
        || node.nodeType !== Node.TEXT_NODE
        || !node.nodeValue
        || !/\S/.test(node.nodeValue)) {
      return false;
    }

    var parent = node.parentElement;
    if (!parent) {
      return false;
    }

    var style = window.getComputedStyle(parent);
    return style.display !== "none" && style.visibility !== "hidden";
  }

  function textNode(forward) {
    var root = document.body || document.documentElement;
    var walker = document.createTreeWalker(
      root,
      NodeFilter.SHOW_TEXT,
      {
        acceptNode: function (node) {
          return visibleTextNode(node)
            ? NodeFilter.FILTER_ACCEPT
            : NodeFilter.FILTER_REJECT;
        }
      }
    );
    var current;
    var found = null;

    while ((current = walker.nextNode())) {
      found = current;
      if (forward) {
        return current;
      }
    }

    return found;
  }

  function rangeInDocument(range) {
    return range
      && document.documentElement.contains(range.startContainer)
      && document.documentElement.contains(range.endContainer);
  }

  function ensureSelection(atEnd) {
    var selection = window.getSelection();

    if (selection.rangeCount > 0 && rangeInDocument(selection.getRangeAt(0))) {
      return selection;
    }

    var node = textNode(!atEnd);
    var range = document.createRange();

    if (node) {
      range.setStart(node, atEnd ? node.nodeValue.length : 0);
    } else {
      range.selectNodeContents(document.body || document.documentElement);
      if (atEnd) {
        range.collapse(false);
      }
    }

    range.collapse(true);
    selection.removeAllRanges();
    selection.addRange(range);
    return selection;
  }

  function collapseToFocus(selection) {
    if (!selection || selection.rangeCount === 0 || !selection.focusNode) {
      return;
    }

    selection.collapse(selection.focusNode, selection.focusOffset);
  }

  function focusRange(selection) {
    if (!selection || selection.rangeCount === 0 || !selection.focusNode) {
      return null;
    }

    var range = document.createRange();
    range.setStart(selection.focusNode, selection.focusOffset);
    range.collapse(true);
    return range;
  }

  function rectFromRange(range) {
    if (!range) {
      return null;
    }

    var rects = range.getClientRects();
    if (rects.length > 0) {
      return rects[rects.length - 1];
    }

    var rect = range.getBoundingClientRect();
    if (rect && (rect.width || rect.height)) {
      return rect;
    }

    var container = range.startContainer;
    var offset = range.startOffset;
    var probe = range.cloneRange();

    if (container && container.nodeType === Node.TEXT_NODE) {
      if (offset > 0) {
        probe.setStart(container, offset - 1);
        probe.setEnd(container, offset);
        rects = probe.getClientRects();
        if (rects.length > 0) {
          rect = rects[rects.length - 1];
          return {
            left: rect.right,
            right: rect.right,
            top: rect.top,
            bottom: rect.bottom,
            width: 0,
            height: rect.height
          };
        }
      }

      if (offset < container.nodeValue.length) {
        probe.setStart(container, offset);
        probe.setEnd(container, offset + 1);
        rects = probe.getClientRects();
        if (rects.length > 0) {
          rect = rects[0];
          return {
            left: rect.left,
            right: rect.left,
            top: rect.top,
            bottom: rect.bottom,
            width: 0,
            height: rect.height
          };
        }
      }
    }

    return null;
  }

  function placeCaret(rect) {
    var caret = ensureCaret();

    if (!rect) {
      caret.style.display = "none";
      return;
    }

    var height = Math.max(rect.height || 0, 14);
    caret.style.display = "block";
    caret.style.left = Math.max(0, rect.left) + "px";
    caret.style.top = Math.max(0, rect.top) + "px";
    caret.style.height = height + "px";
  }

  function scrollRectIntoView(rect) {
    if (!rect) {
      return;
    }

    var margin = 24;
    var dy = 0;

    if (rect.top < margin) {
      dy = rect.top - margin;
    } else if (rect.bottom > window.innerHeight - margin) {
      dy = rect.bottom - window.innerHeight + margin;
    }

    if (dy !== 0) {
      window.scrollBy(0, dy);
    }
  }

  function updateCaret() {
    var selection = ensureSelection(false);
    var range = focusRange(selection);
    var rect = rectFromRange(range);

    scrollRectIntoView(rect);
    window.requestAnimationFrame(function () {
      placeCaret(rectFromRange(focusRange(window.getSelection())));
    });
    return true;
  }

  function move(direction, granularity, count) {
    var selection = ensureSelection(direction === "backward");
    var alter = state.markActive ? "extend" : "move";
    var steps = Math.max(1, count || 1);
    var index;

    if (!selection.modify) {
      return false;
    }

    if (!state.markActive && !selection.isCollapsed) {
      collapseToFocus(selection);
    }

    if (document.activeElement && document.activeElement.blur) {
      document.activeElement.blur();
    }

    for (index = 0; index < steps; index += 1) {
      selection.modify(alter, direction, granularity);
    }

    updateCaret();
    return true;
  }

  function setMark() {
    var selection = ensureSelection(false);

    if (!selection.isCollapsed) {
      collapseToFocus(selection);
    }

    state.markActive = true;
    updateCaret();
    return true;
  }

  function clearSelection() {
    var selection = ensureSelection(false);

    collapseToFocus(selection);
    state.markActive = false;
    updateCaret();
    return true;
  }

  function copyAndClear() {
    var selection = ensureSelection(false);
    var text = selection.toString();

    collapseToFocus(selection);
    state.markActive = false;
    updateCaret();
    return text;
  }

  window.__emacsReadOnlyCaret = {
    version: VERSION,
    move: move,
    setMark: setMark,
    clearSelection: clearSelection,
    copyAndClear: copyAndClear,
    updateCaret: updateCaret
  };

  return updateCaret();
})();

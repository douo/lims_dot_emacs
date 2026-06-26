(function () {
  var VERSION = "2026-06-26";

  if (window.__emacsReadOnlyCaret
      && window.__emacsReadOnlyCaret.version === VERSION) {
    return true;
  }

  var state = {
    version: VERSION,
    markActive: false,
    caret: null,
    style: null,
    anchor: null
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

  function currentSelection() {
    var selection = window.getSelection();

    if (selection.rangeCount > 0 && rangeInDocument(selection.getRangeAt(0))) {
      return selection;
    }

    return null;
  }

  function rangeFromPoint(x, y) {
    var position;
    var range;

    if (document.caretRangeFromPoint) {
      return document.caretRangeFromPoint(x, y);
    }

    if (document.caretPositionFromPoint) {
      position = document.caretPositionFromPoint(x, y);
      if (position && position.offsetNode) {
        range = document.createRange();
        range.setStart(position.offsetNode, position.offset);
        range.collapse(true);
        return range;
      }
    }

    return null;
  }

  function visibleViewportRange(atEnd) {
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
    var margin = 24;
    var current;
    var range;
    var rects;
    var rect;
    var index;
    var score;
    var best = null;
    var bestScore = Infinity;
    var x;
    var y;

    while ((current = walker.nextNode())) {
      range = document.createRange();
      range.selectNodeContents(current);
      rects = range.getClientRects();

      for (index = 0; index < rects.length; index += 1) {
        rect = rects[index];
        if (rect.bottom < margin
            || rect.top > window.innerHeight - margin
            || rect.right < 0
            || rect.left > window.innerWidth) {
          continue;
        }

        score = atEnd
          ? Math.abs(rect.bottom - (window.innerHeight - margin))
          : Math.abs(rect.top - margin);

        if (score < bestScore) {
          bestScore = score;
          best = {
            node: current,
            rect: rect
          };
        }
      }
    }

    if (!best) {
      return null;
    }

    x = atEnd
      ? Math.max(0, Math.min(window.innerWidth - 1, best.rect.right - 1))
      : Math.max(0, Math.min(window.innerWidth - 1, best.rect.left + 1));
    y = Math.max(0, Math.min(window.innerHeight - 1,
                             best.rect.top + (best.rect.height / 2)));
    range = rangeFromPoint(x, y);

    if (range && rangeInDocument(range)) {
      range.collapse(true);
      return range;
    }

    range = document.createRange();
    range.setStart(best.node, atEnd ? best.node.nodeValue.length : 0);
    range.collapse(true);
    return range;
  }

  function captureAnchor(range) {
    if (!range) {
      state.anchor = null;
      return;
    }

    state.anchor = {
      node: range.startContainer,
      offset: range.startOffset,
      atEnd: range.collapsed ? true : false
    };
  }

  function restoreAnchor() {
    var anchor = state.anchor;
    var range;

    if (!anchor || !document.documentElement.contains(anchor.node)) {
      return null;
    }

    range = document.createRange();
    range.setStart(anchor.node, anchor.offset);
    range.collapse(true);
    return range;
  }

  function placeAnchorAtViewport(atEnd) {
    var selection = window.getSelection();
    var range = visibleViewportRange(atEnd);

    if (!selection || !range) {
      return false;
    }

    selection.removeAllRanges();
    selection.addRange(range);
    collapseToFocus(selection);
    captureAnchor(range);
    placeCaret(rectFromRange(range));
    return true;
  }

  function startCaret() {
    state.markActive = false;
    return placeAnchorAtViewport(false);
  }

  function ensureSelection(atEnd, preferViewport) {
    var selection = currentSelection();
    var range;

    if (selection) {
      return selection;
    }

    if (state.anchor) {
      range = restoreAnchor();
      if (range) {
        selection = window.getSelection();
        selection.removeAllRanges();
        selection.addRange(range);
        return selection;
      }
      state.anchor = null;
    }

    if (preferViewport) {
      placeAnchorAtViewport(atEnd);
      selection = window.getSelection();
      selection = currentSelection();
      if (selection) {
        return selection;
      }
    }

    range = preferViewport ? visibleViewportRange(atEnd) : null;

    var node = textNode(!atEnd);

    if (!range && node) {
      range = document.createRange();
      range.setStart(node, atEnd ? node.nodeValue.length : 0);
    } else if (!range) {
      range = document.createRange();
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

  function hideCaret() {
    if (state.caret && document.documentElement.contains(state.caret)) {
      state.caret.style.display = "none";
    }
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

  function updateCaret(shouldScroll) {
    var selection = currentSelection();
    var range = focusRange(selection);
    var rect = rectFromRange(range);

    if (!selection) {
      hideCaret();
      state.anchor = null;
      return false;
    }

    if (shouldScroll) {
      scrollRectIntoView(rect);
    }
    window.requestAnimationFrame(function () {
      placeCaret(rectFromRange(focusRange(window.getSelection())));
    });
    return true;
  }

  function move(direction, granularity, count) {
    var selection = ensureSelection(direction === "backward", true);
    var alter = state.markActive ? "extend" : "move";
    var steps = Math.max(1, count || 1);
    var index;

    if (!selection.modify) {
      return false;
    }

    if (!state.markActive && !selection.isCollapsed) {
      collapseToFocus(selection);
    }

    for (index = 0; index < steps; index += 1) {
      selection.modify(alter, direction, granularity);
    }

    updateCaret(true);
    return true;
  }

  function setMark() {
    var selection = ensureSelection(false, true);

    if (!selection.isCollapsed) {
      collapseToFocus(selection);
    }

    state.markActive = true;
    updateCaret(false);
    return true;
  }

  function clearSelection() {
    var selection = currentSelection();

    if (selection) {
      selection.removeAllRanges();
    }
    state.markActive = false;
    state.anchor = null;
    hideCaret();
    return true;
  }

  function copyAndClear() {
    var selection = currentSelection();
    var text = selection ? selection.toString() : "";

    if (selection) {
      selection.removeAllRanges();
    }
    state.markActive = false;
    state.anchor = null;
    hideCaret();
    return text;
  }

  window.__emacsReadOnlyCaret = {
    version: VERSION,
    move: move,
    startCaret: startCaret,
    setMark: setMark,
    placeAnchorAtViewport: placeAnchorAtViewport,
    clearSelection: clearSelection,
    copyAndClear: copyAndClear,
    hideCaret: hideCaret,
    updateCaret: updateCaret
  };

  return true;
})();

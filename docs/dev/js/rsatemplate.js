/* rsatemplate: small progressive enhancements for pkgdown pages. */
(function () {
  "use strict";

  function onReady(fn) {
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", fn);
    } else {
      fn();
    }
  }

  function finalPath(pathname) {
    var path = String(pathname || "/").replace(/\\/g, "/").toLowerCase();
    path = path.replace(/\/index\.html$/, "/");
    path = path.replace(/\.html$/, "");
    if (path === "") {
      path = "/";
    }
    return path;
  }

  function fixCanonical() {
    var link = document.querySelector('link[rel="canonical"][data-rsa-site-url]');
    if (!link) {
      return;
    }
    var base = link.getAttribute("data-rsa-site-url").replace(/\/+$/, "");
    if (!base) {
      return;
    }
    link.setAttribute("href", base + finalPath(window.location.pathname));
  }

  function initCopyButtons() {
    document.querySelectorAll("pre").forEach(function (pre) {
      if (pre.querySelector(".rsa-copy-btn") || pre.closest(".rsa-install-card")) {
        return;
      }
      var btn = document.createElement("button");
      btn.type = "button";
      btn.className = "rsa-copy-btn";
      btn.setAttribute("aria-label", "Copy code to clipboard");
      btn.textContent = "Copy";
      btn.addEventListener("click", function () {
        var code = pre.querySelector("code");
        var text = code ? code.innerText : pre.innerText;
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(text).then(function () {
            btn.textContent = "Copied";
            btn.classList.add("is-copied");
            window.setTimeout(function () {
              btn.textContent = "Copy";
              btn.classList.remove("is-copied");
            }, 1800);
          }).catch(function () {
            btn.textContent = "Copy failed";
          });
        }
      });
      pre.classList.add("rsa-has-copy");
      pre.appendChild(btn);
    });
  }

  function initReferenceFilter() {
    var grid = document.querySelector(".rsa-ref-grid");
    if (!grid) {
      return;
    }
    var chips = document.querySelectorAll("[data-rsa-ref-filter]");
    var current = "";

    Array.prototype.forEach.call(grid.children, function (el) {
      if (el.classList.contains("rsa-ref-heading")) {
        current = el.getAttribute("data-rsa-ref-section") || "";
      }
      if (
        el.classList.contains("rsa-ref-card") ||
        el.classList.contains("rsa-ref-desc") ||
        el.classList.contains("rsa-ref-subheading")
      ) {
        el.setAttribute("data-rsa-resolved-section", current);
      }
    });

    function apply(filter) {
      chips.forEach(function (chip) {
        var active = chip.getAttribute("data-rsa-ref-filter") === filter;
        chip.classList.toggle("is-active", active);
        chip.setAttribute("aria-pressed", active ? "true" : "false");
      });
      Array.prototype.forEach.call(grid.children, function (el) {
        var section = el.getAttribute("data-rsa-resolved-section") ||
          el.getAttribute("data-rsa-ref-section") || "";
        el.hidden = filter !== "all" && section !== filter;
      });
    }

    chips.forEach(function (chip) {
      chip.addEventListener("click", function () {
        apply(chip.getAttribute("data-rsa-ref-filter") || "all");
      });
    });
  }

  function initPractiseBoxes() {
    var boxes = document.querySelectorAll("[data-rsa-practise]");
    if (!boxes.length) {
      return;
    }
    var file = decodeURIComponent(
      window.location.pathname.split("/").pop() || ""
    ).replace(/\.html$/, "");
    boxes.forEach(function (box) {
      var want = String(box.getAttribute("data-file") || "").replace(/\.html$/, "");
      if (want && want === file) {
        box.hidden = false;
      }
    });
  }

  function initThemeFallback() {
    var buttons = document.querySelectorAll("[data-rsa-theme-toggle]");
    if (!buttons.length) {
      return;
    }
    buttons.forEach(function (btn) {
      btn.addEventListener("click", function () {
        var root = document.documentElement;
        var next = root.getAttribute("data-bs-theme") === "dark" ? "light" : "dark";
        root.setAttribute("data-bs-theme", next);
        try {
          window.localStorage.setItem("theme", next);
        } catch (e) {
          /* storage may be blocked; the toggle still works for this page */
        }
        btn.setAttribute("aria-label", next === "dark" ? "Switch to light mode" : "Switch to dark mode");
      });
    });
  }

  onReady(function () {
    var footerSignup = document.querySelector("footer .rsa-footer form[data-rsa-signup]");
    if (footerSignup) {
      footerSignup.setAttribute("data-site", window.location.hostname);
      document.querySelectorAll("#main [data-rsa-signup]").forEach(function (signup) {
        signup.hidden = true;
        signup.removeAttribute("data-rsa-signup");
      });
    }
    fixCanonical();
    initCopyButtons();
    initReferenceFilter();
    initPractiseBoxes();
    initThemeFallback();
  });
})();

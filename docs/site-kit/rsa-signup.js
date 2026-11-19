/**
 * Rsquared Academy Site-Kit — Email Sign-up (Phase B)
 * Version: v1.3.0
 *
 * Dependency-free email newsletter sign-up that posts straight to Zoho
 * Campaigns with a simple CORS `fetch` request (no Zoho script), renders the
 * result inline, and tags each contact with the site it came from
 * (custom field CONTACT_CF1 = "Signup site").
 *
 * Two ways to use it:
 *   - Render mode: drop a `<div data-rsa-signup></div>` and the kit builds the
 *     whole card. Whatever fallback markup the author puts inside is replaced
 *     once this script runs.
 *   - Enhance mode: put `data-rsa-signup` on an existing `<form>`. The kit
 *     keeps the author's markup and styles and only wires up validation, the
 *     Zoho submission, the inline status message and the GA4 event.
 *
 * Two opt-in script attributes add the eBook behaviour (v1.3.0):
 *   - `data-rsa-signup-quarto-margin`: append a compact sign-up box to
 *     `#quarto-margin-sidebar` (desktop only).
 *   - `data-rsa-signup-experiment="<name>"`: assign the visitor to an A/B
 *     variant, tag every sign-up with `<name>:<A|B|none>` in CONTACT_CF2 and,
 *     for variant B, show a dismissible sticky bar after 50% scroll.
 *
 * With neither attribute the behaviour is exactly v1.2.0.
 *
 * The whole file is wrapped so it creates no globals except
 * `window.RsaSignup`. The pure helpers are returned from the factory and
 * exported for Node.js unit tests; the browser runtime is started inside the
 * factory when `window` and `document` exist.
 */

(function (root, factory) {
  const api = factory();
  if (typeof module !== 'undefined' && module.exports) {
    module.exports = api;
  }
  // The browser runtime is started inside the factory whenever window and
  // document exist; this wrapper only wires up the Node.js export.
})(this, function () {
  'use strict';

  // Zoho Campaigns quick-form submission endpoint.
  const ZOHO_ENDPOINT = 'https://zgnp-zngp.maillist-manage.com/weboptin.zc';

  // Fixed hidden fields required by the Zoho Campaigns quick-form endpoint.
  const ZOHO_HIDDEN_FIELDS = {
    submitType: 'optinCustomView',
    emailReportId: '',
    formType: 'QuickForm',
    zx: '126630e09',
    zcvers: '3.0',
    oldListIds: '',
    mode: 'OptinCreateView',
    zcld: '117e9dae2c8129085',
    zctd: '',
    zc_trackCode: 'ZCFORMVIEW',
    zc_formIx:
      '3z4a0e76496630b2bec6ad39127e0d973ece8e7e9dc0df5f42a50f782dd2aad4b0'
  };

  // Maximum length kept for the optional visitor-supplied text fields.
  const MAX_TEXT_LENGTH = 100;

  // Zoho custom field that stores the A/B variant of an eBook sign-up.
  // Zoho custom field 'Signup variant'; Claude confirms the field id before release.
  const VARIANT_FIELD = 'CONTACT_CF2';

  // localStorage prefixes/keys used by the eBook experiment.
  const EXPERIMENT_STORAGE_PREFIX = 'rsa_signup_exp_';
  const BAR_DISMISSED_KEY = 'rsa_signup_bar_dismissed_until';
  const SIGNUP_DONE_KEY = 'rsa_signup_done';

  // The sticky bar stays dismissed for 30 days.
  const BAR_DISMISS_MS = 30 * 24 * 60 * 60 * 1000;

  // The bar appears after the reader has scrolled past half the document.
  const SCROLL_THRESHOLD = 0.5;

  /**
   * Converts a value to a trimmed string, capped at `maxLength` characters.
   * Returns an empty string for null/undefined input.
   *
   * @param {*} value
   * @param {number} maxLength
   * @returns {string}
   */
  function normaliseText(value, maxLength) {
    if (value == null) return '';
    return String(value).trim().slice(0, maxLength);
  }

  /**
   * Builds the URL-encoded request body for a Zoho Campaigns sign-up.
   *
   * The visitor fields are CONTACT_EMAIL (required), FIRSTNAME (optional),
   * CONTACT_CF1 (the "Signup site" custom field) and, when an A/B experiment
   * is running, CONTACT_CF2 (the "Signup variant" custom field, value
   * `<experiment>:<variant>`). The fixed hidden fields are appended unchanged.
   *
   * Without `variant` the serialised body is byte-identical to v1.2.0: the
   * variant field is not present at all.
   *
   * @param {{ email?: string, firstName?: string, site?: string, variant?: string }} [fields]
   * @returns {URLSearchParams}
   */
  function buildSignupBody({ email, firstName, site, variant } = {}) {
    const body = new URLSearchParams();
    body.set('CONTACT_EMAIL', email == null ? '' : String(email).trim());
    body.set('FIRSTNAME', normaliseText(firstName, MAX_TEXT_LENGTH));
    body.set('CONTACT_CF1', normaliseText(site, MAX_TEXT_LENGTH));
    for (const key of Object.keys(ZOHO_HIDDEN_FIELDS)) {
      body.set(key, ZOHO_HIDDEN_FIELDS[key]);
    }
    const variantValue = normaliseText(variant, MAX_TEXT_LENGTH);
    if (variantValue) {
      body.set(VARIANT_FIELD, variantValue);
    }
    return body;
  }

  /**
   * Decodes the small set of HTML entities Zoho uses inside its JSON island.
   * `&amp;` is decoded last so an encoded entity is never double-decoded.
   *
   * @param {string} value
   * @returns {string}
   */
  function decodeHtmlEntities(value) {
    return value
      .replace(/&#34;/g, '"')
      .replace(/&#39;/g, "'")
      .replace(/&lt;/g, '<')
      .replace(/&gt;/g, '>')
      .replace(/&amp;/g, '&');
  }

  /**
   * Parses the JSON island returned by the Zoho Campaigns endpoint.
   *
   * The response is HTTP 200 HTML containing a JSON object between
   * `##ZCJSONSTART##` and `##ZCJSON##`, with double quotes encoded as `&#34;`.
   * A successful response has `spmSubmit: false` and a non-empty `emailid`;
   * `isNewContact` distinguishes a brand-new contact from an existing one.
   * Anything unparseable, missing the island, or carrying an error flag returns
   * `ok: false`. This function never throws.
   *
   * @param {*} text
   * @returns {{ ok: boolean, isNew: boolean, raw: object|null }}
   */
  function parseZohoResponse(text) {
    try {
      if (typeof text !== 'string') {
        return { ok: false, isNew: false, raw: null };
      }
      const startMarker = '##ZCJSONSTART##';
      const endMarker = '##ZCJSON##';
      const startIndex = text.indexOf(startMarker);
      if (startIndex === -1) {
        return { ok: false, isNew: false, raw: null };
      }
      const from = startIndex + startMarker.length;
      const endIndex = text.indexOf(endMarker, from);
      if (endIndex === -1) {
        return { ok: false, isNew: false, raw: null };
      }
      const jsonText = decodeHtmlEntities(text.slice(from, endIndex).trim());
      const raw = JSON.parse(jsonText);
      if (!raw || typeof raw !== 'object' || Array.isArray(raw)) {
        return { ok: false, isNew: false, raw: null };
      }
      const hasEmail =
        typeof raw.emailid === 'string' && raw.emailid.trim().length > 0;
      const ok = hasEmail && !raw.spmSubmit;
      return { ok, isNew: ok && raw.isNewContact === true, raw };
    } catch (e) {
      return { ok: false, isNew: false, raw: null };
    }
  }

  /**
   * Pragmatic email check: exactly one `@`, a non-empty local part, a dotted
   * domain, no whitespace and at most 254 characters.
   *
   * @param {*} str
   * @returns {boolean}
   */
  function isValidEmail(str) {
    if (typeof str !== 'string') return false;
    const email = str.trim();
    if (email.length === 0 || email.length > 254) return false;
    if (/\s/.test(email)) return false;
    const atIndex = email.indexOf('@');
    if (atIndex <= 0) return false;
    if (email.indexOf('@', atIndex + 1) !== -1) return false;
    const local = email.slice(0, atIndex);
    const domain = email.slice(atIndex + 1);
    if (local.length === 0 || domain.length === 0) return false;
    if (!domain.includes('.')) return false;
    if (domain.startsWith('.') || domain.endsWith('.')) return false;
    if (domain.includes('..')) return false;
    return true;
  }

  /**
   * Cheap bot heuristic: a filled honeypot or a form submitted less than
   * 2000 ms after it was rendered. A missing/blank honeypot and a missing or
   * large `elapsedMs` are treated as human.
   *
   * @param {{ honeypotValue?: string, elapsedMs?: number }} [signals]
   * @returns {boolean}
   */
  function isLikelyBot({ honeypotValue, elapsedMs } = {}) {
    if (honeypotValue != null && String(honeypotValue).trim() !== '') {
      return true;
    }
    if (typeof elapsedMs === 'number' && elapsedMs < 2000) {
      return true;
    }
    return false;
  }

  /**
   * Default value for the "Signup site" tag: the hostname plus the first path
   * segment, when there is one.
   *
   * Rule: strip any query string/hash, split the path on `/`, drop empty
   * segments, and join the hostname with the first remaining segment using a
   * single `/`. Examples:
   *   ('rsquaredacademy.com', '/olsrr')                  -> 'rsquaredacademy.com/olsrr'
   *   ('olsrr.rsquaredacademy.com', '/articles/x.html')  -> 'olsrr.rsquaredacademy.com/articles'
   *   ('rsquaredacademy.com', '/')                       -> 'rsquaredacademy.com'
   *
   * @param {*} hostname
   * @param {*} pathname
   * @returns {string}
   */
  function defaultSiteLabel(hostname, pathname) {
    const host = typeof hostname === 'string' ? hostname.trim() : '';
    const rawPath = typeof pathname === 'string' ? pathname.split(/[?#]/)[0] : '';
    const segments = rawPath.split('/').filter(function (part) {
      return part.length > 0;
    });
    if (host && segments.length > 0) {
      return host + '/' + segments[0];
    }
    return host;
  }

  /**
   * Checks whether an IANA time zone identifier represents a European region
   * requiring explicit consent. This is a deliberate copy of the pure helper
   * in `rsa-analytics.js`: the two files must stay independent (a book may
   * load the sign-up script without the analytics script).
   *
   * @param {string} timeZone
   * @returns {boolean}
   */
  function isEuropeanTimeZone(timeZone) {
    if (!timeZone || typeof timeZone !== 'string') return false;
    if (timeZone.startsWith('Europe/')) return true;
    if (
      timeZone === 'Atlantic/Canary' ||
      timeZone === 'Atlantic/Madeira' ||
      timeZone === 'Atlantic/Reykjavik'
    ) {
      return true;
    }
    return false;
  }

  /**
   * Parses the rsa_consent cookie value from a document.cookie string.
   * Copied from `rsa-analytics.js` so the sign-up script stays independent.
   *
   * @param {string} cookieString
   * @returns {'granted'|'denied'|null}
   */
  function parseConsentCookie(cookieString) {
    if (!cookieString || typeof cookieString !== 'string') return null;
    const match = cookieString.match(/(?:^|;\s*)rsa_consent=(granted|denied)(?:;|$)/);
    return match ? match[1] : null;
  }

  /**
   * localStorage key for an experiment, or '' when the name is blank.
   *
   * @param {string} experiment
   * @returns {string}
   */
  function experimentStorageKey(experiment) {
    const name = typeof experiment === 'string' ? experiment.trim() : '';
    return name ? EXPERIMENT_STORAGE_PREFIX + name : '';
  }

  /**
   * True for a value we are willing to reuse from storage.
   *
   * @param {*} value
   * @returns {boolean}
   */
  function isStoredVariant(value) {
    return value === 'A' || value === 'B';
  }

  /**
   * Reads a stored variant. Any storage failure (missing, disabled, throwing,
   * invalid value) is treated as "nothing stored".
   *
   * @param {{ getItem: (key: string) => * }|null} storage
   * @param {string} key
   * @returns {'A'|'B'|null}
   */
  function readStoredVariant(storage, key) {
    if (!storage || !key) return null;
    try {
      const value = storage.getItem(key);
      return isStoredVariant(value) ? value : null;
    } catch (e) {
      return null;
    }
  }

  /**
   * Stores a variant. Any storage failure is ignored: the visitor still gets
   * a valid variant for this page view.
   *
   * @param {{ setItem: (key: string, value: string) => * }|null} storage
   * @param {string} key
   * @param {'A'|'B'} variant
   * @returns {boolean} true when the value was stored
   */
  function writeStoredVariant(storage, key, variant) {
    if (!storage || !key || !isStoredVariant(variant)) return false;
    try {
      storage.setItem(key, variant);
      return true;
    } catch (e) {
      return false;
    }
  }

  /**
   * Assigns a variant from a random number in [0, 1): A below 0.5, B from 0.5.
   * Non-finite input falls back to Math.random().
   *
   * @param {number} randomNumber
   * @returns {'A'|'B'}
   */
  function assignVariant(randomNumber) {
    const value =
      typeof randomNumber === 'number' && isFinite(randomNumber)
        ? randomNumber
        : Math.random();
    return value < 0.5 ? 'A' : 'B';
  }

  /**
   * Resolves the visitor's variant for an experiment, reusing a stored value.
   *
   * Rules:
   *   - no experiment name -> 'none'
   *   - European time zone without `rsa_consent=granted` -> 'none', with no
   *     read and no write of experiment storage
   *   - otherwise a stored A/B is reused, or a new one is assigned and stored
   *   - `forceVariant` (localhost/file QA override only) wins over all of it
   *     and is never stored
   *
   * `storage` only has to expose `getItem`/`setItem`; everything is wrapped in
   * try/catch so a throwing or missing localStorage never breaks the page.
   *
   * @param {{
   *   experiment?: string,
   *   timeZone?: string,
   *   consentGranted?: boolean,
   *   randomNumber?: number,
   *   storage?: { getItem?: Function, setItem?: Function }|null,
   *   forceVariant?: string
   * }} [options]
   * @returns {'A'|'B'|'none'}
   */
  function resolveVariant({
    experiment,
    timeZone,
    consentGranted,
    randomNumber,
    storage,
    forceVariant
  } = {}) {
    const key = experimentStorageKey(experiment);
    if (!key) return 'none';
    if (isStoredVariant(forceVariant)) return forceVariant;
    if (isEuropeanTimeZone(timeZone) && consentGranted !== true) return 'none';
    const stored = readStoredVariant(storage, key);
    if (stored) return stored;
    const variant = assignVariant(randomNumber);
    writeStoredVariant(storage, key, variant);
    return variant;
  }

  /**
   * Formats the Zoho/GA4 variant label, e.g. `ebook-bar-v1:B`.
   * Returns '' when either part is missing.
   *
   * @param {string} experiment
   * @param {string} variant
   * @returns {string}
   */
  function formatVariantLabel(experiment, variant) {
    const name = typeof experiment === 'string' ? experiment.trim() : '';
    const value = typeof variant === 'string' ? variant.trim() : '';
    if (!name || !value) return '';
    return name + ':' + value;
  }

  /**
   * True once the reader has scrolled past `ratio` of the document height.
   *
   * A page that cannot scroll (`docH <= viewportH`) always returns false, so
   * the sticky bar never appears on short pages. The comparison is strict:
   * exactly at the threshold is not "past" it.
   *
   * @param {number} scrollY
   * @param {number} viewportH
   * @param {number} docH
   * @param {number} [ratio]
   * @returns {boolean}
   */
  function pastScrollThreshold(scrollY, viewportH, docH, ratio) {
    const values = [scrollY, viewportH, docH];
    for (const value of values) {
      if (typeof value !== 'number' || !isFinite(value) || value < 0) {
        return false;
      }
    }
    const share =
      typeof ratio === 'number' && isFinite(ratio) && ratio > 0 && ratio < 1
        ? ratio
        : SCROLL_THRESHOLD;
    if (docH <= viewportH) return false;
    return scrollY > docH * share;
  }

  /**
   * Timestamp (ms) until which a dismissed bar stays hidden: now + 30 days.
   *
   * @param {number} [nowMs]
   * @returns {number}
   */
  function barDismissedUntil(nowMs) {
    const now =
      typeof nowMs === 'number' && isFinite(nowMs) ? nowMs : Date.now();
    return now + BAR_DISMISS_MS;
  }

  /**
   * True while a stored dismissal timestamp is still in the future. Invalid,
   * missing or expired values are not dismissed (the bar may show again).
   *
   * @param {*} untilMs
   * @param {number} [nowMs]
   * @returns {boolean}
   */
  function isBarDismissed(untilMs, nowMs) {
    const until = Number(untilMs);
    const now =
      typeof nowMs === 'number' && isFinite(nowMs) ? nowMs : Date.now();
    if (!isFinite(until)) return false;
    return until > now;
  }

  // Selectors used to find the author's cooperative fields in enhance mode.
  const ENHANCE_SELECTORS = {
    emailInput: 'input[type="email"]',
    nameInput: '[data-rsa-signup-name]',
    honeypotInput: '[data-rsa-signup-hp]',
    button: 'button[type="submit"], input[type="submit"]',
    statusEl: '[data-rsa-signup-status]'
  };

  /**
   * Picks the cooperative elements inside an existing form for enhance mode.
   *
   * `form` only has to expose `querySelector`, so the picker is unit-testable
   * without a DOM library. Missing elements are returned as `null`; the caller
   * decides which are required (only the email input is).
   *
   * @param {{ querySelector: (selector: string) => * }|*} form
   * @returns {{
   *   emailInput: *|null,
   *   nameInput: *|null,
   *   honeypotInput: *|null,
   *   button: *|null,
   *   statusEl: *|null
   * }}
   */
  function pickFormParts(form) {
    if (form == null || typeof form.querySelector !== 'function') {
      return {
        emailInput: null,
        nameInput: null,
        honeypotInput: null,
        button: null,
        statusEl: null
      };
    }
    return {
      emailInput: form.querySelector(ENHANCE_SELECTORS.emailInput),
      nameInput: form.querySelector(ENHANCE_SELECTORS.nameInput),
      honeypotInput: form.querySelector(ENHANCE_SELECTORS.honeypotInput),
      button: form.querySelector(ENHANCE_SELECTORS.button),
      statusEl: form.querySelector(ENHANCE_SELECTORS.statusEl)
    };
  }

  /**
   * Browser runtime execution
   */
  if (typeof window !== 'undefined' && typeof document !== 'undefined') {
    (function initRsaSignupRuntime() {
      let instanceCounter = 0;

      // --- v1.3.0 opt-in attributes -------------------------------------
      // Read the flags from our own <script> tag (document.currentScript),
      // falling back to the first matching script when the kit is injected
      // dynamically. With neither attribute nothing below changes behaviour.
      const scriptEl =
        document.currentScript ||
        document.querySelector('script[src*="rsa-signup.js"]');
      const marginEnabled = !!(
        scriptEl && scriptEl.hasAttribute('data-rsa-signup-quarto-margin')
      );
      const experimentName = scriptEl
        ? (scriptEl.getAttribute('data-rsa-signup-experiment') || '').trim()
        : '';

      // localStorage may be missing or throw (private mode, blocked storage).
      // Every access goes through these two helpers, so a throwing storage
      // can never break the page.
      function browserStorage() {
        try {
          return window.localStorage || null;
        } catch (e) {
          return null;
        }
      }

      function readStored(key) {
        const storage = browserStorage();
        if (!storage) return null;
        try {
          return storage.getItem(key);
        } catch (e) {
          return null;
        }
      }

      function writeStored(key, value) {
        const storage = browserStorage();
        if (!storage) return false;
        try {
          storage.setItem(key, value);
          return true;
        } catch (e) {
          return false;
        }
      }

      function currentTimeZone() {
        try {
          return Intl.DateTimeFormat().resolvedOptions().timeZone || '';
        } catch (e) {
          return '';
        }
      }

      // QA only: `?rsa_variant=A|B` is honoured on localhost and file: pages
      // so the demo can force a variant. It is ignored on every real host and
      // is never written to storage.
      function qaVariantOverride() {
        try {
          const location = window.location;
          const host = (location.hostname || '').toLowerCase();
          const isLocal =
            location.protocol === 'file:' ||
            host === 'localhost' ||
            host === '127.0.0.1' ||
            host === '::1';
          if (!isLocal) return null;
          const value = (
            new URLSearchParams(location.search).get('rsa_variant') || ''
          ).toUpperCase();
          return isStoredVariant(value) ? value : null;
        } catch (e) {
          return null;
        }
      }

      let signupDone = false;
      const qaVariant = experimentName ? qaVariantOverride() : null;
      const variant = experimentName
        ? resolveVariant({
            experiment: experimentName,
            timeZone: currentTimeZone(),
            consentGranted: parseConsentCookie(document.cookie) === 'granted',
            storage: browserStorage(),
            forceVariant: qaVariant
          })
        : '';
      const variantLabel =
        experimentName && variant ? formatVariantLabel(experimentName, variant) : '';

      // QA hook: the resolved variant is exposed on <html> only when the
      // experiment is enabled, so v1.2.0 pages are untouched.
      if (experimentName) {
        document.documentElement.setAttribute(
          'data-rsa-signup-variant',
          variant || 'none'
        );
      }

      // Any successful sign-up (any form on this origin) hides the bar for
      // good and is remembered across page views. No-op without an experiment,
      // so v1.2.0 pages gain no storage writes.
      function markSignupDone() {
        if (!experimentName) return;
        signupDone = true;
        // Consent-region visitors ('none') get no experiment storage at all.
        if (variant !== 'none') writeStored(SIGNUP_DONE_KEY, '1');
        const bar = document.querySelector('[data-rsa-signup-layout="bar"]');
        if (bar) bar.removeAttribute('data-rsa-signup-bar-visible');
      }

      // Inline status messages shown to the visitor.
      const MESSAGE = {
        new: 'Almost done: check your inbox for a confirmation email.',
        existing: "You're already subscribed. Thank you!",
        error: "Sorry, that didn't work. Please try again in a minute.",
        invalid: 'Please enter a valid email address.',
        pending: 'Subscribing\u2026'
      };

      // Sends the anonymous GA4 sign_up event. Never includes the email or
      // name. The experiment variant is added only when one is active.
      function trackSignup(site, signupVariant) {
        if (typeof window.gtag === 'function') {
          const payload = {
            method: 'zoho_campaigns',
            signup_site: site
          };
          if (signupVariant) payload.signup_variant = signupVariant;
          window.gtag('event', 'sign_up', payload);
        }
      }

      // Creates an element with an optional class and text content.
      function makeElement(tagName, className, text) {
        const el = document.createElement(tagName);
        if (className) el.className = className;
        if (text != null) el.textContent = text;
        return el;
      }

      // Builds a labelled input field, returning both wrapper and input.
      function buildField(options) {
        const field = makeElement('div', 'rsa-signup__field');
        const labelClass =
          'rsa-signup__label' +
          (options.labelClass ? ' ' + options.labelClass : '');
        const label = makeElement('label', labelClass, options.label);
        label.setAttribute('for', options.id);
        if (options.optional) {
          label.appendChild(
            makeElement('span', 'rsa-signup__optional', ' (optional)')
          );
        }
        const input = document.createElement('input');
        input.className = 'rsa-signup__input';
        input.id = options.id;
        input.name = options.name;
        input.type = options.type;
        if (options.autocomplete) {
          input.setAttribute('autocomplete', options.autocomplete);
        }
        if (options.required) input.required = true;
        field.appendChild(label);
        field.appendChild(input);
        return { field: field, input: input };
      }

      // Resolves the "Signup site" value for a container or form.
      function siteLabelFor(el) {
        return (
          (el.getAttribute('data-site') || '').trim() ||
          defaultSiteLabel(window.location.hostname, window.location.pathname)
        );
      }

      // Honeypot: off-screen via CSS (never display:none) and never focusable.
      function createHoneypot(id) {
        const honeypot = makeElement('div', 'rsa-signup__hp');
        honeypot.setAttribute('aria-hidden', 'true');
        const honeypotLabel = makeElement('label', null, 'Website');
        honeypotLabel.setAttribute('for', id);
        const input = document.createElement('input');
        input.id = id;
        input.name = 'website';
        input.type = 'text';
        input.tabIndex = -1;
        input.setAttribute('autocomplete', 'off');
        honeypot.appendChild(honeypotLabel);
        honeypot.appendChild(input);
        return { node: honeypot, input: input };
      }

      // Polite live region used by every rendered form.
      function createStatusEl(id) {
        const status = makeElement('p', 'rsa-signup__status');
        status.id = id;
        status.setAttribute('role', 'status');
        status.setAttribute('aria-live', 'polite');
        return status;
      }

      // The small Privacy link shown under every rendered form.
      function createPrivacy(privacyUrl) {
        const privacy = makeElement('p', 'rsa-signup__privacy');
        const privacyLink = makeElement('a', 'rsa-signup__privacy-link', 'Privacy');
        privacyLink.href = privacyUrl;
        privacy.appendChild(privacyLink);
        return privacy;
      }

      // Returns a tiny controller around a status element. The author's original
      // text is remembered; `reset` clears a previous result and restores it.
      function createStatus(statusEl) {
        const originalText = statusEl.textContent;
        function setStatus(message, kind) {
          statusEl.textContent = message;
          statusEl.classList.remove(
            'rsa-signup__status--error',
            'rsa-signup__status--success'
          );
          if (kind === 'error') {
            statusEl.classList.add('rsa-signup__status--error');
          } else if (kind === 'success') {
            statusEl.classList.add('rsa-signup__status--success');
          }
        }
        function reset() {
          statusEl.textContent = originalText;
          statusEl.classList.remove(
            'rsa-signup__status--error',
            'rsa-signup__status--success'
          );
        }
        return { setStatus: setStatus, reset: reset };
      }

      // Wires one submit handler shared by render and enhance modes.
      function attachSubmit(options) {
        const form = options.form;
        const emailInput = options.emailInput;
        const nameInput = options.nameInput;
        const honeypotInput = options.honeypotInput;
        const button = options.button;
        const statusEl = options.statusEl;
        const site = options.site;
        const startedAt = options.startedAt;
        // `<experiment>:<A|B|none>`; empty when no experiment is running.
        const variant = options.variant || '';
        const onSuccess = options.onSuccess;

        const status = createStatus(statusEl);
        // The author may already have linked the email input to some help text.
        const originalDescribedBy = emailInput.getAttribute('aria-describedby');

        form.addEventListener('submit', function (event) {
          event.preventDefault();

          // Start every attempt from a clean result state.
          status.reset();

          const email = emailInput.value;
          const firstName = nameInput ? nameInput.value : '';

          if (
            isLikelyBot({
              honeypotValue: honeypotInput ? honeypotInput.value : '',
              elapsedMs: Date.now() - startedAt
            })
          ) {
            // Silently accept the submission without contacting Zoho.
            status.setStatus(MESSAGE.new, 'success');
            return;
          }

          if (!isValidEmail(email)) {
            status.setStatus(MESSAGE.invalid, 'error');
            emailInput.setAttribute('aria-invalid', 'true');
            emailInput.setAttribute('aria-describedby', statusEl.id);
            try {
              emailInput.focus();
            } catch (e) {
              // Ignore focus failures.
            }
            return;
          }

          emailInput.removeAttribute('aria-invalid');
          if (originalDescribedBy != null) {
            emailInput.setAttribute('aria-describedby', originalDescribedBy);
          } else {
            emailInput.removeAttribute('aria-describedby');
          }
          if (button) button.disabled = true;
          status.setStatus(MESSAGE.pending, null);

          const body = buildSignupBody({ email, firstName, site, variant });

          fetch(ZOHO_ENDPOINT, {
            method: 'POST',
            body: body,
            mode: 'cors',
            credentials: 'omit'
          })
            .then(function (response) {
              if (!response.ok) {
                throw new Error('Unexpected HTTP status ' + response.status);
              }
              return response.text();
            })
            .then(function (text) {
              const result = parseZohoResponse(text);
              if (button) button.disabled = false;
              if (!result.ok) {
                status.setStatus(MESSAGE.error, 'error');
                return;
              }
              if (result.isNew) {
                status.setStatus(MESSAGE.new, 'success');
                form.reset();
                trackSignup(site, variant);
              } else {
                status.setStatus(MESSAGE.existing, 'success');
              }
              // Any successful submission (new or existing) counts for the
              // experiment: the sticky bar never appears again.
              if (typeof onSuccess === 'function') onSuccess();
            })
            .catch(function () {
              if (button) button.disabled = false;
              status.setStatus(MESSAGE.error, 'error');
            });
        });
      }

      // Render mode: builds the whole card into a container element.
      function renderForm(container) {
        const renderedAt = Date.now();
        instanceCounter += 1;
        const uid = 'rsa-signup-' + instanceCounter;

        const site = siteLabelFor(container);
        const headingText =
          container.getAttribute('data-heading') ||
          'Get updates from Rsquared Academy';
        const bodyText =
          container.getAttribute('data-text') ||
          'New courses, eBook chapters and R package releases. Free. Unsubscribe any time.';
        const buttonLabel = container.getAttribute('data-button') || 'Subscribe';
        const showName = container.getAttribute('data-name') !== 'off';
        const headingAsParagraph =
          container.getAttribute('data-heading-level') === 'p';
        const privacyUrl = container.getAttribute('data-privacy-url') || '/privacy/';

        const headingId = uid + '-heading';
        const emailId = uid + '-email';
        const nameId = uid + '-name';
        const honeypotId = uid + '-website';
        const statusId = uid + '-status';

        // Replace whatever fallback markup the author placed inside.
        container.textContent = '';
        container.classList.add('rsa-signup');

        const headingEl = makeElement(
          headingAsParagraph ? 'p' : 'h2',
          'rsa-signup__heading',
          headingText
        );
        headingEl.id = headingId;

        const textEl = makeElement('p', 'rsa-signup__text', bodyText);

        const form = document.createElement('form');
        form.className = 'rsa-signup__form';
        form.setAttribute('novalidate', '');
        form.setAttribute('aria-labelledby', headingId);

        const emailField = buildField({
          id: emailId,
          name: 'email',
          type: 'email',
          autocomplete: 'email',
          label: 'Email address',
          required: true
        });

        const nameField = showName
          ? buildField({
              id: nameId,
              name: 'firstName',
              type: 'text',
              autocomplete: 'given-name',
              label: 'First name',
              optional: true,
              required: false
            })
          : null;

        const honeypot = createHoneypot(honeypotId);

        const button = makeElement(
          'button',
          'rsa-signup__button',
          buttonLabel
        );
        button.type = 'submit';

        const status = createStatusEl(statusId);

        form.appendChild(emailField.field);
        if (nameField) form.appendChild(nameField.field);
        form.appendChild(honeypot.node);
        form.appendChild(button);
        form.appendChild(status);

        container.appendChild(headingEl);
        container.appendChild(textEl);
        container.appendChild(form);
        container.appendChild(createPrivacy(privacyUrl));

        attachSubmit({
          form: form,
          emailInput: emailField.input,
          nameInput: nameField ? nameField.input : null,
          honeypotInput: honeypot.input,
          button: button,
          statusEl: status,
          site: site,
          startedAt: renderedAt,
          variant: variantLabel,
          onSuccess: markSignupDone
        });
      }

      // Enhance mode: wire up an existing author-styled form without touching
      // its markup, classes or styles.
      function enhanceForm(form) {
        const startedAt = Date.now();
        const parts = pickFormParts(form);
        const emailInput = parts.emailInput;
        // No email field: nothing we can do, so leave the form untouched.
        if (!emailInput) return;

        const site = siteLabelFor(form);

        let statusEl = parts.statusEl;
        if (!statusEl) {
          statusEl = makeElement('p', 'rsa-signup__status');
          form.appendChild(statusEl);
        }
        if (!statusEl.id) {
          instanceCounter += 1;
          statusEl.id = 'rsa-signup-status-' + instanceCounter;
        }
        statusEl.setAttribute('role', 'status');
        statusEl.setAttribute('aria-live', 'polite');

        attachSubmit({
          form: form,
          emailInput: emailInput,
          nameInput: parts.nameInput,
          honeypotInput: parts.honeypotInput,
          button: parts.button,
          statusEl: statusEl,
          site: site,
          startedAt: startedAt,
          variant: variantLabel,
          onSuccess: markSignupDone
        });
      }

      // Compact render mode for Quarto's right margin, under the TOC.
      // The CSS hides it below Quarto's lg breakpoint (992px), so mobile
      // readers only see the end-of-chapter card.
      function buildMarginBox(site, privacyUrl) {
        const renderedAt = Date.now();
        instanceCounter += 1;
        const uid = 'rsa-signup-' + instanceCounter;
        const headingId = uid + '-heading';
        const statusId = uid + '-status';

        const box = document.createElement('div');
        box.className = 'rsa-signup rsa-signup--compact';
        box.setAttribute('data-rsa-signup-layout', 'compact');

        const heading = makeElement(
          'h2',
          'rsa-signup__heading',
          'New chapters by email'
        );
        heading.id = headingId;

        const form = document.createElement('form');
        form.className = 'rsa-signup__form';
        form.setAttribute('novalidate', '');
        form.setAttribute('aria-labelledby', headingId);

        const emailField = buildField({
          id: uid + '-email',
          name: 'email',
          type: 'email',
          autocomplete: 'email',
          label: 'Email address',
          required: true
        });
        const honeypot = createHoneypot(uid + '-website');
        const button = makeElement('button', 'rsa-signup__button', 'Subscribe');
        button.type = 'submit';
        const status = createStatusEl(statusId);

        form.appendChild(emailField.field);
        form.appendChild(honeypot.node);
        form.appendChild(button);
        form.appendChild(status);

        box.appendChild(heading);
        box.appendChild(form);
        box.appendChild(createPrivacy(privacyUrl));

        attachSubmit({
          form: form,
          emailInput: emailField.input,
          nameInput: null,
          honeypotInput: honeypot.input,
          button: button,
          statusEl: status,
          site: site,
          startedAt: renderedAt,
          variant: variantLabel,
          onSuccess: markSignupDone
        });

        return box;
      }

      // Sticky bar render mode (variant B only). Not a modal: no overlay, no
      // focus trap, no focus move. The close button is a real 44x44 target.
      function buildBar(site, onSuccess) {
        const renderedAt = Date.now();
        instanceCounter += 1;
        const uid = 'rsa-signup-' + instanceCounter;
        const statusId = uid + '-status';

        const bar = document.createElement('aside');
        bar.className = 'rsa-signup rsa-signup--bar';
        bar.setAttribute('data-rsa-signup-layout', 'bar');
        bar.setAttribute('aria-label', 'Newsletter sign-up');

        const inner = makeElement('div', 'rsa-signup-bar__inner');
        const text = makeElement(
          'p',
          'rsa-signup-bar__text',
          'New eBook chapters by email.'
        );

        const form = document.createElement('form');
        form.className = 'rsa-signup__form rsa-signup-bar__form';
        form.setAttribute('novalidate', '');
        form.setAttribute('aria-label', 'Newsletter sign-up');

        const emailField = buildField({
          id: uid + '-email',
          name: 'email',
          type: 'email',
          autocomplete: 'email',
          label: 'Email address',
          labelClass: 'rsa-signup__label--sr',
          required: true
        });
        const honeypot = createHoneypot(uid + '-website');
        const button = makeElement('button', 'rsa-signup__button', 'Subscribe');
        button.type = 'submit';
        const status = createStatusEl(statusId);
        status.classList.add('rsa-signup-bar__status');

        const close = makeElement('button', 'rsa-signup-bar__close', '\u00D7');
        close.type = 'button';
        close.setAttribute('aria-label', 'Dismiss sign-up bar');

        form.appendChild(emailField.field);
        form.appendChild(honeypot.node);
        form.appendChild(button);

        inner.appendChild(text);
        inner.appendChild(form);
        inner.appendChild(status);
        inner.appendChild(close);
        bar.appendChild(inner);

        attachSubmit({
          form: form,
          emailInput: emailField.input,
          nameInput: null,
          honeypotInput: honeypot.input,
          button: button,
          statusEl: status,
          site: site,
          startedAt: renderedAt,
          variant: variantLabel,
          onSuccess: onSuccess
        });

        return { bar: bar, close: close };
      }

      // Shows the bar only after a real scroll past 50% of the document, only
      // while no card is on screen, and only if not dismissed/finished.
      function initBar(site) {
        if (document.querySelector('[data-rsa-signup-layout="bar"]')) return;
        if (signupDone) return;
        if (isBarDismissed(readStored(BAR_DISMISSED_KEY), Date.now())) return;

        let dismissed = false;
        let cardInView = false;
        let hasScrolled = false;
        let framePending = false;
        // Set after a sign-up from the bar itself: the bar then stays open,
        // ignoring scroll and the end card, so the visitor can read the
        // confirmation until they close it.
        let completed = false;

        const ui = buildBar(site, function () {
          completed = true;
          signupDone = true;
          if (variant !== 'none') writeStored(SIGNUP_DONE_KEY, '1');
        });
        document.body.appendChild(ui.bar);

        function hide() {
          ui.bar.removeAttribute('data-rsa-signup-bar-visible');
        }

        function show() {
          if (dismissed || signupDone || cardInView) return;
          ui.bar.setAttribute('data-rsa-signup-bar-visible', '');
        }

        function update() {
          framePending = false;
          if (completed) return;
          if (dismissed || signupDone || cardInView) {
            hide();
            return;
          }
          const scrollY =
            typeof window.scrollY === 'number'
              ? window.scrollY
              : window.pageYOffset || 0;
          const viewportH = window.innerHeight || 0;
          const docH =
            document.documentElement && document.documentElement.scrollHeight
              ? document.documentElement.scrollHeight
              : 0;
          if (pastScrollThreshold(scrollY, viewportH, docH, SCROLL_THRESHOLD)) {
            show();
          } else {
            hide();
          }
        }

        function onScroll() {
          hasScrolled = true;
          if (framePending) return;
          framePending = true;
          if (typeof window.requestAnimationFrame === 'function') {
            window.requestAnimationFrame(update);
          } else {
            update();
          }
        }

        function dismiss() {
          dismissed = true;
          hide();
          writeStored(
            BAR_DISMISSED_KEY,
            String(barDismissedUntil(Date.now()))
          );
        }

        window.addEventListener('scroll', onScroll, { passive: true });
        ui.close.addEventListener('click', dismiss);
        ui.bar.addEventListener('keydown', function (event) {
          if (event.key !== 'Escape' && event.key !== 'Esc') return;
          if (!ui.bar.contains(document.activeElement)) return;
          dismiss();
        });

        // Two forms must never sit on top of each other: hide the bar while
        // any end card / enhanced form is in the viewport. Skipped when the
        // browser has no IntersectionObserver.
        if (typeof window.IntersectionObserver === 'function') {
          const visible = [];
          const observer = new window.IntersectionObserver(
            function (entries) {
              for (let i = 0; i < entries.length; i += 1) {
                const entry = entries[i];
                const at = visible.indexOf(entry.target);
                if (entry.isIntersecting && at === -1) {
                  visible.push(entry.target);
                } else if (!entry.isIntersecting && at !== -1) {
                  visible.splice(at, 1);
                }
              }
              cardInView = visible.length > 0;
              if (completed) return;
              if (cardInView) {
                hide();
              } else if (hasScrolled) {
                update();
              }
            },
            { threshold: 0 }
          );
          const cards = document.querySelectorAll('[data-rsa-signup]');
          for (let i = 0; i < cards.length; i += 1) {
            observer.observe(cards[i]);
          }
        }
      }

      // Adds the opt-in extras. Safe to call from init() more than once: the
      // existing compact box/bar are detected and skipped. The margin box
      // borrows the end card's site label; the bar appears for variant B only.
      function initExtras(containers) {
        const first = containers.length > 0 ? containers[0] : null;
        const declaredSite = first
          ? (first.getAttribute('data-site') || '').trim()
          : '';
        const site =
          declaredSite ||
          defaultSiteLabel(window.location.hostname, window.location.pathname);
        const privacyUrl =
          (first && first.getAttribute('data-privacy-url')) || '/privacy/';

        if (marginEnabled) {
          const sidebar = document.getElementById('quarto-margin-sidebar');
          if (
            sidebar &&
            !sidebar.querySelector('[data-rsa-signup-layout="compact"]')
          ) {
            sidebar.appendChild(buildMarginBox(site, privacyUrl));
          }
        }

        if (experimentName && variant === 'B') {
          // Read the "already signed up" flag only when the bar could show;
          // consent-region visitors with variant 'none' touch no storage.
          signupDone = readStored(SIGNUP_DONE_KEY) === '1';
          initBar(site);
        }
      }

      // True when the element is a <form> (render mode otherwise).
      function isFormElement(el) {
        return el != null && String(el.tagName).toUpperCase() === 'FORM';
      }

      // Initialises every not-yet-processed container/form on the page.
      function init() {
        const containers = document.querySelectorAll('[data-rsa-signup]');
        initExtras(containers);
        for (let i = 0; i < containers.length; i += 1) {
          const container = containers[i];
          if (container.hasAttribute('data-rsa-signup-ready')) continue;
          container.setAttribute('data-rsa-signup-ready', '');
          if (isFormElement(container)) {
            enhanceForm(container);
          } else {
            renderForm(container);
          }
        }
      }

      window.RsaSignup = { init: init };

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', init);
      } else {
        init();
      }
    })();
  }

  const api = {
    ZOHO_ENDPOINT,
    ZOHO_HIDDEN_FIELDS,
    VARIANT_FIELD,
    EXPERIMENT_STORAGE_PREFIX,
    BAR_DISMISSED_KEY,
    SIGNUP_DONE_KEY,
    BAR_DISMISS_MS,
    buildSignupBody,
    parseZohoResponse,
    isValidEmail,
    isLikelyBot,
    defaultSiteLabel,
    pickFormParts,
    assignVariant,
    resolveVariant,
    formatVariantLabel,
    isEuropeanTimeZone,
    parseConsentCookie,
    pastScrollThreshold,
    barDismissedUntil,
    isBarDismissed
  };

  return api;
});

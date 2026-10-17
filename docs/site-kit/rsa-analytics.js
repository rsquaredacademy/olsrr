/**
 * Rsquared Academy Site-Kit — Analytics & Consent (Phase A)
 * Version: v1.2.0
 *
 * Provides Google Analytics 4 integration with Consent Mode v2,
 * region-based consent banner for EEA/UK/CH visitors, cross-subdomain
 * consent cookie persistence, and an opt-out mechanism.
 */

// EEA member countries (EU 27 + IS, LI, NO) + GB + CH (ISO 3166-1 alpha-2)
const EEA_REGIONS = [
  'AT', 'BE', 'BG', 'CH', 'CY', 'CZ', 'DE', 'DK', 'EE', 'ES',
  'FI', 'FR', 'GB', 'GR', 'HR', 'HU', 'IE', 'IS', 'IT', 'LI',
  'LT', 'LU', 'LV', 'MT', 'NL', 'NO', 'PL', 'PT', 'RO', 'SE',
  'SI', 'SK'
];

/**
 * Checks if a given IANA time zone identifier represents a European region
 * requiring explicit consent under GDPR / ePrivacy directives.
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
 * Determines whether the consent banner should be displayed.
 * Shown only if the visitor's time zone is European AND no prior choice is stored.
 *
 * @param {string} timeZone
 * @param {string|null} storedChoice
 * @returns {boolean}
 */
function shouldShowBanner(timeZone, storedChoice) {
  if (storedChoice === 'granted' || storedChoice === 'denied') {
    return false;
  }
  return isEuropeanTimeZone(timeZone);
}

/**
 * Derives the cookie Domain attribute for cross-subdomain sharing.
 * E.g., 'blog.rsquaredacademy.com' -> '.rsquaredacademy.com'
 *       'www.aravindhebbali.com' -> '.aravindhebbali.com'
 *       'localhost' -> null (no Domain attribute)
 *
 * @param {string} hostname
 * @returns {string|null}
 */
function getCookieDomain(hostname) {
  if (!hostname || typeof hostname !== 'string') return null;
  const host = hostname.toLowerCase().split(':')[0].trim();
  if (
    !host ||
    host === 'localhost' ||
    host === '127.0.0.1' ||
    host === '::1' ||
    /^\d+\.\d+\.\d+\.\d+$/.test(host)
  ) {
    return null;
  }
  if (host === 'rsquaredacademy.com' || host.endsWith('.rsquaredacademy.com')) {
    return '.rsquaredacademy.com';
  }
  if (host === 'aravindhebbali.com' || host.endsWith('.aravindhebbali.com')) {
    return '.aravindhebbali.com';
  }
  // Exclude multi-tenant shared domains from wildcard domain scope
  if (
    host.endsWith('.netlify.app') ||
    host.endsWith('.github.io') ||
    host.endsWith('.pages.dev')
  ) {
    return null;
  }
  const parts = host.split('.');
  if (parts.length >= 2) {
    return '.' + parts.slice(-2).join('.');
  }
  return null;
}

/**
 * Parses the rsa_consent cookie value from a document.cookie string.
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
 * Formats the Set-Cookie string for the rsa_consent cookie.
 * 180 days Max-Age (15,552,000 seconds), Path=/, SameSite=Lax, Secure on HTTPS.
 *
 * @param {'granted'|'denied'} choice
 * @param {string} hostname
 * @param {boolean} isSecure
 * @returns {string}
 */
function formatConsentCookie(choice, hostname, isSecure) {
  const domain = getCookieDomain(hostname);
  const domainAttr = domain ? `; Domain=${domain}` : '';
  const secureAttr = isSecure ? '; Secure' : '';
  return `rsa_consent=${choice}; Path=/; Max-Age=15552000; SameSite=Lax${secureAttr}${domainAttr}`;
}

/**
 * Browser runtime execution
 */
if (typeof window !== 'undefined' && typeof document !== 'undefined') {
  (function initRsaAnalytics() {
    // 1. Read data-ga-id from current script tag
    let gaId = null;
    const currentScript =
      document.currentScript || document.querySelector('script[data-ga-id]');
    if (currentScript) {
      gaId = currentScript.getAttribute('data-ga-id');
    }

    // Initialize dataLayer and gtag function
    window.dataLayer = window.dataLayer || [];
    function gtag() {
      window.dataLayer.push(arguments);
    }
    window.gtag = window.gtag || gtag;

    // 2. Consent Mode v2 defaults in required order:
    // (a) Denied for all EEA countries + GB + CH with 500ms wait_for_update
    gtag('consent', 'default', {
      analytics_storage: 'denied',
      ad_storage: 'denied',
      ad_user_data: 'denied',
      ad_personalization: 'denied',
      region: EEA_REGIONS,
      wait_for_update: 500
    });

    // (b) Granted for all other regions (Google evaluates region by visitor IP)
    gtag('consent', 'default', {
      analytics_storage: 'granted',
      ad_storage: 'denied',
      ad_user_data: 'denied',
      ad_personalization: 'denied'
    });

    // 3. Apply stored choice on every page load
    const storedChoice = parseConsentCookie(document.cookie);
    if (storedChoice === 'granted' || storedChoice === 'denied') {
      gtag('consent', 'update', {
        analytics_storage: storedChoice
      });
    }

    // 4. Asynchronously load Google Tag Manager gtag.js script and configure
    if (gaId) {
      const gtmScript = document.createElement('script');
      gtmScript.async = true;
      gtmScript.src =
        'https://www.googletagmanager.com/gtag/js?id=' + encodeURIComponent(gaId);
      document.head.appendChild(gtmScript);

      gtag('js', new Date());
      gtag('config', gaId, { cookie_domain: 'auto' });
    } else {
      console.warn(
        'rsa-analytics: No data-ga-id attribute found on the script tag. GA4 initialization skipped.'
      );
    }

    // Helper: Persist consent choice in cookie and update gtag
    function setConsentChoice(choice) {
      const isSecure = window.location.protocol === 'https:';
      document.cookie = formatConsentCookie(
        choice,
        window.location.hostname,
        isSecure
      );
      gtag('consent', 'update', {
        analytics_storage: choice
      });
    }

    // Helper: Remove banner from DOM
    function removeBanner() {
      const banner = document.getElementById('rsa-consent-banner');
      if (banner) {
        document.removeEventListener('keydown', handleEscKey);
        banner.remove();
      }
    }

    // Helper: Handle Esc key (Esc = Decline)
    function handleEscKey(e) {
      if (e.key === 'Escape' || e.key === 'Esc') {
        setConsentChoice('denied');
        removeBanner();
      }
    }

    // Helper: Render consent banner
    function showBanner() {
      if (document.getElementById('rsa-consent-banner')) return;

      const banner = document.createElement('aside');
      banner.id = 'rsa-consent-banner';
      banner.className = 'rsa-consent-banner';
      banner.setAttribute('role', 'dialog');
      banner.setAttribute('aria-labelledby', 'rsa-consent-title');
      banner.setAttribute('aria-describedby', 'rsa-consent-desc');
      banner.setAttribute('tabindex', '-1');

      banner.innerHTML = `
        <div class="rsa-consent-container">
          <div class="rsa-consent-content">
            <h2 id="rsa-consent-title" class="rsa-consent-title">Cookie &amp; Privacy Preferences</h2>
            <p id="rsa-consent-desc" class="rsa-consent-desc">
              We use privacy-friendly Google Analytics to measure website traffic and improve our educational resources. We do not use advertising or marketing cookies.
            </p>
          </div>
          <div class="rsa-consent-actions">
            <button type="button" id="rsa-consent-decline" class="rsa-consent-btn rsa-consent-btn-decline">Decline</button>
            <button type="button" id="rsa-consent-accept" class="rsa-consent-btn rsa-consent-btn-accept">Accept</button>
          </div>
        </div>
      `;

      document.body.appendChild(banner);

      // Keyboard support: Esc = Decline
      document.addEventListener('keydown', handleEscKey);

      // Button listeners
      const acceptBtn = document.getElementById('rsa-consent-accept');
      const declineBtn = document.getElementById('rsa-consent-decline');

      if (acceptBtn) {
        acceptBtn.addEventListener('click', function () {
          setConsentChoice('granted');
          removeBanner();
        });
      }

      if (declineBtn) {
        declineBtn.addEventListener('click', function () {
          setConsentChoice('denied');
          removeBanner();
        });
      }

      // Move focus to the dialog for accessibility
      try {
        banner.focus();
      } catch (e) {
        // Ignore focus failures
      }
    }

    // Helper: Show opt-out confirmation toast
    function showOptOutToast() {
      const existing = document.getElementById('rsa-optout-toast');
      if (existing) existing.remove();

      const toast = document.createElement('div');
      toast.id = 'rsa-optout-toast';
      toast.className = 'rsa-optout-toast';
      toast.setAttribute('role', 'status');
      toast.setAttribute('aria-live', 'polite');
      toast.innerHTML =
        '<span class="rsa-toast-icon" aria-hidden="true">&#10003;</span> Analytics tracking has been disabled.';

      document.body.appendChild(toast);

      // Trigger transition
      setTimeout(function () {
        toast.classList.add('rsa-toast-visible');
      }, 10);

      // Dismiss after 4 seconds
      setTimeout(function () {
        toast.classList.remove('rsa-toast-visible');
        setTimeout(function () {
          toast.remove();
        }, 300);
      }, 4000);
    }

    // Helper: Check timezone and show banner if required
    function checkAndShowBanner() {
      let timeZone = '';
      try {
        timeZone = Intl.DateTimeFormat().resolvedOptions().timeZone || '';
      } catch (e) {
        timeZone = '';
      }

      if (shouldShowBanner(timeZone, storedChoice)) {
        showBanner();
      }
    }

    // Run banner check when DOM is ready
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', checkAndShowBanner);
    } else {
      checkAndShowBanner();
    }

    // 5. Opt-out listener: any element with data-rsa-analytics-optout
    document.addEventListener('click', function (e) {
      const optOutTrigger = e.target.closest('[data-rsa-analytics-optout]');
      if (!optOutTrigger) return;

      e.preventDefault();
      setConsentChoice('denied');
      removeBanner();
      showOptOutToast();
    });
  })();
}

// Export functions for Node.js unit tests
if (typeof module !== 'undefined' && module.exports) {
  module.exports = {
    EEA_REGIONS,
    isEuropeanTimeZone,
    shouldShowBanner,
    getCookieDomain,
    parseConsentCookie,
    formatConsentCookie
  };
}

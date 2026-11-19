(function () {
  try {
    var saved = window.localStorage.getItem("theme");
    var dark = saved
      ? saved === "dark"
      : window.matchMedia("(prefers-color-scheme: dark)").matches;
    document.documentElement.setAttribute("data-bs-theme", dark ? "dark" : "light");
  } catch (e) {
    document.documentElement.setAttribute("data-bs-theme", "light");
  }
})();

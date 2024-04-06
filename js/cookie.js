// This is a very small tribute to Orteil's Cookie Clicker game:
// https://orteil.dashnet.org/cookieclicker/

// GAME STATE
const defaultState = {
  cookies: 0,  // Number of cookies the player has
  cps: 0,  // Total cookies per second
  lastPlayed: 0,  // Unix timestamp of the last time the game was played
  acs: {  // Set of all autoclickers available for purchase
    cursors:      {count: 0, cost: 15,    cps: 0.1},
    grandmas:     {count: 0, cost: 100,   cps: 1},
    farms:        {count: 0, cost: 1.1e3, cps: 8},
    mines:        {count: 0, cost: 12e3,  cps: 47},
    factories:    {count: 0, cost: 130e3, cps: 260},
    banks:        {count: 0, cost: 1.4e6, cps: 1.43e3},
    temples:      {count: 0, cost: 20e6,  cps: 7.8e3},
    wizards:      {count: 0, cost: 330e6, cps: 44e3},
    spaceships:   {count: 0, cost: 5.1e9, cps: 260e3},
    alchemists:   {count: 0, cost: 75e9,  cps: 1.6e6},
    portals:      {count: 0, cost: 1e12,  cps: 10e6},
    timeMachines: {count: 0, cost: 14e12, cps: 65e6}
  }
}

let state = defaultState

// ENVIRONMENT STATE
let started = false  // Has the game started?
let ticker = null  // ref to interval for ticker
const tps = 20  // Ticks per second

// DISPLAY CODE
function engineering(number) {
  // Pretty-print a number as a string using engineering notation
  const suffixes = ['', 'K', 'M', 'B', 'T'];
  const fix = Number.isInteger(number) ? 0 : 2
  
  if (number < 1000) return number.toFixed(fix)
  
  const exponent = Math.floor(Math.log10(number) / 3);

  const rounded = (number / Math.pow(1000, exponent)).toFixed(fix);
  const suffix = (exponent >= suffixes.length) ? `e${exponent*3}` : `${suffixes[exponent]}`
  
  return `${rounded}${suffix}`
}

function displayUnit(value, singular, plural) {
  // Display a value with a unit in either singular or plural form
  let unit = (value == 1) ? singular : plural
  return `${engineering(value)} ${unit}`
}

function displayAC(name) {
  // Update the count and cost for an autoclicker
  document.getElementById(name).innerHTML = state.acs[name].count
  document.getElementById("buy"+name).innerHTML = displayUnit(state.acs[name].cost, "cookie", "cookies")
}

function displayCPS() {
  // Display the current cookies per second
  document.getElementById("cps").innerHTML = `${engineering(state.cps)} CPS`
}


// SAVE AND RESTORE
function save() {
  // Save the game to local storage
  state.lastPlayed = Date.now()
  localStorage.setItem("state", JSON.stringify(state))
}

function restore() {
  // Restore from local storage if the game was saved
  let savedState = JSON.parse(localStorage.getItem("state"))
  if (!savedState) return
    
  state = savedState
  console.log("Restored the following state:", state)
  
  // Compute cookies generated during idle time
  now = Date.now()
  if (state.lastPlayed < now) {
    delta = now - state.lastPlayed
    state.cookies += state.cps * delta / 1000
    state.lastPlayed = now
  }
}

function reset() {
  // Reset the game
  if (confirm("Resetting will erase all progress! Are you sure?")) {
    localStorage.removeItem("state")
    state = defaultState
    location.reload(true)  // Reload the page to refresh displayed values
  }
}


// GAME LOGIC
function updateCPS() {
  // Recalculate the total CPS of all ACs the player owns
  total = 0
  for (let i in state.acs) {
    total += state.acs[i].count * state.acs[i].cps
  }
  state.cps = total
}

function tick() {
  // Perform one tick of the game
  state.cookies += state.cps / tps
  cookies.innerHTML = displayUnit(state.cookies, "cookie", "cookies")
}

function init() {
  // Initialise the game values and start the ticker
  started = true
  restore()
  ticker = window.setInterval(tick, 1000/tps)
  for (let name in state.acs) displayAC(name)
  displayCPS()
  console.log("Started cookie clicker")
}

function stop() {
  // Stop the game
  started = false
  clearInterval(ticker)
}


// USER INTERACTION
function click_cookie() {
  // Start the game if it hasn't been started already, and add a cookie
  if (!started) init()
  state.cookies++
  save()
}

function buy(name) {
  // Buy an autoclicker
  let item = state.acs[name]
  if (state.cookies >= item.cost) {
    state.cookies -= item.cost
    item.count++
    item.cost *= 1.15
    updateCPS()
    save()
    
    displayAC(name)
    displayCPS()
  }
}


// COOKIE BAR AND SETTINGS VIEW
function hideBar() {
  bar = document.getElementById("cookie_bar")
  bar.classList.toggle("fadeOut");
}

function showSettings() {
  settings = document.getElementById("cookie_settings")
  settings.style.visibility = "visible"
}

function hideSettings() {
  settings = document.getElementById("cookie_settings")
  settings.classList.toggle("fadeOut")
}

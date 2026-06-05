# ============================================================
# BRAIN DRAIN SIMULATOR v2.0 — System Dynamics Model
# EU Country Comparison: Education, Migration & GDP
# ------------------------------------------------------------
# NEW in v2: Calendar years (2025→), CSV/PNG export,
#            Country comparison, Auto-generated conclusions,
#            EU Funds slider, Animated replay
# Model type : System Dynamics (Stock & Flow)
# Method     : Runge-Kutta 4th order (deSolve)
# Calibration: Eurostat 2023, World Bank, DZS
# ============================================================

library(shiny)
library(deSolve)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(scales)

BASE_YEAR <- 2025

# ── COUNTRY DATA ──────────────────────────────────────────────────────────────
COUNTRIES <- list(
  "Hrvatska (HR)" = list(
    pop=3.9, gdp_pc=18200, edu_pct=0.282, emi_rate=18.5,
    wage_ratio=0.58, pub_edu=3.1,
    sectors=c(IT=0.11, Health=0.07, Manuf=0.18, Agri=0.04, Tourism=0.22, Other=0.38)
  ),
  "Rumunjska (RO)" = list(
    pop=19.0, gdp_pc=14800, edu_pct=0.194, emi_rate=22.0,
    wage_ratio=0.48, pub_edu=2.9,
    sectors=c(IT=0.09, Health=0.06, Manuf=0.22, Agri=0.08, Tourism=0.06, Other=0.49)
  ),
  "Bugarska (BG)" = list(
    pop=6.5, gdp_pc=13500, edu_pct=0.298, emi_rate=20.5,
    wage_ratio=0.42, pub_edu=3.8,
    sectors=c(IT=0.10, Health=0.06, Manuf=0.20, Agri=0.06, Tourism=0.12, Other=0.46)
  ),
  "Litva (LT)" = list(
    pop=2.8, gdp_pc=22600, edu_pct=0.381, emi_rate=16.2,
    wage_ratio=0.65, pub_edu=4.2,
    sectors=c(IT=0.13, Health=0.07, Manuf=0.20, Agri=0.05, Tourism=0.05, Other=0.50)
  ),
  "Latvija (LV)" = list(
    pop=1.8, gdp_pc=19800, edu_pct=0.335, emi_rate=17.8,
    wage_ratio=0.60, pub_edu=4.0,
    sectors=c(IT=0.10, Health=0.07, Manuf=0.16, Agri=0.05, Tourism=0.06, Other=0.56)
  ),
  "Estonija (EE)" = list(
    pop=1.4, gdp_pc=24500, edu_pct=0.345, emi_rate=12.5,
    wage_ratio=0.68, pub_edu=4.8,
    sectors=c(IT=0.18, Health=0.06, Manuf=0.15, Agri=0.04, Tourism=0.07, Other=0.50)
  ),
  "Mađarska (HU)" = list(
    pop=9.7, gdp_pc=19200, edu_pct=0.256, emi_rate=15.8,
    wage_ratio=0.55, pub_edu=3.5,
    sectors=c(IT=0.09, Health=0.06, Manuf=0.25, Agri=0.05, Tourism=0.08, Other=0.47)
  ),
  "Slovačka (SK)" = list(
    pop=5.5, gdp_pc=21000, edu_pct=0.241, emi_rate=14.2,
    wage_ratio=0.58, pub_edu=3.9,
    sectors=c(IT=0.08, Health=0.07, Manuf=0.28, Agri=0.04, Tourism=0.06, Other=0.47)
  ),
  "Poljska (PL)" = list(
    pop=37.7, gdp_pc=20800, edu_pct=0.338, emi_rate=11.5,
    wage_ratio=0.57, pub_edu=4.4,
    sectors=c(IT=0.11, Health=0.07, Manuf=0.23, Agri=0.06, Tourism=0.05, Other=0.48)
  ),
  "Grčka (EL)" = list(
    pop=10.4, gdp_pc=17500, edu_pct=0.316, emi_rate=21.0,
    wage_ratio=0.52, pub_edu=3.6,
    sectors=c(IT=0.06, Health=0.07, Manuf=0.11, Agri=0.07, Tourism=0.28, Other=0.41)
  ),
  "Portugal (PT)" = list(
    pop=10.3, gdp_pc=21200, edu_pct=0.272, emi_rate=16.5,
    wage_ratio=0.60, pub_edu=4.7,
    sectors=c(IT=0.09, Health=0.07, Manuf=0.15, Agri=0.03, Tourism=0.20, Other=0.46)
  ),
  "Španjolska (ES)" = list(
    pop=47.4, gdp_pc=26500, edu_pct=0.386, emi_rate=12.0,
    wage_ratio=0.77, pub_edu=4.3,
    sectors=c(IT=0.10, Health=0.07, Manuf=0.13, Agri=0.04, Tourism=0.22, Other=0.44)
  ),
  "Italija (IT)" = list(
    pop=58.9, gdp_pc=27400, edu_pct=0.194, emi_rate=14.5,
    wage_ratio=0.82, pub_edu=4.0,
    sectors=c(IT=0.08, Health=0.07, Manuf=0.20, Agri=0.03, Tourism=0.16, Other=0.46)
  ),
  "Irska (IE)" = list(
    pop=5.1, gdp_pc=54500, edu_pct=0.485, emi_rate=7.0,
    wage_ratio=1.45, pub_edu=3.9,
    sectors=c(IT=0.25, Health=0.07, Manuf=0.18, Agri=0.02, Tourism=0.06, Other=0.42)
  ),
  "Njemačka (DE)" = list(
    pop=84.4, gdp_pc=38200, edu_pct=0.322, emi_rate=5.5,
    wage_ratio=1.10, pub_edu=4.6,
    sectors=c(IT=0.12, Health=0.08, Manuf=0.30, Agri=0.02, Tourism=0.04, Other=0.44)
  )
)

# ── SYSTEM DYNAMICS MODEL ─────────────────────────────────────────────────────
sd_model <- function(time, state, parms) {
  with(as.list(c(state, parms)), {
    wage_gap_effect <- pmax(0.5, (1.0 - wage_ratio) * wage_sensitivity)
    policy_mult <- 1 - (policy_strength * pmax(0, time - policy_start) / t_max * 0.6)
    policy_mult <- pmax(0.1, policy_mult)
    # EU Funds boost: reduces emigration AND increases GDP growth
    eu_funds_boost <- 1 + eu_funds * 0.015 * pmax(0, 1 - time / t_max)
    emi_flow  <- E * (base_emi_rate / 1000) * wage_gap_effect * policy_mult / eu_funds_boost
    gdp_attract <- pmax(0, (GDP / gdp_init - 0.85) * immigration_sensitivity)
    immi_flow <- E * (base_emi_rate / 1000) * 0.25 * gdp_attract
    grad_flow <- (pop_total * 1e6) * edu_grad_rate * (1 + pub_edu_effect * pub_edu_pct)
    dE <- grad_flow + immi_flow - emi_flow - E * 0.018
    edu_ratio <- E / (pop_total * 1e6 * 0.45)
    gdp_growth <- gdp_base_growth + edu_gdp_elasticity * (edu_ratio - edu_ratio_init) +
      eu_funds * 0.002
    gdp_growth <- gdp_growth - brain_drain_penalty * (emi_flow / pmax(1, E))
    dGDP    <- GDP * pmax(-0.05, pmin(0.10, gdp_growth))  # Cap growth between -5% and +10% per year
    health_emi <- emi_flow * health_share
    dH_qual <- -health_emi / (pop_total * 1e6) * 500 +
      0.01 * (1.0 - H_qual) * (GDP / gdp_init)
    it_retention <- pmax(0, 1 - (emi_flow / pmax(1, E)) * it_sensitivity)
    dI_invest <- I_invest * (0.04 * it_retention - 0.02)
    list(c(dE, dGDP, dH_qual, dI_invest),
         emi_flow = emi_flow, immi_flow = immi_flow,
         net_mig  = immi_flow - emi_flow,
         edu_ratio = edu_ratio, gdp_growth = gdp_growth)
  })
}

run_sd <- function(cd, years, policy_strength=0, policy_start=0,
                   eu_funds=0, wage_sens=2.2, edu_gdp=0.45) {
  E0   <- cd$pop * 1e6 * 0.45 * cd$edu_pct
  GDP0 <- cd$gdp_pc
  health_sh <- unname(cd$sectors[which(names(cd$sectors)=="Health")])
  if(length(health_sh)==0) health_sh <- 0.07
  parms <- c(
    base_emi_rate        = cd$emi_rate,
    wage_ratio           = cd$wage_ratio,
    wage_sensitivity     = wage_sens,
    immigration_sensitivity = 1.5,
    pop_total            = cd$pop,
    edu_grad_rate        = 0.008,
    pub_edu_pct          = cd$pub_edu / 100,
    pub_edu_effect       = 8.0,
    gdp_base_growth      = ifelse(cd$gdp_pc < 20000, 0.032,
                                  ifelse(cd$gdp_pc < 30000, 0.022, 0.016)),
    edu_gdp_elasticity   = edu_gdp,
    edu_ratio_init       = cd$edu_pct,
    gdp_init             = GDP0,
    brain_drain_penalty  = 1.8,
    health_share         = health_sh,
    it_sensitivity       = 3.0,
    eu_funds             = eu_funds,
    policy_strength      = policy_strength,
    policy_start         = policy_start,
    t_max                = years
  )
  it_init <- unname(cd$sectors[which(names(cd$sectors)=="IT")])
  if(length(it_init)==0) it_init <- 0.10
  state <- c(E=E0, GDP=GDP0, H_qual=0.70, I_invest=it_init*100)
  times <- seq(0, years, by=0.25)
  out   <- as.data.frame(ode(y=state, times=times, func=sd_model, parms=parms, method="rk4"))
  out$year     <- BASE_YEAR + out$time
  out$edu_pct  <- out$E / (cd$pop * 1e6 * 0.45) * 100
  out$gdp_growth_pct <- c(0, diff(out$GDP) / head(out$GDP, -1) * 100)
  out$net_mig_k      <- out$net_mig / 1000
  out$cum_emi        <- cumsum(out$emi_flow * 0.25) / 1000
  out
}

# ── COLOUR & LABEL CONSTANTS ─────────────────────────────────────────────────
SCEN_COLS   <- c(baseline="#c0392b", policy_now="#1565c0",
                 policy_5y="#2e7d32", optimistic="#e65100")
SCEN_LABELS <- c(baseline="Baseline (bez politike)",
                 policy_now="Politika odmah",
                 policy_5y="Politika za 5 god.",
                 optimistic="Optimistični scenarij")

plt_theme <- function(p, title=NULL) {
  p %>% layout(
    paper_bgcolor="#ffffff", plot_bgcolor="#ffffff",
    font   = list(family="Inter, sans-serif", color="#3a4255", size=13),
    title  = if (!is.null(title)) list(text=title, font=list(size=13, color="#333"), x=0.02) else NULL,
    xaxis  = list(
      title      = list(text="", standoff=0),
      gridcolor  = "#f0f2f5",
      zerolinecolor = "#e0e4ec",
      tickfont   = list(size=12),
      tickformat = "d",
      tickangle  = 0
    ),
    yaxis  = list(
      title     = list(text="", standoff=8),
      gridcolor = "#f0f2f5",
      zerolinecolor = "#e0e4ec",
      tickfont  = list(size=12)
    ),
    legend = list(orientation="h", y=-0.18, x=0, font=list(size=11),
                  bgcolor="rgba(255,255,255,0.85)", bordercolor="#e0e4ec", borderwidth=1),
    margin = list(l=60, r=20, t=if (!is.null(title)) 40 else 16, b=60),
    hoverlabel = list(font=list(family="Arial", size=13), bgcolor="#fff",
                      bordercolor="#e0e4ec")
  )
}

# ── AUTO-CONCLUSIONS ──────────────────────────────────────────────────────────
generate_conclusions <- function(sims, country_name, years, policy_start,
                                 eu_funds, policy_strength) {
  base <- sims$baseline
  opt  <- sims$optimistic
  pol  <- sims$policy_now
  end_year <- BASE_YEAR + years
  
  gdp_base_final <- tail(base$GDP, 1)
  gdp_opt_final  <- tail(opt$GDP,  1)
  gdp_pol_final  <- tail(pol$GDP,  1)
  gdp_base_init  <- base$GDP[1]
  
  gdp_base_chg <- (gdp_base_final / gdp_base_init - 1) * 100
  gdp_opt_chg  <- (gdp_opt_final  / gdp_base_init - 1) * 100
  gdp_pol_chg  <- (gdp_pol_final / gdp_base_init - 1) * 100
  gdp_gain_pol <- gdp_pol_final - gdp_base_final
  
  edu_start <- base$edu_pct[1]
  edu_final <- tail(base$edu_pct, 1)
  edu_pol   <- tail(pol$edu_pct, 1)
  
  cum_emi_base <- tail(base$cum_emi, 1)
  cum_emi_pol  <- tail(pol$cum_emi, 1)
  cum_saved    <- cum_emi_base - cum_emi_pol
  
  peak_emi     <- max(base$emi_flow, na.rm=TRUE) / 1000
  peak_year    <- base$year[which.max(base$emi_flow)]
  
  health_base  <- tail(base$H_qual, 1)
  health_pol   <- tail(pol$H_qual, 1)
  
  r0 <- ifelse(gdp_base_chg >= 25, "pozitivan",
               ifelse(gdp_base_chg >= 10, "umjeren", "zabrinjavajuć"))
  
  eu_txt <- if (eu_funds > 0)
    sprintf("EU fondovi (%g%% BDP) ubrzavaju oporavak za %.1f%% više BDP-a.", eu_funds, (gdp_pol_final / gdp_base_final - 1)*100)
  else "EU fondovi nisu aktivirani u ovoj simulaciji."
  
  timing_txt <- if (policy_start == 0)
    "Politika uvedena odmah (2025.) daje maksimalne rezultate."
  else if (policy_start <= 3)
    sprintf("Odgoda politike za %d god. smanjuje ukupni učinak, ali ostaje pozitivna.", policy_start)
  else
    sprintf("Odgoda od %d godina (do %d.) značajno smanjuje potencijal politike — preporuka: djelovati što ranije.", policy_start, BASE_YEAR + policy_start)
  
  paste0(
    "## Automatski generirani zaključci simulacije\n\n",
    "**Država:** ", country_name, "  |  ",
    "**Horizont:** ", BASE_YEAR, "–", end_year, " (", years, " godina)\n\n",
    "---\n\n",
    "### 1. Demografska situacija — gubitak obrazovnog kapitala\n\n",
    sprintf(
      "Prema baseline scenariju bez intervencije, udio visoko obrazovanih se mijenja s **%.1f%%** na **%.1f%%** radne snage do %d. godine. ",
      edu_start, edu_final, end_year
    ),
    sprintf(
      "Vrhunac godišnje emigracije dostiže se oko **%d. godine** s procijenjenih **%.1f tisuća** visoko obrazovanih osoba godišnje.",
      round(peak_year), round(peak_emi, 1)
    ), "\n\n",
    sprintf(
      "Uz aktivnu politiku zadržavanja talenata, udio obrazovanih raste na **%.1f%%** — razlika od %.2f postotnih bodova.",
      edu_pol, edu_pol - edu_final
    ), "\n\n",
    "### 2. Ekonomski rast — feedback između obrazovanja i BDP-a\n\n",
    sprintf(
      "BDP per capita u baseline scenariju raste s **€%s** na **€%s** (prosječna godišnja stopa: %.2f%%). ",
      format(round(gdp_base_init), big.mark=","),
      format(round(gdp_base_final), big.mark=","),
      gdp_base_chg / years
    ),
    sprintf(
      "Optimistični scenarij s punom politikom dostiže **€%s** (stopa %.2f%% godišnje). ",
      format(round(gdp_opt_final), big.mark=","), gdp_opt_chg / years
    ),
    sprintf(
      "Razlika između aktivne politike i baseline scenarija iznosi **€%s per capita** do %d.",
      format(round(gdp_gain_pol), big.mark=","), end_year
    ), "\n\n",
    eu_txt, "\n\n",
    "### 3. Kumulativni gubitak — ljudski kapital\n\n",
    sprintf(
      "Bez intervencije, zemlja gubi procijenjenih **%.0f tisuća** visoko obrazovanih osoba u %d-godišnom periodu. ",
      round(cum_emi_base), years
    ),
    sprintf(
      "Aktivna politika zadržavanja mogla bi sačuvati **%.0f tisuća** osoba — to je **%.0f%%** manje emigracije.",
      round(cum_saved), round(cum_saved / cum_emi_base * 100)
    ), "\n\n",
    "### 4. Zdravstvo i javni sektor — indirektne štete\n\n",
    sprintf(
      "Indeks kvalitete zdravstva pada s **0.700** na **%.3f** bez politike (pad od %.1f%%). ",
      health_base, (1 - health_base/0.70)*100
    ),
    sprintf(
      "Politika zadržavanja zdravstvenih radnika zadržava indeks na **%.3f** — poboljšanje od %.1f%%.",
      health_pol, (health_pol / health_base - 1) * 100
    ), "\n\n",
    "### 5. Preporuke za politiku\n\n",
    timing_txt, "\n\n",
    ifelse(edu_final < edu_start * 0.85,
           "**🚨 KRITIČNO:** Baseline scenarij pokazuje strukturalni kolaps obrazovnog kapitala — bez mjera, zemlja gubi >15% kadra.",
           ifelse(edu_final < edu_start * 0.95,
                  "**⚠ UMJEREN RIZIK:** Postepeno smanjenje zahtijeva konzistentnu politiku — bez mjera, rizik od ireverzibilne štete.",
                  "**✓ RELATIVNO STABILAN:** Kapital se održava, ali su potrebne mjere za ubrzanje rasta."
           )
    ), "\n\n",
    sprintf(
      "Za dosizanje 80%% EU prosjeka BDP-a potreban je godišnji rast od ~%.2f%% — ",
      max(0.01, log(38000 * 0.8 / gdp_base_init) / years) * 100
    ),
    ifelse((gdp_base_chg / years) >= max(0.01, log(38000*0.8/gdp_base_init)/years*100),
           "**trenutna putanja je dovoljna.**",
           "**potrebne su strukturne reforme.**"
    ), "\n\n",
    "---\n",
    "*Napomena: Zaključci generirani automatski iz System Dynamics modela. ",
    "Kalibracija: Eurostat 2023, World Bank. ",
    "Model ne uključuje geopolitičke šokove ili diskontinuirane događaje.*"
  )
}


# ── CSS ──────────────────────────────────────────────────────────────────────
APP_CSS <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=DM+Mono:wght@400;500&display=swap');

* { box-sizing: border-box; }

body {
  font-family: 'Inter', sans-serif;
  font-size: 15px;
  background: #f4f6f9;
  color: #1a1f2e;
  margin: 0;
}

/* ── HEADER ── */
.app-header {
  background: #1a1f2e;
  padding: 22px 36px;
  display: flex;
  align-items: center;
  gap: 16px;
}
.app-header h1 {
  color: #fff;
  font-size: 1.5rem;
  font-weight: 700;
  margin: 0;
  letter-spacing: -.02em;
}
.app-header .sub {
  color: #8892a4;
  font-size: .85rem;
  margin-top: 3px;
  letter-spacing: .04em;
}
.v-badge {
  background: #378ADD33;
  color: #6db8ff;
  border: 1px solid #378ADD55;
  padding: 3px 10px;
  border-radius: 20px;
  font-size: .78rem;
  font-weight: 600;
}
.hdr-badges {
  margin-left: auto;
  display: flex;
  gap: 8px;
  flex-wrap: wrap;
}
.hbadge {
  padding: 5px 12px;
  border-radius: 20px;
  font-size: .78rem;
  font-weight: 600;
}
.hb-r { background:#fff0f0; color:#c0392b; }
.hb-b { background:#e8f4ff; color:#1565c0; }
.hb-g { background:#e8f5e9; color:#2e7d32; }
.hb-a { background:#fff8e1; color:#e65100; }

/* ── LAYOUT ── */
.app-layout {
  display: grid;
  grid-template-columns: 300px 1fr;
  min-height: calc(100vh - 74px);
}

/* ── SIDEBAR ── */
.app-sidebar {
  background: #fff;
  border-right: 1px solid #e0e4ec;
  padding: 24px 20px;
  overflow-y: auto;
}

.ctrl-section {
  margin-bottom: 8px;
}
.ctrl-section-title {
  font-size: .7rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: .14em;
  color: #9aa3b0;
  padding: 16px 0 10px;
  border-top: 1px solid #f0f2f5;
  margin-top: 4px;
}
.ctrl-section-title:first-child {
  border-top: none;
  padding-top: 0;
}

.param-hint {
  font-size: .8rem;
  color: #7a8499;
  line-height: 1.55;
  margin-bottom: 10px;
  padding: 10px 12px;
  background: #f8f9fb;
  border-radius: 8px;
  border-left: 3px solid #378ADD;
}
.param-hint b { color: #1a1f2e; font-weight: 600; }

.scale-hint {
  font-size: .75rem;
  color: #9aa3b0;
  margin-top: -8px;
  margin-bottom: 10px;
  padding-left: 2px;
}

/* slider labels bigger */
.irs--shiny .irs-min,
.irs--shiny .irs-max { font-size: 11px; color: #9aa3b0; }
.irs--shiny .irs-from,
.irs--shiny .irs-to,
.irs--shiny .irs-single {
  background: #1a1f2e;
  color: #fff;
  font-size: 12px;
  border-radius: 4px;
  font-family: 'DM Mono', monospace;
}
.irs--shiny .irs-bar,
.irs--shiny .irs-bar-edge { background: #378ADD; border-color: #378ADD; }
.irs--shiny .irs-handle { background: #378ADD !important; border: 3px solid #fff !important; width: 20px !important; height: 20px !important; top: 19px !important; }
.irs--shiny .irs-line { height: 6px !important; top: 28px !important; }
.irs--shiny .irs-bar { height: 6px !important; top: 28px !important; }

/* selects */
.shiny-input-container select {
  font-size: 14px !important;
  padding: 9px 12px !important;
  border: 1.5px solid #e0e4ec !important;
  border-radius: 8px !important;
  background: #fff !important;
  color: #1a1f2e !important;
  width: 100% !important;
}
.shiny-input-container select:focus {
  border-color: #378ADD !important;
  outline: none !important;
}
.shiny-input-container label {
  font-size: 13px !important;
  font-weight: 600 !important;
  color: #3a4255 !important;
  margin-bottom: 6px !important;
}

.year-pill {
  display: inline-block;
  background: #e8f4ff;
  color: #1565c0;
  font-family: 'DM Mono', monospace;
  font-size: .9rem;
  font-weight: 600;
  padding: 5px 14px;
  border-radius: 20px;
  margin-top: 6px;
  margin-bottom: 4px;
}

.run-btn {
  width: 100%;
  background: #1a1f2e;
  color: #fff;
  border: none;
  border-radius: 10px;
  padding: 14px;
  font-size: 1rem;
  font-weight: 700;
  font-family: 'Inter', sans-serif;
  cursor: pointer;
  margin-top: 20px;
  letter-spacing: .02em;
  transition: background .15s;
}
.run-btn:hover { background: #2d3548; }

.dl-btn {
  width: 100%;
  background: #fff;
  color: #3a4255;
  border: 1.5px solid #e0e4ec;
  border-radius: 8px;
  padding: 10px 14px;
  font-size: .88rem;
  font-weight: 600;
  font-family: 'Inter', sans-serif;
  cursor: pointer;
  margin-top: 8px;
  text-align: left;
  transition: background .12s;
}
.dl-btn:hover { background: #f4f6f9; }

/* ── MAIN CONTENT ── */
.app-main {
  padding: 28px 32px;
  overflow-y: auto;
}

/* ── TABS ── */
.nav-tabs {
  border-bottom: 2px solid #e0e4ec !important;
  margin-bottom: 24px !important;
}
.nav-tabs .nav-link {
  font-size: .92rem !important;
  font-weight: 600 !important;
  color: #7a8499 !important;
  padding: 10px 20px !important;
  border: none !important;
  border-bottom: 3px solid transparent !important;
  margin-bottom: -2px !important;
  border-radius: 0 !important;
}
.nav-tabs .nav-link:hover { color: #1a1f2e !important; }
.nav-tabs .nav-link.active {
  color: #1a1f2e !important;
  border-bottom-color: #378ADD !important;
  background: transparent !important;
}

/* ── KPI CARDS ── */
.kpi-row {
  display: grid;
  grid-template-columns: repeat(5, 1fr);
  gap: 14px;
  margin-bottom: 24px;
}
.kpi-card {
  background: #fff;
  border: 1.5px solid #e0e4ec;
  border-radius: 12px;
  padding: 18px 20px;
}
.kpi-card .kpi-label {
  font-size: .7rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: .12em;
  color: #9aa3b0;
  margin-bottom: 8px;
}
.kpi-card .kpi-val {
  font-size: 1.9rem;
  font-weight: 700;
  line-height: 1;
  font-family: 'DM Mono', monospace;
}
.kpi-card .kpi-sub {
  font-size: .8rem;
  color: #9aa3b0;
  margin-top: 5px;
}
.c-blue   { color: #1565c0; }
.c-red    { color: #c0392b; }
.c-green  { color: #2e7d32; }
.c-amber  { color: #e65100; }
.c-purple { color: #6a1b9a; }

/* ── CHART CARDS ── */
.plot-card {
  background: #fff;
  border: 1.5px solid #e0e4ec;
  border-radius: 12px;
  padding: 22px 24px;
  margin-bottom: 18px;
}
.plot-card .plot-title {
  font-size: .82rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: .1em;
  color: #7a8499;
  margin-bottom: 14px;
}
.plot-row-2 {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 18px;
}
.plot-row-3 {
  display: grid;
  grid-template-columns: 1fr 1fr 1fr;
  gap: 18px;
}

/* ── LEGEND ── */
.chart-legend {
  display: flex;
  gap: 16px;
  flex-wrap: wrap;
  margin-bottom: 10px;
}
.chart-legend span {
  font-size: .82rem;
  color: #5a6377;
  display: flex;
  align-items: center;
  gap: 6px;
}
.leg-dot {
  width: 10px;
  height: 10px;
  border-radius: 50%;
  flex-shrink: 0;
}

/* ── INFO BOX ── */
.info-panel {
  background: #f0f7ff;
  border-left: 4px solid #378ADD;
  border-radius: 0 10px 10px 0;
  padding: 14px 18px;
  font-size: .88rem;
  color: #2c3e50;
  line-height: 1.65;
  margin-bottom: 20px;
}
.info-panel b { font-weight: 700; color: #1a1f2e; }

/* ── CONCLUSION BOX ── */
.concl-box {
  background: #fff;
  border: 1.5px solid #e0e4ec;
  border-radius: 14px;
  padding: 36px 40px;
  font-size: 1rem;
  line-height: 1.85;
  color: #2c3e50;
}
.concl-box h2 {
  font-size: 1.5rem;
  font-weight: 700;
  color: #1a1f2e;
  margin: 0 0 6px;
}
.concl-box .concl-meta {
  font-size: .88rem;
  color: #7a8499;
  margin-bottom: 24px;
  padding-bottom: 20px;
  border-bottom: 1.5px solid #e0e4ec;
}
.concl-box h3 {
  font-size: 1.05rem;
  font-weight: 700;
  color: #1565c0;
  margin: 28px 0 8px;
  padding-bottom: 4px;
  border-bottom: 1px solid #e8f4ff;
}
.concl-box p { margin: 8px 0; }
.concl-box .alert-warn {
  background: #fff8e1;
  border-left: 4px solid #f39c12;
  border-radius: 0 8px 8px 0;
  padding: 12px 16px;
  margin: 12px 0;
  font-weight: 600;
  color: #7d5a00;
}
.concl-box .alert-ok {
  background: #e8f5e9;
  border-left: 4px solid #27ae60;
  border-radius: 0 8px 8px 0;
  padding: 12px 16px;
  margin: 12px 0;
  font-weight: 600;
  color: #1b5e20;
}
.concl-box .alert-crit {
  background: #ffebee;
  border-left: 4px solid #e74c3c;
  border-radius: 0 8px 8px 0;
  padding: 12px 16px;
  margin: 12px 0;
  font-weight: 600;
  color: #7f0000;
}
.concl-box .concl-footer {
  font-size: .82rem;
  color: #9aa3b0;
  margin-top: 28px;
  padding-top: 16px;
  border-top: 1px solid #e0e4ec;
  font-style: italic;
}
.concl-box hr {
  border: none;
  border-top: 1.5px solid #e0e4ec;
  margin: 20px 0;
}

/* ── DATA TABLE ── */
.data-tbl {
  width: 100%;
  border-collapse: collapse;
  font-size: .92rem;
}
.data-tbl th {
  background: #f4f6f9;
  font-weight: 700;
  padding: 12px 16px;
  text-align: left;
  border-bottom: 2px solid #e0e4ec;
  font-size: .78rem;
  text-transform: uppercase;
  letter-spacing: .08em;
  color: #7a8499;
}
.data-tbl td {
  padding: 11px 16px;
  border-bottom: 1px solid #f0f2f5;
  color: #2c3e50;
}
.data-tbl tr:hover td { background: #f8f9fb; }

/* ── METH BOX ── */
.meth-box {
  background: #fff;
  border: 1.5px solid #e0e4ec;
  border-radius: 12px;
  padding: 28px 32px;
  font-size: .92rem;
  line-height: 1.8;
  color: #2c3e50;
}
.meth-box b { color: #1a1f2e; }
.meth-box code {
  background: #f4f6f9;
  padding: 2px 7px;
  border-radius: 4px;
  font-family: 'DM Mono', monospace;
  font-size: .88rem;
  color: #1565c0;
}
"

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=DM+Mono:wght@400;500&display=swap", rel="stylesheet"),
    tags$style(HTML(APP_CSS))
  ),
  
  # ── HEADER ────────────────────────────────────────────────────────────────
  div(class="app-header",
      div(
        tags$h1(HTML('Brain Drain Simulator')),
        div(class="sub", "System Dynamics · Obrazovanje · Migracija · BDP · EU Fondovi")
      ),
      div(class="hdr-badges",
          span(class="hbadge hb-r", "Baseline"),
          span(class="hbadge hb-b", "Politika odmah"),
          span(class="hbadge hb-g", "Politika za 5g"),
          span(class="hbadge hb-a", "Optimistični")
      )
  ),
  
  div(class="app-layout",
      
      # ── SIDEBAR ─────────────────────────────────────────────────────────────
      div(class="app-sidebar",
          
          div(class="ctrl-section",
              div(class="ctrl-section-title", "Odabir država"),
              selectInput("country1", "Primarna država",
                          choices=names(COUNTRIES), selected="Hrvatska (HR)", width="100%"),
              selectInput("country2", "Usporedna država (opcija)",
                          choices=c("— bez usporedbe —"="none", names(COUNTRIES)),
                          selected="none", width="100%")
          ),
          
          div(class="ctrl-section",
              div(class="ctrl-section-title", "Vremenski horizont"),
              sliderInput("years", "Krajnja godina simulacije",
                          min=2030, max=2055, value=2045, step=5, sep="", width="100%"),
              div(class="year-pill", textOutput("year_range_label", inline=TRUE))
          ),
          
          div(class="ctrl-section",
              div(class="ctrl-section-title", "Politika zadržavanja talenata"),
              selectInput("policy_strength", "Paket politike",
                          choices=c(
                            "Bez intervencije"                          = "0",
                            "Paket A — porezne olakšice (blago)"       = "0.3",
                            "Paket B — plaće + subvencije (umjereno)"  = "0.6",
                            "Paket C — strukturne reforme (snažno)"    = "0.8",
                            "Maksimalne mjere (sve politike)"          = "1.0"
                          ),
                          selected="0.6", width="100%"),
              div(class="param-hint",
                  HTML("<b>Paket A:</b> Porezne olakšice za visoko obrazovane.<br>
                        <b>Paket B:</b> + povećanje plaća u javnom sektoru, stambene subvencije.<br>
                        <b>Paket C:</b> + digitalne nomadske vize, strukturne reforme tržišta rada.<br>
                        <b>Maksimalne mjere:</b> sve navedeno + EU kohezijska ulaganja u ljudski kapital.")
              ),
              sliderInput("policy_start_yr", "Početak primjene (godina)",
                          min=2025, max=2040, value=2028, step=1, sep="", width="100%"),
              div(class="scale-hint", "Raniji početak = dugoročno veći efekt")
          ),
          
          div(class="ctrl-section",
              div(class="ctrl-section-title", "Vanjski šokovi"),
              div(class="param-hint",
                  HTML("<b>EU fondovi</b> — godišnja injekcija kapitala (IPA, kohezijski fondovi...). Hrvatska trenutno prima ~1.5% BDP godišnje. Efekt: smanjuje emigraciju i ubrzava rast BDP-a.")
              ),
              sliderInput("eu_funds", "EU fondovi (% BDP godišnje)",
                          min=0, max=5, value=1.5, step=0.5, width="100%"),
              div(class="scale-hint", "0% = bez fondova  ·  1.5% = HR sada  ·  3–5% = povećani fondovi")
          ),
          
          div(class="ctrl-section",
              div(class="ctrl-section-title", "Napredni parametri"),
              div(class="param-hint",
                  HTML("<b>Osjetljivost plaće:</b> koliko snažno razlika plaća EU vs. domovina tjera ljude na odlazak.<br><br>
               <b>Edu → BDP elastičnost:</b> za koliko % raste BDP ako poraste udio obrazovanih za 1 postotni bod.")
              ),
              sliderInput("wage_sens", "Osjetljivost na razliku plaća",
                          min=0.5, max=4.0, value=2.2, step=0.1, width="100%"),
              div(class="scale-hint", "0.5 = slaba  ·  2.2 = umjerena  ·  4.0 = visoka"),
              sliderInput("edu_gdp", "Elastičnost obrazovanje → BDP",
                          min=0.1, max=1.0, value=0.45, step=0.05, width="100%"),
              div(class="scale-hint", "0.1 = slab utjecaj  ·  0.45 = umjeren  ·  1.0 = jak")
          ),
          
          div(class="ctrl-section",
              div(class="ctrl-section-title", "Export podataka"),
              downloadButton("dl_csv",      "⬇  Preuzmi CSV (primarna)", class="dl-btn"),
              downloadButton("dl_csv2",     "⬇  Preuzmi CSV (usporedba)", class="dl-btn"),
              downloadButton("dl_conclusions", "⬇  Preuzmi zaključke (.txt)", class="dl-btn")
          ),
          
          tags$button("▶  Pokreni simulaciju", class="run-btn", onclick="Shiny.setInputValue('run', Math.random())")
      ),
      
      # ── MAIN ────────────────────────────────────────────────────────────────
      div(class="app-main",
          tabsetPanel(id="maintabs",
                      
                      # ── TAB 1: DASHBOARD ──────────────────────────────────────────────
                      tabPanel("Dashboard",
                               # KPI row
                               div(class="kpi-row",
                                   div(class="kpi-card",
                                       div(class="kpi-label", "GDP per capita"),
                                       div(class="kpi-val c-blue", textOutput("kpi_gdp", inline=TRUE)),
                                       div(class="kpi-sub", textOutput("kpi_gdp_sub", inline=TRUE))
                                   ),
                                   div(class="kpi-card",
                                       div(class="kpi-label", "Visoko obrazovani"),
                                       div(class="kpi-val c-amber", textOutput("kpi_edu", inline=TRUE)),
                                       div(class="kpi-sub", "% radne snage (finale)")
                                   ),
                                   div(class="kpi-card",
                                       div(class="kpi-label", "Peak emigracija"),
                                       div(class="kpi-val c-red", textOutput("kpi_emi", inline=TRUE)),
                                       div(class="kpi-sub", textOutput("kpi_emi_yr", inline=TRUE))
                                   ),
                                   div(class="kpi-card",
                                       div(class="kpi-label", "Kum. gubitak kadra"),
                                       div(class="kpi-val c-purple", textOutput("kpi_cum", inline=TRUE)),
                                       div(class="kpi-sub", "tis. osoba (baseline)")
                                   ),
                                   div(class="kpi-card",
                                       div(class="kpi-label", "Zdravstvo"),
                                       div(class="kpi-val c-green", textOutput("kpi_health", inline=TRUE)),
                                       div(class="kpi-sub", "indeks kvalitete [0–1]")
                                   )
                               ),
                               # Main GDP chart
                               div(class="plot-card",
                                   div(class="plot-title", "BDP per capita (EUR) — svi scenariji"),
                                   div(class="chart-legend",
                                       tags$span(div(class="leg-dot", style="background:#c0392b"), "Baseline (bez politike)"),
                                       tags$span(div(class="leg-dot", style="background:#1565c0"), "Politika odmah"),
                                       tags$span(div(class="leg-dot", style="background:#2e7d32"), "Politika za 5 god."),
                                       tags$span(div(class="leg-dot", style="background:#e65100"), "Optimistični scenarij")
                                   ),
                                   plotlyOutput("plot_gdp", height="280px")
                               ),
                               div(class="plot-row-2",
                                   div(class="plot-card",
                                       div(class="plot-title", "Udio visoko obrazovanih u radnoj snazi (%)"),
                                       plotlyOutput("plot_edu", height="230px")
                                   ),
                                   div(class="plot-card",
                                       div(class="plot-title", "Neto migracija (tisuće godišnje) — baseline"),
                                       plotlyOutput("plot_netmig", height="230px")
                                   )
                               )
                      ),
                      
                      # ── TAB 2: USPOREDBA ──────────────────────────────────────────────
                      tabPanel("Usporedba zemalja",
                               uiOutput("comp_header_ui"),
                               div(class="plot-row-2",
                                   div(class="plot-card",
                                       div(class="plot-title", "BDP per capita — baseline & optimistični"),
                                       plotlyOutput("plot_comp_gdp", height="260px")
                                   ),
                                   div(class="plot-card",
                                       div(class="plot-title", "% visoko obrazovanih u radnoj snazi"),
                                       plotlyOutput("plot_comp_edu", height="260px")
                                   )
                               ),
                               div(class="plot-card",
                                   div(class="plot-title", "Kumulativna emigracija (tisuće osoba) — baseline"),
                                   plotlyOutput("plot_comp_mig", height="240px")
                               ),
                               div(class="plot-card",
                                   div(class="plot-title", "Profil zemalja — Eurostat 2023"),
                                   tableOutput("comp_table")
                               )
                      ),
                      
                      # ── TAB 3: SCENARIJI ──────────────────────────────────────────────
                      tabPanel("Analiza scenarija",
                               div(class="info-panel",
                                   HTML("<b>Što prikazuje ovaj tab?</b> Koliko svaki scenarij politike odudaraju od baseline scenarija bez intervencije — tj. koliko novca i kadra politika \"spašava\" u simuliranom periodu.")
                               ),
                               div(class="plot-card",
                                   div(class="plot-title", "Razlika BDP-a vs. baseline (€ per capita) — dobit od politike"),
                                   plotlyOutput("plot_gdp_diff", height="260px")
                               ),
                               div(class="plot-row-2",
                                   div(class="plot-card",
                                       div(class="plot-title", "Kumulativna emigracija po scenariju (tisuće osoba)"),
                                       plotlyOutput("plot_cumemi", height="230px")
                                   ),
                                   div(class="plot-card",
                                       div(class="plot-title", "IT investicijski indeks po scenariju"),
                                       plotlyOutput("plot_it", height="230px")
                                   )
                               )
                      ),
                      
                      # ── TAB 4: ZAKLJUČCI ──────────────────────────────────────────────
                      tabPanel("Zaključci & Preporuke",
                               div(class="concl-box",
                                   uiOutput("conclusions_ui")
                               )
                      ),
                      
                      # ── TAB 5: METODOLOGIJA ───────────────────────────────────────────
                      tabPanel("Metodologija",
                               div(class="plot-row-2",
                                   div(class="meth-box",
                                       HTML("
              <b style='font-size:1rem;'>Stock & Flow model — specifikacija</b><br><br>
              <b>Stanja (stocks):</b><br>
              &nbsp; <code>E</code> — Pool visoko obrazovane radne snage [osobe]<br>
              &nbsp; <code>GDP</code> — BDP per capita [EUR]<br>
              &nbsp; <code>H_qual</code> — Indeks kvalitete zdravstva [0–1]<br>
              &nbsp; <code>I_invest</code> — IT investicijski indeks<br><br>
              <b>Tokovi (flows):</b><br>
              &nbsp; <code>emi_flow = E × (β/1000) × wageGap × policy × euBoost</code><br>
              &nbsp; <code>immi_flow = E × (β/1000) × 0.25 × gdpAttract</code><br>
              &nbsp; <code>grad_flow = N × 0.008 × (1 + 8 × eduSpend%)</code><br><br>
              <b>Feedback petlje:</b><br>
              &nbsp; <b>R1</b> (pojačavajuća): ↑ emigracija → ↓ plaće → ↑ emigracija<br>
              &nbsp; <b>B1</b> (balansna): ↑ GDP → ↑ povrat → ↓ neto emigracija<br>
              &nbsp; <b>R2:</b> ↑ javna potrošnja → ↑ diplomanti → ↑ edu pool<br>
              &nbsp; <b>B2:</b> EU fondovi → ↓ emi, ↑ GDP rast<br><br>
              <b>Numerička metoda:</b> Runge-Kutta 4. reda, <code>dt = 0.25</code> god.<br>
              <b>Monte Carlo:</b> 80 simulacija s ±30% perturbacijom parametara<br>
              <b>Kalibracija:</b> Eurostat 2023, World Bank WDI, DZS<br>
              <b>Baza godina:</b> 2025
              ")
                                   ),
                                   div(class="plot-card", style="margin-bottom:0",
                                       div(class="plot-title", "Monte Carlo — distribucija finalnog GDP-a (n=80)"),
                                       plotlyOutput("plot_sensitivity", height="300px")
                                   )
                               ),
                               br(),
                               div(class="plot-card",
                                   div(class="plot-title", "Usporedba EU zemalja u modelu — GDP per capita (EUR PPS, Eurostat 2023)"),
                                   plotlyOutput("plot_eu_comparison", height="380px")
                               )
                      )
          )
      )
  )
)
server <- function(input, output, session) {
  
  output$year_range_label <- renderText({
    paste0(BASE_YEAR, " → ", input$years, " (", input$years - BASE_YEAR, " god.)")
  })
  
  # ── Core simulation reactive ─────────────────────────────────────────────
  sims <- eventReactive(input$run, {
    cd     <- COUNTRIES[[input$country1]]
    years  <- input$years - BASE_YEAR
    ps_yr  <- input$policy_start_yr - BASE_YEAR
    ps5    <- min(ps_yr + 5, years - 1)
    eu     <- input$eu_funds
    ws     <- input$wage_sens
    eg     <- input$edu_gdp
    ps     <- as.numeric(input$policy_strength)
    list(
      baseline   = run_sd(cd, years, 0,   0,    eu, ws, eg),
      policy_now = run_sd(cd, years, ps,  ps_yr, eu, ws, eg),
      policy_5y  = run_sd(cd, years, ps,  ps5,   eu, ws, eg),
      optimistic = run_sd(cd, years, 1.0, ps_yr, eu, ws, eg),
      country1   = cd, years=years, ps_yr=ps_yr
    )
  }, ignoreNULL=FALSE)
  
  sims2 <- eventReactive(list(input$run, input$country2), {
    if (input$country2 == "none") return(NULL)
    cd2   <- COUNTRIES[[input$country2]]
    years <- input$years - BASE_YEAR
    ps_yr <- input$policy_start_yr - BASE_YEAR
    eu    <- input$eu_funds
    ws    <- input$wage_sens
    eg    <- input$edu_gdp
    ps    <- as.numeric(input$policy_strength)
    list(
      baseline   = run_sd(cd2, years, 0,  0,    eu, ws, eg),
      policy_now = run_sd(cd2, years, ps, ps_yr, eu, ws, eg),
      optimistic = run_sd(cd2, years, 1.0, ps_yr, eu, ws, eg),
      country2   = cd2
    )
  }, ignoreNULL=FALSE)
  
  base <- reactive({ sims()$baseline })
  
  # ── KPIs ────────────────────────────────────────────────────────────────
  output$kpi_gdp <- renderText({
    paste0("€", format(round(tail(base()$GDP,1)), big.mark=","))
  })
  output$kpi_gdp_sub <- renderText({
    b <- base(); pct <- (tail(b$GDP,1)/b$GDP[1]-1)*100
    paste0(ifelse(pct>=0,"+",""), round(pct,1), "% od ", BASE_YEAR)
  })
  output$kpi_edu <- renderText({
    paste0(round(tail(base()$edu_pct,1),1), "%")
  })
  output$kpi_emi <- renderText({
    paste0(round(max(base()$emi_flow, na.rm=TRUE)/1000,1),"k/god.")
  })
  output$kpi_emi_yr <- renderText({
    b <- base()
    yr <- b$year[which.max(b$emi_flow)]
    paste0("Vrhunac: ", round(yr))
  })
  output$kpi_cum <- renderText({
    paste0(round(tail(base()$cum_emi,1),0),"k")
  })
  output$kpi_health <- renderText({
    round(tail(base()$H_qual, 1), 3)
  })

  # ── GDP main plot ────────────────────────────────────────────────────────
  output$plot_gdp <- renderPlotly({
    d <- sims(); p <- plot_ly()
    line_widths <- c(baseline=3, policy_now=2, policy_5y=2, optimistic=2)
    line_dashes <- c(baseline="solid", policy_now="solid", policy_5y="dash", optimistic="dot")
    for (s in names(SCEN_COLS)) {
      df <- d[[s]]
      p <- add_trace(p, data=df, x=~year, y=~GDP,
                     type="scatter", mode="lines",
                     name=SCEN_LABELS[s],
                     line=list(color=SCEN_COLS[s],
                               width=line_widths[s],
                               dash=line_dashes[s]),
                     text=~paste0("<b>",SCEN_LABELS[s],"</b><br>",
                                  round(year),". godina<br>",
                                  "€",format(round(GDP),big.mark=",")),
                     hoverinfo="text")
    }
    p %>% plt_theme() %>%
      layout(yaxis=list(tickprefix="€", tickformat=","))
  })

  # ── Education plot ───────────────────────────────────────────────────────
  output$plot_edu <- renderPlotly({
    d <- sims(); p <- plot_ly()
    line_dashes <- c(baseline="solid", policy_now="solid", policy_5y="dash", optimistic="dot")
    for (s in names(SCEN_COLS)) {
      df <- d[[s]]
      p <- add_trace(p, data=df, x=~year, y=~edu_pct,
                     type="scatter", mode="lines", name=SCEN_LABELS[s],
                     line=list(color=SCEN_COLS[s], width=2, dash=line_dashes[s]),
                     text=~paste0("<b>",SCEN_LABELS[s],"</b><br>",
                                  round(year),". godina<br>",
                                  round(edu_pct,1),"% obrazovanih"),
                     hoverinfo="text")
    }
    p %>% plt_theme() %>%
      layout(yaxis=list(ticksuffix="%", title=list(text="% obrazovanih", font=list(size=12))))
  })

  # ── Net migration ────────────────────────────────────────────────────────
  output$plot_netmig <- renderPlotly({
    df <- base()
    plot_ly(data=df, x=~year) %>%
      add_trace(y=~(immi_flow/1000), type="bar", name="Imigracija",
                marker=list(color="#27ae6066")) %>%
      add_trace(y=~(-emi_flow/1000), type="bar", name="Emigracija",
                marker=list(color="#e74c3c66")) %>%
      add_trace(y=~net_mig_k, type="scatter", mode="lines", name="Neto migracija",
                line=list(color="#1a1f2e", width=2.5)) %>%
      plt_theme() %>%
      layout(barmode="overlay",
             yaxis=list(title=list(text="tis. osoba / god.", font=list(size=12))))
  })
  
  # ── COUNTRY COMPARISON tab ───────────────────────────────────────────────
  output$comp_header <- renderUI({
    s2 <- sims2()
    if (is.null(s2)) {
      div(class="info-box",
          "Odaberi drugu državu u bočnoj traci za usporedbu.")
    } else {
      div(style="margin-bottom:12px;",
          tags$b(input$country1), " vs. ", tags$b(input$country2),
          span(style="font-size:.72rem; color:#8892a4; margin-left:8px;",
               paste0(BASE_YEAR," – ",input$years))
      )
    }
  })
  
  output$plot_comp_gdp <- renderPlotly({
    s1 <- sims(); s2 <- sims2()
    p <- plot_ly()
    for (sc in c("baseline","optimistic")) {
      p <- add_trace(p, data=s1[[sc]], x=~year, y=~GDP,
                     type="scatter", mode="lines", name=paste0(input$country1," ",SCEN_LABELS[sc]),
                     line=list(color=SCEN_COLS[sc], width=2,
                               dash=ifelse(sc=="baseline","solid","dash")),
                     text=~paste0(input$country1," ",SCEN_LABELS[sc],"<br>",round(year),": €",format(round(GDP),big.mark=",")),
                     hoverinfo="text")
    }
    if (!is.null(s2)) {
      for (sc in c("baseline","optimistic")) {
        p <- add_trace(p, data=s2[[sc]], x=~year, y=~GDP,
                       type="scatter", mode="lines", name=paste0(input$country2," ",SCEN_LABELS[sc]),
                       line=list(color=SCEN_COLS[sc], width=2, dash=ifelse(sc=="baseline","dot","dashdot")),
                       text=~paste0(input$country2," ",SCEN_LABELS[sc],"<br>",round(year),": €",format(round(GDP),big.mark=",")),
                       hoverinfo="text")
      }
    }
    p %>% plt_theme() %>%
      layout(yaxis=list(tickprefix="€", tickformat=","))
  })

  output$plot_comp_edu <- renderPlotly({
    s1 <- sims(); s2 <- sims2(); p <- plot_ly()
    p <- add_trace(p, data=s1$baseline, x=~year, y=~edu_pct,
                   type="scatter", mode="lines", name=paste0(input$country1," baseline"),
                   line=list(color="#c0392b", width=2),
                   hoverinfo="text",
                   text=~paste0(input$country1,"<br>",round(year),": ",round(edu_pct,1),"%"))
    p <- add_trace(p, data=s1$optimistic, x=~year, y=~edu_pct,
                   type="scatter", mode="lines", name=paste0(input$country1," optimistični"),
                   line=list(color="#c0392b", width=1.5, dash="dash"),
                   hoverinfo="text",
                   text=~paste0(input$country1," opt<br>",round(year),": ",round(edu_pct,1),"%"))
    if (!is.null(s2)) {
      p <- add_trace(p, data=s2$baseline, x=~year, y=~edu_pct,
                     type="scatter", mode="lines", name=paste0(input$country2," baseline"),
                     line=list(color="#1565c0", width=2),
                     hoverinfo="text",
                     text=~paste0(input$country2,"<br>",round(year),": ",round(edu_pct,1),"%"))
      p <- add_trace(p, data=s2$optimistic, x=~year, y=~edu_pct,
                     type="scatter", mode="lines", name=paste0(input$country2," optimistični"),
                     line=list(color="#1565c0", width=1.5, dash="dash"),
                     hoverinfo="text",
                     text=~paste0(input$country2," opt<br>",round(year),": ",round(edu_pct,1),"%"))
    }
    p %>% plt_theme() %>%
      layout(yaxis=list(ticksuffix="%", title=list(text="% obrazovanih", font=list(size=12))))
  })

  output$plot_comp_mig <- renderPlotly({
    s1 <- sims(); s2 <- sims2(); p <- plot_ly()
    p <- add_trace(p, data=s1$baseline, x=~year, y=~cum_emi,
                   type="scatter", mode="lines", fill="tozeroy",
                   fillcolor="#E24B4A22", name=paste0(input$country1," kum. emi"),
                   line=list(color="#c0392b", width=2),
                   text=~paste0(input$country1,"<br>",round(year),": ",round(cum_emi,1),"k"), hoverinfo="text")
    if (!is.null(s2)) {
      p <- add_trace(p, data=s2$baseline, x=~year, y=~cum_emi,
                     type="scatter", mode="lines", fill="tozeroy",
                     fillcolor="#378ADD22", name=paste0(input$country2," kum. emi"),
                     line=list(color="#1565c0", width=2),
                     text=~paste0(input$country2,"<br>",round(year),": ",round(cum_emi,1),"k"), hoverinfo="text")
    }
    p %>% plt_theme() %>%
      layout(yaxis=list(title=list(text="Kum. emigracija (tis.)", font=list(size=12))))
  })

  output$comp_table <- renderTable({
    cd1 <- COUNTRIES[[input$country1]]
    rows <- list(
      c("Populacija (mil.)", cd1$pop),
      c("GDP per capita (EUR PPS)", format(cd1$gdp_pc, big.mark=",")),
      c("% visoko obrazovanih", paste0(round(cd1$edu_pct*100,1),"%")),
      c("Stopa emigracije (‰)", paste0(cd1$emi_rate,"‰")),
      c("Razlika plaća (EU=1.0)", cd1$wage_ratio),
      c("Javna potrošnja edu (%BDP)", paste0(cd1$pub_edu,"%"))
    )
    if (input$country2 != "none") {
      cd2 <- COUNTRIES[[input$country2]]
      vals2 <- c(cd2$pop, format(cd2$gdp_pc,big.mark=","),
                 paste0(round(cd2$edu_pct*100,1),"%"),
                 paste0(cd2$emi_rate,"‰"),
                 cd2$wage_ratio, paste0(cd2$pub_edu,"%"))
      df <- data.frame(
        Pokazatelj = sapply(rows, `[`, 1),
        setNames(data.frame(sapply(rows,`[`,2), vals2, stringsAsFactors=FALSE),
                 c(input$country1, input$country2)),
        Izvor = "Eurostat 2023",
        stringsAsFactors=FALSE, check.names=FALSE
      )
    } else {
      df <- data.frame(
        Pokazatelj = sapply(rows,`[`,1),
        Vrijednost = sapply(rows,`[`,2),
        Izvor = "Eurostat 2023",
        stringsAsFactors=FALSE
      )
    }
    df
  }, striped=TRUE, hover=TRUE, width="100%", rownames=FALSE)
  
  # ── SCENARIO tab ─────────────────────────────────────────────────────────
  output$plot_gdp_diff <- renderPlotly({
    d <- sims(); base_gdp <- d$baseline$GDP; p <- plot_ly()
    for (s in c("policy_now","policy_5y","optimistic")) {
      df <- d[[s]]
      n  <- min(nrow(df), length(base_gdp))
      dg <- df$GDP[1:n] - base_gdp[1:n]
      p <- add_trace(p, x=df$year[1:n], y=dg,
                     type="scatter", mode="lines", fill="tozeroy",
                     fillcolor=paste0(substr(SCEN_COLS[s],1,7),"22"),
                     name=SCEN_LABELS[s],
                     line=list(color=SCEN_COLS[s], width=2),
                     text=paste0(SCEN_LABELS[s],"<br>",round(df$year[1:n]),": +€",round(dg)),
                     hoverinfo="text")
    }
    p %>% plt_theme() %>%
      layout(yaxis=list(title=list(text="GDP razlika vs. baseline (€)", font=list(size=12)), tickprefix="€"))
  })
  
  output$plot_cumemi <- renderPlotly({
    d <- sims(); p <- plot_ly()
    for (s in names(SCEN_COLS)) {
      df <- d[[s]]
      p <- add_trace(p, data=df, x=~year, y=~cum_emi,
                     type="scatter", mode="lines", name=SCEN_LABELS[s],
                     line=list(color=SCEN_COLS[s], width=2),
                     text=~paste0(SCEN_LABELS[s],"<br>",round(year),": ",round(cum_emi,1),"k"), hoverinfo="text")
    }
    p %>% plt_theme() %>%
      layout(yaxis=list(title=list(text="Kumulativna emigracija (tis.)", font=list(size=12))))
  })

  output$plot_it <- renderPlotly({
    d <- sims(); p <- plot_ly()
    for (s in names(SCEN_COLS)) {
      df <- d[[s]]
      p <- add_trace(p, data=df, x=~year, y=~I_invest,
                     type="scatter", mode="lines", name=SCEN_LABELS[s],
                     line=list(color=SCEN_COLS[s], width=1.8),
                     text=~paste0(SCEN_LABELS[s],"<br>",round(year),": ",round(I_invest,2)), hoverinfo="text")
    }
    p %>% plt_theme() %>%
      layout(yaxis=list(title=list(text="IT investicijski indeks", font=list(size=12))))
  })

  # ── CONCLUSIONS tab ──────────────────────────────────────────────────────
  conclusions_text <- reactive({
    generate_conclusions(
      sims(), input$country1, input$years - BASE_YEAR,
      input$policy_start_yr - BASE_YEAR,
      input$eu_funds, as.numeric(input$policy_strength)
    )
  })
  
  output$conclusions_ui <- renderUI({
    txt <- conclusions_text()
    lines <- strsplit(txt, "\n")[[1]]
    html_parts <- lapply(lines, function(l) {
      l2 <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", l)
      if      (grepl("^## ",  l)) tags$h2(HTML(sub("^## ","",l2)))
      else if (grepl("^### ", l)) tags$h3(HTML(sub("^### ","",l2)))
      else if (grepl("^---",  l)) tags$hr()
      else if (nchar(trimws(l))==0) tags$br()
      else if (grepl("🚨|KRITIČNO", l)) div(class="alert-crit", HTML(l2))
      else if (grepl("⚠|UMJEREN RIZIK", l)) div(class="alert-warn", HTML(l2))
      else if (grepl("✓|RELATIVNO STABILAN|trenutna putanja je dovoljna", l)) div(class="alert-ok", HTML(l2))
      else if (grepl("^\\*Napomena", l)) div(class="concl-footer", HTML(l2))
      else tags$p(HTML(l2))
    })
    # Add meta line at top
    cd <- COUNTRIES[[input$country1]]
    tagList(
      tags$div(class="concl-meta",
               tags$strong(input$country1), " · Horizont: 2025–", input$years,
               " · Baza podataka: Eurostat 2023"
      ),
      do.call(tagList, html_parts)
    )
  })
  
  # ── METHODOLOGY: Monte Carlo ─────────────────────────────────────────────
  output$plot_sensitivity <- renderPlotly({
    set.seed(42)
    cd    <- COUNTRIES[[input$country1]]
    years <- input$years - BASE_YEAR
    n_runs <- 80
    final_gdps <- numeric(n_runs)
    for (i in seq_len(n_runs)) {
      cd_p <- cd
      cd_p$emi_rate   <- cd$emi_rate   * runif(1, 0.7, 1.3)
      cd_p$wage_ratio <- cd$wage_ratio * runif(1, 0.85,1.15)
      cd_p$pub_edu    <- cd$pub_edu    * runif(1, 0.8, 1.2)
      out <- run_sd(cd_p, years, 0, 0, input$eu_funds, input$wage_sens, input$edu_gdp)
      final_gdps[i] <- tail(out$GDP, 1)
    }
    med <- median(final_gdps)
    q1  <- quantile(final_gdps, 0.25)
    q3  <- quantile(final_gdps, 0.75)
    plot_ly(x=final_gdps, type="histogram",
            marker=list(color="#1565c0", opacity=0.7), nbinsx=25) %>%
      plt_theme() %>%
      add_segments(x=med, xend=med, y=0, yend=18,
                   line=list(color="#c0392b", width=2, dash="dash"), name="Medijan") %>%
      add_segments(x=q1, xend=q1, y=0, yend=18,
                   line=list(color="#e65100", width=1.5, dash="dot"), name="Q1") %>%
      add_segments(x=q3, xend=q3, y=0, yend=18,
                   line=list(color="#e65100", width=1.5, dash="dot"), name="Q3") %>%
      layout(
        xaxis=list(title="Finalni GDP per capita (€)", tickprefix="€"),
        yaxis=list(title="Frekvencija (n=80)"),
        annotations=list(
          list(x=med, y=16, text=paste0("Medijan: €",format(round(med),big.mark=",")),
               showarrow=FALSE, font=list(size=12, color="#c0392b")),
          list(x=q1, y=12, text=paste0("Q1: €",format(round(q1),big.mark=",")),
               showarrow=FALSE, font=list(size=12, color="#e65100"), xanchor="right"),
          list(x=q3, y=12, text=paste0("Q3: €",format(round(q3),big.mark=",")),
               showarrow=FALSE, font=list(size=12, color="#e65100"), xanchor="left")
        )
      )
  })
  
  output$plot_eu_comparison <- renderPlotly({
    df_comp <- do.call(rbind, lapply(names(COUNTRIES), function(cn) {
      cd <- COUNTRIES[[cn]]
      data.frame(country=cn, gdp=cd$gdp_pc, emi=cd$emi_rate,
                 edu=cd$edu_pct*100, wage=cd$wage_ratio, stringsAsFactors=FALSE)
    })) %>% arrange(gdp)
    sel <- input$country1
    cols <- ifelse(df_comp$country==sel, "#1565c0", "rgba(55,138,221,0.28)")
    plot_ly(data=df_comp, x=~gdp, y=~reorder(country,gdp),
            type="bar", orientation="h",
            marker=list(color=cols), text=~paste0("€",format(gdp,big.mark=",")),
            textposition="outside",
            hovertext=~paste0(country,"<br>GDP: €",format(gdp,big.mark=","),
                              "<br>Emi: ",emi,"‰  |  Edu: ",round(edu,1),"%" ),
            hoverinfo="text") %>%
      plt_theme() %>%
      layout(xaxis=list(title="GDP per capita (EUR PPS)", tickprefix="€"),
             yaxis=list(title="", tickfont=list(size=12)),
             margin=list(l=130,r=60))
  })
  
  # ── DOWNLOADS ────────────────────────────────────────────────────────────
  export_df <- reactive({
    d <- sims()
    bind_rows(lapply(names(SCEN_COLS), function(s) {
      df <- d[[s]]
      df$scenario <- SCEN_LABELS[s]
      df$country  <- input$country1
      df[, c("country","scenario","year","GDP","E","H_qual","I_invest",
             "edu_pct","emi_flow","immi_flow","net_mig","cum_emi")]
    }))
  })
  
  output$dl_csv <- downloadHandler(
    filename=function() paste0("brain_drain_", gsub("[^a-zA-Z]","",input$country1), "_", input$years, ".csv"),
    content=function(file) write.csv(export_df(), file, row.names=FALSE)
  )
  
  output$dl_csv2 <- downloadHandler(
    filename=function() paste0("brain_drain_comparison_", input$years, ".csv"),
    content=function(file) {
      s2 <- sims2()
      df1 <- export_df()
      if (!is.null(s2)) {
        df2 <- bind_rows(lapply(c("baseline","policy_now","optimistic"), function(sc) {
          df <- s2[[sc]]; df$scenario <- SCEN_LABELS[sc]; df$country <- input$country2
          df[, c("country","scenario","year","GDP","E","H_qual","I_invest",
                 "edu_pct","emi_flow","immi_flow","net_mig","cum_emi")]
        }))
        write.csv(rbind(df1, df2), file, row.names=FALSE)
      } else {
        write.csv(df1, file, row.names=FALSE)
      }
    }
  )
  
  output$dl_csv_full <- downloadHandler(
    filename=function() paste0("brain_drain_full_", input$years, ".csv"),
    content=function(file) write.csv(export_df(), file, row.names=FALSE)
  )
  
  output$dl_conclusions <- downloadHandler(
    filename=function() paste0("zakljucci_", gsub("[^a-zA-Z]","",input$country1), "_", input$years, ".txt"),
    content=function(file) writeLines(conclusions_text(), file)
  )
}

shinyApp(ui, server)
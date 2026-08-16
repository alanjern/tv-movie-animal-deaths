"use strict";

/* ---------- Error-bar plugin for Chart.js (works for vertical & horizontal bars, and lines) ---------- */
const errorBarsPlugin = {
  id: "errorBars",
  afterDatasetsDraw(chart) {
    const { ctx } = chart;
    chart.data.datasets.forEach((dataset, dsIndex) => {
      if (!dataset.errorBars) return;
      if (chart.getDatasetMeta(dsIndex).hidden) return;
      const meta = chart.getDatasetMeta(dsIndex);
      const valueScale = meta.vScale;
      const vertical = valueScale.axis === "y";
      ctx.save();
      ctx.strokeStyle = dataset.errorBarColor || "rgba(30,26,22,0.55)";
      ctx.lineWidth = 1.5;
      meta.data.forEach((el, i) => {
        const eb = dataset.errorBars[i];
        if (!eb) return;
        const { low, high } = eb;
        if (vertical) {
          const x = el.x;
          const yLow = valueScale.getPixelForValue(low);
          const yHigh = valueScale.getPixelForValue(high);
          const cap = 5;
          ctx.beginPath(); ctx.moveTo(x, yLow); ctx.lineTo(x, yHigh); ctx.stroke();
          ctx.beginPath(); ctx.moveTo(x - cap, yLow); ctx.lineTo(x + cap, yLow); ctx.stroke();
          ctx.beginPath(); ctx.moveTo(x - cap, yHigh); ctx.lineTo(x + cap, yHigh); ctx.stroke();
        } else {
          const y = el.y;
          const xLow = valueScale.getPixelForValue(low);
          const xHigh = valueScale.getPixelForValue(high);
          const cap = 5;
          ctx.beginPath(); ctx.moveTo(xLow, y); ctx.lineTo(xHigh, y); ctx.stroke();
          ctx.beginPath(); ctx.moveTo(xLow, y - cap); ctx.lineTo(xLow, y + cap); ctx.stroke();
          ctx.beginPath(); ctx.moveTo(xHigh, y - cap); ctx.lineTo(xHigh, y + cap); ctx.stroke();
        }
      });
      ctx.restore();
    });
  },
};
Chart.register(errorBarsPlugin);

/* ---------- Stats helpers ---------- */
function wilsonCI(x, n, z = 1.96) {
  if (n === 0) return { low: 0, high: 0 };
  const p = x / n;
  const denom = 1 + (z * z) / n;
  const center = p + (z * z) / (2 * n);
  const margin = z * Math.sqrt((p * (1 - p)) / n + (z * z) / (4 * n * n));
  return {
    low: Math.max(0, (center - margin) / denom) * 100,
    high: Math.min(1, (center + margin) / denom) * 100,
  };
}

function computeRate(records, category) {
  const n = records.length;
  const x = records.reduce((acc, r) => acc + (r[category].death ? 1 : 0), 0);
  const rate = n ? (x / n) * 100 : 0;
  const ci = wilsonCI(x, n);
  return { n, x, rate, ciLow: ci.low, ciHigh: ci.high };
}

const CATEGORY_LABEL = { dog: "Dog", cat: "Cat", animal: "Other animal" };
const CATEGORY_COLOR = { dog: "#b5563c", cat: "#4a7a6b", animal: "#c99a3a" };
const SNAPSHOT_PALETTE = ["#b5563c", "#4a7a6b", "#c99a3a", "#5b6ea8", "#8a5aa3", "#3f8f8f", "#a4623d", "#6b8c3f"];

/* ---------- App state ---------- */
const state = {
  type: "all",
  yearMin: null,
  yearMax: null,
  minVotes: 0,
  genres: new Set(),
};

let DATA = null; // { count, genres, records }
const snapshots = [];
let snapshotColorIdx = 0;
const trendCategories = new Set(["dog", "cat", "animal"]);

/* ---------- Data loading ---------- */
fetch("data/dataset.json")
  .then((r) => r.json())
  .then((json) => {
    DATA = json;
    init();
  })
  .catch((err) => {
    document.querySelector("main").innerHTML =
      '<p style="color:#b5563c">Failed to load dataset.json: ' + err + "</p>";
  });

function init() {
  const years = DATA.records.map((r) => r.year).filter((y) => y != null);
  state.yearMin = Math.min(...years);
  state.yearMax = Math.max(...years);

  document.getElementById("year-min").value = state.yearMin;
  document.getElementById("year-max").value = state.yearMax;
  document.getElementById("year-min").min = Math.min(...years);
  document.getElementById("year-max").max = Math.max(...years);

  renderGenreChips();
  bindFilterEvents();
  bindTabEvents();
  bindGenreTabEvents();
  bindTrendTabEvents();
  bindCompareTabEvents();

  refreshAll();
}

function renderGenreChips() {
  const wrap = document.getElementById("genre-list");
  wrap.innerHTML = "";
  DATA.genres.forEach((g) => {
    const chip = document.createElement("button");
    chip.type = "button";
    chip.className = "genre-chip";
    chip.textContent = g;
    chip.dataset.genre = g;
    chip.addEventListener("click", () => {
      if (state.genres.has(g)) {
        state.genres.delete(g);
        chip.classList.remove("active");
      } else {
        state.genres.add(g);
        chip.classList.add("active");
      }
      refreshAll();
    });
    wrap.appendChild(chip);
  });
}

/* ---------- Filtering ---------- */
function applyFilters(records, s) {
  return records.filter((r) => {
    if (s.type !== "all" && r.type !== s.type) return false;
    if (r.year == null || r.year < s.yearMin || r.year > s.yearMax) return false;
    if (s.minVotes > 0 && (r.numVotes || 0) < s.minVotes) return false;
    if (s.genres.size > 0 && !r.genres.some((g) => s.genres.has(g))) return false;
    return true;
  });
}

function describeFilters(s) {
  const parts = [];
  parts.push(s.type === "all" ? "All types" : s.type === "movie" ? "Movies" : "TV series");
  parts.push(`${s.yearMin}–${s.yearMax}`);
  if (s.genres.size > 0) parts.push([...s.genres].join(", "));
  if (s.minVotes > 0) parts.push(`≥${s.minVotes.toLocaleString()} votes`);
  return parts.join(" • ");
}

function currentFiltered() {
  return applyFilters(DATA.records, state);
}

/* ---------- Filter UI events ---------- */
function bindFilterEvents() {
  document.querySelectorAll("#type-filter .seg-btn").forEach((btn) => {
    btn.addEventListener("click", () => {
      document.querySelectorAll("#type-filter .seg-btn").forEach((b) => b.classList.remove("active"));
      btn.classList.add("active");
      state.type = btn.dataset.value;
      refreshAll();
    });
  });

  const yMin = document.getElementById("year-min");
  const yMax = document.getElementById("year-max");
  yMin.addEventListener("change", () => {
    let v = parseInt(yMin.value, 10);
    if (Number.isNaN(v)) v = state.yearMin;
    state.yearMin = Math.min(v, state.yearMax);
    yMin.value = state.yearMin;
    refreshAll();
  });
  yMax.addEventListener("change", () => {
    let v = parseInt(yMax.value, 10);
    if (Number.isNaN(v)) v = state.yearMax;
    state.yearMax = Math.max(v, state.yearMin);
    yMax.value = state.yearMax;
    refreshAll();
  });

  document.getElementById("min-votes").addEventListener("change", (e) => {
    const v = parseInt(e.target.value, 10);
    state.minVotes = Number.isNaN(v) || v < 0 ? 0 : v;
    refreshAll();
  });
}

function refreshAll() {
  updateFilterSummary();
  const activeTab = document.querySelector(".tab-btn.active").dataset.tab;
  renderActiveTab(activeTab);
}

function updateFilterSummary() {
  const filtered = currentFiltered();
  const movies = filtered.filter((r) => r.type === "movie").length;
  const tv = filtered.filter((r) => r.type === "tv").length;
  document.getElementById("filter-summary").textContent =
    `${filtered.length.toLocaleString()} titles match (${movies.toLocaleString()} movies, ${tv.toLocaleString()} TV series)`;
}

/* ---------- Tabs ---------- */
function bindTabEvents() {
  document.querySelectorAll(".tab-btn").forEach((btn) => {
    btn.addEventListener("click", () => {
      document.querySelectorAll(".tab-btn").forEach((b) => b.classList.remove("active"));
      document.querySelectorAll(".tab-panel").forEach((p) => p.classList.remove("active"));
      btn.classList.add("active");
      document.getElementById("tab-" + btn.dataset.tab).classList.add("active");
      updateGenreFilterAvailability(btn.dataset.tab);
      renderActiveTab(btn.dataset.tab);
    });
  });
}

function updateGenreFilterAvailability(tab) {
  const disabled = tab === "genre";
  document.getElementById("genre-list").classList.toggle("disabled", disabled);
  const hint = document.getElementById("genre-filter-hint");
  hint.textContent = disabled ? "(disabled on this view — every genre is shown for comparison)" : "(none selected = all)";
  hint.classList.toggle("warning", disabled);
}

function renderActiveTab(tab) {
  if (!DATA) return;
  if (tab === "overview") renderOverview();
  else if (tab === "genre") renderGenreTab();
  else if (tab === "trend") renderTrendTab();
  else if (tab === "compare") renderCompareTab();
}

/* ---------- Overview tab ---------- */
let overviewChart = null;
function renderOverview() {
  const filtered = currentFiltered();
  const cats = ["dog", "cat", "animal"];
  const stats = cats.map((c) => computeRate(filtered, c));

  const ctx = document.getElementById("overview-chart");
  const data = {
    labels: cats.map((c) => CATEGORY_LABEL[c]),
    datasets: [
      {
        label: "Death rate (%)",
        data: stats.map((s) => s.rate),
        backgroundColor: cats.map((c) => CATEGORY_COLOR[c]),
        errorBars: stats.map((s) => ({ low: s.ciLow, high: s.ciHigh })),
      },
    ],
  };

  if (overviewChart) overviewChart.destroy();
  overviewChart = new Chart(ctx, {
    type: "bar",
    data,
    options: {
      responsive: true,
      maintainAspectRatio: false,
      animation: false,
      plugins: {
        legend: { display: false },
        tooltip: {
          callbacks: {
            label: (item) => {
              const s = stats[item.dataIndex];
              return `${s.rate.toFixed(1)}%  (${s.x}/${s.n}, 95% CI ${s.ciLow.toFixed(1)}–${s.ciHigh.toFixed(1)}%)`;
            },
          },
        },
      },
      scales: {
        y: { beginAtZero: true, max: 100, title: { display: true, text: "Death rate (%)" } },
      },
    },
  });

  document.getElementById("overview-n").textContent =
    `n = ${filtered.length.toLocaleString()} titles — ${describeFilters(state)}`;
}

/* ---------- Genre tab ---------- */
let genreChart = null;
function bindGenreTabEvents() {
  document.getElementById("genre-category").addEventListener("change", renderGenreTab);
  document.getElementById("genre-min-n").addEventListener("change", renderGenreTab);
}

function renderGenreTab() {
  const category = document.getElementById("genre-category").value;
  const minN = parseInt(document.getElementById("genre-min-n").value, 10) || 1;

  // Apply all filters except genre selection, so this tab shows the breakdown across genres.
  const baseState = { ...state, genres: new Set() };
  const filtered = applyFilters(DATA.records, baseState);

  const byGenre = new Map();
  filtered.forEach((r) => {
    r.genres.forEach((g) => {
      if (!byGenre.has(g)) byGenre.set(g, []);
      byGenre.get(g).push(r);
    });
  });

  let rows = [...byGenre.entries()]
    .map(([genre, recs]) => ({ genre, ...computeRate(recs, category) }))
    .filter((row) => row.n >= minN)
    .sort((a, b) => b.rate - a.rate);

  const ctx = document.getElementById("genre-chart");
  const data = {
    labels: rows.map((r) => `${r.genre} (${r.n})`),
    datasets: [
      {
        label: "Death rate (%)",
        data: rows.map((r) => r.rate),
        backgroundColor: CATEGORY_COLOR[category],
        errorBars: rows.map((r) => ({ low: r.ciLow, high: r.ciHigh })),
      },
    ],
  };

  if (genreChart) genreChart.destroy();
  genreChart = new Chart(ctx, {
    type: "bar",
    data,
    options: {
      indexAxis: "y",
      responsive: true,
      maintainAspectRatio: false,
      animation: false,
      plugins: {
        legend: { display: false },
        tooltip: {
          callbacks: {
            label: (item) => {
              const r = rows[item.dataIndex];
              return `${r.rate.toFixed(1)}%  (${r.x}/${r.n}, 95% CI ${r.ciLow.toFixed(1)}–${r.ciHigh.toFixed(1)}%)`;
            },
          },
        },
      },
      scales: {
        x: { beginAtZero: true, max: 100, title: { display: true, text: `${CATEGORY_LABEL[category]} death rate (%)` } },
      },
    },
  });
}

/* ---------- Trend tab ---------- */
let trendChart = null;
function bindTrendTabEvents() {
  document.querySelectorAll("#trend-category-toggle .seg-btn").forEach((btn) => {
    btn.addEventListener("click", () => {
      const cat = btn.dataset.value;
      if (trendCategories.has(cat)) {
        trendCategories.delete(cat);
        btn.classList.remove("active");
      } else {
        trendCategories.add(cat);
        btn.classList.add("active");
      }
      renderTrendTab();
    });
  });
  document.getElementById("trend-bucket").addEventListener("change", renderTrendTab);
  document.getElementById("trend-min-n").addEventListener("change", renderTrendTab);
}

function renderTrendTab() {
  const cats = ["dog", "cat", "animal"].filter((c) => trendCategories.has(c));
  const bucketSize = Math.max(1, parseInt(document.getElementById("trend-bucket").value, 10) || 3);
  const minN = parseInt(document.getElementById("trend-min-n").value, 10) || 1;

  const filtered = currentFiltered();
  const buckets = new Map();
  filtered.forEach((r) => {
    const start = state.yearMin + Math.floor((r.year - state.yearMin) / bucketSize) * bucketSize;
    if (!buckets.has(start)) buckets.set(start, []);
    buckets.get(start).push(r);
  });

  const bucketRows = [...buckets.entries()]
    .map(([start, recs]) => ({ start, end: start + bucketSize - 1, n: recs.length, recs }))
    .filter((row) => row.n >= minN)
    .sort((a, b) => a.start - b.start);

  const labels = bucketRows.map((r) => (bucketSize === 1 ? `${r.start}` : `${r.start}–${r.end}`));

  const datasets = cats.map((cat) => {
    const stats = bucketRows.map((r) => computeRate(r.recs, cat));
    return {
      label: CATEGORY_LABEL[cat],
      data: stats.map((s) => s.rate),
      borderColor: CATEGORY_COLOR[cat],
      backgroundColor: CATEGORY_COLOR[cat],
      errorBarColor: "rgba(30,26,22,0.55)",
      errorBars: stats.map((s) => ({ low: s.ciLow, high: s.ciHigh })),
      tension: 0.15,
      pointRadius: 4,
      _stats: stats,
    };
  });

  const ctx = document.getElementById("trend-chart");
  if (trendChart) trendChart.destroy();
  trendChart = new Chart(ctx, {
    type: "line",
    data: { labels, datasets },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      animation: false,
      plugins: {
        legend: { display: cats.length > 1, position: "bottom" },
        tooltip: {
          callbacks: {
            label: (item) => {
              const s = datasets[item.datasetIndex]._stats[item.dataIndex];
              const label = datasets[item.datasetIndex].label;
              return `${label}: ${s.rate.toFixed(1)}%  (${s.x}/${s.n}, 95% CI ${s.ciLow.toFixed(1)}–${s.ciHigh.toFixed(1)}%)`;
            },
          },
        },
      },
      scales: {
        y: { beginAtZero: true, max: 100, title: { display: true, text: "Death rate (%)" } },
      },
    },
  });
}

/* ---------- Compare tab ---------- */
let compareChart = null;
function bindCompareTabEvents() {
  document.getElementById("add-snapshot").addEventListener("click", () => {
    const color = SNAPSHOT_PALETTE[snapshotColorIdx % SNAPSHOT_PALETTE.length];
    snapshotColorIdx++;
    snapshots.push({
      id: Date.now() + Math.random(),
      label: `Snapshot ${snapshots.length + 1}`,
      color,
      filterState: {
        type: state.type,
        yearMin: state.yearMin,
        yearMax: state.yearMax,
        minVotes: state.minVotes,
        genres: new Set(state.genres),
      },
      desc: describeFilters(state),
    });
    renderCompareTab();
  });
}

function renderSnapshotList() {
  const wrap = document.getElementById("snapshot-list");
  wrap.innerHTML = "";
  snapshots.forEach((snap) => {
    const row = document.createElement("div");
    row.className = "snapshot-item";

    const swatch = document.createElement("span");
    swatch.className = "snapshot-color";
    swatch.style.background = snap.color;

    const labelInput = document.createElement("input");
    labelInput.className = "snapshot-label-input";
    labelInput.value = snap.label;
    labelInput.addEventListener("input", () => {
      snap.label = labelInput.value;
      renderCompareTab(true);
    });

    const desc = document.createElement("span");
    desc.className = "snapshot-desc";
    desc.textContent = snap.desc;

    const remove = document.createElement("button");
    remove.className = "snapshot-remove";
    remove.type = "button";
    remove.textContent = "Remove";
    remove.addEventListener("click", () => {
      const idx = snapshots.findIndex((s) => s.id === snap.id);
      if (idx >= 0) snapshots.splice(idx, 1);
      renderCompareTab();
    });

    row.append(swatch, labelInput, desc, remove);
    wrap.appendChild(row);
  });
}

function renderCompareTab(skipListRerender) {
  if (!skipListRerender) renderSnapshotList();

  const cats = ["dog", "cat", "animal"];
  const datasets = snapshots.map((snap) => {
    const recs = applyFilters(DATA.records, snap.filterState);
    const stats = cats.map((c) => computeRate(recs, c));
    return {
      label: `${snap.label} (n=${recs.length})`,
      data: stats.map((s) => s.rate),
      backgroundColor: snap.color,
      errorBars: stats.map((s) => ({ low: s.ciLow, high: s.ciHigh })),
    };
  });

  const ctx = document.getElementById("compare-chart");
  if (compareChart) compareChart.destroy();
  compareChart = new Chart(ctx, {
    type: "bar",
    data: { labels: cats.map((c) => CATEGORY_LABEL[c]), datasets },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      animation: false,
      plugins: {
        legend: { display: snapshots.length > 0, position: "bottom" },
        tooltip: {
          callbacks: {
            label: (item) => {
              const snap = snapshots[item.datasetIndex];
              const recs = applyFilters(DATA.records, snap.filterState);
              const s = computeRate(recs, cats[item.dataIndex]);
              return `${s.rate.toFixed(1)}%  (${s.x}/${s.n}, 95% CI ${s.ciLow.toFixed(1)}–${s.ciHigh.toFixed(1)}%)`;
            },
          },
        },
      },
      scales: {
        y: { beginAtZero: true, max: 100, title: { display: true, text: "Death rate (%)" } },
      },
    },
  });
}


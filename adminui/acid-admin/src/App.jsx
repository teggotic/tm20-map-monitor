import { useQuery, useQueryClient } from "@tanstack/react-query";
import React, {
  useCallback,
  useDeferredValue,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import {
  flexRender,
  getCoreRowModel,
  getSortedRowModel,
  useReactTable,
} from "@tanstack/react-table";
import { useVirtualizer } from "@tanstack/react-virtual";

const LIST_URL = "http://localhost:8083/admin/api/maps";
const SAVE_URL = "http://localhost:8083/admin/api/replace-map";
const SAVE_MS = 700;
const GRID =
  "80px 320px 220px 110px 110px 100px 170px 110px 160px 110px 110px 110px 140px 80px";
const GRID_MIN = 1830;

const low = (x) => String(x ?? "").toLowerCase();
const jclone = (x) => JSON.parse(JSON.stringify(x));
const asArray = (x) => (Array.isArray(x) ? x : Array.isArray(x?.maps) ? x.maps : []);

async function fetchMaps() {
  const r = await fetch(LIST_URL);
  if (!r.ok) throw new Error(`${r.status} ${await r.text()}`);
  return asArray(await r.json());
}

function typeLabel(v) {
  if (v == null) return "";
  if (typeof v === "string") return v;
  if (typeof v === "object") {
    if (typeof v.tag === "string") {
      return v.contents == null ? v.tag : `${v.tag}:${String(v.contents)}`;
    }
    if ("MT_Other" in v) return `MT_Other:${String(v.MT_Other)}`;
  }
  try {
    return JSON.stringify(v);
  } catch {
    return String(v);
  }
}

const INFO_OPTIONS = ["CheatedAt", "BrokenPhysics", "LowInputStrat"];

function normalizeInfoValue(x) {
  const s = String(x ?? "");
  if (s.startsWith("TM")) return s.slice(2);
  return s;
}

function infoList(m) {
  return Array.isArray(m?.info)
    ? m.info.map((x) => normalizeInfoValue(x))
    : [];
}

function hasNadeoInfo(m) {
  return typeof m?.hasNadeoInfo === "boolean" ? m.hasNadeoInfo : m?.authorUid != null;
}

function isUnbeaten(m) {
  return !m?.currentWR || Number(m.currentWR?.time ?? Infinity) > Number(m?.authorMedal ?? -Infinity);
}

function fmtDate(s) {
  if (!s) return "";
  const d = new Date(s);
  if (Number.isNaN(d.getTime())) return String(s);
  return d.toISOString().slice(0, 19).replace("T", " ");
}

function tri(val, want) {
  if (want === "all") return true;
  if (want === "yes") return !!val;
  if (want === "no") return !val;
  if (want === "unknown") return val == null;
  return true;
}

function numOrNull(v) {
  if (v === "" || v == null) return null;
  const n = Number(v);
  return Number.isFinite(n) ? n : null;
}

function InfoMultiSelect({ value, onChange }) {
  const vals = Array.isArray(value) ? value.map(normalizeInfoValue) : [];

  const toggle = (opt) => {
    const has = vals.includes(opt);
    onChange(has ? vals.filter((x) => x !== opt) : [...vals, opt]);
  };

  return (
    <div className="rounded border border-slate-300 bg-white p-2">
      <div className="flex flex-wrap gap-2">
        {INFO_OPTIONS.map((opt) => {
          const active = vals.includes(opt);
          return (
            <button
              key={opt}
              type="button"
              className={`rounded border px-2 py-1 text-xs ${
                active
                  ? "border-sky-600 bg-sky-600 text-white"
                  : "border-slate-300 bg-white text-slate-800 hover:bg-slate-50"
              }`}
              onClick={() => toggle(opt)}
            >
              {opt}
            </button>
          );
        })}
      </div>
    </div>
  );
}

export default function App() {
  const queryClient = useQueryClient();
  const [selectedId, setSelectedId] = useState(null);

  const {
    data: maps = [],
    isLoading: loading,
    error,
    refetch,
  } = useQuery({
    queryKey: ["maps"],
    queryFn: fetchMaps,
  });

  const loadErr = error ? String(error?.message ?? error) : "";

  const [filters, setFilters] = useState({
    search: "",
    beaten: "all",
    type: "all",
    hiddenOnTmx: "all",
    hasNadeoInfo: "all",
    omitted: "all",
    clones: "all",
    validation: "all",
    hasHiddenReason: "all",
    info: "",
    uploadedFrom: "",
    uploadedTo: "",
    fileMin: "",
    fileMax: "",
  });
  const f = useDeferredValue(filters);

  const [sorting, setSorting] = useState([{ id: "tmxId", desc: false }]);

  const [draft, setDraft] = useState(null);
  const [raw, setRaw] = useState("");
  const [rawErr, setRawErr] = useState("");
  const [savedSig, setSavedSig] = useState("");
  const [saveState, setSaveState] = useState("idle");

  const byId = useMemo(
    () => new Map(maps.map((m) => [String(m.tmxId), m])),
    [maps],
  );
  const selected = selectedId == null ? null : byId.get(String(selectedId)) ?? null;

  useEffect(() => {
    if (maps.length && selectedId == null) setSelectedId(maps[0].tmxId);
  }, [maps, selectedId]);

  useEffect(() => {
    if (!selected) {
      setDraft(null);
      setRaw("");
      setRawErr("");
      setSavedSig("");
      setSaveState("idle");
      return;
    }
    const x = jclone(selected);
    const sig = JSON.stringify(x);
    setDraft(x);
    setRaw(JSON.stringify(x, null, 2));
    setRawErr("");
    setSavedSig(sig);
    setSaveState("idle");
  }, [selected]);

  const typeOptions = useMemo(
    () =>
      Array.from(
        new Set(maps.map((m) => typeLabel(m.mapType)).filter(Boolean)),
      ).sort(),
    [maps],
  );

  const filtered = useMemo(() => {
    const q = low(f.search).trim();
    const infoQ = low(f.info).trim();
    const from = f.uploadedFrom ? new Date(f.uploadedFrom).getTime() : null;
    const to = f.uploadedTo
      ? new Date(f.uploadedTo).getTime() + 24 * 3600 * 1000 - 1
      : null;
    const fileMin = numOrNull(f.fileMin);
    const fileMax = numOrNull(f.fileMax);

    return maps.filter((m) => {
      if (q) {
        const hay = [
          m.tmxId,
          m.name,
          m.uid,
          m.authorUid,
          m.hiddenReason,
          typeLabel(m.mapType),
        ]
          .map(low)
          .join(" ");
        if (!hay.includes(q)) return false;
      }

      if (f.beaten === "beaten" && isUnbeaten(m)) return false;
      if (f.beaten === "unbeaten" && !isUnbeaten(m)) return false;
      if (f.type !== "all" && typeLabel(m.mapType) !== f.type) return false;
      if (!tri(m.hiddenOnTmx, f.hiddenOnTmx)) return false;
      if (!tri(hasNadeoInfo(m), f.hasNadeoInfo)) return false;
      if (!tri(m.omittedFromPlugin, f.omitted)) return false;
      if (!tri(m.hasClones, f.clones)) return false;
      if (!tri(m.validationReplay != null, f.validation)) return false;
      if (!tri(m.hiddenReason != null && String(m.hiddenReason).trim() !== "", f.hasHiddenReason)) return false;

      if (infoQ) {
        const s = low(infoList(m).join(" "));
        if (!s.includes(infoQ)) return false;
      }

      if (from != null || to != null) {
        const t = m.uploadedAt ? new Date(m.uploadedAt).getTime() : null;
        if (from != null && (t == null || t < from)) return false;
        if (to != null && (t == null || t > to)) return false;
      }

      if (fileMin != null && Number(m.fileSize ?? -Infinity) < fileMin) return false;
      if (fileMax != null && Number(m.fileSize ?? Infinity) > fileMax) return false;

      return true;
    });
  }, [maps, f]);

  const columns = useMemo(
    () => [
      { accessorKey: "tmxId", header: "id" },
      { accessorKey: "name", header: "name" },
      { accessorKey: "uid", header: "uid" },
      { accessorKey: "authorMedal", header: "AT" },
      {
        id: "wr",
        header: "WR",
        accessorFn: (m) => m.currentWR?.time ?? null,
        cell: (x) => x.getValue() ?? "",
      },
      {
        id: "beaten",
        header: "beaten",
        accessorFn: (m) => (isUnbeaten(m) ? "no" : "yes"),
      },
      {
        id: "mapType",
        header: "type",
        accessorFn: (m) => typeLabel(m.mapType),
      },
      {
        id: "uploadedAt",
        header: "uploaded",
        accessorFn: (m) => (m.uploadedAt ? new Date(m.uploadedAt).getTime() : 0),
        cell: (x) => fmtDate(x.row.original.uploadedAt),
      },
      {
        id: "hiddenOnTmx",
        header: "hidden",
        accessorFn: (m) => (m.hiddenOnTmx ? 1 : 0),
        cell: (x) => (x.row.original.hiddenOnTmx ? "yes" : "no"),
      },
      {
        id: "hiddenReason",
        header: "reason",
        accessorFn: (m) => m.hiddenReason ?? "",
      },
      {
        id: "authorUid",
        header: "authorUid",
        accessorFn: (m) => m.authorUid ?? "",
      },
      {
        id: "hasClones",
        header: "clones",
        accessorFn: (m) => String(m.hasClones),
      },
      {
        id: "omittedFromPlugin",
        header: "omitted",
        accessorFn: (m) => (m.omittedFromPlugin ? 1 : 0),
        cell: (x) => (x.row.original.omittedFromPlugin ? "yes" : "no"),
      },
      {
        id: "fileSize",
        header: "fileSize",
        accessorFn: (m) => Number(m.fileSize ?? 0),
        cell: (x) => x.row.original.fileSize ?? "",
      },
    ],
    [],
  );

  const table = useReactTable({
    data: filtered,
    columns,
    state: { sorting },
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getSortedRowModel: getSortedRowModel(),
  });

  const rows = table.getRowModel().rows;
  const parentRef = useRef(null);
  const vr = useVirtualizer({
    count: rows.length,
    getScrollElement: () => parentRef.current,
    estimateSize: () => 25,
    overscan: 50,
  });

  const saveMap = useCallback(
    async (map) => {
      setSaveState("saving");
      try {
        const r = await fetch(SAVE_URL, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(map),
        });
        if (!r.ok) throw new Error(`${r.status} ${await r.text()}`);

        let next = map;
        const ct = r.headers.get("content-type") || "";
        if (ct.includes("application/json")) {
          next = await r.json();
        }

        queryClient.setQueryData(["maps"], (prev = []) => {
          const i = prev.findIndex((x) => String(x.tmxId) === String(next.tmxId));
          if (i < 0) return prev;
          const out = prev.slice();
          out[i] = next;
          return out;
        });

        const sig = JSON.stringify(next);
        setDraft(jclone(next));
        setRaw(JSON.stringify(next, null, 2));
        setSavedSig(sig);
        setRawErr("");
        setSaveState("saved");
      } catch (e) {
        setSaveState(`error: ${String(e?.message ?? e)}`);
      }
    },
    [queryClient],
  );

  useEffect(() => {
    if (!draft || rawErr) return;
    const sig = JSON.stringify(draft);
    if (sig === savedSig) return;
    setSaveState("dirty");
    const t = setTimeout(() => {
      saveMap(draft);
    }, SAVE_MS);
    return () => clearTimeout(t);
  }, [draft, rawErr, savedSig, saveMap]);

  const patchDraft = (patch) => {
    setDraft((prev) => {
      if (!prev) return prev;
      const next = { ...prev, ...patch };
      setRaw(JSON.stringify(next, null, 2));
      setRawErr("");
      return next;
    });
  };

  const cellCls =
    "px-2 py-1 text-xs whitespace-nowrap overflow-hidden text-ellipsis border-b border-slate-200";
  const inputCls =
    "w-full rounded border border-slate-300 bg-white px-2 py-1.5 text-sm outline-none focus:border-sky-500";
  const readonlyCls =
    "w-full rounded border border-slate-200 bg-slate-100 px-2 py-1.5 text-sm text-slate-700";
  const selCls = `${inputCls} pr-8`;
  const areaCls =
    "w-full rounded border border-slate-300 bg-white px-2 py-1.5 text-xs font-mono outline-none focus:border-sky-500";

  return (
    <div className="h-screen bg-slate-100 text-slate-900">
      <div className="flex h-full">
        <aside className="w-72 shrink-0 border-r border-slate-300 bg-white p-3 overflow-auto">
          <div className="mb-3 flex items-center justify-between">
            <div className="text-sm font-semibold">filters</div>
            <button
              className="rounded border border-slate-300 px-2 py-1 text-xs hover:bg-slate-50"
              onClick={() => refetch()}
            >
              reload
            </button>
          </div>

          <div className="space-y-2">
            <input
              className={inputCls}
              placeholder="search id / name / uid / reason"
              value={filters.search}
              onChange={(e) =>
                setFilters((x) => ({ ...x, search: e.target.value }))
              }
            />

            <select
              className={selCls}
              value={filters.beaten}
              onChange={(e) =>
                setFilters((x) => ({ ...x, beaten: e.target.value }))
              }
            >
              <option value="all">beaten: all</option>
              <option value="beaten">beaten</option>
              <option value="unbeaten">unbeaten</option>
            </select>

            <select
              className={selCls}
              value={filters.type}
              onChange={(e) =>
                setFilters((x) => ({ ...x, type: e.target.value }))
              }
            >
              <option value="all">type: all</option>
              {typeOptions.map((t) => (
                <option key={t} value={t}>
                  {t}
                </option>
              ))}
            </select>

            <select
              className={selCls}
              value={filters.hiddenOnTmx}
              onChange={(e) =>
                setFilters((x) => ({ ...x, hiddenOnTmx: e.target.value }))
              }
            >
              <option value="all">hiddenOnTmx: all</option>
              <option value="yes">hiddenOnTmx: yes</option>
              <option value="no">hiddenOnTmx: no</option>
            </select>

            <select
              className={selCls}
              value={filters.hasHiddenReason}
              onChange={(e) =>
                setFilters((x) => ({ ...x, hasHiddenReason: e.target.value }))
              }
            >
              <option value="all">hiddenReason: all</option>
              <option value="yes">hiddenReason: yes</option>
              <option value="no">hiddenReason: no</option>
            </select>

            <select
              className={selCls}
              value={filters.hasNadeoInfo}
              onChange={(e) =>
                setFilters((x) => ({ ...x, hasNadeoInfo: e.target.value }))
              }
            >
              <option value="all">hasNadeoInfo: all</option>
              <option value="yes">hasNadeoInfo: yes</option>
              <option value="no">hasNadeoInfo: no</option>
            </select>

            <select
              className={selCls}
              value={filters.omitted}
              onChange={(e) =>
                setFilters((x) => ({ ...x, omitted: e.target.value }))
              }
            >
              <option value="all">omitted: all</option>
              <option value="yes">omitted: yes</option>
              <option value="no">omitted: no</option>
            </select>

            <select
              className={selCls}
              value={filters.clones}
              onChange={(e) =>
                setFilters((x) => ({ ...x, clones: e.target.value }))
              }
            >
              <option value="all">hasClones: all</option>
              <option value="yes">hasClones: yes</option>
              <option value="no">hasClones: no</option>
              <option value="unknown">hasClones: unknown</option>
            </select>

            <select
              className={selCls}
              value={filters.validation}
              onChange={(e) =>
                setFilters((x) => ({ ...x, validation: e.target.value }))
              }
            >
              <option value="all">validationReplay: all</option>
              <option value="yes">validationReplay: yes</option>
              <option value="no">validationReplay: no</option>
            </select>

            <input
              className={inputCls}
              placeholder="info contains"
              value={filters.info}
              onChange={(e) =>
                setFilters((x) => ({ ...x, info: e.target.value }))
              }
            />

            <div>
              <div className="mb-1 text-xs text-slate-500">uploaded from</div>
              <input
                className={inputCls}
                type="date"
                value={filters.uploadedFrom}
                onChange={(e) =>
                  setFilters((x) => ({ ...x, uploadedFrom: e.target.value }))
                }
              />
            </div>

            <div>
              <div className="mb-1 text-xs text-slate-500">uploaded to</div>
              <input
                className={inputCls}
                type="date"
                value={filters.uploadedTo}
                onChange={(e) =>
                  setFilters((x) => ({ ...x, uploadedTo: e.target.value }))
                }
              />
            </div>

            <div className="grid grid-cols-2 gap-2">
              <input
                className={inputCls}
                type="number"
                placeholder="file min"
                value={filters.fileMin}
                onChange={(e) =>
                  setFilters((x) => ({ ...x, fileMin: e.target.value }))
                }
              />
              <input
                className={inputCls}
                type="number"
                placeholder="file max"
                value={filters.fileMax}
                onChange={(e) =>
                  setFilters((x) => ({ ...x, fileMax: e.target.value }))
                }
              />
            </div>

            <button
              className="w-full rounded border border-slate-300 px-2 py-1.5 text-sm hover:bg-slate-50"
              onClick={() =>
                setFilters({
                  search: "",
                  beaten: "all",
                  type: "all",
                  hiddenOnTmx: "all",
                  hasNadeoInfo: "all",
                  omitted: "all",
                  clones: "all",
                  validation: "all",
                  hasHiddenReason: "all",
                  info: "",
                  uploadedFrom: "",
                  uploadedTo: "",
                  fileMin: "",
                  fileMax: "",
                })
              }
            >
              clear
            </button>
          </div>
        </aside>

        <main className="min-w-0 flex-1 flex flex-col">
          <div className="border-b border-slate-300 bg-white px-3 py-2 text-sm flex items-center gap-4">
            <div>
              {loading ? "loading..." : `${filtered.length.toLocaleString()} / ${maps.length.toLocaleString()}`}
            </div>
            {loadErr && <div className="text-rose-600">{loadErr}</div>}
          </div>

          <div ref={parentRef} className="min-h-0 flex-1 overflow-auto">
            <div
              className="sticky top-0 z-10 border-b border-slate-300 bg-slate-50 text-xs font-semibold"
              style={{ minWidth: GRID_MIN }}
            >
              <div style={{ display: "grid", gridTemplateColumns: GRID }}>
                {table.getFlatHeaders().map((h) => (
                  <button
                    key={h.id}
                    className="px-2 py-2 text-left border-r border-slate-200 hover:bg-slate-100"
                    onClick={h.column.getToggleSortingHandler()}
                  >
                    {flexRender(h.column.columnDef.header, h.getContext())}
                    {{
                      asc: " ↑",
                      desc: " ↓",
                    }[h.column.getIsSorted()] ?? ""}
                  </button>
                ))}
              </div>
            </div>

            <div
              style={{
                height: vr.getTotalSize(),
                position: "relative",
                minWidth: GRID_MIN,
              }}
            >
              {vr.getVirtualItems().map((v) => {
                const row = rows[v.index];
                const active = String(selectedId) === String(row.original.tmxId);
                return (
                  <div
                    key={row.id}
                    className={`absolute left-0 right-0 cursor-pointer ${
                      active ? "bg-sky-50" : v.index % 2 ? "bg-white" : "bg-slate-50/40"
                    } hover:bg-sky-50`}
                    style={{
                      transform: `translateY(${v.start}px)`,
                    }}
                    onClick={() => setSelectedId(row.original.tmxId)}
                  >
                    <div style={{ display: "grid", gridTemplateColumns: GRID }}>
                      {row.getVisibleCells().map((cell) => (
                        <div key={cell.id} className={cellCls}>
                          {flexRender(
                            cell.column.columnDef.cell ??
                              cell.column.columnDef.accessorFn ??
                              cell.column.columnDef.accessorKey,
                            cell.getContext(),
                          )}
                        </div>
                      ))}
                    </div>
                  </div>
                );
              })}
            </div>
          </div>
        </main>

        <aside className="w-[32rem] shrink-0 border-l border-slate-300 bg-white flex flex-col">
          <div className="border-b border-slate-300 px-3 py-2 flex items-center justify-between">
            <div className="text-sm font-semibold">
              {draft ? `map ${draft.tmxId}` : "editor"}
            </div>
            <div
              className={`text-xs ${
                saveState.startsWith("error")
                  ? "text-rose-600"
                  : saveState === "saving"
                    ? "text-amber-600"
                    : saveState === "saved"
                      ? "text-emerald-600"
                      : "text-slate-500"
              }`}
            >
              {saveState}
            </div>
          </div>

          {!draft ? (
            <div className="p-4 text-sm text-slate-500">select a row</div>
          ) : (
            <div className="min-h-0 flex-1 overflow-auto p-3 space-y-3">
              <div className="grid grid-cols-2 gap-2">
                <div>
                  <div className="mb-1 text-xs text-slate-500">tmxId</div>
                  <input className={readonlyCls} value={draft.tmxId ?? ""} readOnly />
                </div>
                <div>
                  <div className="mb-1 text-xs text-slate-500">fileSize</div>
                  <input
                    className={inputCls}
                    type="number"
                    value={draft.fileSize ?? ""}
                    onChange={(e) =>
                      patchDraft({ fileSize: numOrNull(e.target.value) })
                    }
                  />
                </div>
              </div>

              <div>
                <div className="mb-1 text-xs text-slate-500">name</div>
                <input className={readonlyCls} value={draft.name ?? ""} readOnly />
              </div>

              <div>
                <div className="mb-1 text-xs text-slate-500">uid</div>
                <input className={readonlyCls} value={draft.uid ?? ""} readOnly />
              </div>

              <div className="grid grid-cols-2 gap-2">
                <div>
                  <div className="mb-1 text-xs text-slate-500">authorMedal</div>
                  <input
                    className={readonlyCls}
                    value={draft.authorMedal ?? ""}
                    readOnly
                  />
                </div>
                <div>
                  <div className="mb-1 text-xs text-slate-500">authorUid</div>
                  <input
                    className={readonlyCls}
                    value={draft.authorUid ?? ""}
                    readOnly
                  />
                </div>
              </div>

              <div>
                <div className="mb-1 text-xs text-slate-500">hiddenReason</div>
                <input
                  className={inputCls}
                  value={draft.hiddenReason ?? ""}
                  onChange={(e) =>
                    patchDraft({
                      hiddenReason:
                        e.target.value === "" ? null : e.target.value,
                    })
                  }
                />
              </div>

              <div className="grid grid-cols-2 gap-2">
                <div>
                  <div className="mb-1 text-xs text-slate-500">hiddenOnTmx</div>
                  <select
                    className={selCls}
                    value={draft.hiddenOnTmx ? "true" : "false"}
                    onChange={(e) =>
                      patchDraft({ hiddenOnTmx: e.target.value === "true" })
                    }
                  >
                    <option value="false">false</option>
                    <option value="true">true</option>
                  </select>
                </div>
                <div>
                  <div className="mb-1 text-xs text-slate-500">omittedFromPlugin</div>
                  <select
                    className={selCls}
                    value={draft.omittedFromPlugin ? "true" : "false"}
                    onChange={(e) =>
                      patchDraft({
                        omittedFromPlugin: e.target.value === "true",
                      })
                    }
                  >
                    <option value="false">false</option>
                    <option value="true">true</option>
                  </select>
                </div>
              </div>

              <div className="grid grid-cols-2 gap-2">
                <div>
                  <div className="mb-1 text-xs text-slate-500">hasClones</div>
                  <select
                    className={selCls}
                    value={
                      draft.hasClones == null
                        ? "null"
                        : draft.hasClones
                          ? "true"
                          : "false"
                    }
                    onChange={(e) =>
                      patchDraft({
                        hasClones:
                          e.target.value === "null"
                            ? null
                            : e.target.value === "true",
                      })
                    }
                  >
                    <option value="null">null</option>
                    <option value="false">false</option>
                    <option value="true">true</option>
                  </select>
                </div>
                <div>
                  <div className="mb-1 text-xs text-slate-500">atSetByPlugin</div>
                  <select
                    className={selCls}
                    value={
                      draft.atSetByPlugin == null
                        ? "null"
                        : draft.atSetByPlugin
                          ? "true"
                          : "false"
                    }
                    onChange={(e) =>
                      patchDraft({
                        atSetByPlugin:
                          e.target.value === "null"
                            ? null
                            : e.target.value === "true",
                      })
                    }
                  >
                    <option value="null">null</option>
                    <option value="false">false</option>
                    <option value="true">true</option>
                  </select>
                </div>
              </div>

              <div className="rounded border border-slate-200 bg-slate-50 p-2 text-xs text-slate-600">
                <div>type: {typeLabel(draft.mapType) || "-"}</div>
                <div>uploadedAt: {fmtDate(draft.uploadedAt) || "-"}</div>
                <div>WR: {draft.currentWR?.time ?? "-"}</div>
                <div>beaten: {isUnbeaten(draft) ? "no" : "yes"}</div>
                <div>info: {infoList(draft).join(", ") || "-"}</div>
              </div>

              <div>
                <div className="mb-1 text-xs text-slate-500">info</div>
                <InfoMultiSelect
                  value={draft.info}
                  onChange={(next) => patchDraft({ info: next })}
                />
              </div>

              <div>
                <div className="mb-1 text-xs text-slate-500">raw json</div>
                <textarea
                  className={`${areaCls} h-[28rem]`}
                  value={raw}
                  onChange={(e) => {
                    const v = e.target.value;
                    setRaw(v);
                    try {
                      const x = JSON.parse(v);
                      setDraft(x);
                      setRawErr("");
                    } catch (err) {
                      setRawErr(String(err?.message ?? err));
                    }
                  }}
                />
                {rawErr && <div className="mt-1 text-xs text-rose-600">{rawErr}</div>}
              </div>

              <div className="text-xs text-slate-500">
                protected fields: tmxId, uid, name, authorMedal, authorUid
              </div>

              <div className="flex gap-2">
                <button
                  className="rounded border border-slate-300 px-3 py-1.5 text-sm hover:bg-slate-50"
                  onClick={() => {
                    if (!selected) return;
                    const x = jclone(selected);
                    setDraft(x);
                    setRaw(JSON.stringify(x, null, 2));
                    setRawErr("");
                  }}
                >
                  revert draft
                </button>
                <button
                  className="rounded border border-slate-300 px-3 py-1.5 text-sm hover:bg-slate-50"
                  onClick={() => draft && saveMap(draft)}
                  disabled={!draft || !!rawErr}
                >
                  save now
                </button>
              </div>
            </div>
          )}
        </aside>
      </div>
    </div>
  );
}

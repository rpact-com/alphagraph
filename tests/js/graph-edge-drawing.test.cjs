const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

function setup() {
    function surface() {
        const handlers = new Map();
        return {
            handlers,
            addEventListener(name, fn) {
                if (!handlers.has(name)) handlers.set(name, new Set());
                handlers.get(name).add(fn);
            },
            removeEventListener(name, fn) { handlers.get(name).delete(fn); },
            fire(name, overrides = {}) {
                const event = {button: 2, buttons: 2, pointerId: 1,
                    clientX: 120, clientY: 70, stopped: false,
                    preventDefault() { this.prevented = true; },
                    stopImmediatePropagation() { this.stopped = true; }, ...overrides};
                for (const fn of handlers.get(name) || []) fn(event);
                return event;
            }
        };
    }
    const canvas = surface(), window = surface(), events = [], drawing = [];
    canvas.getBoundingClientRect = () => ({left: 100, top: 50, width: 200, height: 100});
    const networkHandlers = new Map();
    const ctx = Object.fromEntries(["save", "restore", "setLineDash", "beginPath",
        "moveTo", "lineTo", "stroke", "closePath", "fill"].map(name =>
        [name, (...args) => drawing.push([name, ...args])]));
    const network = {
        on(name, fn) { networkHandlers.set(name, fn); },
        off(name, fn) { if (networkHandlers.get(name) === fn) networkHandlers.delete(name); },
        redraw() { networkHandlers.get("afterDrawing")?.(ctx); },
        getScale: () => 2,
        DOMtoCanvas: p => ({x: p.x / 2, y: p.y / 2}),
        getNodeAt: p => p.y >= 10 && p.y <= 30 ?
            (p.x >= 10 && p.x <= 30 ? "A" : p.x >= 110 && p.x <= 130 ? "B" : undefined) : undefined,
        getBoundingBox: id => ({left: id === "A" ? 5 : 55, right: id === "A" ? 15 : 65,
            top: 5, bottom: 15})
    };
    const element = {id: "graph-graph", querySelector: () => canvas};
    window.Shiny = {setInputValue: (name, data) => events.push({name, data})};
    vm.runInNewContext(fs.readFileSync(path.join(__dirname,
        "../../inst/www/js/graph-edge-drawing.js"), "utf8"), {
        window, document: {getElementById: () => ({chart: network})}
    });
    const init = () => window.alphaGraphInitializeEdgeDrawing(element, "graph-");
    init();
    return {canvas, window, events, drawing, network, init, element};
}

test("right drag draws a zoom-aware arrow and submits endpoints only on release", () => {
    const s = setup();
    s.canvas.fire("pointerdown");
    s.canvas.fire("contextmenu"); // Some browsers dispatch this on press.
    assert.equal(s.events.length, 1);
    s.window.fire("pointermove", {clientX: 220});
    assert.ok(s.drawing.some(call => call[0] === "fill"), "arrowhead is rendered");
    assert.ok(s.drawing.some(call => call[0] === "moveTo" && call[1] === 15));
    s.window.fire("pointerup", {clientX: 220});
    s.canvas.fire("contextmenu", {clientX: 220}); // Others dispatch it on release.
    assert.equal(s.events.filter(e => e.name.endsWith("edge_drawn")).length, 1);
    const edge = s.events.find(e => e.name.endsWith("edge_drawn")).data;
    assert.equal(edge.from, "A");
    assert.equal(edge.to, "B");
    assert.equal(s.events.filter(e => e.name.endsWith("node_right_click")).length, 0);
    const count = s.drawing.length;
    s.network.redraw();
    assert.equal(s.drawing.length, count, "preview disappears on release");
});

test("small right-click movement opens the context menu, left drag passes through", () => {
    const s = setup();
    assert.equal(s.canvas.fire("pointerdown", {button: 0, buttons: 1}).stopped, false);
    s.canvas.fire("pointerdown");
    s.window.fire("pointermove", {clientX: 123});
    s.window.fire("pointerup", {clientX: 123});
    assert.equal(s.events.at(-1).name, "graph-node_right_click");
    assert.equal(s.events.at(-1).data.id, "A");
    assert.equal(s.drawing.length, 0);
});

for (const end of ["empty", "outside", "self", "escape", "cancel", "blur", "lost release"]) {
    test("right drag cancels on " + end, () => {
        const s = setup();
        s.canvas.fire("pointerdown");
        s.window.fire("pointermove", {clientX: 220});
        if (end === "escape") s.window.fire("keydown", {key: "Escape"});
        if (end === "cancel") s.window.fire("pointercancel");
        if (end === "blur") s.window.fire("blur");
        if (end === "lost release") s.window.fire("pointermove", {buttons: 0});
        s.window.fire("pointerup", {clientX: end === "empty" ? 180 :
            end === "outside" ? 400 : end === "self" ? 120 : 220});
        s.canvas.fire("contextmenu");
        assert.deepEqual(s.events.map(e => e.name), ["graph-ctx_close"]);
    });
}

test("rerender replaces listeners and cleanup removes global handlers", () => {
    const s = setup();
    s.init();
    s.canvas.fire("pointerdown");
    s.window.fire("pointerup");
    assert.equal(s.events.filter(e => e.name.endsWith("node_right_click")).length, 1);
    s.element.__alphaGraphEdgeCleanup();
    for (const handlers of s.window.handlers.values()) assert.equal(handlers.size, 0);
});

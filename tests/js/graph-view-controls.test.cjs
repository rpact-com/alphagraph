const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

test("central table layout handles initial render, shrink/grow and real overflow", () => {
    let frames = new Map(), nextId = 0;
    const observers = [];
    const tables = [0, 1].map(() => {
        const holder = { clientWidth: 600 };
        const element = {
            parentElement: { clientWidth: 640 },
            width: 600, contentWidth: 600, fits: false, updates: 0,
            classList: { toggle(name, value) {
                assert.equal(name, "alpha-table-fits");
                element.fits = value;
            } },
            querySelector: selector => selector.includes("wtHolder") ? holder :
                { getBoundingClientRect: () => ({ width: element.contentWidth }) },
            closest: () => row
        };
        element.hot = {
            addHook(name, fn) { assert.equal(name, "afterRender"); this.hook = fn; },
            updateSettings(settings) {
                assert.deepEqual(Object.keys(settings), ["width"]);
                element.updates++;
                element.width = holder.clientWidth = settings.width;
                element.contentWidth = Math.max(360, settings.width);
                this.hook();
            }
        };
        return element;
    });
    const row = { querySelectorAll: () => tables };
    const window = {
        addEventListener() {},
        requestAnimationFrame(fn) { frames.set(++nextId, fn); return nextId; },
        cancelAnimationFrame(id) { frames.delete(id); },
        getComputedStyle: () => ({ paddingLeft: "20px", paddingRight: "20px" }),
        HTMLWidgets: { getInstance: el => ({ hot: el.hot }) }
    };
    vm.runInNewContext(fs.readFileSync(path.join(__dirname,
        "../../inst/www/js/graph-view-controls.js"), "utf8"), {
        window,
        ResizeObserver: class {
            constructor(fn) { observers.push(fn); }
            observe() {}
            disconnect() {}
        },
        document: { addEventListener() {} },
        Shiny: { addCustomMessageHandler() {} }
    });
    function flush() {
        while (frames.size) {
            const pending = [...frames.values()];
            frames.clear();
            pending.forEach(fn => fn());
        }
    }
    tables.forEach(el => window.alphaGraphTableRendered(el));
    flush();
    for (const width of [640, 340, 740, 420, 640]) {
        tables.forEach(el => { el.parentElement.clientWidth = width; });
        observers.forEach(fn => fn());
        flush();
        tables.forEach(el => {
            assert.equal(el.width, width - 40);
            assert.equal(el.fits, width - 40 >= 358);
        });
    }
    const previous = tables[0].updates;
    observers.forEach(fn => fn());
    flush();
    assert.equal(tables[0].updates, previous, "unchanged width must not render again");
    for (const excess of [0, 1, 2, 2.1, 100, 0]) {
        tables.forEach(el => {
            el.contentWidth = el.width + excess;
            el.hot.hook();
        });
        flush();
        tables.forEach(el => assert.equal(el.fits, excess <= 2));
    }
});

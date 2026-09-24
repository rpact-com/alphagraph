// Run with: node --test tests/js/graph-view-controls.test.cjs
const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

test("both tables shrink and grow from their container content width", () => {
    const containers = [{ clientWidth: 640 }, { clientWidth: 640 }];
    const tables = containers.map(parentElement => ({
        parentElement,
        inlineWidth: 600,
        updates: [],
        renders: 0,
        getBoundingClientRect() { return { width: this.inlineWidth }; }
    }));
    const row = { querySelectorAll: () => tables };
    let resize;
    let nextFrame;
    const window = {
        addEventListener(name, callback) {
            if (name === "resize") resize = callback;
        },
        requestAnimationFrame(callback) { nextFrame = callback; return 1; },
        cancelAnimationFrame() { nextFrame = null; },
        getComputedStyle: () => ({ paddingLeft: "20px", paddingRight: "20px" }),
        HTMLWidgets: {
            getInstance(element) {
                return { hot: {
                    updateSettings(settings) {
                        element.updates.push(settings);
                        element.inlineWidth = settings.width;
                    },
                    render() { element.renders++; }
                } };
            }
        }
    };
    vm.runInNewContext(fs.readFileSync(path.join(
        __dirname, "../../inst/www/js/graph-view-controls.js"
    ), "utf8"), {
        window,
        document: { addEventListener() {}, querySelector: () => row },
        Shiny: { addCustomMessageHandler() {} }
    });

    for (const containerWidth of [640, 340, 740, 420, 640]) {
        containers.forEach(container => { container.clientWidth = containerWidth; });
        resize();
        nextFrame();
        tables.forEach(table => {
            assert.equal(table.inlineWidth, containerWidth - 40);
            // Resizing must not replace data or reset the user's edits.
            assert.deepEqual(Object.keys(table.updates.at(-1)), ["width"]);
        });
    }
    tables.forEach(table => assert.equal(table.renders, 5));
    containers.forEach(container => { container.clientWidth = 0; });
    resize();
    nextFrame();
    tables.forEach(table => assert.equal(table.renders, 5));
});

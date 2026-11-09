const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

function setup() {
    const messages = [], updates = [];
    const window = {Shiny: {setInputValue: (id, value) => messages.push({id, value})}};
    vm.runInNewContext(fs.readFileSync(path.join(__dirname,
        "../../inst/www/js/graph-grid.js"), "utf8"), {window});
    const network = {body: {nodes: {A: {x: 19, y: -11}, B: {x: -63, y: 96}},
        data: {nodes: {update: values => updates.push(values)}}}, getScale: () => 1};
    return {grid: window.alphaGraphGrid, network, messages, updates};
}

test("initial snapping persists corrected centers once; drag frames do not mutate the DataSet", () => {
    const s = setup(), config = {size: 40, origin: {x: 14, y: -5}};
    s.grid.beforeRedraw(s.network, config, "graph-node_positions");
    assert.deepEqual(s.network.body.nodes, {A: {x: 14, y: -5}, B: {x: -66, y: 115}});
    assert.equal(s.messages.length, 1);
    assert.equal(s.messages[0].id, "graph-node_positions");
    assert.equal(s.updates.length, 1);
    s.grid.beforeRedraw(s.network, config, "graph-node_positions");
    assert.equal(s.messages.length, 1, "no repeated initial position events");
    s.network.body.nodes.A.x = 39;
    s.grid.beforeRedraw(s.network, config, "graph-node_positions");
    assert.equal(s.network.body.nodes.A.x, 54);
    assert.equal(s.updates.length, 1, "live snapping leaves the drag anchor unchanged");
    s.grid.dragEnd(s.network, config, "graph-node_positions", ["A"]);
    assert.equal(s.updates.length, 2);
    assert.equal(s.messages.at(-1).value.nodes[0].x, 54);
    const count = s.messages.length;
    s.grid.dragEnd(s.network, config, "graph-node_positions", []);
    assert.equal(s.messages.length, count, "panning must not save node coordinates");
});

test("hiding the grid preserves snapping and stored node positions", () => {
    const s = setup(), config = {size: 40, origin: {x: 14, y: -5}};
    let redraws = 0;
    s.network.redraw = () => redraws++;
    s.grid.setVisible(s.network, false);
    s.grid.draw({}, s.network, config); // Hidden grid must not touch the canvas.
    assert.equal(s.updates.length, 0, "visibility alone must not change positions");
    s.grid.beforeRedraw(s.network, config, "positions");
    assert.equal(s.network.body.nodes.A.x, 14);
    s.network.body.nodes.A.x = 39;
    s.grid.beforeRedraw(s.network, config, "positions");
    assert.equal(s.network.body.nodes.A.x, 54);
    s.grid.dragEnd(s.network, config, "positions", ["A"]);
    assert.equal(s.messages.at(-1).value.nodes[0].x, 54);
    const updates = s.updates.length;
    s.grid.setVisible(s.network, true);
    assert.equal(s.network.__alphaGraphGridVisible, true);
    assert.equal(s.updates.length, updates);
    assert.equal(redraws, 2);
});

for (const size of [40, 24.5]) {
    test(`grid intersections track node centers with spacing ${size} through zoom, pan and resize`, () => {
        const s = setup(), config = {size, origin: {x: 14.5, y: -5.25}};
        s.grid.beforeRedraw(s.network, config, "positions");
        for (const scale of [0.25, 1, 1.2, 2.5]) {
            for (const dpr of [1, 1.25, 1.5, 2]) {
                for (const offset of [{x: 310.5, y: 190.75}, {x: 640, y: 400}]) {
                    const t = {a: scale * dpr, d: scale * dpr,
                        e: offset.x * dpr, f: offset.y * dpr};
                    const lines = [];
                    let start;
                    const ctx = {canvas: {width: 1280 * dpr, height: 800 * dpr},
                        getTransform: () => t, save() {}, restore() {}, setLineDash() {},
                        beginPath() {}, stroke() {}, moveTo(x, y) { start = {x, y}; },
                        lineTo(x, y) { lines.push({start, end: {x, y}}); }};
                    s.network.getScale = () => scale;
                    s.grid.draw(ctx, s.network, config);
                    assert.equal(ctx.lineWidth, 1 / scale);
                    for (const node of Object.values(s.network.body.nodes)) {
                        const vertical = lines.find(l => l.start.x === l.end.x && l.start.x === node.x);
                        const horizontal = lines.find(l => l.start.y === l.end.y && l.start.y === node.y);
                        assert.ok(vertical && horizontal, "every center is a grid intersection");
                        assert.equal(vertical.start.x * t.a + t.e, node.x * t.a + t.e);
                        assert.equal(horizontal.start.y * t.d + t.f, node.y * t.d + t.f);
                    }
                }
            }
        }
    });
}

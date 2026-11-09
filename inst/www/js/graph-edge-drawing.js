(function() {
    "use strict";

    window.alphaGraphInitializeEdgeDrawing = function(element, inputPrefix) {
        if (element.__alphaGraphEdgeCleanup) element.__alphaGraphEdgeCleanup();
        var graph = document.getElementById("graph" + element.id);
        var network = graph && graph.chart;
        var canvas = element.querySelector("canvas");
        if (!network || !canvas) return;
        var drag = null;
        var listeners = [];
        function listen(target, name, fn) {
            target.addEventListener(name, fn, true);
            listeners.push(function() { target.removeEventListener(name, fn, true); });
        }
        function send(name, value) {
            window.Shiny.setInputValue(inputPrefix + name, value, {priority: "event"});
        }
        function point(event) {
            var rect = canvas.getBoundingClientRect();
            return {x: event.clientX - rect.left, y: event.clientY - rect.top};
        }
        function inside(p) {
            var rect = canvas.getBoundingClientRect();
            return p.x >= 0 && p.y >= 0 && p.x <= rect.width && p.y <= rect.height;
        }
        function nodeAt(p) { return inside(p) ? network.getNodeAt(p) : undefined; }
        function stop(event) {
            event.preventDefault();
            event.stopImmediatePropagation();
        }
        function reset() {
            drag = null;
            network.redraw();
        }
        function contextMenu(event, node) {
            if (node !== undefined) {
                send("node_right_click", {id: node, x: event.clientX,
                    y: event.clientY, nonce: Math.random()});
            } else {
                send("ctx_close", Math.random());
            }
        }
        // Clip the preview at the circular node's outline, including zoom.
        function boundary(id, toward) {
            var box = network.getBoundingBox(id);
            var center = {x: (box.left + box.right) / 2, y: (box.top + box.bottom) / 2};
            var dx = toward.x - center.x, dy = toward.y - center.y;
            var rx = Math.max(1, (box.right - box.left) / 2);
            var ry = Math.max(1, (box.bottom - box.top) / 2);
            var distance = Math.sqrt(dx * dx / (rx * rx) + dy * dy / (ry * ry));
            return distance > 1 ? {x: center.x + dx / distance, y: center.y + dy / distance} : center;
        }
        function draw(ctx) {
            if (!drag || !drag.moved || drag.cancelled || drag.from === undefined) return;
            var end = network.DOMtoCanvas(drag.point);
            var start = boundary(drag.from, end);
            var target = nodeAt(drag.point);
            if (target !== undefined && target !== drag.from) end = boundary(target, start);
            var angle = Math.atan2(end.y - start.y, end.x - start.x);
            var size = 12 / network.getScale();
            ctx.save();
            ctx.strokeStyle = ctx.fillStyle = target === drag.from ? "#c0392b" : "#2878b5";
            ctx.lineWidth = 2 / network.getScale();
            ctx.setLineDash([6 / network.getScale(), 4 / network.getScale()]);
            ctx.beginPath();
            ctx.moveTo(start.x, start.y);
            ctx.lineTo(end.x, end.y);
            ctx.stroke();
            ctx.setLineDash([]);
            ctx.beginPath();
            ctx.moveTo(end.x, end.y);
            ctx.lineTo(end.x - size * Math.cos(angle - Math.PI / 6),
                end.y - size * Math.sin(angle - Math.PI / 6));
            ctx.lineTo(end.x - size * Math.cos(angle + Math.PI / 6),
                end.y - size * Math.sin(angle + Math.PI / 6));
            ctx.closePath();
            ctx.fill();
            ctx.restore();
        }
        listen(canvas, "pointerdown", function(event) {
            if (event.button !== 2) return;
            stop(event);
            var p = point(event);
            drag = {from: nodeAt(p), start: p, point: p, moved: false,
                cancelled: false, pointerId: event.pointerId};
            send("ctx_close", Math.random());
        });
        listen(window, "pointermove", function(event) {
            if (!drag || drag.pointerId !== event.pointerId) return;
            // Recover from a release outside the browser window.
            if (!(event.buttons & 2)) { reset(); return; }
            stop(event);
            drag.point = point(event);
            if (Math.hypot(drag.point.x - drag.start.x, drag.point.y - drag.start.y) >= 6) {
                drag.moved = true;
            }
            network.redraw();
        });
        listen(window, "pointerup", function(event) {
            if (!drag || drag.pointerId !== event.pointerId || event.button !== 2) return;
            stop(event);
            var current = drag, p = point(event), target = nodeAt(p);
            current.moved = current.moved || Math.hypot(p.x - current.start.x, p.y - current.start.y) >= 6;
            reset();
            if (current.cancelled) return;
            if (current.moved) {
                if (current.from !== undefined && target !== undefined && target !== current.from) {
                    send("edge_drawn", {from: current.from, to: target, nonce: Math.random()});
                }
            } else if (inside(p)) {
                contextMenu(event, current.from);
            }
        });
        listen(canvas, "contextmenu", function(event) {
            // Browsers fire contextmenu on either press or release. Menus are
            // opened by pointerup only, after click and drag are distinguished.
            stop(event);
            if (event.button !== 2 && !drag) contextMenu(event, nodeAt(point(event)));
        });
        ["mousedown", "mouseup"].forEach(function(name) {
            listen(canvas, name, function(event) { if (event.button === 2) stop(event); });
        });
        listen(window, "keydown", function(event) {
            if (event.key === "Escape" && drag) {
                stop(event);
                drag.cancelled = true;
                network.redraw();
            }
        });
        listen(window, "pointercancel", function(event) {
            if (drag && drag.pointerId === event.pointerId) reset();
        });
        listen(window, "blur", reset);
        network.on("afterDrawing", draw);
        function cleanup() {
            drag = null;
            listeners.forEach(function(remove) { remove(); });
            network.off("afterDrawing", draw);
            network.off("destroy", cleanup);
            delete element.__alphaGraphEdgeCleanup;
        }
        network.on("destroy", cleanup);
        element.__alphaGraphEdgeCleanup = cleanup;
    };
})();

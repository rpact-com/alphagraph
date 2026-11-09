(function() {
    "use strict";

    function snap(network, grid, ids, persist, changedOnly) {
        var updates = [];
        ids.forEach(function(id) {
            var node = network.body.nodes[id];
            if (!node || !Number.isFinite(node.x) || !Number.isFinite(node.y)) return;
            var x = Math.round((node.x - grid.origin.x) / grid.size) * grid.size + grid.origin.x;
            var y = Math.round((node.y - grid.origin.y) / grid.size) * grid.size + grid.origin.y;
            var changed = node.x !== x || node.y !== y;
            // Change the actual center before edges and nodes are drawn. Updating
            // the DataSet during dragging would interfere with vis's drag state.
            node.x = x;
            node.y = y;
            if (!changedOnly || changed) updates.push({id: id, x: x, y: y});
        });
        if (persist && updates.length) network.body.data.nodes.update(updates);
        return updates;
    }

    function publish(inputId, updates) {
        if (updates.length && window.Shiny) {
            window.Shiny.setInputValue(inputId,
                {nodes: updates, nonce: Math.random()}, {priority: "event"});
        }
    }

    window.alphaGraphGrid = {
        beforeRedraw: function(network, grid, inputId) {
            var initial = !network.__alphaGraphGridInitialized;
            network.__alphaGraphGridInitialized = true;
            var updates = snap(network, grid, Object.keys(network.body.nodes), initial, true);
            // Persist initial corrections as well as completed drags, so another
            // graph edit cannot restore positions that are off the grid.
            if (initial) publish(inputId, updates);
        },
        dragEnd: function(network, grid, inputId, ids) {
            publish(inputId, snap(network, grid, ids, true, false));
        },
        draw: function(ctx, network, grid) {
            // beforeDrawing already has the same translation, scale and device
            // pixel ratio as the nodes. Invert it only to find the visible area.
            var t = ctx.getTransform();
            if (!(t.a > 0 && t.d > 0)) return;
            var left = -t.e / t.a, right = (ctx.canvas.width - t.e) / t.a;
            var top = -t.f / t.d, bottom = (ctx.canvas.height - t.f) / t.d;
            var firstX = Math.ceil((left - grid.origin.x) / grid.size);
            var lastX = Math.floor((right - grid.origin.x) / grid.size);
            var firstY = Math.ceil((top - grid.origin.y) / grid.size);
            var lastY = Math.floor((bottom - grid.origin.y) / grid.size);
            // At extreme zoom-out the grid would be indistinguishable anyway.
            if (lastX - firstX + lastY - firstY > 4000) return;
            ctx.save();
            ctx.strokeStyle = "#e0e0e0";
            ctx.lineWidth = 1 / network.getScale();
            ctx.setLineDash([]);
            ctx.beginPath();
            for (var i = firstX; i <= lastX; i++) {
                var x = grid.origin.x + i * grid.size;
                ctx.moveTo(x, top);
                ctx.lineTo(x, bottom);
            }
            for (var j = firstY; j <= lastY; j++) {
                var y = grid.origin.y + j * grid.size;
                ctx.moveTo(left, y);
                ctx.lineTo(right, y);
            }
            ctx.stroke();
            ctx.restore();
        }
    };
})();

(function() {
    "use strict";
    var drag = null;

    function resizePanel(handle, height) {
        var body = handle.parentElement;
        var graphElement = document.getElementById("graph" + handle.dataset.graphId);
        var network = graphElement && graphElement.chart;
        var position = network && network.getViewPosition();
        var scale = network && network.getScale();
        height = Math.max(300, Math.round(height));
        body.style.height = height + "px";
        handle.setAttribute("aria-valuenow", height);

        // Resize the existing canvas without rebuilding or fitting the graph.
        if (network) {
            network.setSize("100%", "100%");
            network.redraw();
            network.moveTo({ position: position, scale: scale, animation: false });
        }
    }

    var tableResizeFrame = null;

    function scheduleTableResize(row) {
        if (tableResizeFrame !== null) {
            window.cancelAnimationFrame(tableResizeFrame);
        }
        tableResizeFrame = window.requestAnimationFrame(function() {
            tableResizeFrame = null;
            row.querySelectorAll(".rhandsontable").forEach(function(element) {
                var instance = window.HTMLWidgets &&
                    window.HTMLWidgets.getInstance(element);
                // Handsontable sets an inline pixel width on its root. Measuring
                // that root would keep the old narrow width when the panel grows.
                var container = element.parentElement;
                var style = window.getComputedStyle(container);
                var width = Math.floor(container.clientWidth -
                    (parseFloat(style.paddingLeft) || 0) -
                    (parseFloat(style.paddingRight) || 0));
                if (instance && instance.hot && width > 0) {
                    // rhandsontable's widget resize callback is empty. Update the
                    // existing table after layout, without replacing its data.
                    instance.hot.updateSettings({ width: width });
                    instance.hot.render();
                }
            });
        });
    }
    function resizeColumns(handle, percent) {
        var row = handle.parentElement;
        var graphHandle = row.querySelector(".alpha-graph-resize-handle");
        var graphElement = graphHandle &&
            document.getElementById("graph" + graphHandle.dataset.graphId);
        var network = graphElement && graphElement.chart;
        var position = network && network.getViewPosition();
        var scale = network && network.getScale();
        percent = Math.max(25, Math.min(75, percent));
        row.style.setProperty("--alpha-left-width", percent + "%");
        handle.setAttribute("aria-valuenow", percent.toFixed(2));

        // Notify other widgets; Handsontable needs an explicit layout update.
        window.dispatchEvent(new Event("resize"));
        scheduleTableResize(row);
        if (network) {
            network.setSize("100%", "100%");
            network.redraw();
            network.moveTo({ position: position, scale: scale, animation: false });
        }
    }
    window.addEventListener("resize", function() {
        var row = document.querySelector(".alpha-design-split");
        if (row) {
            scheduleTableResize(row);
        }
    });

    // Delegation also supports modules inserted dynamically by Shiny.
    document.addEventListener("pointerdown", function(event) {
        var handle = event.target.closest(".alpha-graph-resize-handle, .alpha-design-splitter");
        if (!handle || event.button !== 0 || drag) {
            return;
        }
        event.preventDefault();
        handle.focus({ preventScroll: true });
        handle.setPointerCapture(event.pointerId);
        drag = {
            handle: handle,
            pointerId: event.pointerId,
            horizontal: handle.classList.contains("alpha-design-splitter"),
            x: event.clientX,
            percent: parseFloat(handle.getAttribute("aria-valuenow")),
            width: handle.parentElement.getBoundingClientRect().width,
            y: event.clientY,
            height: handle.parentElement.getBoundingClientRect().height
        };
    });

    document.addEventListener("pointermove", function(event) {
        if (drag && drag.pointerId === event.pointerId) {
            if (drag.horizontal) {
                if (window.matchMedia("(min-width: 768px)").matches && drag.width > 0) {
                    resizeColumns(drag.handle,
                        drag.percent + (event.clientX - drag.x) / drag.width * 100);
                }
            } else {
                resizePanel(drag.handle, drag.height + event.clientY - drag.y);
            }
        }
    });

    function finishResize(event) {
        if (drag && drag.pointerId === event.pointerId) {
            var handle = drag.handle;
            if (drag.horizontal) {
                scheduleTableResize(handle.parentElement);
            }
            drag = null;
            if (handle.hasPointerCapture(event.pointerId)) {
                handle.releasePointerCapture(event.pointerId);
            }
        }
    }
    document.addEventListener("pointerup", finishResize);
    document.addEventListener("pointercancel", finishResize);
    document.addEventListener("lostpointercapture", finishResize);

    document.addEventListener("keydown", function(event) {
        var handle = event.target.closest(".alpha-graph-resize-handle, .alpha-design-splitter");
        if (handle && handle.classList.contains("alpha-design-splitter")) {
            if (window.matchMedia("(min-width: 768px)").matches &&
                    (event.key === "ArrowLeft" || event.key === "ArrowRight")) {
                event.preventDefault();
                resizeColumns(handle, parseFloat(handle.getAttribute("aria-valuenow")) +
                    (event.key === "ArrowRight" ? 2 : -2));
            }
            return;
        }
        if (!handle || (event.key !== "ArrowUp" && event.key !== "ArrowDown")) {
            return;
        }
        event.preventDefault();
        var delta = event.key === "ArrowDown" ? 20 : -20;
        resizePanel(handle, handle.parentElement.getBoundingClientRect().height + delta);
    });

    Shiny.addCustomMessageHandler("alphaGraphZoom", function(message) {
        // visNetwork stores its Network instance on a child element whose id
        // consists of "graph" followed by the namespaced Shiny output id.
        var graphElement = document.getElementById("graph" + message.id);
        if (!graphElement || !graphElement.chart) {
            return;
        }

        var network = graphElement.chart;
        var factor = Number(message.factor);
        if (!Number.isFinite(factor) || factor <= 0) {
            return;
        }

        // Preserve the current center and change only the zoom scale.
        network.moveTo({
            position: network.getViewPosition(),
            scale: network.getScale() * factor,
            animation: {
                duration: 150,
                easingFunction: "easeInOutQuad"
            }
        });
    });
})();

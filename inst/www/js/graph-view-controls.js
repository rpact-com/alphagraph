(function() {
    "use strict";

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


Shiny.addCustomMessageHandler('toggleButtonClass', function(message) {
    // Remove active classes from inactive button
    $('#' + message.inactiveId).removeClass('btn-primary');
    $('#' + message.inactiveId).addClass('btn-outline-secondary');
    
    // Add active classes to active button
    $('#' + message.activeId).removeClass('btn-outline-secondary');
    $('#' + message.activeId).addClass('btn-primary');
});

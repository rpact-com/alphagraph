
var navbar_dark_skins = [
  'navbar-primary',
  'navbar-secondary',
  'navbar-info',
  'navbar-success',
  'navbar-danger',
  'navbar-indigo',
  'navbar-purple',
  'navbar-pink',
  'navbar-maroon',
  'navbar-fuchsia',
  'navbar-navy',
  'navbar-lightblue',
  'navbar-lime',
  'navbar-teal',
  'navbar-olive',
  'navbar-gray-dark',
  'navbar-gray'
];

var navbar_light_skins = [
  'navbar-warning',
  'navbar-white',
  'navbar-orange'
];

var navbar_all_colors = navbar_dark_skins.concat(navbar_light_skins);

Shiny.addCustomMessageHandler("fixSidebarTheme", function(message) {
  var $sidebar = $('.main-sidebar');
  if ($sidebar.length > 0) {
    // remove all old sidebar skins
    $sidebar.removeClass(function (index, className) {
      return (className.match(/(^|\s)sidebar-(dark|light)-\S+/g) || []).join(' ');
    });

    // set fixed sidebar skin
    $sidebar.addClass('sidebar-dark-secondary');
  }
});

Shiny.addCustomMessageHandler("fixNavbarTheme", function(message) { 
	var mode = message;
	if (typeof mode === "undefined") {
		mode = "light";
	}
  	if (mode == "dark") {
		return;
  	}
	
  	var $main_header = $('.main-header');
  	if ($main_header.length > 0) {
	
		// remove all old main_header skins
		$main_header.removeClass('navbar-dark').removeClass('navbar-light');
		navbar_all_colors.forEach(function (color) {
			$main_header.removeClass(color);
		});
		
		// set fixed main_header skin
		$main_header.addClass('navbar-gray');
		$main_header.addClass('navbar-dark');
  	}
});

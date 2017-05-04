(function($) {
    $(document).ready(function() {
	
	$('#simple_plot').scianimator({
	    'images': ['images/simple.plot1.png', 'images/simple.plot2.png', 'images/simple.plot3.png', 'images/simple.plot4.png', 'images/simple.plot5.png'],
	    'width': 480,
	    'delay': 1500,
	    'loopMode': 'loop'
	});
	$('#simple_plot').scianimator('play');
    });
})(jQuery);

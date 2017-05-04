(function($) {
    $(document).ready(function() {
	
	$('#RanWalk_plot').scianimator({
	    'images': ['images/RanWalk.plot1.png', 'images/RanWalk.plot2.png', 'images/RanWalk.plot3.png', 'images/RanWalk.plot4.png', 'images/RanWalk.plot5.png', 'images/RanWalk.plot6.png', 'images/RanWalk.plot7.png', 'images/RanWalk.plot8.png', 'images/RanWalk.plot9.png', 'images/RanWalk.plot10.png'],
	    'width': 480,
	    'delay': 1500,
	    'loopMode': 'loop'
	});
	$('#RanWalk_plot').scianimator('play');
    });
})(jQuery);

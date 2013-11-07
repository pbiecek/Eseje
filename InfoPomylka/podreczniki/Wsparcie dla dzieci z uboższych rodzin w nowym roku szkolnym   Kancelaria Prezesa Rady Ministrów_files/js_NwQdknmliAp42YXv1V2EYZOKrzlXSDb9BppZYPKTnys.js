jQuery(document).ready(function() {
	if(jQuery('body').is('.node-type-images') || jQuery('body').is('.node-type-event')){
		jQuery('#media-scrollable-horizontal img:first').addClass('active');
		
		jQuery('#media-scrollable-horizontal img').click(function(){
			var thumbnail = jQuery(this).attr('src');
			thumbnail = thumbnail.replace('min190x111','min797x444');
			jQuery('.main-media img').attr('src',thumbnail);
			jQuery('#media-scrollable-horizontal img').removeClass('active');
			jQuery(this).addClass('active');
			var cloned = jQuery('#media-scrollable-horizontal .cloned:first .col').size();
			var index = jQuery('#media-scrollable-horizontal img').index(this);
			jQuery('.counter span').text(index-cloned+1);
		});
	}

	jQuery('.filter-select').change(function() {
		var url = jQuery(this).prop('value');
		//alert(url);
		window.location.href=url;
	});
	
	
	var input = jQuery('#jsTopSearch');
	var defaultTxt = jQuery('#jsTopSearch').attr('value');

	input.on('focus', function() {
		if ( input.attr('value') == defaultTxt ) {
			input.attr('value', '');
		}
	})

	input.on('blur', function() {
		if ( input.attr('value') == '' ) {
			input.attr('value', defaultTxt);
		}
	})

});;

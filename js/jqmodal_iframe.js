var loadInIframeModal = function(hash)
{
	var $trigger = $(hash.t);
	var $modal = $(hash.w);
	var $modalContent = $("iframe", $modal);
	var myUrl = $trigger.attr('href');
	var myTitle= $trigger.html();

	//let's use the anchor "title" attribute as modal window title
	$('.jqmTitle h1', $modal).html( myTitle+'<a href="'+myUrl+'"> open in browser</a>' );

	//Prepare animations
	$modalContent.html('').attr('src', "");
	$modal.jqmShow().css("opacity", "0").css("display", "block");
	$modalContent.hide();

	$($modal)
		.css("top","0")
		.animate({ top: '5%', opacity: 1 },"slow", function()
		{
			$modalContent.show();
			$modalContent.html('').attr('src', myUrl);
		});
}

var animateModal = function(hash)
{
	var $modal = $(hash.w);

	//Prepare animations
	$modal.jqmShow().css("opacity", "0").css("display", "block");

	$($modal)
		.css("bottom","0")
		.animate({ bottom: '10%', opacity: 1 },"slow");
}
/**
 * JS: Global functions
 * @author Darío Ruellan <druellan@netverk.com.ar>
 */

$(function()
{

// ** Alguno' efetoCs ***********************************

	// Inicializamos modal con iFrames
	$('#modalWindow').jqm({
		modal: true,
		trigger: 'a.external',
		target: '#jqmContent',
		onShow:  loadInIframeModal
	});

	// Inicializamos Copyright
	$('#copyright').jqm({
		modal: true,
		trigger: 'a.copyright',
		onShow: animateModal
	});

	// Inicializamos Screenshots
	$('#screenshots').jqm({
		modal: true,
		trigger: 'a.screenshot',
		onShow: makeScreenshot
	});
	
	// Movemos los cartelitos sociales
	$("#header .social a").hover(function()
	{
		$(this).animate( {top: 0, marginRight: 4}, "fast" );
	}, function()
	{
		$(this).animate( {top: -10, marginRight: 0} );
	});

});

function makeScreenshot(e)
{
	console.log(e);
	var os = $(e.t).attr("rel");
	var osName = $("span", e.t).html();

	$('#screenshot').attr("alt", "hPage on "+osName);
	$('#screenshot').attr("src", "images/screenshots/hpage-0.5.2-"+os+".png");
	$('#screenshot').hover(function()
	{
		$(this).attr("src", "images/screenshots/hpage-0.5.2-"+os+"-explained.png");
	}, function()
	{
		$(this).attr("src", "images/screenshots/hpage-0.5.2-"+os+".png");
	});
	
	animateModal(e);
}
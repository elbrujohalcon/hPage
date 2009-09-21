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

	// Movemos los cartelitos sociales
	$("#header .social a").hover(function()
	{
		$(this).animate( {top: 0, marginRight: 4}, "fast" );
	}, function()
	{
		$(this).animate( {top: -10, marginRight: 0} );
	});

});
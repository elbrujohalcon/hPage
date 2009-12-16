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
		trigger: 'a.screenshots',
		onShow: animateModal
	});
	$('#screenshot')[0].alt = "hPage on OSX";
	$('#screenshot')[0].src = "images/screenshots/hpage-0.5.2-OSX.png"
	$('#screenshot').hover(function()
	{
		$(this)[0].src = "images/screenshots/hpage-0.5.2-OSX-explained.png"
	}, function()
	{
		$(this)[0].src = "images/screenshots/hpage-0.5.2-OSX.png"
	});
	$('#screenshotOSX').click(function()
	{
		$('#screenshot')[0].alt = "hPage on OSX";
		$('#screenshot')[0].src = "images/screenshots/hpage-0.5.2-OSX.png"
		$('#screenshot').hover(function()
		{
			$(this)[0].src = "images/screenshots/hpage-0.5.2-OSX-explained.png"
		}, function()
		{
			$(this)[0].src = "images/screenshots/hpage-0.5.2-OSX.png"
		});
	});
	$('#screenshotWindows').click(function()
	{
		$('#screenshot')[0].alt = "hPage on Windows";
		$('#screenshot')[0].src = "images/screenshots/hpage-0.5.2-Windows.png"
		$('#screenshot').hover(function()
		{
			$(this)[0].src = "images/screenshots/hpage-0.5.2-Windows-explained.png"
		}, function()
		{
			$(this)[0].src = "images/screenshots/hpage-0.5.2-Windows.png"
		});
	});
	$('#screenshotLinux').click(function()
	{
		$('#screenshot')[0].alt = "hPage on Linux";
		$('#screenshot')[0].src = "images/screenshots/hpage-0.5.2-Linux.png"
		$('#screenshot').hover(function()
		{
			$(this)[0].src = "images/screenshots/hpage-0.5.2-Linux-explained.png"
		}, function()
		{
			$(this)[0].src = "images/screenshots/hpage-0.5.2-Linux.png"
		});
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
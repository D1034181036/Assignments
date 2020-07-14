$(function() {
	$('#auto_autocomplete').autocomplete({
	source: "./get_title.php",
	minLength: 1
	});
});
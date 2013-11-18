function runExtraction(plan) {
	$.ajax({
		type: "POST",
		url: '/extraction',
		data: JSON.stringify(plan),
		success: function(result) {
			$('#running').append("Running Extraction " + result.id);
		},
		dataType: "json",
		contentType: "application/json",
		processData: false
	});
}

$(document).ready(function() {
    $('#runPlan').click(function(e) {
    	var plan = {
    		inputDir: $('#inputDir').val(),
    		outputDir: $('#outputDir').val(),
    		features: $('#features').val()
    	};

		runExtraction(plan);    	
    });
});

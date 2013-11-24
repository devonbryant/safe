function runExtraction(plan) {
	$.ajax({
		type: "POST",
		url: "/extraction",
		data: JSON.stringify(plan),
		success: addStatus,
		dataType: "json",
		contentType: "application/json",
		processData: false
	});
}

function getStatus(id) {
	$.get("/status/" + id, updateStatus);
}

// Compile the various handlebar templates
var statusTemplate = Handlebars.compile($('#status-template').html());
var progressTemplate = Handlebars.compile($('#progress-template').html());
var successTemplate = Handlebars.compile($('#success-template').html());
var failureTemplate = Handlebars.compile($('#failure-template').html());

function addStatus(status) {
	$('#running').append(statusTemplate(status));
	updateStatus(status);
}

function updateStatus(status) {
	var template = null;
	switch (status.type) {
		case "CompletedStatus":
			template = progressTemplate;
			status.percentComplete = (status.completed / status.total) * 100; // Add % complete

			// We're not finished w/ this plan, query again in 10 s
			setTimeout(function() { getStatus(status.id); }, 10000);
			break;
		case "FinishedStatus":
			template = successTemplate;
			status.extractionTime = status.extractionTime / 1000; // Convert to seconds
			break;
		case "FailedStatus":
			template = failureTemplate;
			break;
	}

	if (template != null) $('#' + status.id).html(template(status));
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

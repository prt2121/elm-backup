//text output -- it no html or graphical output
var fileupload = Elm.worker(Elm.FileUpload, {
	openFromFile: ""
  	});

var upload = document.getElementsByTagName('input')[0]

upload.onchange = function (e) {
	reader = new FileReader();

	reader.onload = function (event) {
		data = event.target.result;
		//file's text data is sent to 'openfromfile' port
		fileupload.ports.openFromFile.send(data);
		}
	reader.readAsText(upload.files[0]);
	};

function logger(x) { console.log(x) }

//data from 'output' port is sent to logger function
fileupload.ports.output.subscribe(logger);

default:
	elmstatic

serve:
	http-server _site

watch:
	chokidar . -d 1000 -s -i "elm.js" -i "elm-stuff/**/*" -i ".git/" -i "_site/**/*" -c elmstatic --initial

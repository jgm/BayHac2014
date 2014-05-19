ALL=slides.html fuel.hs.html cng_fuel_chicago.json.html exercises.pdf

.PHONY: all clean server web upload

all: $(ALL)

%.hs.html: %.hs
	Highlight $< > $@

%.json.html: %.json
	Highlight $< > $@

exercises.pdf : exercises.txt
	pandoc $^ -o $@ -Vgeometry="margin=1in" --parse-raw

reveal.js:
	git clone https://github.com/hakimel/reveal.js

slides.html : slides.txt reveal.js
	pandoc $< -o $@ -t revealjs --css slides.css -S --highlight-style=espresso

# for speaker notes:
server:
	python -m SimpleHTTPServer

clean:
	-rm $(ALL)
	-rm -r BayHac2014

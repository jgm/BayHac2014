ALL=slides.html fuel.hs.html cng_fuel_chicago.json.html exercises.pdf

.PHONY: all clean server

all: $(ALL)

%.hs.html: %.hs
	Highlight $< > $@

%.json.html: %.json
	Highlight $< > $@

exercises.pdf : exercises.txt
	pandoc $^ -o $@ -Vgeometry="margin=1in" -Vmainfont='Bookman Old Style' --parse-raw --latex-engine=xelatex

slides.html : slides.txt
	pandoc $^ -o $@ -t revealjs --css slides.css -S $(SELFCONTAINED) --highlight-style=espresso

server:
	python -m SimpleHTTPServer
clean:
	rm $(ALL)

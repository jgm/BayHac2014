ALL=slides.html fuel.hs.html cng_fuel_chicago.json.html

.PHONY: all clean server

all: $(ALL)

%.hs.html: %.hs
	Highlight $< > $@

%.json.html: %.json
	Highlight $< > $@

slides.html : slides.txt
	pandoc $< -o $@ -t revealjs --css slides.css -S $(SELFCONTAINED) --highlight-style=espresso

server:
	python -m SimpleHTTPServer
clean:
	rm $(ALL)

ALL=slides.html

.PHONY: all clean server

all: $(ALL)

slides.html : slides.txt
	pandoc $< -o $@ -t revealjs --css slides.css -S $(SELFCONTAINED) --highlight-style=espresso

server:
	python -m SimpleHTTPServer
clean:
	rm $(ALL)

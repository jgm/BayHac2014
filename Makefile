ALL=slides.html

.PHONY: all clean

all: $(ALL)

%.html : %.txt
	pandoc $< -o $@ -t revealjs --css slides.css -S

clean:
	rm $(ALL)

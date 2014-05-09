ALL=slides.html

.PHONY: all clean

all: $(ALL)

slides.html : slides.txt
	pandoc $< -o $@ -t revealjs --css slides.css -S $(SELFCONTAINED)

clean:
	rm $(ALL)

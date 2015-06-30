default: $(subst svg,bmp,$(subst :,\:,$(wildcard *.svg)))

%.png: %.svg
	inkscape $< --export-png=$@ --export-width=500 --export-height=500

%.bmp: %.png
	convert $< $@

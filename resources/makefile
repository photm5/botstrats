default: $(subst svg,png,$(subst :,\:,$(wildcard *.svg)))

%.png: %.svg
	inkscape $< --export-png=$@

PROJECT=note
SOURCE=note.asm
SONG=sourceflux.prg
TARGET=$(PROJECT).prg
TARGETSYM=$(PROJECT).sym
TARGETMON=$(PROJECT)-mon.txt
EMU=x64
EMUSC=x64sc
EMUFLAGS=
RM=rm
RMFLAGS=-f
LABELMON=labelmon
LABELMONDIR=../../../tools/$(LABELMON)
LABELMONFLAGS=--release

all: $(TARGET) $(TARGETMON)

$(TARGET) $(TARGETSYM): $(SOURCE) $(SONG)
	kickass -showmem -symbolfile $(SOURCE)

test: $(TARGET) $(TARGETMON)
	$(EMU) $(EMUFLAGS) -moncommands $(TARGETMON) $(TARGET)

testsc: $(TARGET) $(TARGETMON)
	$(EMUSC) $(EMUFLAGS) -moncommands $(TARGETMON) $(TARGET)

$(TARGETMON): $(TARGETSYM)
	cd $(LABELMONDIR) && cargo run $(LABELMONFLAGS) $(CURDIR)/$(TARGETSYM) $(CURDIR)/$(TARGETMON)

clean:
	$(RM) $(RMFLAGS) $(TARGET) $(TARGETSYM) $(TARGETMON) && cd $(LABELMONDIR) && cargo clean
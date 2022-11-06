TARGET = winter
BUILD = release
CFLAGS = -Ilib/glad/include -Ilib/cb/include -Wall -pedantic
LFLAGS = -lm -lzip -lpng -lcglm -lglfw -lassimp

ifeq ($(BUILD),debug)
CFLAGS += -g -O0 -DCB_LOG_VERBOSITY=CB_LOG_DBUG
# CFLAGS += -g -Og -DCB_LOG_VERBOSITY=CB_LOG_INFO
else
CFLAGS += -O2 -DCB_LOG_VERBOSITY=CB_LOG_WARN -DCB_NO_PROF -DCB_NO_ASSERT_DETAIL -Wno-unused-value
endif

define SRC
src/main.c
src/player.c
src/robin.c
lib/cb/src/cb.c
lib/cb/src/cbi.c
lib/cb/src/core/wad.c
lib/cb/src/core/window.c
lib/cb/src/data/model.c
lib/cb/src/data/texture.c
lib/cb/src/data/shader.c
lib/cb/src/gfx/entity.c
lib/cb/src/gfx/skybox.c
lib/cb/src/gfx/transform.c
lib/cb/src/util/alloc.c
lib/cb/src/util/assert.c
lib/cb/src/util/log.c
lib/cb/src/util/prof.c
lib/glad/src/gl.c
endef

define WAD
res/plane.obj
res/box.obj
res/box.png
res/robin/body.obj
res/robin/left_leg.obj
res/robin/right_leg.obj
res/robin/left_wing.obj
res/robin/right_wing.obj
res/robin/tail.obj
res/robin/body.png
res/robin/left_leg.png
res/robin/right_leg.png
res/robin/left_wing.png
res/robin/right_wing.png
res/robin/tail.png
res/basic.frag.glsl
res/basic.vert.glsl
res/light.frag.glsl
res/light.vert.glsl
endef

.PHONY: all run clean

all: bin/$(BUILD)/$(TARGET) bin/$(BUILD)/data.wad

run: all
	./bin/$(BUILD)/$(TARGET)

clean:
	$(RM) -r obj bin

bin/$(BUILD)/data.wad: $(WAD:%=%) Makefile
	@mkdir -p $(@D)
	zip $@ $(WAD:%=%)

bin/$(BUILD)/$(TARGET): $(SRC:%.c=obj/$(BUILD)/%.o)
	@mkdir -p $(@D)
	$(CC) $^ $(LFLAGS) -o $@

obj/$(BUILD)/%.o: %.c Makefile
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

-include $(SRC:%.c=obj/$(BUILD)/%.d)

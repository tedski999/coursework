TARGET = winter_wonderland
BUILD = release
CFLAGS = -Ilib/glad/include -Ilib/cglm/include -Wall -pedantic
LFLAGS = -lm -lzip -lpng -lcglm -lglfw -lassimp

ifeq ($(BUILD),debug)
CFLAGS += -g -O0 -DWW_NO_PROF
else
CFLAGS += -O2 -DWW_NO_PROF -DWW_NO_ASSERT_DETAIL
endif

define SRC
src/alloc.c
src/assert.c
src/entity.c
src/log.c
src/main.c
src/model.c
src/prof.c
src/shader.c
src/texture.c
src/wad.c
src/window.c
src/application/main.c
src/application/ground.c
src/application/skybox.c
src/application/present.c
src/application/snow.c
src/application/snowman.c
src/application/tree.c
src/application/candycane.c
lib/glad/src/gl.c
endef

define WAD
res/ground.vert.glsl
res/ground.frag.glsl
res/skybox.glb
res/skybox.png
res/skybox.vert.glsl
res/skybox.frag.glsl
res/present.glb
res/present.png
res/present.vert.glsl
res/present.frag.glsl
res/snow.vert.glsl
res/snow.frag.glsl
res/snowman.glb
res/snowman.vert.glsl
res/snowman.frag.glsl
res/tree.glb
res/tree.png
res/tree.vert.glsl
res/tree.frag.glsl
res/candycane.glb
res/candycane.png
res/candycane.vert.glsl
res/candycane.frag.glsl
endef

.PHONY: all clean

all: bin/$(BUILD)/$(TARGET) bin/$(BUILD)/data.wad

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

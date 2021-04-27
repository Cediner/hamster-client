package hamster.gob.sprites;

import hamster.gfx.SimpleTextMesh;
import haven.Clickable;
import haven.Sprite;
import haven.render.BaseColor;
import haven.render.Pipe;
import haven.render.RenderTree;

import java.awt.*;

public class SimpleTextSprite extends Sprite implements Sprite.CDel {
    private final SimpleTextMesh mesh;
    private boolean alive = true;

    public SimpleTextSprite(final Owner owner, final String name) {
	super(owner, null);
	mesh = SimpleTextMesh.get(name);
    }

    @Override
    public void added(RenderTree.Slot slot) {
	slot.add(mesh.texture.draw.apply(mesh), Pipe.Op.compose(new BaseColor(Color.WHITE), Clickable.No));
    }

    @Override
    public boolean tick(double dt) {
	return !alive;
    }

    @Override
    public void delete() {
	alive = false;
    }
}

package hamster.gob.sprites;

import hamster.gfx.GrowthMesh;
import haven.Gob;
import haven.Sprite;
import haven.render.RenderTree;

public class GrowthSprite extends Sprite {
    public static final int id = -40914912;
    private final GrowthMesh mesh;
    private boolean alive = true;

    public GrowthSprite(final Gob owner, final int stage, final int min, final int max) {
        super(owner, null);
        mesh = GrowthMesh.mesh(stage, min, max);
    }

    @Override
    public void added(RenderTree.Slot slot) {
        super.added(slot);
        slot.add(mesh);
    }

    public void rem() {
        alive = false;
    }

    @Override
    public boolean tick(double dt) {
        return !alive;
    }
}

package hamster.gob.sprites;

import hamster.gfx.GobDirMesh;
import haven.Gob;
import haven.Sprite;
import haven.render.RenderTree;

public class HaloSprite extends Sprite {
    public static final int id = -40914912;
    private static final GobDirMesh mesh = GobDirMesh.getmesh();
    private boolean alive = true;

    public HaloSprite(final Gob owner) {
        super(owner, null);
    }

    @Override
    public void added(RenderTree.Slot slot) {
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

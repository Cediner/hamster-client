package hamster.gob.sprites;

import hamster.gfx.MyGobIndicatorMesh;
import haven.Gob;
import haven.Sprite;
import haven.render.RenderTree;

public class MyGobIndicatorSprite extends Sprite {
    public static final int id = -942194129;
    private static final MyGobIndicatorMesh mesh = MyGobIndicatorMesh.getmesh();
    private boolean alive = true;

    public MyGobIndicatorSprite(final Gob g) {
        super(g, null);
    }

    @Override
    public void added(RenderTree.Slot slot) {
        slot.add(mesh, MyGobIndicatorMesh.circlecol);
    }

    public void rem() {
        alive = false;
    }

    @Override
    public boolean tick(double dt) {
        return !alive;
    }
}

package hamster.gob.sprites;

import hamster.script.pathfinding.Hitbox;
import haven.FastMesh;
import haven.Gob;
import haven.Sprite;
import haven.UI;
import haven.render.RenderTree;

public class HitboxSprite extends Sprite {
    public static final int id = -412412491;
    private final FastMesh mesh;
    private boolean alive;

    public HitboxSprite(final Gob g) {
        super(g, null);
        final Hitbox hb = g.hitboxo().orElse(Hitbox.hbfor(g));
        if(hb != null) {
            mesh = hb.mesh();
        } else {
            mesh = null;
        }
        alive = true;
    }

    public void rem() {
        alive = false;
    }

    @Override
    public void added(RenderTree.Slot slot) {
        final UI ui = ((Gob)owner).glob.ui.get();
        if(mesh != null && ui != null && ui.gui != null) {
            slot.add(mesh, ui.gui.settings.GOBHITBOXCOL.get());
        }
    }

    @Override
    public boolean tick(double dt) {
        return !alive;
    }
}

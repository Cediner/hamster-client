package hamster.gob.sprites;

import hamster.gfx.ColoredCircleMesh;
import haven.Gob;
import haven.OCache;
import haven.RUtils;
import haven.Sprite;
import haven.render.BaseColor;
import haven.render.RenderTree;

import java.awt.*;

public class FriendlyMark extends Sprite {
    public static final int id = -42141286;

    private ColoredCircleMesh mesh;
    private Color col;
    private boolean alive = true;

    public FriendlyMark(final Gob g, final Color col) {
        super(g, null);
        this.col = col;
        this.mesh = ColoredCircleMesh.getmesh(col);
    }

    public void rem() {
        alive = false;
    }

    @Override
    public boolean tick(double ddt) {
        return !alive;
    }

    public void update(final Color col) {
        if(!this.col.equals(col)) {
            this.col = col;
            this.mesh = ColoredCircleMesh.getmesh(col);
            ((Gob) owner).glob.oc.mailbox.mail(new OCache.RefreshGobByObject((Gob) owner));
        }
    }

    @Override
    public void added(RenderTree.Slot slot) {
        super.added(slot);
        slot.add(mesh);
    }
}

package hamster.gob.sprites;

import hamster.gfx.ColoredCircleMesh;
import haven.*;
import haven.render.BaseColor;
import haven.render.RenderTree;

import java.awt.*;

/**
 * Color mod for Gobs we have Aggro'd and Sprite Model to display the floating pointer above them
 */
public class AggroMark extends Sprite {
    private static final BaseColor col = new BaseColor(Color.RED);
    public static final int id = -4214129;

    private final ColoredCircleMesh mesh;
    private boolean alive = true;

    public AggroMark(final Gob g) {
        super(g, null);
        this.mesh = ColoredCircleMesh.getmesh(Color.RED);
    }

    public void rem() {
        alive = false;
    }

    @Override
    public boolean tick(double ddt) {
        return !alive;
    }

    @Override
    public void added(RenderTree.Slot slot) {
        super.added(slot);
        slot.add(mesh, col);
    }
}

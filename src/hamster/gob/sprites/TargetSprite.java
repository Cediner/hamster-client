package hamster.gob.sprites;

import hamster.gfx.ObstMesh;
import hamster.script.pathfinding.Obst;
import haven.Coord2d;
import haven.Gob;
import haven.Sprite;
import haven.render.BaseColor;
import haven.render.RenderTree;

import java.awt.*;
import java.util.regex.Pattern;

public class TargetSprite extends Sprite {
    public static final int id = -59129421;
    public static final String target_pat = "$Target{%d}";
    public static final Pattern TARGET_PATTERN = Pattern.compile("\\$Target\\{([0-9]+)}");
    private static final ObstMesh mesh;
    private static final BaseColor col = new BaseColor(Color.RED);
    private boolean alive = true;

    static {
        final Coord2d[][] shapes = new Coord2d[2][4];
        final Coord2d offset = new Coord2d(10, 0);
        {
            shapes[0][0] = offset.rotate(Math.toRadians(35));
            shapes[0][1] = offset.rotate(Math.toRadians(180 + 35));
            shapes[0][2] = offset.rotate(Math.toRadians(55));
            shapes[0][3] = offset.rotate(Math.toRadians(180 + 55));

            shapes[1][0] = offset.rotate(Math.toRadians(125));
            shapes[1][1] = offset.rotate(Math.toRadians(180 + 125));
            shapes[1][2] = offset.rotate(Math.toRadians(145));
            shapes[1][3] = offset.rotate(Math.toRadians(180 + 145));
        }
        mesh = Obst.makeMesh(shapes, Color.RED, 3);
    }

    public TargetSprite(final Gob g) {
        super(g, null);
    }

    @Override
    public void added(RenderTree.Slot slot) {
        super.added(slot);
        slot.add(mesh, col);
    }

    @Override
    public boolean tick(double dt) {
        return !alive;
    }

    public void rem() {
        alive = false;
    }

    @Override
    public String toString() {
        return "TargetSprite[" +
                "alive=" + alive +
                ']';
    }
}

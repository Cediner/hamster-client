package hamster.gob.sprites;

import hamster.gfx.ObstMesh;
import hamster.script.pathfinding.Obst;
import haven.Coord2d;
import haven.Gob;
import haven.Sprite;
import haven.render.BaseColor;
import haven.render.RenderTree;

import java.awt.*;

public class CurAggroSprite extends Sprite {
    public static final int id = -59129521;
    private static final ObstMesh mesh;
    private static final BaseColor col = new BaseColor(Color.ORANGE);

    static {
	final Coord2d[][] shapes = new Coord2d[4][4];
	final Coord2d offset = new Coord2d(10, 0);
	{
	    shapes[0][0] = offset.rotate(Math.toRadians(35));
	    shapes[0][1] = offset.rotate(Math.toRadians(180 + 55));
	    shapes[0][2] = offset.rotate(Math.toRadians(180 + 35));
	    shapes[0][3] = offset.rotate(Math.toRadians(55));

	    shapes[1][0] = offset.rotate(Math.toRadians(125));
	    shapes[1][1] = offset.rotate(Math.toRadians(180 + 145));
	    shapes[1][2] = offset.rotate(Math.toRadians(180 + 125));
	    shapes[1][3] = offset.rotate(Math.toRadians(145));

	    shapes[2][0] = offset.rotate(Math.toRadians(-10));
	    shapes[2][1] = offset.rotate(Math.toRadians(180 + 10));
	    shapes[2][2] = offset.rotate(Math.toRadians(180 - 10));
	    shapes[2][3] = offset.rotate(Math.toRadians(10));

	    shapes[3][0] = offset.rotate(Math.toRadians(80));
	    shapes[3][1] = offset.rotate(Math.toRadians(180 + 100));
	    shapes[3][2] = offset.rotate(Math.toRadians(180 + 80));
	    shapes[3][3] = offset.rotate(Math.toRadians(100));
	}
	mesh = Obst.makeMesh(shapes, col.color(), 2);
    }

    public CurAggroSprite(final Gob g) {
	super(g, null);
    }

    @Override
    public void added(RenderTree.Slot slot) {
	super.added(slot);
	slot.add(mesh, col);
    }

    @Override
    public String toString() {
	return "CurAggroSprite";
    }
}

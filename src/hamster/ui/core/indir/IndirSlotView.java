package hamster.ui.core.indir;

import hamster.ui.core.MovableWidget;
import haven.*;

/**
 * Smaller widget for viewing equiped items
 */
public class IndirSlotView extends MovableWidget implements DTarget {
    private static final Tex invsq = Resource.loadtex("gfx/hud/invsq");
    private static final Coord spacer = UI.scale(new Coord(1,1));
    private static final Coord sqsz = UI.scale(new Coord(33, 33)).add(spacer);
    public final Coord isz;
    private final int[][] mapping;

    public IndirSlotView(final Coord isz, final String name, final int[][] mapping) {
        super(sqsz.mul(isz), name);
        this.isz = isz;
        this.mapping = mapping;
    }

    public void draw(GOut g) {
        Coord c = new Coord();
        for (c.y = 0; c.y < isz.y; c.y++) {
            for (c.x = 0; c.x < isz.x; c.x++) {
                g.image(invsq, c.mul(sqsz));
            }
        }
        super.draw(g);
    }

    public void additm(final WItem itm, final Coord slot) {
        add(new IndirWidget(itm), slot.mul(sqsz));
    }

    public void remitm(final WItem itm) {
        for (Widget wdg = lchild; wdg != null; wdg = wdg.prev) {
            if (wdg instanceof IndirWidget && ((IndirWidget) wdg).backer().equals(itm)) {
                wdg.unlink();
                break;
            }
        }
    }

    public boolean drop(Coord cc, Coord ul) {
        final Coord slot = cc.div(sqsz);
        if (slot.y >= 0 && slot.y < mapping.length &&
                slot.x >= 0 && slot.x < mapping[slot.y].length) {
            ui.gui.equ.wdgmsg("drop", mapping[slot.y][slot.x]);
            return (true);
        } else {
            return false;
        }
    }

    @Override
    public boolean iteminteract(Coord cc, Coord ul) {
        return false;
    }

    @Override
    protected boolean moveHit(Coord c, int btn) {
        return btn == 3;
    }
}

package hamster.ui.core.layout;

import haven.Coord;
import haven.Text;
import haven.Widget;

public class LinearGrouping extends Grouping {
    public enum Direction {
        VERTICAL, HORIZONTAL
    }

    private final Coord spacer;
    private final Direction dir;


    public LinearGrouping(final String cap, final Text.Foundry fnd, final Coord spacer,
                          final boolean box, final Direction dir) {
        super(cap, box, fnd);
        this.spacer = spacer;
        this.dir = dir;
    }

    public LinearGrouping(final String cap, final Coord spacer, final boolean box, final Direction dir) {
        super(cap, box);
        this.spacer = spacer;
        this.dir = dir;
    }

    public LinearGrouping(final String cap, final Coord spacer, final boolean box) {
        this(cap, spacer, box, Direction.VERTICAL);
    }

    public LinearGrouping(final String cap, final int spacer) {
        this(cap, new Coord(0, spacer), true, Direction.VERTICAL);
    }

    public LinearGrouping(final String cap, final int spacer, final Direction dir) {
        this(cap, new Coord(dir == Direction.VERTICAL ? 0 : spacer, dir == Direction.VERTICAL ? spacer: 0),
                true, dir);
    }

    public LinearGrouping(final int spacer, final boolean box) {
        this(null, new Coord(0, spacer), box, Direction.VERTICAL);
    }

    public LinearGrouping(final int spacer, final boolean box, final Direction dir) {
        this(null, new Coord(dir == Direction.VERTICAL ? 0 : spacer, dir == Direction.VERTICAL ? spacer: 0),
                box, dir);
    }

    public LinearGrouping(final Coord spacer, final boolean box, final Direction dir) {
        this(null, spacer, box, dir);
    }


    public LinearGrouping(final int spacer) {
        this(spacer, true);
    }

    public LinearGrouping(final int spacer, final Direction dir) {
        this(null, new Coord(dir == Direction.VERTICAL ? 0 : spacer, dir == Direction.VERTICAL ? spacer: 0),
                true, dir);
    }

    @Override
    public void pack() {
        switch (dir) {
            case VERTICAL -> {
                int y = 0;
                for (Widget wdg = child; wdg != null; wdg = wdg.next) {
                    if (wdg.visible) {
                        wdg.c = new Coord(spacer.x, y);
                        y += wdg.sz.y + spacer.y;
                    }
                }
            }
            case HORIZONTAL -> {
                int x = 0;
                for (Widget wdg = child; wdg != null; wdg = wdg.next) {
                    if (wdg.visible) {
                        wdg.c = new Coord(x, spacer.y);
                        x += wdg.sz.x + spacer.x;
                    }
                }
            }
        }
        super.pack();
    }
}

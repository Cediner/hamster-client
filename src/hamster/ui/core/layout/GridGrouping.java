package hamster.ui.core.layout;

import haven.Coord;
import haven.Widget;

public class GridGrouping extends Grouping {
    public enum Direction {
        VERTICAL, HORIZONTAL
    }

    private final Direction dir;
    private final Coord spacer;
    private final int indent;
    private final int max;

    public GridGrouping(final String cap, final Coord spacer,
                        final int indent, final int max,
                        final boolean box, final Direction dir) {
        super(cap, box);
        this.spacer = spacer;
        this.indent = indent;
        this.max = max;
        this.dir = dir;
    }

    public GridGrouping(final String cap, final Coord spacer, final int indent, final int maxy, final boolean box) {
        this(cap, spacer, indent, maxy, box, Direction.VERTICAL);
    }

    public GridGrouping(final String cap, final int spacer, final int maxy) {
        this(cap, new Coord(spacer, spacer), 0, maxy, false, Direction.VERTICAL);
    }

    @Override
    public void pack() {
        if(dir == Direction.VERTICAL) {
            int y = 0;
            int x = spacer.x;
            int stepx = 0;

            for (Widget wdg = child; wdg != null; wdg = wdg.next) {
                if (y + wdg.sz.y + spacer.y > max) {
                    y = 0;
                    x += stepx + spacer.x;
                    stepx = 0;
                }
                stepx = Math.max(stepx, wdg.sz.x);
                wdg.c = new Coord(x, y);
                y += wdg.sz.y + spacer.y;
            }
        } else {
            int y = spacer.y;
            int x = 0;
            int stepy = 0;

            for (Widget wdg = child; wdg != null; wdg = wdg.next) {
                if (x + wdg.sz.x + spacer.x > max) {
                    x = 0;
                    y += stepy + spacer.y;
                    stepy = 0;
                }
                stepy = Math.max(stepy, wdg.sz.y);
                wdg.c = new Coord(x, y);
                x += wdg.sz.x + spacer.x;
            }
        }

        super.pack();
    }
}

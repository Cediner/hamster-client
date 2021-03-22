package hamster.ui.core.layout;

import haven.Coord;
import haven.Widget;

public class GridGrouping extends Grouping {
    private final Coord spacer;
    private final int indent;
    private final int maxy;

    public GridGrouping(final String cap, final Coord spacer, final int indent, final int maxy, final boolean box) {
        super(cap, box);
        this.spacer = spacer;
        this.indent = indent;
        this.maxy = maxy;
    }

    public GridGrouping(final String cap, final int spacer, final int maxy) {
        this(cap, new Coord(spacer, spacer), 0, maxy, false);
    }

    @Override
    public void pack() {
        int y = 0;
        int x = spacer.x;
        int stepx = 0;

        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            if (y + wdg.sz.y + spacer.y > maxy) {
                y = 0;
                x += stepx + spacer.x;
                stepx = 0;
            }
            stepx = Math.max(stepx, wdg.sz.x);
            wdg.c = new Coord(x, y);
            y += wdg.sz.y + spacer.y;
        }

        super.pack();
    }
}

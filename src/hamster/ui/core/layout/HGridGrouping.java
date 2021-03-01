package hamster.ui.core.layout;

import haven.Coord;
import haven.Widget;

public class HGridGrouping extends Grouping {
    private final Coord spacer;
    private final int maxx;
    private final int indent;

    public HGridGrouping(final String cap, final Coord spacer, final int indent, final int maxx, final boolean box) {
        super(cap, box);
        this.spacer = spacer;
        this.indent = indent;
        this.maxx = maxx;
    }

    @Override
    public void pack() {
        int y = 0;
        int x = indent;
        int stepy = 0;

        for (Widget wdg = child; wdg != null; wdg = wdg.next) {
            if (x + wdg.sz.x + spacer.x > maxx) {
                x = indent;
                y += stepy + spacer.y;
                stepy = 0;
            }
            stepy = Math.max(stepy, wdg.sz.y);
            wdg.c = new Coord(x, y);
            x += wdg.sz.x + spacer.x;
        }

        super.pack();
    }
}

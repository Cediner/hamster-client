package hamster.ui.core.layout;

import hamster.ui.core.Scrollport;
import haven.Coord;
import haven.Widget;

import java.util.HashMap;
import java.util.Map;

public class StaticCellSheet extends Scrollport {
    private final Map<Coord, Widget> cells = new HashMap<>();
    private Coord min = null, max = null;

    public StaticCellSheet(final Coord sz) {
        super(sz);
    }

    private void addToCell(final Widget child, final Coord c) {
        if (cells.containsKey(c)) {
            final Widget owdg = cells.get(c);
            owdg.destroy();
        }
        cells.put(c, child);

        if (min != null) {
            min = min.min(c);
            max = max.max(c);
        } else {
            min = max = c;
        }
    }

    @Override
    public <T extends Widget> T add(T child, Coord c) {
        addToCell(child, c);
        return super.add(child, c);
    }

    @Override
    public void cdestroy(Widget w) {
        super.cdestroy(w);
    }

    private void arrange() {
        // Local maximums of row height and column width
        final Map<Integer, Integer> rowh = new HashMap<>();
        final Map<Integer, Integer> colw = new HashMap<>();
        // X / Y locations of the cells
        final Map<Integer, Integer> colx = new HashMap<>();
        final Map<Integer, Integer> rowy = new HashMap<>();

        for (final Coord c : cells.keySet()) {
            final Widget wdg = cells.get(c);
            rowh.put(c.y, Math.max(rowh.getOrDefault(c.y, 0), wdg.sz.y));
            colw.put(c.x, Math.max(colw.getOrDefault(c.x, 0), wdg.sz.x));
        }

        for(final Coord c : cells.keySet()) {
            colx.put(c.x, colx.getOrDefault(c.x-1, 0) +  colw.getOrDefault(c.x-1, 0));
            rowy.put(c.y, rowy.getOrDefault(0, c.y-1) +  rowh.getOrDefault(0, c.y-1));
        }

        for (final Coord c : cells.keySet()) {
            final Widget wdg = cells.get(c);
            wdg.move(new Coord(colx.get(c.x), rowy.get(c.y)));
        }
    }

    @Override
    public void pack() {
        arrange();
        super.pack();
    }
}

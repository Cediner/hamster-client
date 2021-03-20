package hamster.ui;

import hamster.ui.core.ColorPreview;
import hamster.ui.core.NumberEntry;
import hamster.ui.core.layout.LinearGrouping;
import hamster.util.IDPool;
import haven.*;
import haven.Label;
import haven.Window;

import java.awt.*;
import java.util.function.Consumer;


public class DowseWnd extends Window {
    private static final IDPool ids = new IDPool(0, Integer.MAX_VALUE);
    //Arc is a1 to a2, a1 < a2
    public final Coord2d startc;
    public final double a1;
    public final double a2;
    private final Runnable onClose;
    private final Consumer<Integer> changeDist;
    private final CustomPointer ptr;

    public DowseWnd(final Coord2d startc, final double a1, final double a2, final int dist,
		    final Consumer<Color> changeCol, final Consumer<Integer> changeDist, final Runnable onClose) {
        super(Coord.z, "Dowse " + ids.next(), "Dowse");
        this.startc = startc;
        this.a1 = normalize(Math.toDegrees(a1));
        this.a2 = normalize(Math.toDegrees(a2));
        this.onClose = onClose;
        this.changeDist = changeDist;
        this.ptr = new CustomPointer(cap.text, Coord2d.z);
        updateDist(dist/11);
        final double delta = normalize(Math.toDegrees(a2-a1));
        final double center = normalize(Math.toDegrees((a1+a2)/2));
        final LinearGrouping grp = new LinearGrouping(5, false);
        grp.add(new Label(String.format("Center Angle: %.2f", center)));
        grp.add(new Label(String.format("Delta: %.2f", delta)));
        grp.add(new Label("Assumed distance:"));
        grp.add(new NumberEntry(150, dist/11, 1, 10000, this::updateDist, this::updateDist));
        {
            final var container = new Widget();
            final Label lcol = new Label("Dowse Color:");
            final ColorPreview col = new ColorPreview(UI.scale(16, 16),
                    new Color(255, 0, 0, (int) (255 * .30)),
                    changeCol);
            container.add(lcol);
            container.add(col, lcol.c.add(lcol.sz.x + UI.scale(5), 0));
            container.pack();
            grp.add(container);
        }
        grp.pack();
        add(grp);
        pack();
        resize(asz.max(UI.scale(125, asz.y+5)));
    }

    public void updateDist(final int dist) {
        changeDist.accept(dist*11);
        final double mid = Math.toRadians((a1 + a2) / 2);
        ptr.updatetc(startc.add(new Coord2d(Math.cos(mid), Math.sin(mid)).mul(dist*11)));
    }

    @Override
    protected void added() {
        super.added();
        ui.gui.add(ptr);
    }

    @Override
    public void remove() {
        super.remove();
        ui.destroy(ptr);
    }

    public double a1() { return a1; }
    public double a2() { return a2; }

    private double normalize(final double a) {
        if(a > 0) {
            return a % 360;
        } else if(a < 0) {
            return Math.abs(a);
            //return 360 + a;
        } else { //0
            return a;
        }
    }

    @Override
    public void close() {
        onClose.run();
        ui.gui.remDowseWnd(this);
        ui.destroy(this);
    }
}

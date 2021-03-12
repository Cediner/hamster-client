package hamster.ui;

import hamster.ui.core.layout.LinearGrouping;
import haven.Coord;
import haven.Profile;
import haven.UI;
import haven.Window;

public class ProfWnd extends Window {
    private final LinearGrouping container;

    public ProfWnd() {
        super(Coord.z, "Profiler", "Profiler");
        container = add(new LinearGrouping("Profilers", UI.scale(5)));
    }

    public void add(final Profile prof, final String title) {
        final LinearGrouping grp = container.add(new LinearGrouping(title, new Coord(0, 0), false));
        grp.add(new ProfWdg(prof));
        grp.pack();
        container.pack();
        pack();
    }

    @Override
    public void close() {
        ui.destroy(this);
    }
}

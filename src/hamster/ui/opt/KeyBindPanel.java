package hamster.ui.opt;

import hamster.IndirSetting;
import hamster.KeyBind;
import hamster.ui.core.Scrollport;
import hamster.ui.core.TabManager;
import hamster.ui.core.layout.GridGrouping;
import hamster.ui.core.layout.Grouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

import java.util.List;
import java.util.Map;

public class KeyBindPanel extends Widget {
    private static Widget KeyBindEditWithLabel(final String text, final IndirSetting<String> keybind) {
        final Widget container = new Widget();
        final Label lbl = new Label(text);
        final KeyBindEdit kbe = new KeyBindEdit(keybind);
        final int height = Math.max(lbl.sz.y, kbe.sz.y) / 2;
        container.add(lbl, new Coord(0, height - lbl.sz.y / 2));
        container.add(kbe, new Coord(lbl.sz.x + 5, height - kbe.sz.y / 2));
        container.pack();
        return container;
    }

    public KeyBindPanel(final UI ui) {
        super(new Coord(UI.scale(500), UI.scale(395)));
        final Coord spacer = new Coord(UI.scale(20), UI.scale(5));
        final LinearGrouping grp = new LinearGrouping(UI.scale(5), false);
        grp.add(new Img(RichText.render("Click on the black box to start editing. Right click to cancel or Enter to confirm. If your choice shows up Red/Purple then it overlaps another keybind.", UI.scale(400)).tex()));
        final TabManager tabs = grp.add(new TabManager(UI.scale(500)));
        {//Key Binds
            final Map<String, List<KeyBind>> groupings = KeyBind.generateGroupings();
            for (final String group : groupings.keySet()) {
                final Scrollport view = new Scrollport(new Coord(UI.scale(480), UI.scale(400)));
                final Grouping binds = new GridGrouping(group + " Keybinds", spacer, spacer.x, UI.scale(600), false);
                for (final KeyBind kb : groupings.get(group)) {
                    binds.add(KeyBindEditWithLabel(kb.name, kb.bind));
                }
                binds.pack();
                view.add(binds);
                view.pack();
                tabs.addtab(view, group);
            }
        }

        tabs.pack();
        add(grp);
        pack();
    }
}

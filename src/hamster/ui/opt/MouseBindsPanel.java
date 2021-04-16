package hamster.ui.opt;

import hamster.IndirSetting;
import hamster.KeyBind;
import hamster.MouseBind;
import hamster.data.TranslationLookup;
import hamster.ui.core.Scrollport;
import hamster.ui.core.TabManager;
import hamster.ui.core.layout.GridGrouping;
import hamster.ui.core.layout.Grouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

import java.util.List;
import java.util.Map;

public class MouseBindsPanel extends Scrollport {
    public static Widget MouseBindEditWithLabel(final String text, final String group, final IndirSetting<String> bind) {
        final Widget container = new Widget();
        final Label lbl = new Label(text);
        final MouseBindEdit kbe = new MouseBindEdit(group, bind);
        final int height = Math.max(lbl.sz.y, kbe.sz.y) / 2;
        container.add(lbl, new Coord(0, height - lbl.sz.y / 2));
        container.add(kbe, new Coord(lbl.sz.x + 5, height - kbe.sz.y / 2));
        container.pack();
        return container;
    }

    public MouseBindsPanel(final UI ui) {
        super(new Coord(UI.scale(500), UI.scale(395)));
        final Coord spacer = new Coord(0,  UI.scale(5));
        final LinearGrouping grp = new LinearGrouping(UI.scale(5), false);
        grp.add(new Img(RichText.render(TranslationLookup.get("opt_mb_description"), UI.scale(400)).tex()));
        final TabManager tabs = grp.add(new TabManager(UI.scale(500)));
        {
            final Map<String, List<MouseBind>> groupings = MouseBind.bindgrps;
            for (final String group : groupings.keySet()) {
                final Scrollport view = new Scrollport(new Coord(UI.scale(480), UI.scale(400)));
                final Grouping binds = new GridGrouping(String.format("%s %s", group, TranslationLookup.get("mb_mousebind")), spacer, spacer.x, UI.scale(600), false);
                for (final MouseBind mb : groupings.get(group)) {
                    binds.add(MouseBindEditWithLabel(mb.name, mb.grouping, mb.bind));
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

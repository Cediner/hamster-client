package hamster.ui.opt;

import hamster.IndirSetting;
import hamster.MouseBind;
import hamster.data.TranslationLookup;
import hamster.ui.core.Scrollport;
import hamster.ui.core.TabManager;
import hamster.ui.core.layout.Grouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

import java.util.List;
import java.util.Map;

public class MouseBindsPanel extends Widget {
    public static Widget MouseBindEditWithLabel(final int width, final String text,
                                                final String group, final IndirSetting<String> bind) {
        final Widget container = new Widget();
        final Label lbl = new Label(text);
        final MouseBindEdit kbe = new MouseBindEdit(group, bind);
        final int height = Math.max(lbl.sz.y, kbe.sz.y) / 2;
        container.add(lbl, new Coord(0, height - lbl.sz.y / 2));
        container.adda(Frame.with(kbe, false), new Coord(width, height), 1, 0.5);
        container.pack();
        return container;
    }

    public MouseBindsPanel(final UI ui) {
        super(OptionsWnd.PANEL_SIZE);
        final Coord spacer = new Coord(0,  UI.scale(5));
        final LinearGrouping grp = new LinearGrouping(UI.scale(5), false);
        grp.add(new Img(RichText.render(TranslationLookup.get("opt_mb_description"), UI.scale(400)).tex()));
        final TabManager tabs = grp.add(new TabManager(UI.scale(500)));
        {
            final Map<String, List<MouseBind>> groupings = MouseBind.bindgrps;
            for (final String group : groupings.keySet()) {
                final Scrollport view = new Scrollport(new Coord(UI.scale(480), UI.scale(400)));
                final Grouping binds = new LinearGrouping(String.format("%s %s", group, TranslationLookup.get("mb_mousebind")), spacer, false);
                for (final MouseBind mb : groupings.get(group)) {
                    binds.add(MouseBindEditWithLabel(view.msz().x, mb.name, mb.grouping, mb.bind));
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

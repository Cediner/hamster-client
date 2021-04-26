package hamster.ui.opt;

import hamster.IndirSetting;
import hamster.KeyBind;
import hamster.data.TranslationLookup;
import hamster.ui.core.Scrollport;
import hamster.ui.core.TabManager;
import hamster.ui.core.layout.GridGrouping;
import hamster.ui.core.layout.Grouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

import java.util.List;
import java.util.Map;

public class KeyBindPanel extends Widget {
    private static Widget KeyBindEditWithLabel(final int width, final String text,
                                               final IndirSetting<String> keybind) {
        final Widget container = new Widget();
        final Label lbl = new Label(text);
        final KeyBindEdit kbe = new KeyBindEdit(keybind);
        final int height = Math.max(lbl.sz.y, kbe.sz.y) / 2;
        container.adda(lbl, new Coord(0, height), 0, 0.5);
        container.adda(Frame.with(kbe, false), new Coord(width, height), 1, 0.5);
        container.pack();
        return container;
    }

    public KeyBindPanel(final UI ui) {
        super(OptionsWnd.PANEL_SIZE);
        final Coord spacer = new Coord(0,  UI.scale(5));
        final LinearGrouping grp = new LinearGrouping(UI.scale(5), false);
        grp.add(new Img(RichText.render(TranslationLookup.get("opt_kb_description"), UI.scale(400)).tex()));
        final TabManager tabs = grp.add(new TabManager(UI.scale(500)));
        {//Key Binds
            final Map<String, List<KeyBind>> groupings = KeyBind.generateGroupings();
            for (final String group : groupings.keySet()) {
                final Scrollport view = new Scrollport(new Coord(UI.scale(480), UI.scale(400)));
                final Grouping binds = new LinearGrouping(String.format("%s %s", group, TranslationLookup.get("kb_keybind")), spacer, false);
                for (final KeyBind kb : groupings.get(group)) {
                    binds.add(KeyBindEditWithLabel(view.msz().x, kb.name, kb.bind));
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

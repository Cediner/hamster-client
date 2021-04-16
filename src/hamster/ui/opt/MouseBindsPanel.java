package hamster.ui.opt;

import hamster.IndirSetting;
import hamster.MouseBind;
import hamster.data.TranslationLookup;
import hamster.ui.core.Scrollport;
import hamster.ui.core.layout.GridGrouping;
import hamster.ui.core.layout.Grouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

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
        final Grouping binds = new LinearGrouping("Mousebinds", spacer, false, LinearGrouping.Direction.VERTICAL);
        {
            binds.add(new Img(RichText.render(TranslationLookup.get("opt_mb_description"), UI.scale(400)).tex()));
            for (final String grp : MouseBind.bindgrps.keySet()) {
                final Grouping bindgrp = new LinearGrouping(grp, spacer, false);
                for (final MouseBind mb : MouseBind.bindgrps.get(grp)) {
                    bindgrp.add(MouseBindEditWithLabel(mb.name, mb.grouping, mb.bind));
                }
                bindgrp.pack();
                binds.add(bindgrp);
            }
            binds.pack();
        }

        add(binds);
        pack();
    }
}

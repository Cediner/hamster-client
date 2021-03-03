package hamster.ui.opt;

import hamster.ui.core.Scrollport;
import hamster.ui.core.indir.IndirLabel;
import hamster.ui.core.indir.IndirRadioGroup;
import hamster.ui.core.layout.Grouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.Coord;
import haven.UI;

import java.io.File;
import java.util.ArrayList;

import static hamster.GlobalSettings.*;

public class ThemePanel extends Scrollport {
    public ThemePanel(final UI ui) {
        super(new Coord(500, 400));
        final Coord spacer = new Coord(20, 5);

        final Grouping theme = new LinearGrouping("Theme Settings", spacer, false);

        { //theme
            final ArrayList<String> huds = new ArrayList<>();
            final File dir = new File("data/res/custom/hud/");
            if (dir.exists()) {
                final File[] files = dir.listFiles();
                if (files != null) {
                    for (final File f : files) {
                        huds.add(f.getName());
                    }
                }
            }

            //TODO: Most windows should repack on theme change
            final IndirRadioGroup<String> rgrp = new IndirRadioGroup<>("Main Hud Theme (requires restart)", 450, HUDTHEME);
            for (final String name : huds) {
                rgrp.add(name, name);
            }
            theme.add(rgrp);
            theme.add(new IndirLabel(() -> String.format("Settings for %s", HUDTHEME.get())));
            theme.add(OptionsWnd.ColorPreWithLabel("Window Color: ", WNDCOL));
            theme.add(OptionsWnd.ColorPreWithLabel("Button Color: ", BTNCOL));
            theme.add(OptionsWnd.ColorPreWithLabel("Textbox Color: ", TXBCOL));
            theme.add(OptionsWnd.ColorPreWithLabel("Slider Color: ", SLIDERCOL));
            theme.pack();
        }

        int y = 0;

        y += add(theme, new Coord(0, y)).sz.y + spacer.y;
        pack();
    }
}

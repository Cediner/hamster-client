package hamster.ui.opt;

import hamster.data.TranslationLookup;
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
        super(new Coord(UI.scale(500), UI.scale(395)));
        final Coord spacer = new Coord(UI.scale(20), UI.scale(5));

        final Grouping theme = new LinearGrouping( TranslationLookup.get("opt_theme_settings"), spacer, false);

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
            final IndirRadioGroup<String> rgrp = new IndirRadioGroup<>(TranslationLookup.get("opt_theme_hud_theme"), 450, HUDTHEME);
            for (final String name : huds) {
                rgrp.add(name, name);
            }
            theme.add(rgrp);
            theme.add(new IndirLabel(() -> String.format("%s%s", TranslationLookup.get("opt_theme_for"), HUDTHEME.get())));
            theme.add(OptionsWnd.ColorPreWithLabel(TranslationLookup.get("opt_theme_win_col"), WNDCOL));
            theme.add(OptionsWnd.ColorPreWithLabel(TranslationLookup.get("opt_theme_btn_col"), BTNCOL));
            theme.add(OptionsWnd.ColorPreWithLabel(TranslationLookup.get("opt_theme_txtbox_col"), TXBCOL));
            theme.add(OptionsWnd.ColorPreWithLabel(TranslationLookup.get("opt_theme_slider_col"), SLIDERCOL));
            theme.pack();
        }

        int y = 0;

        y += add(theme, new Coord(0, y)).sz.y + spacer.y;
        pack();
    }
}

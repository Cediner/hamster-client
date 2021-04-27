package hamster.ui.core;

import hamster.GlobalSettings;
import hamster.ui.core.indir.IndirThemeRes;
import hamster.ui.core.indir.IndirThemeTex;
import hamster.ui.core.layout.GridGrouping;
import haven.*;

import java.util.HashMap;

public class TabManager extends Widget {
    private static final IndirThemeRes res = Theme.themeres("tabs");
    private static final IndirThemeTex
            tabLeft = res.tex(0),
            tabLeftOn = res.tex( 1),
            tabMid = res.tex( 2),
            tabMidOn = res.tex(3),
            tabRight = res.tex( 4),
            tabRightOn = res.tex( 5);

    private static class TabButton extends Button {
        public boolean on = false;
        private Coord lc, mc, msz, rc;

        public TabButton(String text) {
            super(text);
            sz = new Coord(cont.getWidth() + tabLeft.tex().sz().x + tabRight.tex().sz().x, tabRight.tex().sz().y);
            setup();
        }

        public void setup() {
            lc = Coord.z;
            mc = lc.add(tabLeft.tex().sz().x, 0);
            rc = sz.sub(tabRight.tex().sz().x, sz.y);
            msz = new Coord(rc.x - mc.x, tabMid.tex().sz().y);
        }

        public void draw(GOut g) {
            g.chcolor(GlobalSettings.BTNCOL.get());
            if (!on) {
                g.image(tabLeft.tex(), lc);
                g.image(tabMid.tex(), mc, msz);
                g.image(tabRight.tex(), rc);
            } else {
                g.image(tabLeftOn.tex(), lc);
                g.image(tabMidOn.tex(), mc, msz);
                g.image(tabRightOn.tex(), rc);
            }

            Coord tc = sz.sub(Utils.imgsz(cont)).div(2);
            if (a)
                tc = tc.add(1, 1);
            g.image(cont, tc);
            g.chcolor();
        }
    }

    private final HashMap<String, TabButton> namemap = new HashMap<>();
    private final HashMap<Widget, Tabs.Tab> tabmap = new HashMap<>();
    public final Tabs tabs;
    private final GridGrouping gridtabs;
    public final int height;
    private TabButton last;

    public TabManager(int w) {
        super(Coord.z);
        height = tabMid.tex().sz().y + 2;
        tabs = new Tabs(new Coord(0, height), Coord.z, this);
        gridtabs = new GridGrouping(null, new Coord(0, 0), 0, w, false, GridGrouping.Direction.HORIZONTAL);
        add(gridtabs);
    }

    public void addtab(Widget ch, String name, boolean show) {
        TabButton btn = new TabButton(name);
        Tabs.Tab tab = add(tabs.new Tab() {
            public void cresize(Widget ch) {
                TabManager.this.pack();
            }

            public void show() {
                super.show();
            }
        }, tabs.c);
        gridtabs.add(btn);
        tab.add(ch, Coord.z);
        pack();

        namemap.put(name, btn);
        tabmap.put(btn, tab);

        if (tabmap.size() == 1 || show) {
            changetab(tab, btn);
        }
    }


    public void addtab(Widget ch, String name) {
        addtab(ch, name, false);
    }

    public void pack() {
        gridtabs.pack();
        tabs.c = new Coord(0, gridtabs.sz.y);
        for(final var tab : tabmap.values())
            tab.move(tabs.c);
        tabs.indpack();
        super.pack();
        parent.pack();
    }

    public void remtab(String name) {
        TabButton btn = namemap.get(name);
        Tabs.Tab tab = tabmap.get(btn);
        tab.destroy();
        btn.destroy();

        namemap.remove(name);
        tabmap.remove(btn);
        pack();
    }

    public void changetab(Tabs.Tab tab, TabButton btn) {
        if (last != btn) {
            if (last != null) {
                last.change(last.text.text);
                last.on = false;
            }
            last = btn;
            btn.on = true;
            tabs.showtab(tab);
            pack();
            parent.pack();
        }
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        Tabs.Tab tab = tabmap.get(sender);
        if (tab != null) {
            changetab(tab, (TabButton) sender);
        } else {
            super.wdgmsg(sender, msg, args);
        }
    }
}
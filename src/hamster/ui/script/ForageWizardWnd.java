package hamster.ui.script;

import hamster.ui.core.NumberEntry;
import hamster.ui.core.layout.GridGrouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ForageWizardWnd extends Window {
    private static class Foragable {
        public final String name;
        public final String res;

        public Foragable(final String name, final String res) {
            this.name = name;
            this.res = res;
        }
    }

    private final Set<String> foragables = new HashSet<>();
    private final List<Coord2d> points = new ArrayList<>();
    private final List<Coord2d> rpoints = new ArrayList<>();
    private boolean useboat, crawl, logoutonplayers, logoutonanimals;
    private final TextEntry filename;
    private final NumberEntry radius;

    private Coord2d last;

    public ForageWizardWnd() {
        super(Coord.z, "Forage Wizard", "forage-wizard");
        final int spacer = UI.scale(5);
        final int lsth = UI.scale(20);
        final int width = UI.scale(150);
        final LinearGrouping load = new LinearGrouping("Load Data", spacer);
        {
            final List<String> saves = getSavedData();
            final Listbox<String> files = load.add(new Listbox<String>(width, 15, lsth) {
                @Override
                protected int listitems() {
                    return saves.size();
                }

                @Override
                protected String listitem(int i) {
                    return saves.get(i);
                }

                @Override
                protected void drawitem(GOut g, String item, int i) {
                    FastText.print(g, Coord.o, item);
                }
            });
            load.add(new Button(width, "Load", () -> {
                if(files.sel != null) {
                    ui.sess.details.context.dispatchmsg(this, "load-data", "data/scripts/forage/" + files.sel + ".dat");
                    ui.destroy(this);
                }
            }));
            load.pack();
        }
        final GridGrouping newg = new GridGrouping("Create New", spacer, UI.scale(300));
        {
            final LinearGrouping basic = new LinearGrouping("Basic Details", spacer);
            {
                basic.add(new Label("Name:"));
                filename = basic.add(new TextEntry(width, ""));
                basic.add(new Label("Search Radius (Tiles):"));
                radius = basic.add(new NumberEntry(width, 45, 1, 100));
                basic.add(new CheckBox("Use a boat", (val) -> useboat = val, false));
                basic.add(new CheckBox("Crawl", (val) -> crawl = val, false));
                basic.add(new CheckBox("Logout on bad players", (val) -> logoutonplayers = val, true));
                basic.add(new CheckBox("Logout on bad animals", (val) -> logoutonanimals = val, true));
                basic.add(new Button(width, "Save", () -> {
                    if(!filename.text.equals("") && foragables.size() > 0 && points.size() > 0) {
                        ui.sess.details.context.dispatchmsg(this, "new-data", "data/scripts/forage/" + filename.text + ".dat", radius.value(),
                                useboat, crawl, logoutonplayers, logoutonanimals,
                                foragables.toArray(new String[0]), points.toArray(new Coord2d[0]), rpoints.toArray(new Coord2d[0]));
                        ui.destroy(this);
                    }
                }));
                basic.pack();
            }
            final LinearGrouping pointsg = new LinearGrouping("Path", spacer);
            {
                pointsg.add(new Button(width, "Add Point", this::addPoint));
                pointsg.add(new Listbox<Coord2d>(width, 14, lsth) {
                    @Override
                    protected int listitems() {
                        return points.size();
                    }

                    @Override
                    protected Coord2d listitem(int i) {
                        return points.get(i);
                    }

                    @Override
                    protected void drawitem(GOut g, Coord2d item, int i) {
                        FastText.printf(g, Coord.o, "(%.2f, %.2f)", item.x, item.y);
                    }
                });
                pointsg.pack();
            }
            final GridGrouping foragegrid = new GridGrouping("Foragables", spacer, UI.scale(300));
            {
                final Foragable[] forages = {
                        new Foragable("Lady's Mantle", "gfx/terobjs/herbs/ladysmantle"),
                        new Foragable("Candleberry", "gfx/terobjs/herbs/candleberry"),
                        new Foragable("Common Starfish", "gfx/terobjs/herbs/commonstarfish"),
                        new Foragable("Royal Toadstool", "gfx/terobjs/herbs/royaltoadstool"),
                        new Foragable("Cavebulb", "gfx/terobjs/herbs/cavebulb"),
                        new Foragable("Frogs Crown", "gfx/terobjs/herbs/frogscrown"),
                        new Foragable("Glimmermoss", "gfx/terobjs/herbs/glimmermoss"),
                        new Foragable("Stalagoom", "gfx/terobjs/herbs/stalagoom"),
                        new Foragable("Cave Lantern", "gfx/terobjs/herbs/cavelantern"),
                        new Foragable("Cave Clay", "gfx/terobjs/herbs/clay-cave"),
                        new Foragable("Bluebell", "gfx/terobjs/herbs/chimingbluebell"),
                        new Foragable("Bloated Bolete", "gfx/terobjs/herbs/bloatedbolete"),
                        new Foragable("Edelweiss", "gfx/terobjs/herbs/edelweiss"),
                        new Foragable("Clover", "gfx/terobjs/herbs/clover"),
                        new Foragable("Snapdragon", "gfx/terobjs/herbs/snapdragon"),
                        new Foragable("Stinging Nettle", "gfx/terobjs/herbs/stingingnettle"),
                        new Foragable("Blueberry", "gfx/terobjs/herbs/blueberry"),
                        new Foragable("Chantrelle", "gfx/terobjs/herbs/chantrelle"),
                };
                for(final Foragable foragable : forages) {
                    foragegrid.add(new CheckBox(foragable.name, (val) -> {
                        if(val)
                            foragables.add(foragable.res);
                        else
                            foragables.remove(foragable.res);
                    }, false));
                }
                foragegrid.pack();
            }

            newg.add(basic);
            newg.add(pointsg);
            newg.add(foragegrid);
            newg.pack();
        }
        add(load, Coord.z);
        add(newg, load.c.add(load.sz.x, 0));
        pack();
    }

    private List<String> getSavedData() {
        final File dir = new File("data/scripts/forage/");
        if(!dir.exists() && !dir.mkdirs())
            return new ArrayList<>();
        else {
            final List<String> itms = new ArrayList<>();
            final File[] files = dir.listFiles((dir2, name) -> name.endsWith(".dat"));
            if(files != null) {
                for (final File file : files) {
                    itms.add(file.getName().substring(0, file.getName().lastIndexOf(".dat")));
                }
            }
            return itms;
        }
    }

    private void addPoint() {
        final Coord2d cur = ui.sess.glob.oc.getgob(ui.gui.map.rlplgob).rc;
        final Coord2d dif = cur.sub(last);
        points.add(dif);
        rpoints.add(0, dif.mul(-1, -1));
        last = cur;
    }

    @Override
    protected void added() {
        super.added();
        last = ui.sess.glob.oc.getgob(ui.gui.map.rlplgob).rc;
    }

    @Override
    public void close() {
        ui.sess.details.context.dispatchmsg(this, "cancel");
        ui.destroy(this);
    }
}

package hamster.ui.minimap;

import hamster.data.map.MarkerData;
import haven.Button;
import haven.Window;
import haven.*;

import java.awt.*;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

public class MapMarkerWnd extends Window {
    private final static List<String> types = Arrays.asList("Placed", "Natural", "Custom", "Linked", "Realm", "Village");
    private final static Predicate<Marker> pmarkers = (m -> m instanceof PMarker);
    private final static Predicate<Marker> smarkers = (m -> m instanceof SMarker);
    private final static Predicate<Marker> slmarkers = (m -> m instanceof CustomMarker && !(m instanceof LinkedMarker) && !(m instanceof WaypointMarker));
    private final static Predicate<Marker> lmarkers = (m -> m instanceof LinkedMarker);
    private final static Predicate<Marker> kmarkers = (m -> m instanceof RealmMarker);
    private final static Predicate<Marker> vmarkers = (m -> m instanceof VillageMarker);
    private final static Comparator<Marker> namecmp = Comparator.comparing(Marker::name);
    private Predicate<Marker> mflt;
    private List<Marker> markers = Collections.emptyList();
    private int markerseq = -1;
    private final Comparator<Marker> mcmp = namecmp;
    private final MapWnd map;
    public final MarkerList list;
    private TextEntry namesel, realmedit;
    private BuddyWnd.GroupSelector colsel;
    private Button mremove;
    private final Dropbox<String> typesel;
    private Dropbox<Marker> linker;

    public MapMarkerWnd(final MapWnd map) {
        super(Coord.z, "Markers", "Markers");
        this.map = map;
        mflt = pmarkers;
        list = add(new MarkerList(200, 20));
        resize(list.sz.add(0, 120));
        typesel = add(new Dropbox<>(200, types.size(), 20) {
            {
                sel = types.get(0);
            }

            @Override
            protected int listitems() {
                return types.size();
            }

            @Override
            protected String listitem(int i) {
                return types.get(i);
            }

            @Override
            protected void drawitem(GOut g, String item, int i) {
                FastText.aprint(g, new Coord(5, sz.y / 2), 0.0, 0.5, item);
            }

            @Override
            public void change(String item) {
                super.change(item);
                switch (item) {
                    case "Placed" -> {
                        mflt = pmarkers;
                        markerseq = -1;
                    }
                    case "Natural" -> {
                        mflt = smarkers;
                        markerseq = -1;
                    }
                    case "Custom" -> {
                        mflt = slmarkers;
                        markerseq = -1;
                    }
                    case "Linked" -> {
                        mflt = lmarkers;
                        markerseq = -1;
                    }
                    case "Realm" -> {
                        mflt = kmarkers;
                        markerseq = -1;
                    }
                    case "Village" -> {
                        mflt = vmarkers;
                        markerseq = -1;
                    }
                }
                list.display(0);
            }
        }, list.c.add(0, list.sz.y + 10));
        pack();
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    public void tick(double dt) {
        super.tick(dt);
        if (visible && (markerseq != map.view.file.markerseq)) {
            if (map.view.file.lock.readLock().tryLock()) {
                try {
                    this.markers = map.view.file.markers.stream().filter(mflt).sorted(mcmp).collect(java.util.stream.Collectors.toList());
                } finally {
                    map.view.file.lock.readLock().unlock();
                }
            }
        }
    }

    public static final Color every = new Color(255, 255, 255, 16), other = new Color(255, 255, 255, 32), found = new Color(255, 255, 0, 32);

    public class MarkerList extends Searchbox<Marker> {
        private final Text.Foundry fnd = CharWnd.attrf;

        public Marker listitem(int idx) {
            return (markers.get(idx));
        }

        public int listitems() {
            return (markers.size());
        }

        public boolean searchmatch(int idx, String txt) {
            return markers.get(idx).nm.toLowerCase().contains(txt.toLowerCase());
        }

        private MarkerList(int w, int n) {
            super(w, n, 20);
        }

        private final Function<String, Text> names = new CachedFunction<>(500, fnd::render);

        protected void drawbg(GOut g) {
        }

        public void drawitem(GOut g, Marker mark, int idx) {
            if (soughtitem(idx)) {
                g.chcolor(found);
                g.frect(Coord.z, g.sz());
            }
            g.chcolor(((idx % 2) == 0) ? every : other);
            g.frect(Coord.z, g.sz());
            if (mark instanceof PMarker)
                g.chcolor(((PMarker) mark).color);
            else if (mark instanceof CustomMarker)
                g.chcolor(((CustomMarker) mark).color);
            else if(mark instanceof VillageMarker)
                g.chcolor(MarkerData.getVillageBoldColor(((VillageMarker) mark).village));
            else
                g.chcolor();

            if (!(mark instanceof LinkedMarker || mark instanceof RealmMarker || mark instanceof VillageMarker))
                g.aimage(names.apply(mark.nm).tex(), new Coord(5, itemh / 2), 0, 0.5);
            else if(mark instanceof LinkedMarker)
                g.aimage(names.apply(String.format("[%d ⟶ %d] %s", ((LinkedMarker) mark).id, ((LinkedMarker) mark).lid, mark.nm)).tex(),
                        new Coord(5, itemh / 2), 0, 0.5);
            else if(mark instanceof RealmMarker)
                g.aimage(names.apply(String.format("[%s] %s", ((RealmMarker) mark).realm, mark.nm)).tex(),
                        new Coord(5, itemh / 2), 0 , 0.5);
            else { //Village
                g.aimage(names.apply(String.format("[%s] %s", ((VillageMarker) mark).village, mark.nm)).tex(),
                        new Coord(5, itemh / 2), 0 , 0.5);
            }
        }

        public void change(Marker mark) {
            change2(mark);
            if (mark != null)
                map.view.center(new MiniMap.SpecLocator(mark.seg, mark.tc));
        }

        //TODO: Clean this all up
        public void change2(Marker mark) {
            this.sel = mark;

            if (namesel != null) {
                ui.destroy(namesel);
                namesel = null;
                if (colsel != null) {
                    ui.destroy(colsel);
                    colsel = null;
                }
                if (linker != null) {
                    ui.destroy(linker);
                    linker = null;
                }
                if (mremove != null) {
                    ui.destroy(mremove);
                    mremove = null;
                }
                if(realmedit != null) {
                    ui.destroy(realmedit);
                    realmedit = null;
                }
                MapMarkerWnd.this.pack();
            }

            if (mark != null) {
                markerseq = -1;
                if (mark instanceof PMarker) {
                    typesel.sel = types.get(0);
                    mflt = pmarkers;
                } else if (mark instanceof SMarker) {
                    typesel.sel = types.get(1);
                    mflt = smarkers;
                } else if (mark instanceof LinkedMarker) {
                    typesel.sel = types.get(3);
                    mflt = lmarkers;
                } else if(mark instanceof RealmMarker) {
                    typesel.sel = types.get(4);
                    mflt = kmarkers;
                } else if(mark instanceof VillageMarker) {
                    typesel.sel = types.get(5);
                    mflt = vmarkers;
                } else {
                    typesel.sel = types.get(2);
                    mflt = slmarkers;
                }

                if (namesel == null) {
                    namesel = MapMarkerWnd.this.add(new TextEntry(200, "") {
                        {
                            dshow = true;
                        }

                        public void activate(String text) {
                            mark.nm = text;
                            map.view.file.update(mark);
                            commit();
                            change2(null);
                        }
                    }, new Coord(0, MapMarkerWnd.this.csz.y));
                }
                namesel.settext(mark.nm);
                namesel.setReadOnly(false);
                namesel.buf.point = mark.nm.length();
                namesel.commit();
                if (mark instanceof PMarker) {
                    PMarker pm = (PMarker) mark;
                    colsel = MapMarkerWnd.this.add(new BuddyWnd.GroupSelector(0) {
                        public void changed(int group) {
                            this.group = group;
                            pm.color = BuddyWnd.gc[group];
                            map.view.file.update(mark);
                        }
                    }, namesel.c.add(0, namesel.sz.y + 10));
                    if ((colsel.group = Utils.index(BuddyWnd.gc, pm.color)) < 0)
                        colsel.group = 0;
                    mremove = MapMarkerWnd.this.add(new Button(200, "Remove", false) {
                        public void click() {
                            map.view.file.remove(mark);
                            change2(null);
                        }
                    }, colsel.c.add(0, colsel.sz.y + 10));
                } else if (mark instanceof CustomMarker) {
                    CustomMarker pm = (CustomMarker) mark;
                    colsel = MapMarkerWnd.this.add(new BuddyWnd.GroupSelector(0) {
                        public void changed(int group) {
                            this.group = group;
                            pm.color = BuddyWnd.gc[group];
                            map.view.file.update(mark);
                        }
                    }, namesel.c.add(0, namesel.sz.y + 10));
                    if ((colsel.group = Utils.index(BuddyWnd.gc, pm.color)) < 0)
                        colsel.group = 0;
                    if (mark instanceof LinkedMarker) {
                        linker = MapMarkerWnd.this.add(new Dropbox<>(200, 5, 20) {
                            private List<Marker> lst;

                            {
                                if (map.view.file.lock.readLock().tryLock()) {
                                    try {
                                        lst = map.view.file.markers.stream()
                                                .filter(m -> m != mark && m instanceof LinkedMarker &&
                                                        LinkedMarker.canLink(((LinkedMarker) mark).type, ((LinkedMarker) m).type))
                                                .sorted(mcmp).collect(java.util.stream.Collectors.toList());
                                        list.display();
                                    } finally {
                                        map.view.file.lock.readLock().unlock();
                                    }
                                }
                                if (((LinkedMarker) mark).lid != -1)
                                    sel = map.view.file.lmarkers.get(((LinkedMarker) mark).lid);
                            }

                            @Override
                            public void change(Marker item) {
                                super.change(item);
                                final LinkedMarker link = (LinkedMarker) item;
                                if (((LinkedMarker) mark).lid != -1) {
                                    map.view.file.lmarkers.get(((LinkedMarker) mark).lid).lid = -1;
                                }
                                ((LinkedMarker) mark).lid = link.id;
                                link.lid = ((LinkedMarker) mark).id;
                                map.view.file.update(mark);
                                map.view.file.update(link);
                            }

                            @Override
                            protected Marker listitem(int i) {
                                return lst.get(i);
                            }

                            @Override
                            protected int listitems() {
                                return lst.size();
                            }

                            @Override
                            protected void drawitem(GOut g, Marker item, int i) {
                                FastText.aprintf(g, new Coord(5, itemh / 2), 0.0, 0.5, "[%d] %s", ((LinkedMarker) item).id, item.nm);
                            }
                        }, colsel.c.add(0, colsel.sz.y + 10));
                        mremove = MapMarkerWnd.this.add(new Button(200, "Remove", false) {
                            public void click() {
                                map.view.file.remove(mark);
                                change2(null);
                            }
                        }, linker.c.add(0, linker.sz.y + 10));
                    } else {
                        mremove = MapMarkerWnd.this.add(new Button(200, "Remove", false) {
                            public void click() {
                                map.view.file.remove(mark);
                                change2(null);
                            }
                        }, colsel.c.add(0, colsel.sz.y + 10));
                    }
                } else if (mark instanceof RealmMarker) {
                    RealmMarker pm = (RealmMarker) mark;
                    colsel = MapMarkerWnd.this.add(new BuddyWnd.GroupSelector(0) {
                        public void changed(int group) {
                            this.group = group;
                            MarkerData.setRealmColor(pm.realm, group);
                        }
                    }, namesel.c.add(0, namesel.sz.y + 10));
                    if ((colsel.group = Utils.index(BuddyWnd.gc, MarkerData.getRealmColor(pm.realm))) < 0)
                        colsel.group = 0;
                    realmedit = MapMarkerWnd.this.add(new TextEntry(200, "") {
                        {
                            dshow = true;
                        }

                        public void activate(String text) {
                            ((RealmMarker) mark).realm = text;
                            colsel.group = MarkerData.getRealmColorID(text);
                            map.view.file.update(mark);
                            commit();
                            change2(null);
                        }
                    }, colsel.c.add(0, colsel.sz.y + 10));
                    realmedit.settext(pm.realm);
                    realmedit.buf.point = pm.realm.length();
                    realmedit.commit();
                    mremove = MapMarkerWnd.this.add(new Button(200, "Remove", false) {
                        public void click() {
                            map.view.file.remove(mark);
                            change2(null);
                        }
                    }, realmedit.c.add(0, realmedit.sz.y + 10));
                } else if(mark instanceof VillageMarker) {
                    namesel.setReadOnly(true);
                    VillageMarker pm = (VillageMarker) mark;
                    colsel = MapMarkerWnd.this.add(new BuddyWnd.GroupSelector(0) {
                        public void changed(int group) {
                            this.group = group;
                            MarkerData.setVillageColor(pm.village, group);
                        }
                    }, namesel.c.add(0, namesel.sz.y + 10));
                    if ((colsel.group = Utils.index(BuddyWnd.gc, MarkerData.getVillageColor(pm.village))) < 0)
                        colsel.group = 0;
                    realmedit = MapMarkerWnd.this.add(new TextEntry(200, "") {
                        {
                            dshow = true;
                        }

                        public void activate(String text) {
                            ((VillageMarker) mark).village = text;
                            colsel.group = MarkerData.getVillageColorID(text);
                            map.view.file.update(mark);
                            commit();
                            change2(null);
                        }
                    }, colsel.c.add(0, colsel.sz.y + 10));
                    realmedit.settext(pm.village);
                    realmedit.buf.point = pm.village.length();
                    realmedit.commit();
                    mremove = MapMarkerWnd.this.add(new Button(200, "Remove", false) {
                        public void click() {
                            map.view.file.remove(mark);
                            change2(null);
                        }
                    }, realmedit.c.add(0, realmedit.sz.y + 10));
                }
                MapMarkerWnd.this.pack();
            }
            MapMarkerWnd.this.show();


            if (map.view.file.lock.readLock().tryLock()) {
                try {
                    MapMarkerWnd.this.markers = map.view.file.markers.stream().filter(mflt).sorted(mcmp).collect(java.util.stream.Collectors.toList());
                } finally {
                    map.view.file.lock.readLock().unlock();
                }
            }
        }
    }
}

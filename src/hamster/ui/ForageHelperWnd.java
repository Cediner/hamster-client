package hamster.ui;

import hamster.data.gob.ForageData;
import hamster.data.gob.ObjData;
import haven.Label;
import haven.Window;
import haven.*;

import java.awt.*;

public class ForageHelperWnd extends Window {
    private final Coord valc;
    private long prc = 0L, explore = 0L;
    private long perexp = 0L;
    private Glob.CAttr prcattr, expattr;

    public ForageHelperWnd() {
        super(Coord.z, "Forage Helper", "Forage Helper");
        final Label lbl = new Label("Your Per * Exp: ");
        add(lbl, new Coord(450 / 2, 0).sub(lbl.sz.x / 2, 0));
        valc = lbl.c.add(lbl.sz.x, 0);
        //find max length name
        final int maxName = maxNameLength() + 1;
        final int minValStart = 30 + maxName + 4;
        final int maxValStart = minValStart + 45;
        final int spStart = maxValStart + 55;
        final int suStart = spStart + 55;
        final int auStart = suStart + 55;
        final int wiStart = auStart + 55;
        //Text
        final Tex name = Text.renderstroked("Name").tex();
        final Tex min = Text.renderstroked("Min").tex();
        final Tex seeall = Text.renderstroked("See All").tex();
        final Tex spring = Text.renderstroked("Spring").tex();
        final Tex summer = Text.renderstroked("Summer").tex();
        final Tex autumn = Text.renderstroked("Autumn").tex();
        final Tex winter = Text.renderstroked("Winter").tex();
        //Header
        add(new Widget(new Coord(470, 30)) {
            @Override
            public void draw(GOut g) {
                g.chcolor(Color.BLACK);
                g.frect(Coord.z, sz);
                g.chcolor();

                g.chcolor(Color.WHITE);
                g.rect(Coord.z, g.sz());
                g.line(new Coord(30, 0), new Coord(30, 30), 1);
                g.aimage(name, new Coord(31, 15), 0, 0.5f);
                g.line(new Coord(30 + maxName + 2, 0), new Coord(30 + maxName + 2, 30), 1);
                g.aimage(min, new Coord(minValStart, 15), 0, 0.5f);
                g.line(new Coord(maxValStart, 0), new Coord(maxValStart, 30), 1);
                g.aimage(seeall, new Coord(maxValStart, 15), 0, 0.5f);
                g.line(new Coord(spStart, 0), new Coord(spStart, 30), 1);
                g.aimage(spring, new Coord(spStart + 2, 15), 0, 0.5f);
                g.line(new Coord(suStart, 0), new Coord(suStart, 30), 1);
                g.aimage(summer, new Coord(suStart + 2, 15), 0, 0.5f);
                g.line(new Coord(auStart, 0), new Coord(auStart, 30), 1);
                g.aimage(autumn, new Coord(auStart + 2, 15), 0, 0.5f);
                g.line(new Coord(wiStart, 0), new Coord(wiStart, 30), 1);
                g.aimage(winter, new Coord(wiStart + 2, 15), 0, 0.5f);
                g.chcolor();
            }
        }, new Coord(0, lbl.sz.y + 5));
        //Listbox
        final Listbox<ForageData> foragebox = new Listbox<>(480, 20, 30) {
            @Override
            protected ForageData listitem(int i) {
                return ObjData.getForageables().get(i);
            }

            @Override
            protected int listitems() {
                return ObjData.getForageables().size();
            }

            @Override
            protected void drawitem(GOut g, ForageData item, int i) {
                final Color bg;
                if (perexp >= item.max_value()) {
                    bg = new Color(0, 153, 76);
                } else if (perexp >= item.min_value()) {
                    bg = Color.ORANGE;
                } else {
                    bg = Color.RED;
                }
                g.chcolor(bg);
                g.frect(Coord.z, new Coord(30, 30));
                g.chcolor();

                g.chcolor(Color.WHITE);
                g.rect(Coord.z, g.sz());
                //Draw icon if any
                if (item.res() != null) {
                    try {
                        final Resource res = item.res().get();
                        if (res.layer(Resource.imgc) != null) {
                            g.image(res.layer(Resource.imgc).tex(), new Coord(1, 1), new Coord(29, 29));
                        }
                    } catch (Resource.Loading e) {
                        //Ignore for now
                    }
                }
                g.line(new Coord(30, 0), new Coord(30, 30), 1);
                //Draw name
                g.image(item.name().tex(), new Coord(31, 15).sub(0, item.name().img.getHeight() / 2));
                g.line(new Coord(30 + maxName + 2, 0), new Coord(30 + maxName + 2, 30), 1);
                //Draw min
                FastText.print(g, new Coord(minValStart, 15).sub(FastText.size("" + item.min_value()).mul(0f, 0.5f)),
                        "" + item.min_value());
                g.line(new Coord(maxValStart, 0), new Coord(maxValStart, 30), 1);
                //Draw max
                FastText.print(g, new Coord(maxValStart + 2, 15).sub(FastText.size("" + item.max_value()).mul(0f, 0.5f)),
                        "" + item.max_value());
                //Draw season status'
                g.line(new Coord(spStart, 0), new Coord(spStart, 30), 1);
                g.aimage(item.spring(), new Coord(spStart + 2, 15), 0, 0.5f);
                g.line(new Coord(suStart, 0), new Coord(suStart, 30), 1);
                g.aimage(item.summer(), new Coord(suStart + 2, 15), 0, 0.5f);
                g.line(new Coord(auStart, 0), new Coord(auStart, 30), 1);
                g.aimage(item.autumn(), new Coord(auStart + 2, 15), 0, 0.5f);
                g.line(new Coord(wiStart, 0), new Coord(wiStart, 30), 1);
                g.aimage(item.winter(), new Coord(wiStart + 2, 15), 0, 0.5f);
                g.chcolor();
            }

            @Override
            protected Object itemtooltip(final Coord c, final ForageData item) {
                if (c.isect(new Coord(30, 0), new Coord(maxName, 30))) {
                    return item.location();
                } else {
                    return null;
                }
            }

            @Override
            public void change(ForageData item) {
                //do nothing
            }
        };
        add(foragebox, new Coord(0, lbl.sz.y + 30 + 5));
        pack();
    }

    private int maxNameLength() {
        int max = 0;
        for (final ForageData data : ObjData.getForageables()) {
            max = Math.max(max, data.name().img.getWidth());
        }
        return max;
    }

    @Override
    protected void added() {
        super.added();
        prcattr = ui.sess.glob.getcattr("prc");
        expattr = ui.sess.glob.getcattr("explore");
        this.prc = prcattr.comp;
        this.explore = expattr.comp;
        perexp = prc * explore;
    }

    @Override
    public void tick(double dt) {
        super.tick(dt);
        if(prc != prcattr.comp || explore != expattr.comp) {
            prc = prcattr.comp;
            explore = expattr.comp;
            perexp = prc * explore;
        }
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    public void cdraw(GOut g) {
        super.cdraw(g);
        FastText.print(g, valc, "" + perexp);
    }
}

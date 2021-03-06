package hamster.ui.chr;


import com.google.common.flogger.FluentLogger;
import com.google.gson.Gson;
import haven.Button;
import haven.*;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.*;
import java.util.List;

/**
 * Credo Tree is a
 */
public class CredoTree extends Widget {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static final RichText.Foundry ifnd = new RichText.Foundry(Resource.remote(),
            java.awt.font.TextAttribute.FAMILY, "SansSerif", java.awt.font.TextAttribute.SIZE, 9).aa(true);
    private static final Tex bought = Resource.loadtex("custom/credos/states", 0);
    private static final Tex unlocked = Resource.loadtex("custom/credos/states", 1);
    private static final Tex locked = Resource.loadtex("custom/credos/states", 2);
    private static final List<CredoData> credoData = new ArrayList<>();
    private static final Coord csz = new Coord(90, 150);
    public static void init() {
        logger.atInfo().log("Loading Credo Data");
        final var gson = new Gson();
        try {
            final var credos = gson.fromJson(new FileReader("data/CredoTree.json5"), CredoData[].class);
            for(final var credo : credos) {
                credoData.add(credo);
                credo.init();
            }
        } catch (FileNotFoundException e) {
            logger.atSevere().withCause(e).log("Failed to load Credo data");
        }
    }

    private static class CredoData {
        int id;
        String name;
        Indir<Resource> res;
        Coord2d loc;
        List<Integer> parents;

        public void init() {
            this.res = Resource.remote().load(name);
        }
    }


    private interface CredoWidget {
        Coord c();
        Coord sz();
        CharWnd.Credo credo();
    }

    private static class Credo extends Widget implements CredoWidget{
        private Button buy;
        final CredoData data;
        CharWnd.Credo credo;
        private Tex name;
        private Tex tt;
        private Tex lvl;
        private Tex quests;

        public Credo(final CredoData data) {
            super(new Coord(110, 100));
            this.data = data;
        }

        private void mkbuy() {
            if (buy == null) {
                buy = new Button(58, "pursue", () -> ui.gui.chrwdg.wdgmsg("crpursue", credo.nm));
                add(buy, new Coord(26, 74));
            }
        }

        public void setSkill(final CharWnd.Credo credo) {
            this.credo = credo;
            if(credo.on) {
                lvl = Text.renderstroked(String.format("L %d/%d", credo.crl, credo.crlt)).tex();
                quests = Text.renderstroked(String.format("Q %d/%d", credo.crql, credo.crqlt)).tex();
            } else {
                if(lvl != null) {
                    lvl.dispose();
                    quests.dispose();
                }
                lvl = null;
                quests = null;
            }

            if (!credo.has && !credo.on)
                mkbuy();
            else if (buy != null) {
                ui.destroy(buy);
                buy = null;
            }
        }

        @Override
        public CharWnd.Credo credo() {
            return credo;
        }

        @Override
        public Coord c() {
            return c;
        }

        @Override
        public Coord sz() {
            return sz;
        }

        @Override
        public void draw(GOut g) {
            try {
                final Resource res = data.res.get();
                if (name == null) {
                    name = Text.renderstroked(res.layer(Resource.tooltip).t).tex();
                }
                //Draw name
                g.aimage(name, new Coord(sz.x / 2, 0), 0.5f, 0.0f);
                //Icon + Border
                final Tex tex = res.layer(Resource.imgc).tex();
                g.image(tex, sz.add(0, name.sz().y).div(2).sub(tex.sz().div(4)), tex.sz().div(2));
                if (credo != null) {
                    if (credo.has) {
                        g.image(bought, sz.add(0, name.sz().y).div(2).sub(tex.sz().div(4)));
                    } else {
                        g.image(unlocked, sz.add(0, name.sz().y).div(2).sub(tex.sz().div(4)));
                    }
                } else {
                    g.image(locked, sz.add(0, name.sz().y).div(2).sub(tex.sz().div(4)));
                }
                //If  has levels/quests draw them
                if(lvl != null) {
                    g.chcolor(new Color(192, 192, 192, 128));
                    g.frect(sz.add(0, name.sz().y).div(2), tex.sz().div(4));
                    g.chcolor();
                    g.aimage(lvl, sz.add(0, name.sz().y).div(2), 0.0f, -0.5f);
                    g.aimage(quests, sz.add(0, name.sz().y).div(2), 0.0f, -1.5f);
                }
                super.draw(g);
            } catch (Loading l) {
                //Skip until loaded
            }
        }

        private String rendertt() {
            final StringBuilder buf = new StringBuilder();
            final Resource res = data.res.get();
            buf.append("$b{$font[serif,16]{");
            buf.append(res.layer(Resource.tooltip).t);
            buf.append("}}\n\n\n");
            buf.append(res.layer(Resource.pagina).text);
            return (buf.toString());
        }

        @Override
        public Object tooltip(Coord c, Widget prev) {
            if (tt != null)
                return tt;
            try {
                tt = ifnd.render(rendertt(), 300).tex();
                return tt;
            } catch (Loading l) {
                //Skip until loaded
                return super.tooltip(c, prev);
            }
        }
    }

    private final Map<Integer, CredoWidget> id2credo = new HashMap<>();
    private final Map<String, Credo> name2credo = new HashMap<>();
    private Tex bg = null;

    public CredoTree() {
        super(csz.mul(11.5f, 7.0f));
        for (final CredoData data : credoData) {
            final Credo sk = new Credo(data);
            id2credo.put(data.id, sk);
            name2credo.put(data.name.substring(data.name.lastIndexOf('/') + 1), sk);
            add(sk, csz.mul(data.loc).floor());
        }
        pack();
    }

    private void buildBG() {
        final BufferedImage img = new BufferedImage(sz.x, sz.y, BufferedImage.TYPE_INT_ARGB);
        final Graphics g = img.getGraphics();
        final int mid = 17;
        final Map<Integer, Integer> rowsuby = new HashMap<>();
        for (final Credo sk : children(Credo.class)) {
            if (sk.data.parents.size() > 0) {
                final List<Integer> pars = new ArrayList<>(sk.data.parents);
                pars.sort((l, r) -> {
                    final CredoWidget lsk = id2credo.get(l);
                    final CredoWidget rsk = id2credo.get(r);
                    if ((rsk.credo() == null && lsk.credo() == null) ||
                            (rsk.credo() != null && lsk.credo() != null && rsk.credo().has == lsk.credo().has)) {
                        return 0;
                    } else if (lsk.credo() != null && lsk.credo().has) {
                        return -1;
                    } else {
                        return 1;
                    }
                });

                int sub = 0, suby = rowsuby.getOrDefault(sk.c.y, 0);
                for (final int pid : pars) {
                    final CredoWidget parent = id2credo.get(pid);
                    final Color col = parent.credo() == null ? Color.RED : parent.credo().on ? Color.ORANGE : parent.credo().has ? Color.GREEN : Color.RED;
                    g.setColor(col);
                    //Draw line up from our skill
                    g.drawLine(sk.c.x + sk.sz.x / 2 + sub, sk.c.y, sk.c.x + sk.sz.x / 2 + sub - 3, sk.c.y - 5);
                    g.drawLine(sk.c.x + sk.sz.x / 2 + sub, sk.c.y, sk.c.x + sk.sz.x / 2 + sub + 3, sk.c.y - 5);
                    g.drawLine(sk.c.x + sk.sz.x / 2 + sub, sk.c.y, sk.c.x + sk.sz.x / 2 + sub, sk.c.y - (mid - suby));
                    //Draw line over to the right column center
                    g.drawLine(sk.c.x + sk.sz.x / 2 + sub, sk.c.y - (mid - suby), parent.c().x + parent.sz().x / 2 + sub, sk.c.y - (mid - suby));
                    //Draw up to parent
                    g.drawLine(parent.c().x + parent.sz().x / 2 + sub, sk.c.y - (mid - suby),
                            parent.c().x + parent.sz().x / 2 + sub, parent.c().y + parent.sz().y);
                    sub -= 3;
                    suby -= 2;
                }
                rowsuby.put(sk.c.y, suby);
            }
        }
        g.dispose();
        bg = new TexI(img);
    }

    public void update(List<CharWnd.Credo> sks) {
        sks.forEach((skill) -> {
            if (name2credo.containsKey(skill.nm)) {
                name2credo.get(skill.nm).setSkill(skill);
            }
        });
        buildBG();
    }

    @Override
    public void draw(GOut g) {
        if (bg != null)
            g.image(bg, Coord.z);
        super.draw(g);
    }
}

package hamster.ui;

import hamster.data.HighlightData;
import hamster.data.ShortenData;
import hamster.gob.Alerted;
import hamster.gob.Deleted;
import hamster.gob.Hidden;
import hamster.gob.Tag;
import hamster.gob.sprites.*;
import hamster.ui.map.ObjPreview;
import hamster.util.JobSystem;
import haven.*;

import java.util.*;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import static haven.MCache.tilesz;

/**
 * Extension of the MapView custom context menu for slave control
 * Menu Setup:
 * On Gobs:
 * Mark for party
 * Mark for script
 * Highlight
 * Hide
 * Sound
 * Delete
 * Delete This
 * Add Master
 * Add Slave
 * Slaves -> { All, [slave-1-name]...} ->  {
 * Go to
 * Follow [On other humans or animals]
 * Wait   [On self]
 * Lift
 * Shoot  [On other humans/animals]
 * Attack [On other humans/animals]
 * Destroy
 * Farm this          [On farm crops, ready for harvest]
 * Farm alike crop    [On farm crops, ready for harvest]
 * Farm area          [On farm crops, ready for harvest]
 * }
 * <p>
 * On a tile:
 * Mark for party
 * Mark for script
 * Slaves -> { All, [slave-1-name]... } -> {
 * Go to
 * Place lifted item
 * Shoot here
 * }
 */
public class MapViewExt {
    private static final String DISPATCH = "(dispatch %s %s %s)";
    private static final Executor worker = Executors.newSingleThreadExecutor();
    private static final Map<String, String> gcmdmap = new HashMap<>();

    static {
        gcmdmap.put("Go to", "go-to-gob");
        gcmdmap.put("Follow", "follow-gob");
        gcmdmap.put("Lift", "lift-gob");
        gcmdmap.put("Destroy", "destroy-gob");
        gcmdmap.put("Chop", "chop-gob");
        gcmdmap.put("Chip", "chip-gob");
    }

    private interface SlaveAct {
        void run(final List<Long> slaves);
    }

    private final MapView mv;
    private final Map<Long, String> id2name = new HashMap<>();
    private final Set<Long> masters = new HashSet<>();
    private final List<Long> slaves = new ArrayList<>();

    public MapViewExt(final MapView mv) {
        this.mv = mv;
    }

    private void addSlave(final long bid) {
        slaves.add(bid);
    }

    private void remSlave(final long bid) {
        slaves.remove(bid);
    }

    private void addMaster(final long bid) {
        masters.add(bid);
    }

    private void remMaster(final long bid) {
        masters.remove(bid);
    }

    public boolean isMaster(final long gob) {
        return masters.contains(gob);
    }

    private void send(final ChatUI.EntryChannel channel, final List<Long> slaves, final String cmd) {
        worker.execute(() -> {
            for (final long slave : slaves) {
                channel.send(String.format(cmd, slave));
                try {
                    Thread.sleep(350);
                } catch (InterruptedException e) {
                    break;
                }
            }
        });
    }

    private void gobSlaveActMenu(final List<Long> slaves, final Gob g) {
        if (mv.ui.gui.chat.party != null) {
            final List<String> opts = new ArrayList<>();
            final String name = g.name();
            opts.add("Go to");
            if(g.hasTag(Tag.HUMAN) || g.hasTag(Tag.ANIMAL)) {
                opts.add("Follow");
            }
            opts.add("Lift");
            opts.add("Destroy");
            if (name.startsWith("gfx/terobjs/trees/")) {
                opts.add("Chop");
            } else if (name.startsWith("gfx/terobjs/bumblings")) {
                opts.add("Chip");
            }

            final FlowerMenu menu = new FlowerMenu((selection) -> {
                final String command = gcmdmap.getOrDefault(selection, null);
                if (command != null) {
                    send(mv.ui.gui.chat.party, slaves, String.format(DISPATCH, "%d", command,
                            String.format("(number %d) (coord2d %f %f)", g.id, g.rc.x, g.rc.y)));
                }
            }, opts.toArray(new String[0]));
            mv.ui.gui.add(menu, mv.ui.mc);
        } else {
            mv.ui.gui.msg("You need to be in a party to transmit slave messages");
        }
    }

    private void tileSlaveActMenu(final Coord2d mc, final List<Long> slaves) {
        if (mv.ui.gui.chat.party != null) {
            final FlowerMenu menu = new FlowerMenu((selection) -> {
                switch (selection) {
                    case "Go to" -> send(mv.ui.gui.chat.party, slaves, String.format(DISPATCH, "%d", "go-to",
                            String.format("(coord2d %f %f)", mc.x, mc.y)));
                    case "Dig" -> send(mv.ui.gui.chat.party, slaves, String.format(DISPATCH, "%d", "dig",
                            String.format("(coord2d %f %f)", mc.x, mc.y)));
                    case "Place lifted item" -> send(mv.ui.gui.chat.party, slaves, String.format(DISPATCH, "%d", "place-item",
                            String.format("(coord2d %f %f)", mc.x, mc.y)));
                    case "Shoot here" -> send(mv.ui.gui.chat.party, slaves, String.format(DISPATCH, "%d", "shoot-at-tile",
                            String.format("(coord2d %f %f)", mc.x, mc.y)));
                }
            }, "Go to", "Dig", "Place lifted item", "Shoot here");
            mv.ui.gui.add(menu, mv.ui.mc);
        } else {
            mv.ui.gui.msg("You need to be in a party to transmit slave messages");
        }
    }

    public void selectSlavesAndDo(final SlaveAct act) {
        final ArrayList<String> opts = new ArrayList<>();
        final Map<String, Long> name2id = new HashMap<>();
        final List<Long> selslaves = new ArrayList<>();

        if (slaves.size() > 1) {
            opts.add("All");
            for (final long slave : slaves) {
                name2id.put(id2name.get(slave), slave);
                opts.add(id2name.get(slave));
            }

            final FlowerMenu modmenu = new FlowerMenu((selection) -> {
                if (selection.equals("All")) {
                    selslaves.addAll(slaves);
                } else {
                    selslaves.add(name2id.get(selection));
                }
                act.run(selslaves);
            }, opts.toArray(new String[0]));
            mv.ui.gui.add(modmenu, mv.ui.mc);
        } else {
            selslaves.addAll(slaves);
            act.run(selslaves);
        }
    }

    public void showSpecialMenu(final Gob g) {
        if (!g.virtual) {
            g.resname().ifPresent((name) -> {
                final ArrayList<String> opts = new ArrayList<>();
                opts.add("Mark for party");
                opts.add("Mark for script");
                if (mv.ui.gui.curtar != g.id)
                    opts.add("Target for party");
                else
                    opts.add("Cancel Target for party");
                opts.add(!HighlightData.isHighlighted(name) ? "Highlight" : "Remove Highlight");
                opts.add(Hidden.isHidden(name) ? "Unhide" : "Hide");
                opts.add(Alerted.shouldAlert(name) ? "Remove Sound" : "Add Sound");
                opts.add(ShortenData.getShortenScaler(name).isEmpty() ? "Shorten" : "Remove Shorten");
                opts.add("Delete");
                opts.add("Delete this");
                opts.add("Clone in previewer");
                if (g.findol(DamageText.id) != null) {
                    opts.add("Delete Damage Text");
                }
                if (g.hasTag(Tag.HUMAN) && g.id != mv.plgob) {
                    if (!masters.contains(g.id))
                        opts.add("Add as master");
                    else
                        opts.add("Remove master");
                    if (!slaves.contains(g.id))
                        opts.add("Add as slave");
                    else
                        opts.add("Remove slave");
                }
                if (slaves.size() > 0)
                    opts.add("Slaves");


                final FlowerMenu modmenu = new FlowerMenu((selection) -> {
                    switch (selection) {
                        case "Mark for party": //Mark for party
                            g.mark(2000);
                            for (Widget wdg = mv.ui.gui.chat.lchild; wdg != null; wdg = wdg.prev) {
                                if (wdg instanceof ChatUI.PartyChat) {
                                    final ChatUI.PartyChat chat = (ChatUI.PartyChat) wdg;
                                    chat.send(String.format(Mark.CHAT_FMT, g.id, 2000));
                                }
                            }
                            break;
                        case "Mark for script": //Mark for script
                            mv.ui.sess.details.context.dispatchmsg(mv, "click-gob", g);
                            break;
                        case "Cancel Target for party": {
                            final Gob old = mv.ui.sess.glob.oc.getgob(mv.ui.gui.curtar);
                            if (old != null) {
                                final Gob.Overlay ol = old.findol(TargetSprite.id);
                                if (ol != null) {
                                    ((TargetSprite) ol.spr).rem();
                                }
                            }
                            mv.ui.gui.curtar = 0;
                            for (Widget wdg = mv.ui.gui.chat.lchild; wdg != null; wdg = wdg.prev) {
                                if (wdg instanceof ChatUI.PartyChat) {
                                    final ChatUI.PartyChat chat = (ChatUI.PartyChat) wdg;
                                    chat.send(String.format(TargetSprite.target_pat, 0));
                                }
                            }
                        }
                        break;
                        case "Target for party": {
                            final Gob old = mv.ui.sess.glob.oc.getgob(mv.ui.gui.curtar);
                            if (old != null) {
                                final Gob.Overlay ol = old.findol(TargetSprite.id);
                                if (ol != null) {
                                    ((TargetSprite) ol.spr).rem();
                                }
                            }
                            mv.ui.gui.curtar = g.id;
                            g.queueDeltas(Collections.singletonList((gob) -> gob.addol(new Gob.Overlay(gob, TargetSprite.id, new TargetSprite(gob)))));
                            if (mv.ui.gui.chat.party != null)
                                mv.ui.gui.chat.party.send(String.format(TargetSprite.target_pat, g.id));
                        }
                        break;
                        case "Delete Damage Text":
                            if (g.findol(DamageText.id) != null)
                                ((DamageText) g.findol(DamageText.id).spr).rem();
                            break;
                        case "Highlight": //Highlight for yourself
                            HighlightData.add(name);
                            break;
                        case "Remove Highlight":
                            HighlightData.remove(name);
                            break;
                        case "Unhide":
                            Hidden.remove(name);
                            OCache.MessageBus.send(new OCache.UnhideGobsByName(name));
                            break;
                        case "Hide":
                            Hidden.add(name);
                            OCache.MessageBus.send(new OCache.HideGobsByName(name));
                            break;
                        case "Remove Sound":
                            Alerted.remove(name);
                            break;
                        case "Add Sound":
                            mv.ui.gui.add(new SoundSelector(name), mv.ui.mc);
                            break;
                        case "Shorten":
                            ShortenData.add(name, 0.75f);
                            break;
                        case "Remove Shorten":
                            ShortenData.rem(name);
                            break;
                        case "Delete":
                            Deleted.add(name);
                            OCache.MessageBus.send(new OCache.RemoveGobByRes(name));
                            break;
                        case "Delete this":
                            g.dispose();
                            mv.ui.sess.glob.oc.remove(g);
                            break;
                        case "Clone in previewer":
                            mv.ui.gui.add(new ObjPreview(g));
                            break;
                        case "Add as master":
                            addMaster(g.id);
                            break;
                        case "Remove master":
                            remMaster(g.id);
                            break;
                        case "Add as slave":
                            addSlave(g.id);
                            id2name.put(g.id, g.gobname());
                            break;
                        case "Remove slave":
                            remSlave(g.id);
                            id2name.remove(g.id);
                            break;
                        case "Slaves":
                            selectSlavesAndDo((slaves) -> gobSlaveActMenu(slaves, g));
                            break;
                    }
                }, opts.toArray(new String[0]));
                mv.ui.gui.add(modmenu, mv.ui.mc);
            });
        } else {
            final ArrayList<String> opts = new ArrayList<>();
            opts.add("Delete This");
            final FlowerMenu modmenu = new FlowerMenu((selection) -> {
                if(selection.equals("Delete This")) {
                    g.dispose();
                    mv.ui.sess.glob.oc.remove(g);
                }
            }, opts.toArray(new String[0]));
            mv.ui.gui.add(modmenu, mv.ui.mc);
        }
    }


    public void showSpecialMenu(final Coord2d mc) {
        final ArrayList<String> opts = new ArrayList<>();
        opts.add("Mark for party");
        opts.add("Mark for script");
        opts.add("Mark tile with number");

        if (slaves.size() > 0)
            opts.add("Slaves");

        final FlowerMenu modmenu = new FlowerMenu((selection) -> {
            switch (selection) {
                case "Mark for party" -> { //Mark for party
                    //Translate to Grid + Gird Offset
                    final Coord tc = mc.floor(tilesz);
                    final Coord2d tcd = mc.div(tilesz);

                    //Send it to party chat
                    mv.ui.sess.glob.map.getgridto(tc).ifPresent(grid -> {
                        final Coord2d offset = tcd.sub(new Coord2d(grid.ul));
                        if(mv.ui.gui.chat.party != null)
                           mv.ui.gui.chat.party.send(String.format(Mark.CHAT_TILE_FMT, grid.id, offset.x, offset.y));
                        else {
                            //Mark for just yourself
                            mv.ui.sess.glob.loader.defer(() -> {
                                final Gob g = mv.ui.sess.glob.oc.new ModdedGob(mc, 0);
                                g.addol(new Gob.Overlay(g, Mark.id, new Mark(2000)));
                                mv.ui.sess.glob.oc.add(g);
                            }, null);
                        }
                    });
                }
                case "Mark for script" -> mv.ui.sess.details.context.dispatchmsg(mv, "click-tile", mc);
                case "Mark tile with number" -> {
                    final var sopts = new String[] { "1", "2", "3", "4", "5", "6", "7", "8", "9" };
                    final var submenu = new FlowerMenu((sel) -> {
                        mv.ui.sess.glob.loader.defer(() -> {
                            final Gob g = mv.ui.sess.glob.oc.new Virtual(mc, Math.toRadians(270));
                            g.canclick = true;
                            synchronized (g) {
                                g.addol(new SimpleTextSprite(g, sel));
                                g.addol(new AggroMark(g));
                            }
                            mv.ui.sess.glob.oc.add(g);
                        }, null);
                    }, sopts);
                    mv.ui.gui.add(submenu, mv.ui.mc);
                }
                case "Slaves" -> selectSlavesAndDo((slaves) -> tileSlaveActMenu(mc, slaves));
            }
        }, opts.toArray(new String[0]));
        mv.ui.gui.add(modmenu, mv.ui.mc);
    }
}

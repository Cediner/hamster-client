package hamster.ui.Timer;

import hamster.GlobalSettings;
import hamster.data.TimerData;
import hamster.util.ObservableListener;
import haven.*;

import java.util.Collection;

public class TimerWdg extends Widget implements ObservableListener<TimerData.TimerInstance> {
    private static final Resource timersfx = Resource.local().loadwait("custom/sfx/timer");
    private final static int height = 31;
    private final static Coord timec = UI.scale(new Coord(200, 8));

    private class TimerInstWdg extends Widget {
        final TimerData.TimerInstance inst;
        private double lastTime; // Last update tick
        private double timeLeft; // Glob Time

        private TimerInstWdg(final TimerData.TimerInstance inst, final double start) {
            this.inst = inst;
            lastTime = start;
            timeLeft = (time.duration * Config.SERVER_TIME_RATIO) - (start - inst.start);
            final int width = FastText.textw(timeFormat((long)(timeLeft / Config.SERVER_TIME_RATIO)));
            add(new Button(UI.scale(50), "Cancel", this::cancel), timec.add(width + UI.scale(2), -UI.scale(5)));
            pack();
        }

        @Override
        public void draw(GOut g) {
            FastText.print(g, timec, timeFormat((long)(timeLeft / Config.SERVER_TIME_RATIO)));
            super.draw(g);
        }

        @Override
        public void tick(double dt) {
            final double gtime = ui.sess.glob.globtime();
            timeLeft -= (gtime - lastTime);
            lastTime = gtime;
            if (timeLeft <= 0) {
                ui.gui.add(new TimerDoneWindow(time.name), new Coord(50, 50));
                Audio.play(timersfx, (GlobalSettings.TIMERVOL.get() / 1000f));
                time.finish(inst);
            }
            super.tick(dt);
        }

        private void cancel() {
            time.finish(inst);
        }
    }

    public TimerData.Timer time;
    private final int base_height;

    /*
     *
     * <timer-name>                            <duration> <start-btn> <X-button>
     *
     */
    TimerWdg(TimerData.Timer t) {
        time = t;
        final int spacer = UI.scale(2);
        final var namelbl = new Label(time.name);
        final var durlbl = new Label(timeFormat(time.duration));
        final var strbtn = new Button(UI.scale(50), "Start", this::start);
        add(namelbl, UI.scale(5, 8));
        add(durlbl, timec);
        add(strbtn, durlbl.c.add(durlbl.sz.x + spacer, -UI.scale(5)));
        add(new Button(UI.scale(20), "X", this::delete), strbtn.c.add(strbtn.sz.x + spacer, 0));
        pack();
        base_height = sz.y;
    }

    @Override
    protected void added() {
        super.added();
        time.listen(this);
    }

    private static String timeFormat(long ts) {
        return String.format("%03d:%02d:%02d", (int) (ts / 3600), (int) ((ts % 3600) / 60), (int) (ts % 60));
    }

    public void start() {
        time.makeInstance((long) ui.sess.glob.globtime());
    }

    public void delete() {
        TimerData.remTimer(time);
    }

    @Override
    public void init(Collection<TimerData.TimerInstance> base) {
        for (final TimerData.TimerInstance inst : base) {
            add(new TimerInstWdg(inst, ui.sess.glob.globtime()));
        }
        pack();
        parent.pack();
    }

    @Override
    public void added(TimerData.TimerInstance item) {
        add(new TimerInstWdg(item, ui.sess.glob.globtime()));
        pack();
        parent.pack();
    }

    @Override
    public void remove(TimerData.TimerInstance item) {
        ui.destroy(find(item));
        pack();
        parent.pack();
    }

    public void pack() {
        int y = base_height;
        Widget next;

        for (Widget wdg = child; wdg != null; wdg = next) {
            next = wdg.next;
            if (wdg instanceof TimerInstWdg) {
                wdg.c = new Coord(wdg.c.x, y);
                y += wdg.sz.y;
            }
        }
        super.pack();
    }

    public TimerInstWdg find(TimerData.TimerInstance t) {
        Widget next;

        for (Widget wdg = child; wdg != null; wdg = next) {
            next = wdg.next;
            if (wdg instanceof TimerInstWdg) {
                TimerInstWdg tw = (TimerInstWdg) wdg;
                if (tw.inst == t)
                    return tw;
            }
        }

        return null;
    }

    @Override
    protected void removed() {
        time.unlisten(this);
    }

    private static class TimerDoneWindow extends Window {
        private TimerDoneWindow(String timername) {
            super(new Coord(300, 130), "Hooray!", "Hooray!");

            Label lbltimer = new Label(timername);
            add(lbltimer, new Coord(300 / 2 - lbltimer.sz.x / 2, 20));

            Label lblinf = new Label("has finished running");
            add(lblinf, new Coord(300 / 2 - lblinf.sz.x / 2, 50));

            add(new Button(60, "Close") {
                @Override
                public void click() {
                    parent.reqdestroy();
                }
            }, new Coord(300 / 2 - 60 / 2, 90));
        }

        public void close() {
            ui.destroy(this);
        }
    }
}

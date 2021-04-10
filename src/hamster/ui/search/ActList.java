package hamster.ui.search;

import hamster.ui.search.filters.AndFilter;
import hamster.ui.search.filters.Filter;
import hamster.util.msg.MailBox;
import hamster.util.msg.Message;
import haven.*;

import java.util.*;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class ActList extends Listbox<ActList.ActItem> {
    public static final Filter includeall = (itm) -> true;
    private static final Text.Foundry font = new Text.Foundry(Text.serif, 15).aa(true);
    private final Map<MenuGrid.Pagina, ActItem> map = new HashMap<>();
    private final List<ActItem> items = new ArrayList<>();
    private List<ActItem> filtered = new ArrayList<>();
    private Filter currentFilter = includeall;
    private MailBox<ActListMail> mail = null;
    private final Map<Pattern, Function<Matcher, Optional<Filter>>> filters = new HashMap<>();

    private static abstract class ActListMail extends Message {
        public abstract void apply(final ActList me);
    }
    private static class AddItemMail extends ActListMail {
        private final ActItem item;
        public AddItemMail(final ActItem item) { this.item = item; }

        @Override
        public void apply(ActList me) {
            if(me.currentFilter.included(item)) {
                me.filtered.add(item);
                me.filtered.sort(Comparator.comparing(l -> l.name.text));
            }
        }
    }
    private static class RemoveItemMail extends ActListMail {
        private final ActItem item;
        public RemoveItemMail(final ActItem item) { this.item = item; }

        @Override
        public void apply(ActList me) {
            me.filtered.remove(item);
        }
    }

    public ActList(int w, int h) {
        super(w, h, font.height() + 2);
    }

    @Override
    protected void attached() {
        super.attached();
        mail = new MailBox<>(ui.office);
    }

    public void add(MenuGrid.Pagina pagina) {
        ActItem item = new ActItem(pagina);
        map.put(pagina, item);
        synchronized (items) {
            items.add(item);
        }
        mail.mail(new AddItemMail(item));
    }

    public void remove(MenuGrid.Pagina pagina) {
        ActItem item = map.remove(pagina);
        synchronized (items) {
            items.remove(item);
        }
        mail.mail(new RemoveItemMail(item));
    }

    public void add(final String filterpat, final Function<Matcher, Optional<Filter>> mkfun) {
        filters.put(Pattern.compile(filterpat), mkfun);
    }

    public void filter(final String search) {
        final List<Filter> flts = new ArrayList<>();
        for(final var filter : search.split(";")) {
            for(final var flt : filters.keySet()) {
                final var match = flt.matcher(filter);
                if(match.find()) {
                    filters.get(flt).apply(match).ifPresent(flts::add);
                    break;
                }
            }
        }
        final Filter flt = new AndFilter(flts);
        filter(flt);
    }

    public synchronized void filter(final Filter filter) {
        this.currentFilter = filter;
        filtered = items.parallelStream()
                .filter(filter::included)
                .sorted(Comparator.comparing(l -> l.name.text))
                .collect(Collectors.toList());
    }

    @Override
    protected ActItem listitem(int i) {
        return filtered.get(i);
    }

    @Override
    protected int listitems() {
        return filtered.size();
    }

    @Override
    protected void drawbg(GOut g) {
        g.chcolor(0, 0, 0, 128);
        g.frect(Coord.z, sz);
        g.chcolor();
    }

    @Override
    protected Object itemtooltip(Coord c, ActItem item) {
        return item.pagina.button().rendertt(true);
    }

    @Override
    protected void drawitem(GOut g, ActItem item, int i) {
        g.image(item.icon, Coord.z);
        g.aimage(item.name.tex(), new Coord(itemh + 5, itemh / 2), 0, 0.5);
    }

    @Override
    public void tick(double dt) {
        super.tick(dt);
        mail.processMail(mail -> mail.apply(this));
    }

    private static final OwnerContext.ClassResolver<ActItem> ctxr = new OwnerContext.ClassResolver<ActItem>()
            .add(Glob.class, p -> p.parent().ui.sess.glob)
            .add(Session.class, p -> p.parent().ui.sess);
    public class ActItem implements ItemInfo.Owner {
        public final MenuGrid.Pagina pagina;
        private List<ItemInfo> info = null;
        public final Text name;
        public final Tex icon;

        private ActItem(MenuGrid.Pagina pagina) {
            this.pagina = pagina;
            this.name = font.render(this.pagina.act().name);
            this.icon = new TexI(PUtils.convolvedown(pagina.res.get().layer(Resource.imgc).img,
                    new Coord(itemh, itemh), CharWnd.iconfilter));
        }

        public ActList parent() {
            return ActList.this;
        }

        public List<ItemInfo> info() {
            if(info == null)
                info = ItemInfo.buildinfo(this, pagina.rawinfo);
            return(info);
        }

        public <T> Optional<T> getinfo(Class<T> type) {
            try {
                for (final ItemInfo info : info()) {
                    if (type.isInstance(info)) {
                        return Optional.of(type.cast(info));
                    }
                }
                return Optional.empty();
            } catch (Exception e) {
                return Optional.empty();
            }
        }

        public <T> Optional<T> getinfo(Class<T> type, List<ItemInfo> infolst) {
            try {
                for (final ItemInfo info : infolst) {
                    if (type.isInstance(info)) {
                        return Optional.of(type.cast(info));
                    }
                }
                return Optional.empty();
            } catch (Exception e) {
                return Optional.empty();
            }
        }

        public <T> List<T> getinfos(Class<T> type) {
            final List<T> infos = new ArrayList<>();
            try {
                for (final ItemInfo info : info()) {
                    if (type.isInstance(info)) {
                        infos.add(type.cast(info));
                    }
                }
                return infos;
            } catch (Exception e) {
                return infos;
            }
        }

        public <T> T context(Class<T> cl) {return(ctxr.context(cl, this));}
    }
}

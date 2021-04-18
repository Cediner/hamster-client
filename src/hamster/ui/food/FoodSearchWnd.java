package hamster.ui.food;

import hamster.data.food.FoodData;
import hamster.ui.core.layout.LinearGrouping;
import hamster.ui.food.filters.*;
import hamster.ui.food.sort.*;
import haven.*;
import haven.Button;
import haven.Scrollbar;
import haven.Window;

import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.stream.Collectors;

import static hamster.data.food.FoodData.FepType.*;

public class FoodSearchWnd extends Window {
    private static final Coord iconc, iconsz;
    private static final Coord namec, iconnamesz;
    private static final Coord statsz;
    private static final Coord strc, agic, intc, conc, perc, chac, dexc, wilc, psyc;
    private static final Color strcol, agicol, intcol, concol, percol, chacol, dexcol, wilcol, psycol;
    private static final Coord twoplusoffset;
    private static final Coord ingc, ingsz;
    private static final Coord fepperhunc, fepperhunsz;
    private static final Coord tfepc, tfepsz;
    private static final Coord hunc, hunsz;
    private static final Coord energyc, energysz;
    private static final Coord offset = UI.scale(1, 1);
    private static final Map<FoodData.FepType, Coord> fepcmap = new HashMap<>();
    static {
        final var spacer = UI.scale(2, 0);
        iconc = new Coord(0, 0);
        iconsz = Inventory.sqsz;
        namec = iconc.add(iconsz.x, 0);
        iconnamesz = new Coord(iconsz.x + FoodData.longestname, iconsz.y).add(spacer);
        statsz = Inventory.sqsz.max(FastText.size("00.00")).add(spacer);
        strc = iconnamesz.add(0, -iconnamesz.y);
        agic = strc.add(statsz.x, 0);
        intc = agic.add(statsz.x, 0);
        conc = intc.add(statsz.x, 0);
        perc = conc.add(statsz.x, 0);
        chac = perc.add(statsz.x, 0);
        dexc = chac.add(statsz.x, 0);
        wilc = dexc.add(statsz.x, 0);
        psyc = wilc.add(statsz.x, 0);
        twoplusoffset = new Coord(0, statsz.y / 2).add(offset);
        ingc = psyc.add(statsz.x, 0);
        ingsz = new Coord(UI.scale(300), Inventory.sqsz.y).add(spacer);
        fepperhunc = ingc.add(ingsz.x, 0);
        fepperhunsz = Inventory.sqsz.max(FastText.size("Fep/Hunger")).add(spacer);
        tfepc = fepperhunc.add(fepperhunsz.x, 0);
        tfepsz = Inventory.sqsz.max(FastText.size("Total Fep")).add(spacer);
        hunc = tfepc.add(tfepsz.x, 0);
        hunsz = Inventory.sqsz.max(FastText.size("Hunger")).add(spacer);
        energyc = hunc.add(hunsz.x, 0);
        energysz = Inventory.sqsz.max(FastText.size("Energy")).add(spacer);

        strcol = new Color(191, 151, 148);
        agicol = new Color(154, 149, 185);
        intcol = new Color(157, 183, 185);
        concol = new Color(194, 154, 180);
        percol = new Color(228, 191, 152);
        chacol = new Color(155, 238, 177);
        dexcol = new Color(254, 253, 204);
        wilcol = new Color(228, 243, 143);
        psycol = new Color(196, 141, 253);

        fepcmap.put(STR, strc);
        fepcmap.put(FoodData.FepType.STR2, strc);
        fepcmap.put(FoodData.FepType.AGI, agic);
        fepcmap.put(FoodData.FepType.AGI2, agic);
        fepcmap.put(INT, intc);
        fepcmap.put(FoodData.FepType.INT2, intc);
        fepcmap.put(FoodData.FepType.CON, conc);
        fepcmap.put(FoodData.FepType.CON2, conc);
        fepcmap.put(FoodData.FepType.PER, perc);
        fepcmap.put(FoodData.FepType.PER2, perc);
        fepcmap.put(FoodData.FepType.CHA, chac);
        fepcmap.put(FoodData.FepType.CHA2, chac);
        fepcmap.put(FoodData.FepType.DEX, dexc);
        fepcmap.put(FoodData.FepType.DEX2, dexc);
        fepcmap.put(FoodData.FepType.WIL, wilc);
        fepcmap.put(FoodData.FepType.WIL2, wilc);
        fepcmap.put(FoodData.FepType.PSY, psyc);
        fepcmap.put(FoodData.FepType.PSY2, psyc);
    }

    private static class FoodList extends Listbox<FoodData> {
        private List<FoodData> filter;

        public FoodList() {
            super(energyc.add(energysz.x + Scrollbar.sflarp.sz().x, 0).x, 20,  Inventory.sqsz.add(1,1).y);
            filter = FoodData.foods;
        }

        public void apply(final Filter flt, final Sort sort) {
            filter = FoodData.foods.parallelStream().filter(flt::included).sorted(sort).collect(Collectors.toList());
        }

        public void apply(final Sort sort) {
            filter = filter.parallelStream().sorted(sort).collect(Collectors.toList());
        }

        @Override
        protected FoodData listitem(int i) {
            return filter.get(i);
        }

        @Override
        protected int listitems() {
            return filter.size();
        }

        @Override
        protected void drawitem(GOut g, FoodData item, int i) {
            // Draw the icon
            final Indir<Resource> res = Resource.remote().load(item.resourceName);
            try {
                final Resource ires = res.get();
                if (ires.layer(Resource.imgc) != null) {
                    g.image(ires.layer(Resource.imgc).tex(), offset, iconsz);
                }
            } catch (Loading ignored) {}
            // Draw the name
            FastText.aprint(g, namec.add(0, statsz.y / 2).add(offset), 0, 0.5f, item.itemName);
            g.line(new Coord(iconnamesz.x, 0), iconnamesz, 1);
            // Draw stat bgs
            g.chcolor(strcol);
            g.frect(strc, statsz);
            g.line(strc, new Coord(strc.x, statsz.y), 1);
            g.chcolor(agicol);
            g.frect(agic, statsz);
            g.line(agic, new Coord(agic.x, statsz.y), 1);
            g.chcolor(intcol);
            g.frect(intc, statsz);
            g.line(intc, new Coord(intc.x, statsz.y), 1);
            g.chcolor(concol);
            g.frect(conc, statsz);
            g.line(conc, new Coord(conc.x, statsz.y), 1);
            g.chcolor(percol);
            g.frect(perc, statsz);
            g.line(perc, new Coord(perc.x, statsz.y), 1);
            g.chcolor(chacol);
            g.frect(chac, statsz);
            g.line(chac, new Coord(chac.x, statsz.y), 1);
            g.chcolor(dexcol);
            g.frect(dexc, statsz);
            g.line(dexc, new Coord(dexc.x, statsz.y), 1);
            g.chcolor(wilcol);
            g.frect(wilc, statsz);
            g.line(wilc, new Coord(wilc.x, statsz.y), 1);
            g.chcolor(psycol);
            g.frect(psyc, statsz);
            g.line(psyc, new Coord(psyc.x, statsz.y), 1);
            // Draw the stats it gives
            g.chcolor(Color.BLACK);
            for(final var fep : item.feps) {
                final var c = fepcmap.get(fep.type);
                final Coord offset = switch (fep.type) {
                    case STR2, AGI2, INT2, CON2, PER2, CHA2, PSY2, WIL2, DEX2 -> twoplusoffset;
                    default -> FoodSearchWnd.offset;
                };
                FastText.aprintf(g, c.add(offset).add(statsz.div(2)), 0.5, 1,"%.2f", fep.value);
            }
            g.chcolor(Color.WHITE);
            //Draw ingredients
            item.ingredientsText().ifPresent(tex -> g.image(tex, ingc.add(offset)));
            g.line(ingc, ingc.add(0, ingsz.y), 1);
            //Draw Fep/Hunger
            FastText.aprintf(g, fepperhunc.add(offset).add(fepperhunsz.div(2)), 0.5, 0.5, "%.2f", item.fepPerHunger());
            g.line(fepperhunc, fepperhunc.add(0, fepperhunsz.y), 1);
            //Draw Total FEP
            FastText.aprintf(g, tfepc.add(offset).add(tfepsz.div(2)), 0.5, 0.5,"%.2f", item.totalFep);
            g.line(tfepc, tfepc.add(0, tfepsz.y), 1);
            //Draw Hunger
            FastText.aprintf(g, hunc.add(offset).add(hunsz.div(2)), 0.5, 0.5,"%.2f", item.hunger);
            g.line(hunc, hunc.add(0, hunsz.y), 1);
            //Draw Energy
            FastText.aprintf(g, energyc.add(offset).add(energysz.div(2)), 0.5, 0.5, "%d%%", item.energy);
            g.line(energyc, energyc.add(0, energysz.y), 1);
            //Draw main borders
            g.rect(Coord.z, g.sz());
        }
    }

    private static class Header extends Widget {
        private final FoodSearchWnd foodwnd;
        private Sort sortmethod;

        public Header(final FoodSearchWnd foodwnd) {
            super(energyc.add(energysz.x, Inventory.sqsz.add(1,1).y));
            this.foodwnd = foodwnd;
            sortmethod = new FieldSort(SortMethod.Name, Direction.ASC, (o1, o2) -> o1.itemName.compareTo(o2.itemName));
        }

        @Override
        public void draw(GOut g) {
            // Draw the name
            FastText.aprint(g, namec.add(0, statsz.y / 2).add(offset), 0, 0.5f, "Name");
            g.line(new Coord(iconnamesz.x, 0), iconnamesz, 1);
            // Draw stat bgs
            FastText.aprint(g, strc.add(0, statsz.y / 2).add(offset), 0, 0.5f, "Str");
            g.line(strc, new Coord(strc.x, statsz.y), 1);
            FastText.aprint(g, agic.add(0, statsz.y / 2).add(offset), 0, 0.5f, "Agi");
            g.line(agic, new Coord(agic.x, statsz.y), 1);
            FastText.aprint(g, intc.add(0, statsz.y / 2).add(offset), 0, 0.5f, "Int");
            g.line(intc, new Coord(intc.x, statsz.y), 1);
            FastText.aprint(g, conc.add(0, statsz.y / 2).add(offset), 0, 0.5f, "Con");
            g.line(conc, new Coord(conc.x, statsz.y), 1);
            FastText.aprint(g, perc.add(0, statsz.y / 2).add(offset), 0, 0.5f, "Per");
            g.line(perc, new Coord(perc.x, statsz.y), 1);
            FastText.aprint(g, chac.add(0, statsz.y / 2).add(offset), 0, 0.5f, "Cha");
            g.line(chac, new Coord(chac.x, statsz.y), 1);
            FastText.aprint(g, dexc.add(0, statsz.y / 2).add(offset), 0, 0.5f, "Dex");
            g.line(dexc, new Coord(dexc.x, statsz.y), 1);
            FastText.aprint(g, wilc.add(0, statsz.y / 2).add(offset), 0, 0.5f, "Wil");
            g.line(wilc, new Coord(wilc.x, statsz.y), 1);
            FastText.aprint(g, psyc.add(0, statsz.y / 2).add(offset), 0, 0.5f, "Psy");
            g.line(psyc, new Coord(psyc.x, statsz.y), 1);
            //Draw ingredients
            FastText.aprint(g, ingc.add(0, statsz.y / 2).add(offset), 0, 0.5f, "Ingredients");
            g.line(ingc, ingc.add(0, ingsz.y), 1);
            //Draw Fep/Hunger
            FastText.aprint(g, fepperhunc.add(offset).add(0, statsz.y / 2), 0, 0.5f, "Fep/Hunger");
            g.line(fepperhunc, fepperhunc.add(0, fepperhunsz.y), 1);
            //Draw Total FEP
            FastText.aprint(g, tfepc.add(offset).add(0, statsz.y / 2), 0, 0.5f,"Total Fep");
            g.line(tfepc, tfepc.add(0, tfepsz.y), 1);
            //Draw Hunger
            FastText.aprint(g, hunc.add(offset).add(0, statsz.y / 2), 0, 0.5f, "Hunger");
            g.line(hunc, hunc.add(0, hunsz.y), 1);
            //Draw Energy
            FastText.aprint(g, energyc.add(offset).add(0, statsz.y / 2), 0, 0.5f, "Energy");
            g.line(energyc, energyc.add(0, energysz.y), 1);
            //Draw main borders
            g.rect(Coord.z, g.sz());
        }

        private void sortByAttr(final FoodData.FepType type) {
            if(sortmethod instanceof AttrSort) {
                final AttrSort sort = (AttrSort) sortmethod;
                if(sort.attr() == type)
                    sortmethod = sort.reverse();
                else {
                    sortmethod = new AttrSort(SortMethod.Attr, Direction.DESC, type);
                }
            } else {
                sortmethod = new AttrSort(SortMethod.Attr, Direction.DESC, type);
            }
            foodwnd.lst.apply(sortmethod);
        }

        private void sortByField(final SortMethod method, final Supplier supplier) {
            if (sortmethod.method() == method) {
                sortmethod = sortmethod.reverse();
            } else {
                sortmethod = new FieldSort(method, Direction.ASC, supplier);
            }
            foodwnd.lst.apply(sortmethod);
        }

        @Override
        public boolean mousedown(Coord c, int button) {
            if(c.isect(iconc, iconnamesz)) {
                sortByField(SortMethod.Name, (o1, o2) -> o1.itemName.compareTo(o2.itemName));
                return true;
            } else if(c.isect(strc, statsz)) {
                sortByAttr(STR);
                return true;
            } else if(c.isect(agic, statsz)) {
                sortByAttr(AGI);
                return true;
            } else if(c.isect(intc, statsz)) {
                sortByAttr(INT);
                return true;
            } else if(c.isect(conc, statsz)) {
                sortByAttr(CON);
                return true;
            } else if(c.isect(perc, statsz)) {
                sortByAttr(PER);
                return true;
            } else if(c.isect(chac, statsz)) {
                sortByAttr(CHA);
                return true;
            } else if(c.isect(dexc, statsz)) {
                sortByAttr(DEX);
                return true;
            } else if(c.isect(wilc, statsz)) {
                sortByAttr(WIL);
                return true;
            } else if(c.isect(psyc, statsz)) {
                sortByAttr(PSY);
                return true;
            } else if(c.isect(fepperhunc, fepperhunsz)) {
                sortByField(SortMethod.FepPerHunger, (o1, o2) -> Float.compare(o1.fepPerHunger(), o2.fepPerHunger()));
                return true;
            } else if(c.isect(tfepc, tfepsz)) {
                sortByField(SortMethod.TotalFep, (o1, o2) -> Float.compare(o1.totalFep, o2.totalFep));
                return true;
            } else if(c.isect(hunc, hunsz)) {
                sortByField(SortMethod.Hunger, (o1, o2) -> Float.compare(o1.hunger, o2.hunger));
                return true;
            } else if(c.isect(energyc, energysz)) {
                sortByField(SortMethod.Energy, (o1, o2) -> Integer.compare(o1.energy, o2.energy));
                return true;
            }

            return super.mousedown(c, button);
        }
    }

    private class SearchBar extends LinearGrouping {
        final TextEntry search;
        public SearchBar(final int width) {
            super(UI.scale(5), false, Direction.HORIZONTAL);
            search = new TextEntry(UI.scale(200), "", FoodSearchWnd.this::refilter, FoodSearchWnd.this::refilter);
            add(new Button("Clear", () -> search.settext("")));
            add(new Button("Make Expression", () -> ui.gui.add(new FoodExpressionCreator(this::refilterExt), ui.mc)));
            add(search);
            pack();
            if(search.c.x + search.sz.x < width)
                search.resize(width - search.c.x);
            pack();
        }

        private void refilterExt(final String filter) {
            search.settext(filter);
        }
    }

    private final FoodList lst;
    private final Header header;

    public FoodSearchWnd() {
        super(Coord.z, "Food Searcher", "Food Searcher");
        final var cont = new LinearGrouping(UI.scale(0), false);
        lst = new FoodList();
        cont.add(new SearchBar(lst.sz.x));
        header = cont.add(new Header(this));
        cont.add(lst);
        cont.pack();
        add(cont);
        pack();
        refilter("");
    }

    private void refilter(final List<Filter> filters) {
        final Filter flt = new AndFilter(filters);
        lst.apply(flt, header.sortmethod);
    }

    private void refilter(final String filters) {
        final List<Filter> flts = new ArrayList<>();
        flts.add(new FieldFilter(itm -> itm.totalFep > 0));

        for(final String filter : filters.split(";")) {
            if(filter.contains(":") && !filter.endsWith(":")) {
                final var exp = filter.split(":");
                switch (exp[0]) {
                    case "-name" -> flts.add(new NameFilter(exp[1], NameFilter.Op.Exclude));
                    case "name" -> flts.add(new NameFilter(exp[1], NameFilter.Op.Include));
                    case "-from" -> flts.add(new IngredientFilter(exp[1], IngredientFilter.Op.Exclude));
                    case "from" -> flts.add(new IngredientFilter(exp[1], IngredientFilter.Op.Include));
                }
            } else {
                final var exp = filter.split("((<=)|(>=)|=|<|>)");
                if(exp.length == 2 && exp[1].matches("^[0-9]+(\\.[0-9]+)?$")) {
                    switch (exp[0]) {
                        case "str", "agi", "int", "con", "per", "cha", "dex", "wil", "psy",
                                "str2", "agi2", "int2", "con2", "per2", "cha2", "dex2", "wil2", "psy2" -> {
                            final AttrFilter.Op op;
                            if(filter.contains("<=")) op = AttrFilter.Op.LessThanOrEqual;
                            else if(filter.contains(">=")) op = AttrFilter.Op.GreaterThanOrEqual;
                            else if(filter.contains("=")) op = AttrFilter.Op.Equal;
                            else if(filter.contains("<")) op = AttrFilter.Op.Less;
                            else op = AttrFilter.Op.Greater;

                            flts.add(new AttrFilter(FoodData.feptypemap.get(exp[0]), op, Float.parseFloat(exp[1])));
                        }
                    }
                }
            }
        }
        refilter(flts);
    }

    @Override
    public void close() {
        hide();
    }
}

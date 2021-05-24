package hamster.ui.search;

import hamster.ui.search.filters.InclusionOp;
import hamster.ui.search.filters.InputFilter;
import hamster.ui.search.filters.NameFilter;
import hamster.ui.search.filters.TypeFilter;
import hamster.ui.search.filters.curio.LPFilter;
import hamster.ui.search.filters.curio.LPHFilter;
import hamster.ui.search.filters.curio.MentalWeightFilter;
import hamster.ui.search.filters.curio.StudyTimeFilter;
import hamster.ui.search.filters.food.AttrFilter;
import hamster.ui.search.filters.food.EnergyFilter;
import hamster.ui.search.filters.food.HungerFilter;
import hamster.ui.search.filters.gear.*;
import hamster.ui.search.filters.gildable.ChanceFilter;
import hamster.ui.search.filters.gildable.GildAttrFilter;
import hamster.ui.search.filters.gilding.AttrModFilter;
import hamster.ui.search.filters.symbol.BonusFilter;
import hamster.ui.search.filters.symbol.ReductionFilter;
import hamster.util.JobSystem;
import hamster.util.ObservableListener;
import haven.*;

import java.awt.event.KeyEvent;
import java.util.*;

public class MakeWnd extends Window implements ObservableListener<MenuGrid.Pagina> {
    public static final int WIDTH = 200;
    private final TextEntry entry;
    private final ActList list;

    public MakeWnd() {
	super(Coord.z, "Crafting", "Crafting");
	setcanfocus(true);
	setfocusctl(true);
	entry = add(new TextEntry(WIDTH, "") {
	    public void activate(String text) {
		if (list.sel != null) {
		    act(list.sel.pagina);
		}
	    }

	    protected void changed() {
		super.changed();
		refilter();
	    }

	    public boolean keydown(KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_UP) {
		    final Optional<Integer> idx = list.selindex();
		    if (idx.isPresent()) {
			list.change(Math.max(idx.get() - 1, 0));
		    } else {
			list.change(0);
		    }
		    return true;
		} else if (e.getKeyCode() == KeyEvent.VK_DOWN) {
		    final Optional<Integer> idx = list.selindex();
		    if (idx.isPresent()) {
			list.change(Math.min(idx.get() + 1, list.listitems() - 1));
		    } else {
			list.change(0);
		    }
		    return true;
		} else {
		    return super.keydown(e);
		}
	    }
	});
	setfocus(entry);
	list = add(new ActList(WIDTH, 15) {
	    protected void itemclick(ActItem item, int button) {
		if (sel == item) {
		    act(list.sel.pagina);
		} else {
		    super.itemclick(item, button);
		}
	    }
	}, 0, entry.sz.y + 5);
	list.add(NameFilter.pattern, NameFilter::make);
	list.add(InputFilter.pattern, InputFilter::make);
	list.add(TypeFilter.pattern, TypeFilter::make);
	list.add(LPFilter.pattern, LPFilter::make);
	list.add(LPHFilter.pattern, LPHFilter::make);
	list.add(MentalWeightFilter.pattern, MentalWeightFilter::make);
	list.add(StudyTimeFilter.pattern, StudyTimeFilter::make);
	list.add(AttrFilter.pattern, AttrFilter::make);
	list.add(EnergyFilter.pattern, EnergyFilter::make);
	list.add(HungerFilter.pattern, HungerFilter::make);
	list.add(ArmorPenFilter.pattern, ArmorPenFilter::make);
	list.add(AttackWeightFilter.pattern, AttackWeightFilter::make);
	list.add(DmgFilter.pattern, DmgFilter::make);
	list.add(GrievousDmgFilter.pattern, GrievousDmgFilter::make);
	list.add(HardArmorFilter.pattern, HardArmorFilter::make);
	list.add(SlotFilter.pattern, SlotFilter::make);
	list.add(SoftArmorFilter.pattern, SoftArmorFilter::make);
	list.add(StatFilter.pattern, StatFilter::make);
	list.add(GildAttrFilter.pattern, GildAttrFilter::make);
	list.add(ChanceFilter.pattern, ChanceFilter::make);
	list.add(AttrModFilter.pattern, AttrModFilter::make);
	list.add(BonusFilter.pattern, BonusFilter::make);
	list.add(ReductionFilter.pattern, ReductionFilter::make);
	pack();
	hide();
    }

    private void refilter() {
	list.filter(entry.text.toLowerCase());
    }

    public void act(MenuGrid.Pagina act) {
	if (ui.gui != null) {
	    ui.gui.menu.use(act.button(), new MenuGrid.Interaction(1, ui.modflags()), false);
	}
    }

    @Override
    public void init(Collection<MenuGrid.Pagina> base) {
	for (final MenuGrid.Pagina pag : base) {
	    JobSystem.submit(() -> {
		try {
		    if(pag.res().name.startsWith("paginae/craft/"))
		    	list.add(pag);
		} catch (Loading e) {
		    throw new JobSystem.DependencyNotMet();
		}
	    });
	}
    }

    @Override
    public void added(final MenuGrid.Pagina item) {
	JobSystem.submit(() -> {
	    try {
		if(item.res().name.startsWith("paginae/craft/"))
		    list.add(item);
	    } catch (Loading e) {
		throw new JobSystem.DependencyNotMet();
	    }
	});
    }

    @Override
    public void remove(MenuGrid.Pagina item) {
	list.remove(item);
    }

    @Override
    protected void added() {
	super.added();
	ui.gui.menu.paginae.addListener(this);
    }

    @Override
    protected void removed() {
	ui.gui.menu.paginae.removeListener(this);
    }

    @Override
    public void close() {
	hide();
	for (Widget wdg = lchild; wdg != null; wdg = wdg.prev) {
	    if (wdg instanceof Makewindow) {
		wdg.wdgmsg("close");
		break;
	    }
	}
    }
}

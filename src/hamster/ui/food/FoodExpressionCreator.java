package hamster.ui.food;

import hamster.data.food.FoodData;
import hamster.ui.core.NumberEntry;
import hamster.ui.core.layout.LinearGrouping;
import hamster.ui.food.filters.AttrFilter;
import hamster.ui.food.filters.Filter;
import hamster.ui.food.filters.IngredientFilter;
import hamster.ui.food.filters.NameFilter;
import haven.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

public class FoodExpressionCreator extends Window {
    private static final Map<String, AttrFilter.Op> attropmap = new HashMap<>();
    static {
        //"Greater", "Greater Than Or Equal", "Less", "Less Than Or Equal", "Equal"
        attropmap.put("Greater", AttrFilter.Op.Greater);
	attropmap.put("Greater Than Or Equal", AttrFilter.Op.GreaterThanOrEqual);
	attropmap.put("Less", AttrFilter.Op.Less);
	attropmap.put("Less Than Or Equal", AttrFilter.Op.LessThanOrEqual);
	attropmap.put("Equal", AttrFilter.Op.Equal);
    }

    private final List<Filter> filters = new ArrayList<>();
    private final Consumer<String> setFilter;

    private final LinearGrouping expcont, namecont, attrcont;
    private final TextEntry nameinput; //For name / ingredients
    private final Dropbox<String> filterinput; // For filter type
    private final Dropbox<String> nameopinput; // For name / ingredients selection
    private final Dropbox<String> attrinput; // For attribute selection
    private final Dropbox<String> attropinput; // For attribute selection
    private final NumberEntry attrvalue; // For attribute selection

    public FoodExpressionCreator(final Consumer<String> setFilter) {
        super(Coord.z, "Food Expression Creator" ,"Food Expression Creator");
        this.setFilter = setFilter;
        final var spacer = UI.scale(5);
   	final var cont = add(new LinearGrouping(spacer, false, LinearGrouping.Direction.HORIZONTAL));
	{
	    final var lstcont = cont.add(new LinearGrouping("Filter List", Text.std12, spacer));
	    final var filterlst = lstcont.add(new Listbox<Filter>(UI.scale(200), 15, UI.scale(20)) {
		@Override
		protected Filter listitem(int i) {
		    return filters.get(i);
		}

		@Override
		protected int listitems() {
		    return filters.size();
		}

		@Override
		protected void drawitem(GOut g, Filter item, int i) {
		    item.render(g);
		}
	    });
	    lstcont.add(new Button(UI.scale(200), "Remove", () -> {
		if(filterlst.sel != null) {
		    filters.remove(filterlst.sel);
		}
	    }));
	    lstcont.add(new Button(UI.scale(200), "Clear", () -> filters.removeIf(itm -> true)));
	    lstcont.add(new Button(UI.scale(200), "Finish", () -> {
		final StringBuilder sb = new StringBuilder();
		for(final var filter : filters) {
		    if(sb.length() > 0)
			sb.append(';');
		    sb.append(filter);
		}
		setFilter.accept(sb.toString());
	    }));
	    lstcont.pack();
	}
	{
	    expcont = cont.add(new LinearGrouping("Expression Select", Text.std12, spacer));
	    final var itms = new String[]{"Name Filter", "Ingredient Filter", "Attribute Filter"};
	    filterinput = expcont.add(new Dropbox<String>(UI.scale(150), 3, UI.scale(20)) {
		{ sel = itms[0]; }
		@Override
		protected String listitem(int i) {
		    return itms[i];
		}

		@Override
		protected int listitems() {
		    return itms.length;
		}

		@Override
		protected void drawitem(GOut g, String item, int i) {
		    FastText.aprint(g, g.sz().div(2), 0.5, 0.5, item);
		}

		@Override
		public void change(String item) {
		    super.change(item);
		    switch (item) {
		        case "Name Filter", "Ingredient Filter" -> {
		            namecont.show();
		            attrcont.hide();
			    expcont.pack();
			}
			default -> {
			    namecont.hide();
			    attrcont.show();
			    expcont.pack();
			}
		    }
		    nameopinput.sel = null;
		    nameinput.settext("");
		    attrinput.sel = null;
		    attropinput.sel = null;
		}
	    });
	    namecont = expcont.add(new LinearGrouping(spacer, false));
	    {
	        namecont.add(new Label("Operation"));
		final var nameops = new String[]{"Include", "Exclude"};
		nameopinput = namecont.add(new Dropbox<String>(UI.scale(150), 2, UI.scale(20)) {
		    @Override
		    protected String listitem(int i) {
			return nameops[i];
		    }

		    @Override
		    protected int listitems() {
			return nameops.length;
		    }

		    @Override
		    protected void drawitem(GOut g, String item, int i) {
			FastText.aprint(g, g.sz().div(2), 0.5, 0.5, item);
		    }
		});
		namecont.add(new Label("Filter String"));
		nameinput = namecont.add(new TextEntry(UI.scale(150), ""));
		namecont.pack();
	    }
	    attrcont = expcont.add(new LinearGrouping(spacer, false));
	    {
		final var attrs = new String[]{
		    "Strength +1", "Agility +1", "Intelligence +1", "Constitution +1", "Charisma +1", "Dexterity +1", "Will +1", "Psyche +1",
		    "Strength +2", "Agility +2", "Intelligence +2", "Constitution +2", "Charisma +2", "Dexterity +2", "Will +2", "Psyche +2",
		};
		attrcont.add(new Label("Attribute"));
		attrinput = attrcont.add(new Dropbox<String>(UI.scale(150), 10, UI.scale(20)) {
		    @Override
		    protected String listitem(int i) {
			return attrs[i];
		    }

		    @Override
		    protected int listitems() {
			return attrs.length;
		    }

		    @Override
		    protected void drawitem(GOut g, String item, int i) {
			FastText.aprint(g, g.sz().div(2), 0.5, 0.5, item);
		    }
		});
		attrcont.add(new Label("Attribute Operation"));
		final var attrops = new String[]{"Greater", "Greater Than Or Equal", "Less", "Less Than Or Equal", "Equal"};
		attropinput = attrcont.add(new Dropbox<String>(UI.scale(150), 5, UI.scale(20)) {
		    @Override
		    protected String listitem(int i) {
			return attrops[i];
		    }

		    @Override
		    protected int listitems() {
			return attrops.length;
		    }

		    @Override
		    protected void drawitem(GOut g, String item, int i) {
			FastText.aprint(g, g.sz().div(2), 0.5, 0.5, item);
		    }
		});
		attrcont.add(new Label("Attribute Value"));
		attrvalue = attrcont.add(new NumberEntry(UI.scale(150), 0, 0, 200));
	        attrcont.pack();
	        attrcont.hide();
	    }
	    expcont.add(new Button(UI.scale(150), "Add Filter", this::addFilter));
	    expcont.pack();
	}
	cont.pack();
        pack();
    }

    private void addFilter() {
        if(filterinput.sel != null) {
            switch (filterinput.sel) {
                case "Name Filter" -> {
                    if(nameopinput.sel != null) {
                        filters.add(new NameFilter(nameinput.text, NameFilter.Op.valueOf(nameopinput.sel)));
		    }
		}
		case "Ingredient Filter" -> {
		    if(nameopinput.sel != null) {
			filters.add(new IngredientFilter(nameinput.text, IngredientFilter.Op.valueOf(nameopinput.sel)));
		    }
		}
		case "Attribute Filter" -> {
                    if(attropinput.sel != null && attrinput.sel != null) {
                        filters.add(new AttrFilter(FoodData.fepmap.get(attrinput.sel), attropmap.get(attropinput.sel), attrvalue.value()));
		    }
		}
	    }
	}
    }

    @Override
    public void close() {
	ui.destroy(this);
    }
}

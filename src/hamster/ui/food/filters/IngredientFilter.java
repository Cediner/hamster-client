package hamster.ui.food.filters;

import hamster.data.food.FoodData;
import haven.FastText;
import haven.GOut;

public class IngredientFilter implements Filter {
    public enum Op {
	Include, Exclude
    }

    private final String ingredient;
    private final Op op;

    public IngredientFilter(final String ingredient, final Op op) {
	this.ingredient = ingredient.toLowerCase();
	this.op = op;
    }

    @Override
    public boolean included(FoodData item) {
	if(op == Op.Include) {
	    for(final var ing : item.ingredients) {
		if(ing.name.toLowerCase().contains(ingredient))
		    return true;
	    }
	    return false;
	} else { //Exclude
	    for(final var ing : item.ingredients) {
		if(ing.name.toLowerCase().contains(ingredient))
		    return false;
	    }
	    return true;
	}
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "%s %s", op, ingredient);
    }

    @Override
    public String toString() {
	return String.format("%sfrom:%s", op  == Op.Include ? "" : "-", ingredient);
    }
}

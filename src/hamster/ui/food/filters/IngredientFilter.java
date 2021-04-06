package hamster.ui.food.filters;

import hamster.data.food.FoodData;

public class IngredientFilter implements Filter {
    public enum Op {
	INCLUDE, EXCLUDE
    }

    private final String ingredient;
    private final Op op;

    public IngredientFilter(final String ingredient, final Op op) {
	this.ingredient = ingredient.toLowerCase();
	this.op = op;
    }

    @Override
    public boolean included(FoodData item) {
	if(op == Op.INCLUDE) {
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
}

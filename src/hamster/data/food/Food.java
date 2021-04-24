package hamster.data.food;

import haven.*;

import java.util.List;
import java.util.Optional;

public class Food implements Disposable {
    public String itemName;
    public String resourceName;
    public int energy;
    public float hunger;
    public float totalFep;
    public List<Ingredient> ingredients;
    public List<Fep> feps;

    private Text ingredientsText = null;
    private Defer.Future<Text> dIngText = null;

    public float fepPerHunger() {
	return totalFep / hunger;
    }

    public Fep getFep(final FepType type) {
	for(final var fep : feps) {
	    if(fep.type == type)
		return fep;
	}
	return null;
    }

    public Optional<Tex> ingredientsText() {
	FoodData.textcache.access(this);
	if(ingredientsText != null)
	    return Optional.of(ingredientsText.tex());
	if(dIngText != null) {
	    if(dIngText.done()) {
		ingredientsText = dIngText.get();
		dIngText = null;
		return Optional.of(ingredientsText.tex());
	    }
	} else {
	    dIngText = Defer.later(() -> {
		final StringBuilder sb = new StringBuilder();
		for (final var ing : ingredients) {
		    if (sb.length() > 0)
			sb.append(", ");
		    sb.append(ing.name);
		    sb.append(": ");
		    sb.append(ing.percentage);
		    sb.append('%');
		}
		return RichText.render(sb.toString(), UI.scale(300));
	    });
	}
	return Optional.empty();
    }

    @Override
    public void dispose() {
	if(ingredientsText != null) {
	    ingredientsText.tex().dispose();
	}
	if(dIngText != null) {
	    if (dIngText.done())
		dIngText.get().tex().dispose();
	    else {
		dIngText.cancel();
	    }
	}
	ingredientsText = null;
	dIngText = null;
    }
}

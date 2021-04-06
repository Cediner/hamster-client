package hamster.ui.food.filters;

import hamster.data.food.FoodData;

public class AttrFilter implements Filter {
    public enum Op {
        LESS, LESSOREQUAL, EQUAL, GREATEROREQUAL, GREATER
    }

    private final FoodData.FepType feptype;
    private final Op op;
    private final float value;

    public AttrFilter(final FoodData.FepType fep, final Op op, final float value) {
        this.feptype = fep;
        this.op = op;
        this.value = value;
    }

    @Override
    public boolean included(FoodData item) {
        for(final var fep : item.feps) {
            if(fep.type == feptype) {
                switch (op) {
		    case LESS -> {
			return fep.value < value;
		    }
		    case LESSOREQUAL -> {
			return fep.value <= value;
		    }
		    case EQUAL -> {
			return fep.value == value;
		    }
		    case GREATEROREQUAL -> {
			return fep.value >= value;
		    }
		    case GREATER -> {
			return fep.value > value;
		    }
		}
	    }
	}
        return false;
    }
}

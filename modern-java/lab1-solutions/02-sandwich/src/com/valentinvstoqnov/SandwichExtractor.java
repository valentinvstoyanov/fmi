package com.valentinvstoqnov;

import java.util.Arrays;

public class SandwichExtractor {
    private static final String INITIAL_INGREDIENT = "bread";
    private static final char INGREDIENT_DELIMITER = '-';
    private static final String DISLIKED_INGREDIENT = "olives";

    private static boolean isDislikedIngredient(String ingredient) {
        return DISLIKED_INGREDIENT.equals(ingredient);
    }

    private static String[] addIngredient(String[] ingredients, String ingredient) {
        if (isDislikedIngredient(ingredient)) {
            return ingredients;
        }

        if (ingredients[ingredients.length - 1] != null) {
            ingredients = Arrays.copyOf(ingredients, ingredients.length + 1);
        }

        int freeIdx = ingredients.length - 1;
        while (freeIdx >= 0) {
            if (ingredients[freeIdx] != null) {
                ++freeIdx;
                break;
            }
            --freeIdx;
        }
        freeIdx = Math.max(freeIdx, 0);

        ingredients[freeIdx] = ingredient;
        return ingredients;
    }

    public static String[] extractIngredients(String sandwich) {
        final int firstBreadIdx = sandwich.indexOf(INITIAL_INGREDIENT);
        final int secondBreadIdx = sandwich.lastIndexOf(INITIAL_INGREDIENT);

        if (firstBreadIdx == secondBreadIdx) {
            return new String[0];
        }

        String[] ingredients = new String[1];
        int currentIngredientIdx = firstBreadIdx + INITIAL_INGREDIENT.length();
        while (currentIngredientIdx < secondBreadIdx) {
            final int delimIdx = sandwich.indexOf(INGREDIENT_DELIMITER, currentIngredientIdx + 1);
            final int endIngredientIdx = delimIdx < 0 ? secondBreadIdx : delimIdx;
            final String ingredient = sandwich.substring(currentIngredientIdx, endIngredientIdx);
            ingredients = addIngredient(ingredients, ingredient);
            currentIngredientIdx = endIngredientIdx + 1;
        }

        Arrays.sort(ingredients);
        return ingredients;
    }
}
